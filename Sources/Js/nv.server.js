export class TServer {
    constructor(port) {
        this.FPort = port || 80;
        if (typeof TNvLocalhandler != "undefined")
            this.FWebSocket = new TLocalHandler();
        else
            this.FWebSocket = new TWebSocket('ws://' + document.location.host);
        this.FWebSocket.connect();
        this.FWebSocket.onmessage = function (message) {
            App.Messages.QueueMessage(JSON.parse(message));
            //App.ParseJson(JSON.parse(message));
        }
    }

    SendChanges() {
        if (this.FWebSocket.wsState == WsStateConnected) {
            this.FWebSocket.postmessage($.stringifySafe(App.FChanges));
            App.FChanges = {};
        }
        else {
            $.ajax({
                url: document.location.href + (App.FChanges != {}) ? "?callback=1" : "",
                type: "POST",
                data: JSON.stringify(App.FChanges),
                dataType: "json",
                beforeSend: this._BeforeSubmit,
                timeout: 600000,
                statusCode: {
                    401: function (xhr) {
                        window.location = xhr.getResponseHeader("Location");
                    }
                }
            })
                .done(this._DoneSubmit)
                .fail(this._FailSubmit)
                .always(this._AlwaysSubmit);
        }
    }

    _BeforeSubmit(data) {
        // $("#blockUI").removeClass("hidden");
    }

    //process data received in callback
    _DoneSubmit(data, xhr) {
        App.FChanges = {};
        App.Messages.QueueMessage(data);
        //App.ParseJson(data);
    }

    _FailSubmit(xhr) {
        console.log('FAil post. Data received: ' + xhr);
    }

    _AlwaysSubmit(data) {
        //    window.inCallBack = false;
        //   processCallbackQueue();
        //   $("#blockUI").addClass("hidden");
    }

}

const WsStateDisconnected = 0;
const WsStateDisconnecting = 1;
const WsStateConnected = 2;
const WsStateConnecting = 3;

export class TWebSocket {
    constructor(url) {
        this.wsState = WsStateDisconnected;
        this.timer = null;
        this.url = url;
        this.ws = null;
        this.isreconnect = false;
    }

    connect() {
        if (this.wsState === WsStateConnected) {
            this.disconnect();
        }

        if (this.wsState !== WsStateDisconnected) {
            // tlog('connection is busy')
            return
        }

        this.wsState = WsStateConnecting;
        this.ws = null;
        this.ws = new WebSocket(this.url);
        this.ws.binaryType = 'arraybuffer';

        this.ws.onmessage = function (e) {
            if (typeof e.data === 'string') {
                // TODO process string message
                console.log('string:', e.data)
                this.onmessage(e.data)
            } else {
                if (e.data.byteLength > 0) {
                    // TODO process arraybuffer message
                }
            }
        }.bind(this);

        this.ws.onclose = function (e) {
            //tlog(e);
            this.wsState = WsStateDisconnected;
            this.onclose();

            if (this.isreconnect) {
                if (typeof this.timer !== 'undefined' || this.timer !== null) {
                    clearInterval(this.timer);
                    this.timer = null;
                }

                this.timer = setInterval(function () {
                    if (this.isconnected() == false) {
                        this.connect();
                    }
                }.bind(this), 5000);
            }
        }.bind(this);

        this.ws.onerror = function (e) {
            // TODO
            this.disconnect();
        }.bind(this);

        this.ws.onopen = function (e) {
            this.wsState = WsStateConnected;
            if (this.wsState === WsStateConnected) {
                this.onopen();
            } else {
                // tlog('connection is closed or closing')
            }
        }.bind(this);
    }

    disconnect() {
        this.setreconnect(false);
        if (this.ws !== null) {
            if (this.wsState === WsStateConnected) {
                this.onclose();
                this.wsState = WsStateDisconnecting;
                this.ws.close(1000, 'doclose');
            } else {
                // tlog('connection is not complete');
            }
        } else {
            // tlog('WebSocket session is null');
        }
    }

    isconnected() {
        return this.wsState === WsStateConnected ? true : false;
    }

    setreconnect(ok) {
        if (ok) {
            this.isreconnect = true;
        } else {
            this.isreconnect = false;
            if (typeof this.timer !== 'undefined' || this.timer !== null) {
                clearInterval(this.timer);
                this.timer = null;
            }
        }
    }

    postmessage(message) {
        if (this.wsState === WsStateConnected) {
            this.ws.send(message);
        } else {
            console.log('connection is closed or closing')
        }
    }

    // virtual function
    onclose() {
        // TODO
    }

    // virtual function
    onopen() {
        // TODO
    }

    // message dispatch // virtual function
    onmessage(message) {
    }
}

// EMULATE WEBSOCKET FOR LOCAL SCREEN
export class TLocalHandler {
    constructor(url) {
        this.wsState = WsStateConnected;
        this.FLocalHandler = new TNvLocalhandler();
    }

    connect() {
        this.wsState = WsStateConnected;
    }

    disconnect() {
        this.wsState = WsStateConnected;
    }

    isconnected() {
        return this.wsState === WsStateConnected ? true : false;
    }

    setreconnect(ok) {

    }

    postmessage(message) {
        if (this.wsState === WsStateConnected) {
            this.FLocalHandler.SendData(message);
        } else {
            console.log('connection is closed or closing')
        }
    }

    // virtual function
    onclose() {
        // TODO
    }

    // virtual function
    onopen() {
        // TODO
    }

    // message dispatch // virtual function
    onmessage(message) {
    }

}
