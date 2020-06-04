const WsStateDisconnected = 0;
const WsStateDisconnecting = 1;
const WsStateConnected = 2;
const WsStateConnecting = 3;
class SimpleWSocket {
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
      tlog('connection is busy')
      return
    }

    this.wsState = WsStateConnecting;
    this.ws = null;
    this.ws = new WebSocket(this.url);
    this.ws.binaryType = 'arraybuffer';

    this.ws.onmessage = function (e) {
      if (typeof e.data === 'string') {
          // TODO process string message
          // console.log('string:', e.data)
      } else {
        if (e.data.byteLength > 0) {
            // TODO process arraybuffer message
        }
      }
    }.bind(this);

    this.ws.onclose = function (e) {
      tlog(e);
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
        tlog('connection is closed or closing')
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
        tlog('connection is not complete');
      }
    } else {
      tlog('WebSocket session is null');
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
      // TODO
  }
}