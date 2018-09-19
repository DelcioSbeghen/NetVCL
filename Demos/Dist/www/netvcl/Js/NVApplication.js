"use strict";
var __extends = (this && this.__extends) || (function() {
    var extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] }
            instanceof Array && function(d, b) { d.__proto__ = b; }) ||
        function(d, b) {
            for (var p in b)
                if (b.hasOwnProperty(p)) d[p] = b[p];
        };
    return function(d, b) {
        extendStatics(d, b);

        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
exports.__esModule = true;
//var VXO = require("./VXObject");
//var V = require("./VCL");
//var VXM = require("./VXMenu");
//var VXServer = require("./VXServer");
//var VXDS = require("./VXServer");
var TApplication = /** @class */ (function() {
    function TApplication() {
        this._freezeRouting = false;
        this.igonreaAthenticationPage = "";
        this.navbaritems = new TCollection();
        this.serverURL = "backEnd";
        this.___botstrapversion = -1;
        /**
         * specify the server side method for login
         **/
        this.loginServerClass = "Login";
        this._contentfluid = false;
        /**
         * enable a button on the bottom of the screen that scoll the screen to the top
         */
        this._showgototopicon = false;
        this._mainpage = "PageHome";
        this._sessionevents = 'mousemove keydown DOMMouseScroll mousewheel mousedown';
        this._loginpage = "PageLogin";
        this._decimalseparator = '.';
        this._thousandseparator = ",";
        this.newItemIntranslationTable = false;
        this.languageTable = new Object();
        this.translationTable = new Object();
        this._enableapplicationcache = true;
        this._applicationtitle = "VCL.JS Application";
        this.MB = "MB";
        this.GB = "GB";
        this.LocaleSettings = new TLocaleSettings();
        // Internationalization strings
        this.dateFormat_i18n = {
            dayNames: [
                "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat",
                "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
            ],
            monthNames: [
                "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"
            ]
        };
    }
    TApplication.prototype.initialize = function() {
        var self = this;
        $('head').append('<style type = "text/css" > .no-space [class*="span"] {    margin-left: 0;}</style>');
        this.sammy = Sammy(function() {
            this.use("Session");
            this.use("Local");
            this.get('#/', function() {
                self.navigateToPage(this.buildPageURL(self.MainPage));
            });
            this.get('#show/:class/:params', function() {
                if (self._freezeRouting)
                    return;
                var className = self.hexToString(this.params["class"]);
                var args = [];
                if (self.AuthenticationRequired && !self.Authenticated &&
                    className.toUpperCase() != self.LoginPage.toUpperCase() &&
                    className.toUpperCase() != self.igonreaAthenticationPage.toUpperCase()) {
                    self.navigateToPage(self.LoginPage, ["#show/" + this.params["class"] + '/' + this.params["params"]]);
                } else {
                    var className = self.hexToString(this.params["class"]);
                    var paramArgStr = self.hexToString(this.params["params"]);
                    var paramArg = JSON.parse(paramArgStr, jsonCreateDateParserTemp);
                    $.each(paramArg, function(index, item) {
                        args.push(item);
                    });
                    if (self.OnPageLoad != null) {
                        var rc = self.OnPageLoad(className, args);
                        if (!rc)
                            return true;
                    }
                    self.loadPage(className, args, function(page) {
                        page.show(); //will show on contenet - main container
                    });
                }
            });
            this.get('#:params', function() {
                if (self.onNavigateToPage)
                    self.onNavigateToPage(this.params);
            });
        });
    };
    TApplication.prototype.hexColorToRGB = function(color, opacity) {
        if (opacity === void 0) { opacity = 1; }
        if (exports.Application.checkColorString(color)) {
            var hex = color.replace('#', '');
            var r = parseInt(hex.substring(0, 2), 16);
            var g = parseInt(hex.substring(2, 4), 16);
            var b = parseInt(hex.substring(4, 6), 16);
            if (opacity > 1)
                opacity = opacity / 100;
            return 'rgba(' + r + ',' + g + ',' + b + ',' + opacity + ')';
        }
    };
    /**
    convert color names to hex codes
    */
    TApplication.prototype.colourNameToHex = function(colour) {
        var colours = {
            "aliceblue": "#f0f8ff",
            "antiquewhite": "#faebd7",
            "aqua": "#00ffff",
            "aquamarine": "#7fffd4",
            "azure": "#f0ffff",
            "beige": "#f5f5dc",
            "bisque": "#ffe4c4",
            "black": "#000000",
            "blanchedalmond": "#ffebcd",
            "blue": "#0000ff",
            "blueviolet": "#8a2be2",
            "brown": "#a52a2a",
            "burlywood": "#deb887",
            "cadetblue": "#5f9ea0",
            "chartreuse": "#7fff00",
            "chocolate": "#d2691e",
            "coral": "#ff7f50",
            "cornflowerblue": "#6495ed",
            "cornsilk": "#fff8dc",
            "crimson": "#dc143c",
            "cyan": "#00ffff",
            "darkblue": "#00008b",
            "darkcyan": "#008b8b",
            "darkgoldenrod": "#b8860b",
            "darkgray": "#a9a9a9",
            "darkgreen": "#006400",
            "darkkhaki": "#bdb76b",
            "darkmagenta": "#8b008b",
            "darkolivegreen": "#556b2f",
            "darkorange": "#ff8c00",
            "darkorchid": "#9932cc",
            "darkred": "#8b0000",
            "darksalmon": "#e9967a",
            "darkseagreen": "#8fbc8f",
            "darkslateblue": "#483d8b",
            "darkslategray": "#2f4f4f",
            "darkturquoise": "#00ced1",
            "darkviolet": "#9400d3",
            "deeppink": "#ff1493",
            "deepskyblue": "#00bfff",
            "dimgray": "#696969",
            "dodgerblue": "#1e90ff",
            "firebrick": "#b22222",
            "floralwhite": "#fffaf0",
            "forestgreen": "#228b22",
            "fuchsia": "#ff00ff",
            "gainsboro": "#dcdcdc",
            "ghostwhite": "#f8f8ff",
            "gold": "#ffd700",
            "goldenrod": "#daa520",
            "gray": "#808080",
            "green": "#008000",
            "greenyellow": "#adff2f",
            "honeydew": "#f0fff0",
            "hotpink": "#ff69b4",
            "indianred ": "#cd5c5c",
            "indigo": "#4b0082",
            "ivory": "#fffff0",
            "khaki": "#f0e68c",
            "lavender": "#e6e6fa",
            "lavenderblush": "#fff0f5",
            "lawngreen": "#7cfc00",
            "lemonchiffon": "#fffacd",
            "lightblue": "#add8e6",
            "lightcoral": "#f08080",
            "lightcyan": "#e0ffff",
            "lightgoldenrodyellow": "#fafad2",
            "lightgrey": "#d3d3d3",
            "lightgreen": "#90ee90",
            "lightpink": "#ffb6c1",
            "lightsalmon": "#ffa07a",
            "lightseagreen": "#20b2aa",
            "lightskyblue": "#87cefa",
            "lightslategray": "#778899",
            "lightsteelblue": "#b0c4de",
            "lightyellow": "#ffffe0",
            "lime": "#00ff00",
            "limegreen": "#32cd32",
            "linen": "#faf0e6",
            "magenta": "#ff00ff",
            "maroon": "#800000",
            "mediumaquamarine": "#66cdaa",
            "mediumblue": "#0000cd",
            "mediumorchid": "#ba55d3",
            "mediumpurple": "#9370d8",
            "mediumseagreen": "#3cb371",
            "mediumslateblue": "#7b68ee",
            "mediumspringgreen": "#00fa9a",
            "mediumturquoise": "#48d1cc",
            "mediumvioletred": "#c71585",
            "midnightblue": "#191970",
            "mintcream": "#f5fffa",
            "mistyrose": "#ffe4e1",
            "moccasin": "#ffe4b5",
            "navajowhite": "#ffdead",
            "navy": "#000080",
            "oldlace": "#fdf5e6",
            "olive": "#808000",
            "olivedrab": "#6b8e23",
            "orange": "#ffa500",
            "orangered": "#ff4500",
            "orchid": "#da70d6",
            "palegoldenrod": "#eee8aa",
            "palegreen": "#98fb98",
            "paleturquoise": "#afeeee",
            "palevioletred": "#d87093",
            "papayawhip": "#ffefd5",
            "peachpuff": "#ffdab9",
            "peru": "#cd853f",
            "pink": "#ffc0cb",
            "plum": "#dda0dd",
            "powderblue": "#b0e0e6",
            "purple": "#800080",
            "red": "#ff0000",
            "rosybrown": "#bc8f8f",
            "royalblue": "#4169e1",
            "saddlebrown": "#8b4513",
            "salmon": "#fa8072",
            "sandybrown": "#f4a460",
            "seagreen": "#2e8b57",
            "seashell": "#fff5ee",
            "sienna": "#a0522d",
            "silver": "#c0c0c0",
            "skyblue": "#87ceeb",
            "slateblue": "#6a5acd",
            "slategray": "#708090",
            "snow": "#fffafa",
            "springgreen": "#00ff7f",
            "steelblue": "#4682b4",
            "tan": "#d2b48c",
            "teal": "#008080",
            "thistle": "#d8bfd8",
            "tomato": "#ff6347",
            "turquoise": "#40e0d0",
            "violet": "#ee82ee",
            "wheat": "#f5deb3",
            "white": "#ffffff",
            "whitesmoke": "#f5f5f5",
            "yellow": "#ffff00",
            "yellowgreen": "#9acd32"
        };
        if (typeof colours[colour.toLowerCase()] != 'undefined')
            return colours[colour.toLowerCase()];
        return null;
    };
    TApplication.prototype.createPageInstance = function(prototype, html, __args) {
        var instance = Object.create(prototype);
        instance.__HTML__ = html;
        instance.constructor.apply(instance, __args);
        return instance;
    };
    TApplication.prototype.loadJSLibraries = function(modulesName, callBack) {
        require(modulesName, function(modl) {
            if (callBack)
                callBack();
        });
    };
    TApplication.prototype.getBootstrapVersion = function() {
        if (this.___botstrapversion > -1)
            return this.___botstrapversion;
        this.___botstrapversion = 0;
        if ((typeof($().emulateTransitionEnd) == 'function'))
            this.___botstrapversion = 3;
        else if ((typeof $().modal == 'function'))
            this.___botstrapversion = 2;
        return this.___botstrapversion;
    };
    TApplication.prototype.loadJSLibrary = function(moduleName, callBack) {
        require([moduleName], function(modl) {
            if (callBack)
                callBack(modl);
        });
    };
    /**
     * create and load page & html file asynchronously  an return an object instance in a callback function
     **/
    TApplication.prototype.loadPage = function(classPathName, __args, __callback) {
        var self = this;
        var classes = [];
        classPathName = classPathName.replace('\\', '/');
        var className = classPathName.split('/')[classPathName.split('/').length - 1];
        classes.push(classPathName);
        classes.push('VCL/Scripts/text.js!' + classPathName + '.html');
        require(classes, function(page, html) {
            var classExists = true;
            try {
                typeof(page[className].prototype);
            } catch (err) {
                exports.Application.raiseException("Class '" + className + "' was not found in module '" + classPathName + ".ts'");
                classExists = false;
            }
            if (classExists) {
                var instance = self.createPageInstance(page[className].prototype, html, __args);
                if (__callback)
                    __callback(instance);
            }
        });
    };
    TApplication.prototype.rewritePageURL = function(args) {
        var params = window.location.hash.split('/');
        if (params.length == 3) {
            var classUrl = window.location.hash.split('/')[1];
            var className = this.hexToString(classUrl);
            try {
                this._freezeRouting = true;
                var url = this.buildPageURL(className, args);
                window.location.replace(url);
            } finally {
                this._freezeRouting = false;
            }
        }
    };
    /**
     create and load page & html file synchronously  an return an object instance
     @class : the container class
     @__args  : pass argumant as array
    **/
    TApplication.prototype.createPage = function(Class, __args, htmlPath, html) {
        var self = this;
        var instance;
        var defaultPath = Class.getClassPath ? Class.getClassPath() + "" : "";
        if (defaultPath)
            defaultPath = defaultPath + "/";
        var path = (htmlPath ? htmlPath + '/' : defaultPath) + Class.getClassName() + ".html";
        if (html && html.length > 3)
            instance = self.createPageInstance(Class.prototype, html, __args);
        else
            new exports.TServer(false).getHTML(path, function(html) {
                instance = self.createPageInstance(Class.prototype, html, __args);
            }, function(errorMessage) {
                exports.Application.raiseException("cant find :" + path);
            });
        return instance;
    };
    TApplication.prototype.downloadFile = function(filename, preparingMessage, failMessage, onSuccess, onFail) {
        this.loadJSLibraries(["jquery-ui", "fileDownload", "VCL/Scripts/css.js!jquery-uicss"], function() {
            var options = {
                failMessageHtml: failMessage,
                preparingMessageHtml: preparingMessage,
                successCallback: function(url) {
                    if (onSuccess)
                        onSuccess();
                },
                failCallback: function(responseHtml, url) {
                    if (onFail)
                        onFail();
                }
            };
            $.fileDownload(encodeURI(exports.Application.serverURL + "?METHOD=DOWNLOAD&PARAMS={FILENAME:\"" +
                filename + "\"}"), options);
        });
    };
    TApplication.prototype.getLocalValue = function(name, defaultValue) {
        var session = this.sammy.local("VCL", function() {
            return {};
        });
        if (!session[name]) {
            session[name] = defaultValue;
        }
        return session[name];
    };
    TApplication.prototype.setLocalValue = function(name, value) {
        var session = this.sammy.local("VCL", function() {
            return {};
        });
        session[name] = value;
        this.sammy.local("VCL", session);
    };
    /**
     * The showMessage procedure displays a string of Text in a simple dialog with an OK button. with an optional callback
     */
    TApplication.prototype.showMessage = function(message, callback) {
        bootbox.alert(message, callback);
    };
    /**
     * The MessageDlg function is used to display messages to the user. These messages may be informational, or warnings or whatever.
     **/
    TApplication.prototype.messageDlg = function(message, title, buttons, callback) {
        var json = {};
        json.message = message;
        json.title = title;
        json.buttons = new Array();
        buttons.forEach(function(item) {
            var btn = {};
            btn["label"] = item;
            btn["callback"] = function(rc) {
                if (callback)
                    callback(rc.currentTarget.textContent);
            };
            json.buttons.push(btn);
        });
        bootbox.dialog(json);
    };
    /**
     * The MessagePrompt function is used to display messages to the user. These messages may be informational, or warnings or whatever.
     **/
    TApplication.prototype.messageDlgPrompt = function(message, callback) {
        bootbox.prompt(message, function(promptedText) {
            if (callback)
                callback(promptedText);
        });
    };
    TApplication.prototype.getSessionValue = function(name, defaultValue) {
        var session = this.sammy.session("VCL", function() {
            return {};
        });
        if (session[name] == undefined) {
            session[name] = defaultValue;
        }
        return session[name];
    };
    TApplication.prototype.setSessionValue = function(name, value) {
        var session = this.sammy.session("VCL", function() {
            return {};
        });
        session[name] = value;
        this.sammy.session("VCL", session);
    };
    TApplication.prototype.run = function() {
        exports.Application.refreshDefaultPage();
        this.sammy.run(this.buildPageURL(this.MainPage));
    };
    /**
     * return the encoded page url as string
     **/
    TApplication.prototype.buildPageURL = function(className, ConstructorArgs) {
        return "#show/" + stringToHex(className) + '/' + (decodeURIComponent(stringToHex(JSON.stringify(ConstructorArgs ? ConstructorArgs : new Object(), jsonCreateDateParserTemp))));
    };
    TApplication.prototype.login = function(email, password, onSuccuess, onFail) {
        var _this = this;
        var server = new VXServer.TServer();
        server.send(this.loginServerClass, { USER: email, PASS: password }, function(data) {
            if (data.STATUS == "OK") {
                _this.UserRole = data.ROLE;
                _this.UserName = data.USER;
                _this.UserEmail = data.EMAIL;
                onSuccuess(data);
                if (_this.onLoggedIn)
                    _this.onLoggedIn();
            } else if (onFail)
                onFail(data);
        }, function(error) {
            if (onFail)
                onFail(error);
        });
    };
    TApplication.prototype.logOff = function() {
        this.Authenticated = false;
        var server = new VXServer.TServer();
        server.send(this.loginServerClass, {}, function(data) {}, function(txt) {});
        this.navigateToPage(this.MainPage, [+Math.random()]);
        if (this.onLoggedOff)
            this.onLoggedOff();
    };
    TApplication.prototype.navigateToPage = function(className, ConstructorArgs, igonreaAthentication, openInNewWindow) {
        if (igonreaAthentication === void 0) { igonreaAthentication = false; }
        if (openInNewWindow === void 0) { openInNewWindow = false; }
        var url = this.buildPageURL(className, ConstructorArgs);
        if (igonreaAthentication) {
            this.igonreaAthenticationPage = className;
        }
        if (openInNewWindow)
            this.windowOpenURL(url, "popup");
        else
            this.sammy.setLocation(url);
        //this.sammy.setLocation(url);
    };
    TApplication.prototype.navigateToURL = function(URL) {
        this.sammy.setLocation(URL);
    };
    /**
     * The windowOpenURL() method opens a new browser window.
     **/
    TApplication.prototype.windowOpenURL = function(URL, target) {
        window.open(URL, target);
    };
    TApplication.prototype.checkColorString = function(str) {
        var isOk = /^#[0-9A-F]{6}$/i.test(str);
        if (!isOk) {
            exports.Application.raiseException("'" + str + "' is not valid hex color string");
            return false;
        }
        return true;
    };
    TApplication.prototype.raiseException = function(errorMessage) {
        if (this.onException == null) {
            alert(errorMessage);
        } else
            this.onException(errorMessage);
    };
    TApplication.prototype.addNavbarItem = function(text, icon, onClick) {
        var item = new TNavbarItem(text, icon, onClick);
        this.navbaritems.add(item);
        return item;
    };
    Object.defineProperty(TApplication.prototype, "ContentFluid", {
        get: function() {
            return this._contentfluid;
        },
        set: function(val) {
            if (val != this._contentfluid) {
                this._contentfluid = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "ShowGotoTopWidget", {
        get: function() {
            return this._showgototopicon;
        },
        set: function(val) {
            if (val != this._showgototopicon) {
                this._showgototopicon = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "MainPage", {
        /**
         * Identifies the page in the application that is the main page.
         * The main page is the first page created in the main body of the default page.
         */
        get: function() {
            return this._mainpage;
        },
        set: function(val) {
            if (val != this._mainpage) {
                this._mainpage = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "SessionTimeout", {
        /**
         * Identifies the page in the application that is the main page.
         * The main page is the first page created in the main body of the default page.
         */
        get: function() {
            return this._sessiontimeout;
        },
        set: function(val) {
            //if (val != this._sessiontimeout) {
            this._sessiontimeout = val;
            this._setsessiontimeout();
            //}
        },
        enumerable: true,
        configurable: true
    });
    TApplication.prototype._setsessiontimeout = function() {
        var _this = this;
        $(document).unbind($.trim((this._sessionevents + ' ').split(' ').join('.idleTimer ')));
        if (this._setsessiontimeoutHandle)
            clearTimeout(this._setsessiontimeoutHandle);
        if (this.SessionTimeout < 1)
            return;
        $(document).bind($.trim((this._sessionevents + ' ').split(' ').join('.idleTimer ')), function(event) {
            if (_this._lastsessiontime && new Date().getTime() - _this._lastsessiontime.getTime() < 1000)
                return;
            _this._lastsessiontime = new Date();
            if (!_this.Authenticated)
                return;
            if (_this._setsessiontimeoutHandle)
                clearTimeout(_this._setsessiontimeoutHandle);
            _this._setsessiontimeoutHandle = setTimeout(function() {
                $(document).unbind($.trim((_this._sessionevents + ' ').split(' ').join('.idleTimer ')));
                if (_this.onSessionTimeout)
                    _this.onSessionTimeout();
            }, _this.SessionTimeout * 1000);
        });
    };
    Object.defineProperty(TApplication.prototype, "LoginPage", {
        /**
         * Identifies the page in the application that is the main page.
         * The main page is the first page created in the main body of the default page.
         */
        get: function() {
            return this._loginpage;
        },
        set: function(val) {
            if (val != this._loginpage) {
                this._loginpage = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "UserName", {
        /**
         * Specifies the logged user name
         */
        get: function() {
            return this.getSessionValue('_username', "");
        },
        set: function(val) {
            if (val != this.UserName) {
                this.setSessionValue('_username', val);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "UserRole", {
        /**
         * Specifies the logged user role
         */
        get: function() {
            return this.getSessionValue('_userrole', "");
        },
        set: function(val) {
            if (val != this.UserRole) {
                this.setSessionValue('_userrole', val);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "UserId", {
        /**
         * Specifies the logged userId
         */
        get: function() {
            return this.getSessionValue('_userid', "");
        },
        set: function(val) {
            if (val != this.UserEmail) {
                this.setSessionValue('_userid', val);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "UserEmail", {
        /**
         * Specifies the logged user email address
         */
        get: function() {
            return this.getSessionValue('_useremail', "");
        },
        set: function(val) {
            if (val != this.UserEmail) {
                this.setSessionValue('_useremail', val);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "CurrencyString", {
        /**
         * Specifies the local currency symbol.
         * CurrencyString specifies the currency symbol, which can be a single character or multiple characters.
         */
        get: function() {
            return this.getSessionValue('_currencystring', "$");
        },
        set: function(val) {
            if (val != this.CurrencyString) {
                this.setSessionValue('_currencystring', val);
            }
        },
        enumerable: true,
        configurable: true
    });
    TApplication.prototype.getDeviceType = function() {
        var envs = ['phone', 'tablet', 'desktop'];
        var $el = $('<div>');
        $el.appendTo($('body'));
        for (var i = envs.length - 1; i >= 0; i--) {
            var env = envs[i];
            $el.addClass('hidden-' + env);
            if ($el.is(':hidden')) {
                $el.remove();
                if (env == 'phone')
                    return exports.DeviceType.Phone;
                if (env == 'tablet')
                    return exports.DeviceType.Tablet;
                if (env == 'desktop') {
                    if ($(window).width() >= 1200)
                        return exports.DeviceType.LargeDisplay;
                    return exports.DeviceType.Default;
                }
            }
        };
        return exports.DeviceType.Default;
    };
    Object.defineProperty(TApplication.prototype, "DateFormat", {
        /**
         * Specifies format in which the date is presented
         */
        get: function() {
            return this.getSessionValue('_dateformat', "mm/dd/yyyy");
        },
        set: function(val) {
            if (val != this.DateFormat) {
                this.setSessionValue('_dateformat', val);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "LongDateFormat", {
        get: function() {
            return this.getSessionValue('_longdateformat', "mm/dd/yyyy hh:MM:ss");
        },
        set: function(val) {
            if (val != this.DateFormat) {
                this.setSessionValue('_longdateformat', val);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "AuthenticationRequired", {
        get: function() {
            return this.getSessionValue('_authenticationrequired', false);
        },
        set: function(val) {
            if (val != this.AuthenticationRequired) {
                this.setSessionValue('_authenticationrequired', val);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "Authenticated", {
        get: function() {
            return this.getSessionValue('_authenticated', false);
        },
        set: function(val) {
            if (val != this.Authenticated) {
                this.setSessionValue('_authenticated', val);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "CurrencyDecimals", {
        /**
         * Specifies the maximum number of digits, after decimal point, for a currency value.
         */
        get: function() {
            return this.getSessionValue('_currencydecimals', 2);
        },
        set: function(val) {
            val = Number(val);
            if (val != this.CurrencyDecimals) {
                this.setSessionValue('_currencydecimals', val);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "DecimalSeparator", {
        /**
         * Specifies the character used to separate the integer part from the fractional part of a number.
         */
        get: function() {
            return this._decimalseparator;
        },
        set: function(val) {
            if (val != this._decimalseparator) {
                this._decimalseparator = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "ThousandSeparator", {
        /**
         * Specifies the character used to separate thousands in numbers with more than three digits to the left of the decimal separator.
         */
        get: function() {
            return this._thousandseparator;
        },
        set: function(val) {
            if (val != this._thousandseparator) {
                this._thousandseparator = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "ActiveLanguage", {
        /**
         * change the current selected language of the system
         */
        get: function() {
            return this._activeLanguage;
        },
        set: function(val) {
            if (val && !this.languageTable[val]) {
                this.raiseException("Language:" + val + " was not found. use exports.Application.addLanguage function");
                return;
            }
            if (val != this._activeLanguage) {
                this._activeLanguage = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    TApplication.prototype.addLanguage = function(languageCode, languageName) {
        this.languageTable[languageCode] = languageName;
        if (!this.translationTable[languageCode])
            this.translationTable[languageCode] = new Object();
    };
    TApplication.prototype.addLanguageTranslation = function(languageCode, sourceString, translatedString) {
        if (!this.languageTable[languageCode])
            this.raiseException("Language:" + languageCode + " was not found. use exports.Application.addLanguage function");
        else
            this.translationTable[languageCode][sourceString.trim().toLocaleLowerCase()] = translatedString;
    };
    TApplication.prototype.getLanguageTranslation = function(languageCode, sourceString) {
        if (!sourceString)
            return sourceString;
        if (!this.languageTable[languageCode])
            this.raiseException("Language:" + languageCode + " was not found. use exports.Application.addLanguage function");
        if (!this.translationTable[languageCode])
            this.translationTable[languageCode] = new Object();
        var res = this.translationTable[languageCode][sourceString.trim().toLocaleLowerCase()];
        if (!res) {
            for (var lang in this.languageTable) {
                if (!this.translationTable[lang])
                    this.translationTable[lang] = new Object();
                if (!this.translationTable[lang][sourceString.trim().toLocaleLowerCase()]) {
                    this.translationTable[lang][sourceString.trim().toLocaleLowerCase()] = null;
                    this.newItemIntranslationTable = true;
                }
            }
            return sourceString;
        }
        return res;
    };
    Object.defineProperty(TApplication.prototype, "EnableApplicationCache", {
        /**
         * Contains the text that appears in the browser title.
         */
        get: function() {
            return this._enableapplicationcache;
        },
        set: function(val) {
            if (val != this._enableapplicationcache) {
                this._enableapplicationcache = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "ApplicationTitle", {
        /**
         * Contains the text that appears in the browser title.
         */
        get: function() {
            return this._applicationtitle;
        },
        set: function(val) {
            if (val != this._applicationtitle) {
                this._applicationtitle = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TApplication.prototype, "ApplicationBrandName", {
        get: function() {
            return this._applicationbrandname;
        },
        set: function(val) {
            if (val != this._applicationbrandname) {
                this._applicationbrandname = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    /**
     * Rebuild the default page
     */
    TApplication.prototype.refreshDefaultPage = function() {
        var _this = this;
        $('#AppNavBar').find('.brand').text(this.ApplicationBrandName).off("click").click(function() {
            if (_this.onBrandClicked != null)
                (exports.tryAndCatch(function() { _this.onBrandClicked(); }));
            return true; //hadeling the menu
        });
        if (this.ContentFluid)
            $('#content').removeClass('container').addClass('container-fluid');
        else
            $('#content').removeClass('container-fluid').addClass('container');
        if (this.ShowGotoTopWidget) {
            if ($(window).find('#toTop_____').length == 0) {
                var btn = $("<a id='toTop_____'  style='display: inline'>");
                $('body').append(btn);
                btn.fadeOut();
                btn.on('click', function() {
                    $("html, body").animate({ scrollTop: 0 }, "slow", function() {
                        btn.fadeOut("slow");
                    });
                });
                $(window).on('scroll', function() {
                    if ($(this).scrollTop() > 100) {
                        btn.fadeIn('slow');
                    } else {
                        btn.fadeOut("slow");
                    }
                });
            }
        }
        var navLeft = $('#NavLeft');
        var navRight = $('#NavRight');
        navLeft.empty();
        navRight.empty();
        this.navbaritems.forEach(function(item) {
            if (item.Visible) {
                var baritem = $('<a/>');
                baritem.attr('href', '#');
                baritem.off("click").click(function() {
                    if (item.onClick != null)
                        exports.tryAndCatch(function() { item.onClick(); });
                    return false;
                });
                if (item.Icon) {
                    $('<i/>').addClass(item.Icon).appendTo(baritem);
                } else if (item.ImageURL && item.ImageURL != "") {
                    $('<img/>').attr('src', item.ImageURL).appendTo(baritem);
                }
                if (item.Text)
                    $("<span/>").text(item.Text).appendTo(baritem);
                var lineItem = $('<li>/');
                if (item.Active)
                    lineItem.addClass('active');
                baritem.appendTo(lineItem);
                if (item.menuItems.length() > 0) {
                    item.menuItems.createmenu('dropdown-menu').appendTo(lineItem);
                    baritem.addClass('dropdown-toggle');
                    baritem.attr('data-toggle', "dropdown");
                    lineItem.addClass('dropdown');
                    $('<b class="caret">').appendTo(baritem);
                }
                if (item.ItemAlignment == exports.ItemAlignment.Left)
                    lineItem.appendTo(navLeft);
                else
                    lineItem.appendTo(navRight);
            }
            return true;
        });
        document.title = this.ApplicationTitle;
        $('.dropdown-toggle').dropdown();
    };
    TApplication.prototype.getSpanSize = function() {
        var span = $("<div class='span1'/>");
        $('body').append(span);
        var ret = span.width();
        span.remove();
        return ret;
    };
    TApplication.prototype.s4 = function() { return Math.floor((1 + Math.random()) * 0x10000).toString(16).substring(1); };
    TApplication.prototype.h2d = function(h) {
        return parseInt(h, 16);
    };
    TApplication.prototype.hexToString = function(tmp) {
        var str = '',
            i = 0,
            c;
        for (; i < tmp.length; i += 2) {
            var c = tmp.substring(i, i + 2);
            c = String.fromCharCode(this.h2d(c));
            str += c;
        }
        return str;
    };
    /**
     *  rate password strength
     */
    TApplication.prototype.PasswordStrength = function(password) {
        return new passwordStrength().analyze(password);
    };
    /**
     * Creates a globally unique identifier.
     */
    TApplication.prototype.genGUID = function() {
        return this.s4() + this.s4() + this.s4() + this.s4() +
            this.s4() + this.s4() + this.s4() + this.s4();
    };
    TApplication.prototype.formatNumberK = function(value) {
        value = (value / 1000);
        return this.FormatNumber(value, 0) + "K";
    };
    TApplication.prototype.FormatMin = function(value, precision) {
        if (precision === void 0) { precision = 0; }
        return this.FormatNumber(value, precision) + " Min";
    };
    TApplication.prototype.FormatGB = function(value, precision, type) {
        if (precision === void 0) { precision = 0; }
        if (type === void 0) { type = this.GB; }
        return this.FormatNumber(value, precision) + type;
    };
    TApplication.prototype.FormatPercent = function(value, precision) {
        if (precision === void 0) { precision = 0; }
        return this.FormatNumber(value, precision) + this.LocaleSettings.PercentString;
    };
    TApplication.prototype.FormatCurrency = function(value, precision) {
        if (precision === void 0) { precision = 2; }
        return this.CurrencyString + this.FormatNumber(value, precision);
    };
    TApplication.prototype.formatHumanFriendly = function(value, roundfactor) {
        if (roundfactor === void 0) { roundfactor = 0; }
        var p, d2, i, s;
        var isNegative = value < 0;
        value = Math.abs(value);
        var res = "0";
        p = Math.pow;
        d2 = p(10, roundfactor);
        i = 7;
        var found = false;
        while (i) {
            s = p(10, i-- * 3);
            if (s <= value) {
                res = Math.round(value * d2 / s) / d2 + "KMGTPE" [i];
                found = true;
                break;
            }
        }
        if (!found)
            res = this.FormatNumber(value, roundfactor);
        if (isNegative)
            res = "-" + res;
        return res;
    };
    TApplication.prototype.FormatNumber = function(value, precision, removeExtraZeros) {
        if (precision === void 0) { precision = 2; }
        if (removeExtraZeros === void 0) { removeExtraZeros = false; }
        var isNegative = value < 0;
        value = Math.abs(value);
        var intp = (Math.floor(value).toString());
        var decp = (value - Math.floor(value)).toString();
        decp = (decp.substr(2, 1000) + '000000000').substr(0, precision);
        var res = "";
        if (precision == 0)
            res = intp.replace(/\B(?=(\d{3})+(?!\d))/g, this.ThousandSeparator);
        else {
            res = intp.replace(/\B(?=(\d{3})+(?!\d))/g, this.ThousandSeparator) + this.DecimalSeparator + decp;
            //remove extra zeros
            if (removeExtraZeros) {
                while (res.charAt(res.length - 1) == "0") {
                    //res = res.replace(new RegExp("0+$"), ""); - also working
                    res = res.substring(0, res.length - 1);
                }
                if (res.charAt(res.length - 1) == this.DecimalSeparator) {
                    res = res.substring(0, res.length - 1);
                }
            }
        }
        if (isNegative)
            res = "-" + res;
        return res;
    };
    /**
     Rich formatting of a Date variable into a string
     d	Day of the month as digits; no leading zero for single-digit days.
     dd	Day of the month as digits; leading zero for single-digit days.
     ddd	Day of the week as a three-letter abbreviation.
     dddd	Day of the week as its full name.
     m	Month as digits; no leading zero for single-digit months.
     mm	Month as digits; leading zero for single-digit months.
     mmm	Month as a three-letter abbreviation.
     mmmm	Month as its full name.
     yy	Year as last two digits; leading zero for years less than 10.
     yyyy	Year represented by four digits.
     h	Hours; no leading zero for single-digit hours (12-hour clock).
     hh	Hours; leading zero for single-digit hours (12-hour clock).
     H	Hours; no leading zero for single-digit hours (24-hour clock).
     HH	Hours; leading zero for single-digit hours (24-hour clock).
     M	Minutes; no leading zero for single-digit minutes.Uppercase M unlike CF timeFormat's m to avoid conflict with months.
     MM	Minutes; leading zero for single-digit minutes.
     Uppercase MM unlike CF timeFormat's mm to avoid conflict with months.
     s	Seconds; no leading zero for single-digit seconds.
     ss	Seconds; leading zero for single-digit seconds.
     l or L	Milliseconds. l gives 3 digits. L gives 2 digits.
     t	Lowercase, single-character time marker string: a or p.No equivalent in CF.
     tt	Lowercase, two-character time marker string: am or pm.No equivalent in CF.
     T	Uppercase, single-character time marker string: A or P.Uppercase T unlike CF's t to allow for user-specified casing.
     TT	Uppercase, two-character time marker string: AM or PM.Uppercase TT unlike CF's tt to allow for user-specified casing.
     Z	US timezone abbreviation, e.g. EST or MDT. With non-US timezones or in the Opera browser, the GMT/UTC offset is returned, e.g. GMT-0500No equivalent in CF.
     o	GMT/UTC timezone offset, e.g. -0500 or +0230.No equivalent in CF.
    */
    TApplication.prototype.formatDateTime = function(date, mask, utc) {
        if (!date || !date.getMonth)
            return "";
        var token = /d{1,4}|m{1,4}|yy(?:yy)?|([HhMsTt])\1?|[LloSZ]|"[^"]*"|'[^']*'/g,
            timezone = /\b(?:[PMCEA][SDP]T|(?:Pacific|Mountain|Central|Eastern|Atlantic) (?:Standard|Daylight|Prevailing) Time|(?:GMT|UTC)(?:[-+]\d{4})?)\b/g,
            timezoneClip = /[^-+\dA-Z]/g,
            pad = function(val, len) {
                //val = string(val);
                len = len || 2;
                while (val.toString().length < len)
                    val = "0" + val;
                return val;
            };
        // Regexes and supporting functions are cached through closure
        // Allow setting the utc argument via the mask
        if (mask.slice(0, 4) == "UTC:") {
            mask = mask.slice(4);
            utc = true;
        }
        var _ = utc ? "getUTC" : "get",
            d = date[_ + "Date"](),
            D = date[_ + "Day"](),
            m = date[_ + "Month"](),
            y = date[_ + "FullYear"](),
            H = date[_ + "Hours"](),
            M = date[_ + "Minutes"](),
            s = date[_ + "Seconds"](),
            L = date[_ + "Milliseconds"](),
            o = utc ? 0 : date.getTimezoneOffset(),
            flags = {
                d: d,
                dd: pad(d, 2),
                ddd: this.dateFormat_i18n.dayNames[D],
                dddd: this.dateFormat_i18n.dayNames[D + 7],
                m: m + 1,
                mm: pad(m + 1, 2),
                mmm: this.dateFormat_i18n.monthNames[m],
                mmmm: this.dateFormat_i18n.monthNames[m + 12],
                yy: String(y).slice(2),
                yyyy: y,
                h: H % 12 || 12,
                hh: pad((H % 12 || 12).toString(), 2),
                H: H,
                HH: pad(H, 2),
                M: M,
                MM: pad(M, 2),
                s: s,
                ss: pad(s, 2),
                l: pad(L, 3),
                L: pad(L > 99 ? Math.round(L / 10) : L, 2),
                t: H < 12 ? "a" : "p",
                tt: H < 12 ? "am" : "pm",
                T: H < 12 ? "A" : "P",
                TT: H < 12 ? "AM" : "PM",
                Z: utc ? "UTC" : (String(date).match(timezone) || [""]).pop().replace(timezoneClip, ""),
                o: (o > 0 ? "-" : "+") + pad((Math.floor(Math.abs(o) / 60) * 100 + Math.abs(o) % 60).toString(), 4)
            };
        return mask.replace(token, function($0) {
            return $0 in flags ? flags[$0] : $0.slice(1, $0.length - 1);
        });
    };
    return TApplication;
}());
exports.TApplication = TApplication;
var TFacebookAPI = /** @class */ (function() {
    function TFacebookAPI() {
        this._accessToken = null;
        this._appid = null;
    }
    Object.defineProperty(TFacebookAPI.prototype, "ApplicationID", {
        /**
         * Text specify the text string that labels the control.
         **/
        get: function() {
            return this._appid;
        },
        set: function(val) {
            if (val != this._appid) {
                this._appid = val;
                this.init();
            }
        },
        enumerable: true,
        configurable: true
    });
    TFacebookAPI.prototype.init = function() {
        if (!this.ApplicationID)
            return;
        var self = this;
        var d = document;
        var s = 'script';
        var id = 'facebook-jssdk';
        window.fbAsyncInit = function() {
            (FB).init({
                appId: self.ApplicationID,
                xfbml: true,
                version: 'v2.0'
            });
            if (self.onInitialized)
                self.onInitialized();
        };
        var js, fjs = d.getElementsByTagName(s)[0];
        if (d.getElementById(id)) {
            return;
        }
        js = d.createElement(s);
        js.id = id;
        js.src = "http://connect.facebook.net/en_US/sdk.js";
        fjs.parentNode.insertBefore(js, fjs);
    };
    TFacebookAPI.prototype.loginRresponseToEnum = function(status) {
        if (status === 'connected')
            return exports.FacebookLoginState.Connected;
        else if (status === 'not_authorized')
            return exports.FacebookLoginState.NotAuthorized;
        else
            return exports.FacebookLoginState.NotConnected;
    };
    TFacebookAPI.prototype.getLoginState = function(callback) {
        var self = this;
        FB.getLoginStatus(function(response) {
            var rc = self.loginRresponseToEnum(response.status);
            if (rc == exports.FacebookLoginState.Connected) {
                FB.api('/me', function(response) {
                    self.UserName = response.name;
                    self.UserID = response.id;
                    if (callback)
                        callback(rc);
                });
            } else if (callback)
                callback(rc);
        });
    };
    TFacebookAPI.prototype.login = function(callback) {
        var self = this;
        FB.login(function(response) {
            this._accessToken = response.authResponse.accessToken;
            var rc = self.loginRresponseToEnum(response.status);
            if (rc == exports.FacebookLoginState.Connected) {
                FB.api('/me', function(response) {
                    self.UserName = response.name;
                    self.UserID = response.id;
                    if (callback)
                        callback(rc);
                });
            } else if (callback)
                callback(rc);
        });
    };
    TFacebookAPI.prototype.logout = function(callback) {
        FB.logout(function(response) {
            if (callback)
                callback();
        });
    };
    return TFacebookAPI;
}());
exports.TFacebookAPI = TFacebookAPI;
var TLocaleSettings = /** @class */ (function() {
    function TLocaleSettings() {
        this.MSG_This_value_is_required = "This value is required.";
        this.MSG_This_value_is_not_minimum = " Must be more than %s characters.";
        this.PercentString = "%";
        this.CaptionPosition = 3; //exports.CaptionPosition.TopLeft;
    }
    return TLocaleSettings;
}());
exports.TLocaleSettings = TLocaleSettings;
var TNavbarItem = /** @class */ (function(_super) {
    __extends(TNavbarItem, _super);

    function TNavbarItem(text, icon, onClick) {
        var _this = _super.call(this) || this;
        _this.menuItems = new VXM.TMenuItemCollection();
        _this._active = false;
        _this._visible = true;
        _this._itemaligment = exports.ItemAlignment.Left;
        _this._imageURL = null;
        _this._text = text;
        _this._icon = icon;
        _this.onClick = onClick;
        return _this;
    }
    TNavbarItem.prototype.addMenuItem = function(text) {
        var menuItem = new VXM.TMenuItem();
        menuItem.Text = text;
        this.menuItems.add(menuItem);
        return menuItem;
    };
    Object.defineProperty(TNavbarItem.prototype, "Active", {
        get: function() {
            return this._active;
        },
        set: function(val) {
            if (val != this._active) {
                this._active = val;
                exports.Application.refreshDefaultPage();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TNavbarItem.prototype, "Visible", {
        get: function() {
            return this._visible;
        },
        set: function(val) {
            if (val != this._visible) {
                this._visible = val;
                exports.Application.refreshDefaultPage();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TNavbarItem.prototype, "ItemAlignment", {
        get: function() {
            return this._itemaligment;
        },
        set: function(val) {
            if (val != this._itemaligment) {
                this._itemaligment = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TNavbarItem.prototype, "ImageURL", {
        /**
         * Text specify the text string that labels the control.
         **/
        get: function() {
            return this._imageURL;
        },
        set: function(val) {
            if (val != this._imageURL) {
                this._imageURL = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TNavbarItem.prototype, "Text", {
        /**
         * Text specify the text string that labels the control.
         **/
        get: function() {
            return this._text;
        },
        set: function(val) {
            if (val != this._text) {
                this._text = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TNavbarItem.prototype, "Icon", {
        get: function() {
            return this._icon;
        },
        set: function(val) {
            if (val != this._icon) {
                this._icon = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    return TNavbarItem;
}(TCollectionItem));
exports.TNavbarItem = TNavbarItem;
var __reISOdeffordate__ = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}(?:\.{0,1}\d*))(?:Z|(\+|-)([\d|:]*))?$/;

function jsonCreateDateParserTemp(key, value) {
    if (typeof value === 'undefined')
        return null;
    if (typeof value === 'string') {
        if (value.substring(0, 4) == "~@~!")
            return new Date(value.substring(4));
        var a = __reISOdeffordate__.exec(value);
        if (a)
            return "~@~!" + (new Date(value));
    }
    return value;
}

function stringToHex(tmp) {
    var str = '',
        i = 0,
        tmp_len = tmp.length,
        c;
    for (; i < tmp_len; i += 1) {
        c = tmp.charCodeAt(i);
        str += d2h(c);
    }
    return str;
}

function d2h(d) {
    return d.toString(16);
}
var passwordStrength = /** @class */ (function() {
    function passwordStrength() {
        this.patterns = [
            '0123456789',
            'abcdefghijklmnopqrstuvwxyz',
            'qwertyuiopasdfghjklzxcvbnm',
            'azertyuiopqsdfghjklmwxcvbn',
            '!#$*+-.:?@^'
        ];
        this.character = { DIGIT: 1, LOWERCASE: 2, UPPERCASE: 4, PUNCTUATION: 8 };
        this.threshold = { medium: 16, high: 22, extreme: 36 };
        this.dictionary = [];
    }
    passwordStrength.prototype.analyze = function(password) {
        var score = Math.floor(password.length * 2);
        var i = password.length;
        score += this.analizePatterns(password);
        score += this.analizeDictionary(password);
        while (i--)
            score += this.analizeCharacter(password.charAt(i));
        return this.analizeScore(score);
    };
    passwordStrength.prototype.analizeScore = function(score) {
        if (score >= this.threshold.extreme)
            return exports.PasswordStrength.EXTREME;
        if (score >= this.threshold.high)
            return exports.PasswordStrength.HIGH;
        if (score >= this.threshold.medium)
            return exports.PasswordStrength.MEDIUM;
        return exports.PasswordStrength.LOW;
    };
    passwordStrength.prototype.analizePatterns = function(password) {
        var chars = password.toLowerCase();
        var score = 0;
        for (var i in this.patterns) {
            var pattern = this.patterns[i].toLowerCase();
            score += this.analizePattern(chars, pattern);
        }
        // patterns are bad man!
        return score * -5;
    };
    passwordStrength.prototype.analizePattern = function(password, pattern) {
        var lastmatch = -1;
        var score = -2;
        for (var i = 0; i < password.length; i++) {
            var match = pattern.indexOf(password.charAt(i));
            if (lastmatch === match - 1) {
                lastmatch = match;
                score++;
            }
        }
        return Math.max(0, score);
    };
    passwordStrength.prototype.analizeCharacter = function(character) {
        var code = character.charCodeAt(0);
        if (code >= 97 && code <= 122)
            return 1; // lower case
        if (code >= 48 && code <= 57)
            return 2; // numeric
        if (code >= 65 && code <= 90)
            return 3; // capital
        if (code <= 126)
            return 4; // punctuation
        return 5; // foreign characters etc
    };
    passwordStrength.prototype.analizeDictionary = function(password) {
        var chars = password.toLowerCase();
        var score = 0;
        for (var i in this.dictionary) {
            var word = this.dictionary[i].toLowerCase();
            if (password.indexOf(word) >= 0)
                score++;
        }
        // using words are bad too!
        return score * -5;
    };
    return passwordStrength;
}());