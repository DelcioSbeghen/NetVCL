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
//var V = require("./VCL");
//var VXO = require("./VXObject");
var TMenuItem = /** @class */ (function(_super) {
    __extends(TMenuItem, _super);

    function TMenuItem() {
        var _this = _super.call(this) || this;
        _this._enabled = true;
        _this._iconclass = "";
        _this._visible = true;
        _this._imageurl = null;
        _this._icon = null;
        _this._iconcolor = null;
        _this._text = "";
        _this._divider = false;
        return _this;
    }
    TMenuItem.prototype.create = function() {
        var _this = this;
        var self = this;
        if (this.jComponent == null)
            this.jComponent = $('<li/>');
        if (!this.Enabled)
            this.jComponent.addClass('disabled');
        else
            this.jComponent.removeClass('disabled');
        if (this.Visible)
            this.jComponent.show();
        else
            this.jComponent.hide();
        if (this.Divider)
            this.jComponent.addClass('divider');
        else {
            this.jComponent.empty();
            //var itemRef: JQuery = $('<a href="#"/>');
            var itemRef = $('<a style="cursor:pointer;text-align:left"/>');
            itemRef.off("click").click(function() {
                if (_this.Enabled && _this.onClicked != null) {
                    (V.tryAndCatch(function() { _this.onClicked(self); }));
                }
            });
            itemRef.html(this.Text);
            itemRef.appendTo(this.jComponent);
            if (this.Icon != null || this.ImageURL != null) {
                var imageRef;
                if (this.Icon) {
                    imageRef = $('<i/>');
                    imageRef.addClass(V.iconEnumToBootstrapStyle(this.Icon) + " " + this.IconClass);
                    if (this.IconColor)
                        imageRef.css('color', this.IconColor);
                } else if (this.ImageURL) {
                    imageRef = $('<img/>');
                    imageRef.attr('src', this.ImageURL);
                }
                this.jIcon = imageRef;
                imageRef.prependTo(itemRef);
            }
        }
    };
    Object.defineProperty(TMenuItem.prototype, "Enabled", {
        get: function() {
            return this._enabled;
        },
        set: function(val) {
            if (val != this._enabled) {
                this._enabled = val;
                this.create();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TMenuItem.prototype, "IconClass", {
        get: function() {
            return this._iconclass;
        },
        set: function(val) {
            if (val != this._iconclass) {
                this._iconclass = val;
                this.create();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TMenuItem.prototype, "GroupHeader", {
        get: function() {
            return this._groupheader;
        },
        set: function(val) {
            if (val != this._groupheader) {
                this._groupheader = val;
                this.create();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TMenuItem.prototype, "Visible", {
        get: function() {
            return this._visible;
        },
        set: function(val) {
            if (val != this._visible) {
                this._visible = val;
                this.create();
            }
        },
        enumerable: true,
        configurable: true
    });
    /**
     * Display the component by fading them to opaque
     */
    TMenuItem.prototype.fadeIn = function(duration, complete) {
        this.jComponent.fadeIn(duration, function() {
            if (complete != null)
                complete();
        });
    };
    /**
     * Hide the matched elements by fading them to transparent.
     */
    TMenuItem.prototype.fadeOut = function(duration, complete) {
        this.jComponent.fadeOut(duration, function() {
            if (complete != null)
                complete();
        });
    };
    Object.defineProperty(TMenuItem.prototype, "ImageURL", {
        get: function() {
            return this._imageurl;
        },
        set: function(val) {
            if (val != this._imageurl) {
                this._imageurl = val;
                this.create();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TMenuItem.prototype, "Icon", {
        get: function() {
            return this._icon;
        },
        set: function(val) {
            if (val != this._icon) {
                this._icon = val;
                this.create();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TMenuItem.prototype, "IconColor", {
        get: function() {
            return this._iconcolor;
        },
        set: function(val) {
            if (V.Application.checkColorString(val)) {
                if (val != this._iconcolor) {
                    this._iconcolor = val;
                    this.create();
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TMenuItem.prototype, "Text", {
        get: function() {
            return this._text;
        },
        set: function(val) {
            if (val != this._text) {
                this._text = val;
                this.create();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TMenuItem.prototype, "Divider", {
        get: function() {
            return this._divider;
        },
        set: function(val) {
            if (val != this._divider) {
                this._divider = val;
                this.create();
            }
        },
        enumerable: true,
        configurable: true
    });
    return TMenuItem;
}(TCollectionItem));
exports.TMenuItem = TMenuItem;
var TMenuItemCollection = /** @class */ (function(_super) {
    __extends(TMenuItemCollection, _super);

    function TMenuItemCollection() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    TMenuItemCollection.prototype.createmenu = function(headerClass, groupHeader) {
        var menu = $('<ul>');
        menu.addClass(headerClass);
        this.forEach(function(menuItem) {
            if (!groupHeader ||
                (menuItem.GroupHeader && groupHeader.toUpperCase() == menuItem.GroupHeader.toUpperCase())) {
                menuItem.create();
                menuItem.jComponent.appendTo(menu);
                return true;
            }
        });
        return menu;
    };
    TMenuItemCollection.prototype.remove = function(element) {
        var rc = _super.prototype.remove.call(this, element);
        return rc;
    };
    TMenuItemCollection.prototype.add = function(element) {
        var rc = _super.prototype.add.call(this, element);
        return rc;
    };
    return TMenuItemCollection;
}(TCollection));
exports.TMenuItemCollection = TMenuItemCollection;