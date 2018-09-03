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
//var VXC = require("./VXComponent");
//var VXT = require("./VXTextBase");
//var VXU = require("./VXUtils");
//var VXO = require("./VXObject");
//var VXM = require("./VXMenu");
var TText = /** @class */ (function(_super) {
    __extends(TText, _super);

    function TText(aOwner, renderTo, text) {
        var _this = _super.call(this, aOwner, renderTo, text) || this;
        _this._textstyle = exports.TextStyle.Default;
        return _this;
    }
    Object.defineProperty(TText.prototype, "Href", {
        get: function() {
            return this._href;
        },
        set: function(val) {
            if (val != this._href) {
                this._href = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TText.prototype.create = function() {
        var _this = this;
        var self = this;
        if (this.TextStyle == exports.TextStyle.h1)
            this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'h1', this.FitToWidth, this.FitToHeight);
        else if (this.TextStyle == exports.TextStyle.h2)
            this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'h2', this.FitToWidth, this.FitToHeight);
        else if (this.TextStyle == exports.TextStyle.h3)
            this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'h3', this.FitToWidth, this.FitToHeight);
        else if (this.TextStyle == exports.TextStyle.h4)
            this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'h4', this.FitToWidth, this.FitToHeight);
        else if (this.TextStyle == exports.TextStyle.h5)
            this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'h5', this.FitToWidth, this.FitToHeight);
        else if (this.TextStyle == exports.TextStyle.h6)
            this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'h6', this.FitToWidth, this.FitToHeight);
        else if (this.TextStyle == exports.TextStyle.strong)
            this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'strong', this.FitToWidth, this.FitToHeight);
        else if (this.TextStyle == exports.TextStyle.lead) {
            this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'p', this.FitToWidth, this.FitToHeight);
            this.jComponent.addClass('lead');
        } else if (this.TextStyle == exports.TextStyle.small) {
            this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'small', this.FitToWidth, this.FitToHeight);
        } else
            this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'p', this.FitToWidth, this.FitToHeight);
        this.jComponent.off("click").click(function(e) {
            if (_this.onClicked != null) {
                (exports.tryAndCatch(function() { _this.onClicked(self, e.shiftKey); }));
                return false;
            } else if (_this.Href)
                location.href = _this.Href;
            else
                return true;
        });
        if (this.Href)
            this.jComponent.attr('href', this.Href);
        if (this.onClicked || this.Href)
            this.jComponent.addClass('btn-link');
        if (this.TextColor)
            this.jComponent.css('color', this.TextColor);
        if (this.TextAlignment == exports.TextAlignment.Left)
            this.jComponent.css('text-align', 'left');
        if (this.TextAlignment == exports.TextAlignment.Right)
            this.jComponent.css('text-align', 'right');
        if (this.TextAlignment == exports.TextAlignment.Center)
            this.jComponent.css('text-align', 'center');
        _super.prototype.create.call(this);
    };
    Object.defineProperty(TText.prototype, "TextStyle", {
        get: function() {
            return this._textstyle;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._textstyle) {
                this._textstyle = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TText.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        this.jComponent.html(this.Text);
        if (this.onClicked != null)
            this.jComponent.css('cursor', 'pointer');
        else
            this.jComponent.css('cursor', '');
    };
    return TText;
}(TTextBase));
exports.TText = TText;
var TDBText = /** @class */ (function(_super) {
    __extends(TDBText, _super);

    function TDBText() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    TDBText.prototype.create = function() {
        if (!this.Dataset)
            this.Dataset = this.guessDataset();
        this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'span', this.FitToWidth, this.FitToHeight);
        _super.prototype.create.call(this);
    };
    TDBText.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        this.jComponent.text(this.DataValue);
    };
    return TDBText;
}(TDBTextBase));
exports.TDBText = TDBText;
var TLabel = /** @class */ (function(_super) {
    __extends(TLabel, _super);

    function TLabel() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Object.defineProperty(TLabel.prototype, "LabelStyle", {
        get: function() {
            return this._labelstyle;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._labelstyle) {
                this._labelstyle = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TLabel.prototype.create = function() {
        var _this = this;
        var self = this;
        this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'span', this.FitToWidth, this.FitToHeight);
        this.jComponent.off("click").click(function(e) {
            if (_this.onClicked != null) {
                (exports.tryAndCatch(function() { _this.onClicked(self, e.shiftKey); }));
                return false;
            } else
                return true;
        });
        this.jComponent.addClass('label');
        if (this.LabelStyle == exports.LabelStyle.Success)
            this.jComponent.addClass('label-success');
        else if (this.LabelStyle == exports.LabelStyle.Warning)
            this.jComponent.addClass('label-warning');
        else if (this.LabelStyle == exports.LabelStyle.Important)
            this.jComponent.addClass('label-important');
        else if (this.LabelStyle == exports.LabelStyle.Info)
            this.jComponent.addClass('label-info');
        if (this.TextColor)
            this.jComponent.css('color', this.TextColor);
        if (this.TextAlignment == exports.TextAlignment.Left)
            this.jComponent.css('text-align', 'left');
        if (this.TextAlignment == exports.TextAlignment.Right)
            this.jComponent.css('text-align', 'right');
        if (this.TextAlignment == exports.TextAlignment.Center)
            this.jComponent.css('text-align', 'center');
        _super.prototype.create.call(this);
    };
    TLabel.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        this.jComponent.text(this.LocalizeText(this.Text));
    };
    return TLabel;
}(TTextBase));
exports.TLabel = TLabel;
var TDBLabel = /** @class */ (function(_super) {
    __extends(TDBLabel, _super);

    function TDBLabel() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Object.defineProperty(TDBLabel.prototype, "LabelStyle", {
        get: function() {
            return this._labelstyle;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._labelstyle) {
                this._labelstyle = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TDBLabel.prototype.create = function() {
        var _this = this;
        if (!this.Dataset)
            this.Dataset = this.guessDataset();
        this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'span', this.FitToWidth, this.FitToHeight);
        this.jComponent.addClass('label');
        if (this.Rtl == true)
            this.jComponent.attr("dir", "RTL");
        this.jComponent.off("click").click(function() {
            if (_this.onClicked != null) {
                (exports.tryAndCatch(function() { _this.onClicked(); }));
                return false;
            } else
                return true;
        });
        if (this.LabelStyle == exports.LabelStyle.Success)
            this.jComponent.addClass('label-success');
        else if (this.LabelStyle == exports.LabelStyle.Warning)
            this.jComponent.addClass('label-warning');
        else if (this.LabelStyle == exports.LabelStyle.Important)
            this.jComponent.addClass('label-important');
        else if (this.LabelStyle == exports.LabelStyle.Info)
            this.jComponent.addClass('label-info');
        if (this.TextColor)
            this.jComponent.css('color', this.TextColor);
        _super.prototype.create.call(this);
    };
    TDBLabel.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        this.jComponent.text(this.DataValue);
    };
    return TDBLabel;
}(TDBTextBase));
exports.TDBLabel = TDBLabel;
var TBadge = /** @class */ (function(_super) {
    __extends(TBadge, _super);

    function TBadge() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Object.defineProperty(TBadge.prototype, "BadgeStyle", {
        get: function() {
            return this._badgestyle;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._badgestyle) {
                this._badgestyle = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TBadge.prototype.create = function() {
        var _this = this;
        var self = this;
        this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'span', this.FitToWidth, this.FitToHeight);
        this.jComponent.addClass('badge');
        if (this.Rtl == true)
            this.jComponent.attr("dir", "RTL");
        this.jComponent.off("click").click(function(e) {
            if (_this.onClicked != null) {
                (exports.tryAndCatch(function() { _this.onClicked(self, e.shiftKey); }));
                return false;
            } else
                return true;
        });
        if (this.BadgeStyle == exports.BadgeStyle.Success)
            this.jComponent.addClass('badge-success');
        else if (this.BadgeStyle == exports.BadgeStyle.Warning)
            this.jComponent.addClass('badge-warning');
        else if (this.BadgeStyle == exports.BadgeStyle.Important)
            this.jComponent.addClass('badge-important');
        else if (this.BadgeStyle == exports.BadgeStyle.Info)
            this.jComponent.addClass('badge-info');
        if (this.TextColor)
            this.jComponent.css('color', this.TextColor);
        if (this.TextAlignment == exports.TextAlignment.Left)
            this.jComponent.css('text-align', 'left');
        if (this.TextAlignment == exports.TextAlignment.Right)
            this.jComponent.css('text-align', 'right');
        if (this.TextAlignment == exports.TextAlignment.Center)
            this.jComponent.css('text-align', 'center');
        _super.prototype.create.call(this);
    };
    TBadge.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        this.jComponent.text(this.LocalizeText(this.Text));
    };
    return TBadge;
}(TTextBase));
exports.TBadge = TBadge;
var TDBBadge = /** @class */ (function(_super) {
    __extends(TDBBadge, _super);

    function TDBBadge() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Object.defineProperty(TDBBadge.prototype, "BadgeStyle", {
        get: function() {
            return this._badgestyle;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._badgestyle) {
                this._badgestyle = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TDBBadge.prototype.create = function() {
        var _this = this;
        if (!this.Dataset)
            this.Dataset = this.guessDataset();
        this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'span', this.FitToWidth, this.FitToHeight);
        this.jComponent.addClass('badge');
        if (this.Rtl == true)
            this.jComponent.attr("dir", "RTL");
        this.jComponent.off("click").click(function() {
            if (_this.onClicked != null) {
                (exports.tryAndCatch(function() { _this.onClicked(); }));
                return false;
            } else
                return true;
        });
        if (this.BadgeStyle == exports.BadgeStyle.Success)
            this.jComponent.addClass('badge-success');
        else if (this.BadgeStyle == exports.BadgeStyle.Warning)
            this.jComponent.addClass('badge-warning');
        else if (this.BadgeStyle == exports.BadgeStyle.Important)
            this.jComponent.addClass('badge-important');
        else if (this.BadgeStyle == exports.BadgeStyle.Info)
            this.jComponent.addClass('badge-info');
        if (this.TextColor)
            this.jComponent.css('color', this.TextColor);
        _super.prototype.create.call(this);
    };
    TDBBadge.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        this.jComponent.text(this.DataValue);
    };
    return TDBBadge;
}(TDBTextBase));
exports.TDBBadge = TDBBadge;
var TTagCloud = /** @class */ (function(_super) {
    __extends(TTagCloud, _super);
    /**
    @aOwner     Indicates the component that is responsible for streaming and freeing this component.Onwer must be TContainer
    @renderTo   (Optional) the id of the html element that will be the parent node for this component
    **/
    function TTagCloud(aOwner, renderTo) {
        var _this = _super.call(this, aOwner, renderTo) || this;
        _this._fontstart = 10;
        _this._fontend = 18;
        _this._brackets = false;
        _this._colorstart = exports.getClassStyleHexColor('btn-primary', 'background-color');
        _this._colorend = exports.getClassStyleHexColor('btn-danger', 'background-color');
        _this.items = new exports.TCollection();
        return _this;
    }
    TTagCloud.prototype.compareWeights = function(a, b) {
        return a - b;
    };
    // Converts hex to an RGB array
    TTagCloud.prototype.toRGB = function(code) {
        if (code.length === 4) {
            code = code.replace(/(\w)(\w)(\w)/gi, "\$1\$1\$2\$2\$3\$3");
        }
        var hex = /(\w{2})(\w{2})(\w{2})/.exec(code);
        return [parseInt(hex[1], 16), parseInt(hex[2], 16), parseInt(hex[3], 16)];
    };
    // Converts an RGB array to hex
    TTagCloud.prototype.toHex = function(ary) {
        return "#" + jQuery.map(ary, function(i) {
            var hex = i.toString(16);
            hex = (hex.length === 1) ? "0" + hex : hex;
            return hex;
        }).join("");
    };
    TTagCloud.prototype.colorIncrement = function(range) {
        var self = this;
        return jQuery.map(self.toRGB(self.ColorEnd), function(n, i) {
            return (n - self.toRGB(self.ColorStart)[i]) / range;
        });
    };
    TTagCloud.prototype.tagColor = function(increment, weighting) {
        var rgb = jQuery.map(this.toRGB(this.ColorStart), function(n, i) {
            var ref = Math.round(n + (increment[i] * weighting));
            if (ref > 255) {
                ref = 255;
            } else {
                if (ref < 0) {
                    ref = 0;
                }
            }
            return ref;
        });
        return this.toHex(rgb);
    };
    Object.defineProperty(TTagCloud.prototype, "FontStart", {
        get: function() {
            return this._fontstart;
        },
        set: function(val) {
            val = Number(val);
            if (val != this._fontstart) {
                this._fontstart = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTagCloud.prototype, "FontEnd", {
        get: function() {
            return this._fontend;
        },
        set: function(val) {
            val = Number(val);
            if (val != this._fontend) {
                this._fontend = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTagCloud.prototype, "BracketsAroundText", {
        get: function() {
            return this._brackets;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._brackets) {
                this._brackets = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTagCloud.prototype, "ColorStart", {
        get: function() {
            return this._colorstart;
        },
        set: function(val) {
            if (exports.Application.checkColorString(val)) {
                if (val != this._colorstart) {
                    this._colorstart = val;
                    this.drawDelayed(true);
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTagCloud.prototype, "ColorEnd", {
        get: function() {
            return this._colorend;
        },
        set: function(val) {
            if (exports.Application.checkColorString(val)) {
                if (val != this._colorend) {
                    this._colorend = val;
                    this.drawDelayed(true);
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    TTagCloud.prototype.create = function() {
        var _this = this;
        var self = this;
        this.jComponent.empty(); //clear all subcomponents
        this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'div', this.FitToWidth, this.FitToHeight);
        this.jComponent.css('vertical-align', 'middle');
        this.jComponent.css('text-align', 'center');
        this.jComponent.css('display', 'table-cell');
        var lowest = 11111111110;
        var highest = 0;
        this.items.forEach(function(item) {
            if (item.Value < lowest)
                lowest = item.Value;
            if (item.Value > highest)
                highest = item.Value;
        });
        var range = highest - lowest;
        if (range === 0) {
            range = 1;
        }
        // Sizes
        var fontIncr, colorIncr;
        fontIncr = (this.FontEnd - this.FontStart) / range;
        colorIncr = this.colorIncrement(range);
        this.items.forEach(function(item) {
            var tagItem = $('<p style=" cursor: pointer;display:inline-block">');
            var weighting = item.Value - lowest;
            tagItem.css({ "font-size": _this.FontStart + (weighting * fontIncr) + "pt" });
            tagItem.css({ "color": _this.tagColor(colorIncr, weighting) });
            var txt = item.Text;
            if (_this.BracketsAroundText)
                txt = "[" + txt + "]";
            tagItem.text(txt);
            tagItem.css('margin', "3px");
            tagItem.attr('data-toggl', 'tooltip');
            if (_this.ToolTipFormat)
                tagItem.attr('title', _this.ToolTipFormat(item));
            else if (item.Tooltip)
                tagItem.attr('title', item.Tooltip);
            else
                tagItem.attr('title', item.Value);
            tagItem.tooltip();
            tagItem.off("click").click(function() {
                if (self.selectedTagItem)
                    self.selectedTagItem.css("background-color", "");
                self.selectedTagItem = tagItem;
                self.selectedTagItem.css("background-color", "yellow");
                if (self.onClicked != null)
                    (exports.tryAndCatch(function() { self.onClicked(item); }));
                return false;
            });
            tagItem.appendTo(_this.jComponent);
        });
        _super.prototype.create.call(this);
    };
    TTagCloud.prototype.createItem = function(text, value) {
        var col = new TTagCloudItem();
        this.items.add(col);
        col.Value = value;
        col.Text = text;
        return col;
    };
    TTagCloud.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
    };
    return TTagCloud;
}(TComponent));
exports.TTagCloud = TTagCloud;
var TTagCloudItem = /** @class */ (function(_super) {
    __extends(TTagCloudItem, _super);

    function TTagCloudItem() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this._text = null;
        _this._value = null;
        _this._tooltip = null;
        return _this;
    }
    Object.defineProperty(TTagCloudItem.prototype, "Text", {
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
    Object.defineProperty(TTagCloudItem.prototype, "Value", {
        get: function() {
            return this._value;
        },
        set: function(val) {
            if (val != this._value) {
                this._value = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTagCloudItem.prototype, "Tooltip", {
        get: function() {
            return this._tooltip;
        },
        set: function(val) {
            if (val != this._tooltip) {
                this._tooltip = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    return TTagCloudItem;
}(TCollectionItem));
exports.TTagCloudItem = TTagCloudItem;
var TPillBoxItem = /** @class */ (function(_super) {
    __extends(TPillBoxItem, _super);

    function TPillBoxItem() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.menuItems = new VXM.TMenuItemCollection();
        _this._value = null;
        _this._text = null;
        _this._width = null;
        _this._pillboxstyle = exports.PillBoxStyle.Default;
        _this._enableremove = true;
        _this._tooltip = "";
        return _this;
    }
    TPillBoxItem.prototype.createMenuItem = function(text, onClicked) {
        var menuItem = new VXM.TMenuItem();
        menuItem.Text = text;
        menuItem.onClicked = onClicked;
        this.menuItems.add(menuItem);
        if (this.OwnerCollection != null)
            this.OwnerCollection.refresh();
        return menuItem;
    };
    Object.defineProperty(TPillBoxItem.prototype, "Value", {
        get: function() {
            return this._value;
        },
        set: function(val) {
            if (val != this._value) {
                this._value = val;
                if (this.OwnerCollection != null)
                    this.OwnerCollection.refresh();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TPillBoxItem.prototype, "Text", {
        get: function() {
            return this._text;
        },
        set: function(val) {
            if (val != this._text) {
                this._text = val;
                if (this.OwnerCollection != null)
                    this.OwnerCollection.refresh();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TPillBoxItem.prototype, "Width", {
        get: function() {
            return this._width;
        },
        set: function(val) {
            val = Number(val);
            if (val != this._width) {
                this._width = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TPillBoxItem.prototype, "PillBoxItemStyle", {
        get: function() {
            return this._pillboxstyle;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._pillboxstyle) {
                this._pillboxstyle = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TPillBoxItem.prototype, "EnableRemove", {
        get: function() {
            return this._enableremove;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._enableremove) {
                this._enableremove = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TPillBoxItem.prototype, "Tooltip", {
        get: function() {
            return this._tooltip;
        },
        set: function(val) {
            this._tooltip = val;
        },
        enumerable: true,
        configurable: true
    });
    return TPillBoxItem;
}(TCollectionItem));
exports.TPillBoxItem = TPillBoxItem;
var TPillBox = /** @class */ (function(_super) {
    __extends(TPillBox, _super);

    function TPillBox() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.items = new exports.TCollection();
        return _this;
    }
    TPillBox.prototype.createItem = function(text, style) {
        if (style === void 0) { style = exports.PillBoxStyle.Default; }
        var col = new TPillBoxItem();
        this.items.add(col);
        col.PillBoxItemStyle = style;
        col.Text = text;
        this.drawDelayed(true);
        return col;
    };
    TPillBox.prototype.removeItem = function(item) {
        this.items.remove(item);
        if (this.onRemoved)
            this.onRemoved(item);
        this.drawDelayed(true);
    };
    TPillBox.prototype.create = function() {
        var _this = this;
        var self = this;
        this.jComponent.empty(); //clear all subcomponents
        this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'div', this.FitToWidth, this.FitToHeight);
        this.jComponent.addClass('pillbox');
        this.items.forEach(function(item) {
            var jItem = $('<li>').addClass('pillboxitem dropdown');
            if (item.Tooltip)
                jItem.attr("title", item.Tooltip);
            var jtext = $('<span>').html(item.Text);
            jtext.appendTo(jItem);
            if (item.Width)
                jItem.css('width', item.Width + "px");
            if (item.EnableRemove)
                jItem.attr('data-content', String.fromCharCode(215));
            jItem.data("ID", item);
            jItem.addClass('status-' + exports.PillBoxStyle[item.PillBoxItemStyle].toLowerCase());
            if (item.menuItems.length() > 0) {
                jtext.append($('<span class="caret"/>'));
                jtext.addClass("dropdown-toggle");
                jtext.attr('data-toggle', "dropdown");
                item.menuItems.createmenu('dropdown-menu').appendTo(jItem);
                $('.dropdown-toggle').dropdown();
            }
            jItem.off("click").click(function(e) {
                var $li = $(e.currentTarget);
                var item = $li.data("ID");
                if (item.EnableRemove && $li.width() + $li.offset().left - e.pageX < 16) {
                    var rc;
                    if (self.onRemove)
                        rc = self.onRemove(item);
                    if (rc != false) {
                        $li.remove();
                        self.removeItem(item);
                    }
                } else {
                    if (self.onClicked)
                        self.onClicked(item);
                }
                e.preventDefault();
            });
            _this.jComponent.append(jItem);
        });
    };
    TPillBox.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
    };
    return TPillBox;
}(TComponent));
exports.TPillBox = TPillBox;
var TBreadCrumbItem = /** @class */ (function(_super) {
    __extends(TBreadCrumbItem, _super);

    function TBreadCrumbItem() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this._value = null;
        _this._text = null;
        _this._enabled = true;
        return _this;
    }
    Object.defineProperty(TBreadCrumbItem.prototype, "Value", {
        get: function() {
            return this._value;
        },
        set: function(val) {
            if (val != this._value) {
                this._value = val;
                if (this.OwnerCollection != null)
                    this.OwnerCollection.refresh();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TBreadCrumbItem.prototype, "TextColor", {
        get: function() {
            return this._textcolor;
        },
        set: function(val) {
            if (exports.Application.checkColorString(val)) {
                if (val != this._textcolor) {
                    this._textcolor = val;
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TBreadCrumbItem.prototype, "Text", {
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
    Object.defineProperty(TBreadCrumbItem.prototype, "Enabled", {
        get: function() {
            return this._enabled;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._enabled) {
                this._enabled = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    return TBreadCrumbItem;
}(TCollectionItem));
exports.TBreadCrumbItem = TBreadCrumbItem;
var TBreadCrumb = /** @class */ (function(_super) {
    __extends(TBreadCrumb, _super);

    function TBreadCrumb() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.items = new exports.TCollection();
        return _this;
    }
    TBreadCrumb.prototype.createItem = function(text) {
        var col = new TBreadCrumbItem();
        this.items.add(col);
        col.Text = text;
        return col;
    };
    TBreadCrumb.prototype.create = function() {
        var _this = this;
        var self = this;
        this.jComponent.empty(); //clear all subcomponents
        this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'ul', this.FitToWidth, this.FitToHeight);
        this.jComponent.addClass('breadcrumb');
        var jItem;
        var sepItem;
        this.items.forEach(function(item) {
            jItem = $('<li>');
            var aItem;
            jItem.data("ID", item);
            if (!item.Enabled) {
                jItem.addClass('active');
                jItem.text(item.Text);
                if (item.TextColor)
                    jItem.css('color', item.TextColor);
                jItem.css('cursor', 'text');
            } else {
                aItem = $('<a>');
                aItem.text(item.Text);
                //aItem.attr('href', '#');
                aItem.appendTo(jItem);
                if (item.TextColor)
                    aItem.css('color', item.TextColor);
                aItem.css('cursor', 'pointer');
            }
            sepItem = $('<span >').addClass('divider');
            sepItem.text(' > ');
            sepItem.appendTo(jItem);
            jItem.off("click").click(function(e) {
                var $li = $(e.currentTarget);
                var item = $li.data("ID");
                if (self.onClicked)
                    self.onClicked(item);
                e.preventDefault();
            });
            _this.jComponent.append(jItem);
        });
        if (sepItem)
            sepItem.remove(); //remove seperator from last item
    };
    TBreadCrumb.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
    };
    return TBreadCrumb;
}(TComponent));
exports.TBreadCrumb = TBreadCrumb;
var TPaginationItem = /** @class */ (function(_super) {
    __extends(TPaginationItem, _super);

    function TPaginationItem(aOwner) {
        var _this = _super.call(this) || this;
        _this._pagination = null;
        _this.jImage = null;
        _this.jItem = null;
        _this.jText = null;
        _this._text = null;
        _this._enabled = true;
        _this._buttonicon = null;
        _this._iconalignment = exports.IconAlignment.Left;
        _this._pagination = aOwner;
        return _this;
    }
    Object.defineProperty(TPaginationItem.prototype, "Text", {
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
    Object.defineProperty(TPaginationItem.prototype, "Enabled", {
        get: function() {
            return this._enabled;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._enabled) {
                this._enabled = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TPaginationItem.prototype, "ButtonIcon", {
        get: function() {
            return this._buttonicon;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._buttonicon) {
                this._buttonicon = val;
                this._pagination.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TPaginationItem.prototype, "IconAlignment", {
        get: function() {
            return this._iconalignment;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._iconalignment) {
                this._iconalignment = val;
                this._pagination.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TPaginationItem.prototype.create = function() {
        var self = this;
        this.jImage = $('<i/>');
        this.jItem = $('<li>');
        this.jText = $('<span/>');
        this.jText.text(this.Text);
        var aItem = $('<a>');
        if (this.ButtonIcon) {
            $(this.jImage).addClass(exports.iconEnumToBootstrapStyle(this.ButtonIcon));
            if (this.ButtonIcon == exports.ButtonIcon.icon_spinner)
                $(this.jImage).addClass('icon-spin');
            aItem.append(this.jImage);
            if (this._pagination.PaginationSize == exports.PaginationSize.Large) {
                this.jImage.addClass('icon-large');
                this.jImage.css('margin-top', '1px');
                if (this.IconAlignment == exports.IconAlignment.Left)
                    this.jText.css('padding-left', '8px');
                else
                    this.jText.css('padding-right', '8px');
            }
            if (this._pagination.PaginationSize == exports.PaginationSize.Default) {
                this.jImage.css('margin-top', '3px');
            }
            if (this._pagination.PaginationSize == exports.PaginationSize.Small) {
                this.jImage.css('margin-top', '2px');
            }
            if (this._pagination.PaginationSize == exports.PaginationSize.Mini) {
                this.jImage.css('margin-top', '3px');
            }
            if (this.IconAlignment == exports.IconAlignment.Left)
                this.jImage.addClass('pull-left');
            else
                this.jImage.addClass('pull-right');
        }
        aItem.append(this.jText);
        if (!this.Enabled) {
            this.jItem.addClass('disabled');
        } else {
            //aItem.attr('href', '#');
            this.jItem.off("click").click(function(e) {
                var li = $(this);
                var item = li.data("ID");
                item._pagination.items.forEach(function(item) {
                    item.jItem.removeClass('active');
                });
                li.addClass('active');
                if (item._pagination.onClicked)
                    item._pagination.onClicked(item);
                e.preventDefault();
            });
        }
        this.jItem.append(aItem);
    };
    return TPaginationItem;
}(TCollectionItem));
exports.TPaginationItem = TPaginationItem;
var TPagination = /** @class */ (function(_super) {
    __extends(TPagination, _super);

    function TPagination() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.items = new exports.TCollection();
        _this._alignment = exports.PaginationAlignment.Left;
        _this._paginationsize = exports.PaginationSize.Default;
        return _this;
    }
    TPagination.prototype.createItem = function(text) {
        var col = new TPaginationItem(this);
        this.items.add(col);
        col.Text = text;
        this.drawDelayed(true);
        return col;
    };
    Object.defineProperty(TPagination.prototype, "PaginationAlignment", {
        get: function() {
            return this._alignment;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._alignment) {
                this._alignment = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TPagination.prototype, "PaginationSize", {
        get: function() {
            return this._paginationsize;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._paginationsize) {
                this._paginationsize = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TPagination.prototype.create = function() {
        var self = this;
        this.jComponent.empty(); //clear all subcomponents
        this.jComponent = exports.VXUtils.changeJComponentType(this.jComponent, 'ul', this.FitToWidth, this.FitToHeight);
        this.jComponent.addClass('pagination');
        switch (this._paginationsize) {
            case exports.PaginationSize.Large:
                this.jComponent.addClass("pagination-large");
                break;
            case exports.PaginationSize.Small:
                this.jComponent.addClass("pagination-small");
                break;
            case exports.PaginationSize.Mini:
                this.jComponent.addClass("pagination-mini");
                break;
        }
        switch (this._alignment) {
            case exports.PaginationAlignment.Right:
                this.jComponent.addClass("pagination-right");
                break;
            case exports.PaginationAlignment.Center:
                this.jComponent.addClass("pagination-centered");
                break;
            default:
                this.jComponent.removeClass("pagination-right");
                this.jComponent.removeClass("pagination-centered");
                break;
        }
        var picker = $('<ul>');
        this.items.forEach(function(item) {
            item.create();
            item.jItem.data("ID", item);
            picker.append(item.jItem);
        });
        this.jComponent.append(picker);
    };
    TPagination.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
    };
    return TPagination;
}(TComponent));
exports.TPagination = TPagination;