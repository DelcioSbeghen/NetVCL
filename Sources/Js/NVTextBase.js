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
//var VXD = require("./VXDataset");
var TTextBase = /** @class */ (function(_super) {
    __extends(TTextBase, _super);
    /**
    @aOwner     Indicates the component that is responsible for streaming and freeing this component.Onwer must be TContainer
    @renderTo   (Optional) the id of the html element that will be the parent node for this component
    @text       (Optional) the initial text value of the component
    **/
    function TTextBase(aOwner, renderTo, text) {
        var _this = _super.call(this, aOwner, renderTo) || this;
        _this._rtl = false;
        _this._localizable = false;
        _this.Text = text;
        return _this;
    }
    Object.defineProperty(TTextBase.prototype, "Rtl", {
        get: function() {
            return this._rtl;
        },
        set: function(val) {
            if (val != this._rtl) {
                this._rtl = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTextBase.prototype, "Localizable", {
        /**
         * In order to localize application each page or component of the application has to have Localizable property set true.
         */
        get: function() {
            return this._localizable;
        },
        set: function(val) {
            if (val != this._localizable) {
                this._localizable = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTextBase.prototype, "TextColor", {
        get: function() {
            return this._textcolor;
        },
        set: function(val) {
            if (V.Application.checkColorString(val)) {
                if (val != this._textcolor) {
                    this._textcolor = val;
                    this.drawDelayed(true);
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTextBase.prototype, "TextAlignment", {
        get: function() {
            return this._textaliggment;
        },
        set: function(val) {
            if (val != this._textaliggment) {
                this._textaliggment = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTextBase.prototype, "Text", {
        /**
         * Text specify the text string that labels the control.
         */
        get: function() {
            return this._text;
        },
        set: function(val) {
            if (val != this._text) {
                this._text = val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    TTextBase.prototype.draw = function(reCreate) {
        _super.prototype.draw.call(this, reCreate);
    };
    TTextBase.prototype.create = function() {
        _super.prototype.create.call(this);
    };
    return TTextBase;
}(TComponent));
exports.TTextBase = TTextBase;
var TDBTextBase = /** @class */ (function(_super) {
    __extends(TDBTextBase, _super);
    /**
    @aOwner     Indicates the component that is responsible for streaming and freeing this component.Onwer must be TContainer
    @renderTo   (Optional) the id of the html element that will be the parent node for this component
    **/
    function TDBTextBase(aOwner, renderTo) {
        var _this = _super.call(this, aOwner, renderTo) || this;
        _this._rtl = false;
        return _this;
    }
    Object.defineProperty(TDBTextBase.prototype, "Rtl", {
        get: function() {
            return this._rtl;
        },
        set: function(val) {
            if (val != this._rtl) {
                this._rtl = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBTextBase.prototype, "TextColor", {
        get: function() {
            return this._textcolor;
        },
        set: function(val) {
            if (V.Application.checkColorString(val)) {
                if (val != this._textcolor) {
                    this._textcolor = val;
                    this.draw(true);
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBTextBase.prototype, "Dataset", {
        /**
         * Specifies the dataset that contains the field it represents.
         */
        get: function() {
            return this._dataset;
        },
        set: function(val) {
            var _this = this;
            val = this.checkDataset(val);
            if (val != this._dataset) {
                if (this._dataset != null) {
                    this._dataset.removeEventListener(TDataset.EVENT_DATA_CHANGED, this);
                    this._dataset.removeEventListener(TDataset.EVENT_SELECTION_CHANGED, this);
                    this._dataset.removeEventListener(TDataset.EVENT_STATE_CHANGED, this);
                }
                this._dataset = val;
                if (this._dataset != null) {
                    this._dataset.registerEventListener(TDataset.EVENT_DATA_CHANGED, this, function() { _this.draw(false); });
                    this._dataset.registerEventListener(TDataset.EVENT_SELECTION_CHANGED, this, function() { _this.draw(false); });
                    this._dataset.registerEventListener(TDataset.EVENT_STATE_CHANGED, this, function() { _this.draw(false); });
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBTextBase.prototype, "DataField", {
        /**
         * Specifies the field from which the edit control displays data.
         */
        get: function() {
            return this._datafield;
        },
        set: function(val) {
            if (val != this._datafield) {
                this._datafield = val.toUpperCase();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBTextBase.prototype, "DataValue", {
        get: function() {
            if (this.Dataset == null || this.Dataset.Active == false || this.Dataset.RecordCount <= 0)
                return null;
            if (this.DataField == null || this.DataField.toString() == "")
                return null;
            return this.Dataset.getFieldValue(this._datafield);
        },
        set: function(val) {
            if (this.Dataset == null || this.Dataset.Active == false)
                return;
            if (this.DataField == null || this.DataField.toString() == "")
                return;
            if (val != this._datafield) {
                this.Dataset.setFieldValue(this.DataField.toString(), val);
                this.draw(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    return TDBTextBase;
}(TComponent));
exports.TDBTextBase = TDBTextBase;