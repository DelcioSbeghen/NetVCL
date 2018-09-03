"use strict";
var __extends = (this && this.__extends) || (function() {
    var extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] }
            instanceof Array && function(d, b) { d.__proto__ = b; }) ||
        function(d, b) { for (var p in b)
                if (b.hasOwnProperty(p)) d[p] = b[p]; };
    return function(d, b) {
        extendStatics(d, b);

        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
exports.__esModule = true;
//var VXB = require("./VXInputBase");
//var VXD = require("./VXDataset");
//var VXM = require("./VXMenu");
//var V = require("./VCL");
//var VXO = require("./VXObject");
var TInput = /** @class */ (function(_super) {
    __extends(TInput, _super);

    function TInput() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this._buttonclickonenter = false;
        return _this;
    }
    Object.defineProperty(TInput.prototype, "Text", {
        get: function() {
            return this._text;
        },
        set: function(val) {
            if (val != this._text) {
                this._text = val;
                this.drawDelayed(false);
                //if (this.onChanged) this.onChanged(this);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInput.prototype, "ButtonClickOnEnter", {
        get: function() {
            return this._buttonclickonenter;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._buttonclickonenter) {
                this._buttonclickonenter = val;
                this.drawDelayed(false);
                //if (this.onChanged) this.onChanged(this);
            }
        },
        enumerable: true,
        configurable: true
    });
    TInput.prototype.create = function() {
        var _this = this;
        var self = this;
        _super.prototype.create.call(this);
        this.jEdit.on("propertychange input paste", function() {
            _this.Text = _this.jEdit.val();
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(self); }));
        });
        this.jEdit.keyup(function(event) {
            _this.Text = _this.jEdit.val();
            if (_this.onKeyUp != null)
                (exports.tryAndCatch(function() { _this.onKeyUp(event.char); }));
        });
        this.jEdit.keypress(function(event) {
            if (_this.ButtonClickOnEnter && event.charCode == 13 && _this.jBtn)
                _this.jBtn.click();
            var rc = true;
            if (_this.onKeyDown != null)
                (exports.tryAndCatch(function() {
                    var key = event.which;
                    if (event.char)
                        rc = _this.onKeyDown(event.char);
                    else if (key) {
                        var letter = String.fromCharCode(key);
                        rc = _this.onKeyDown(letter.replace('\r', '\n'));
                    }
                    //rc = this.onKeyDown(event.char ? event.char : String.fromCharCode(key));
                }));
            return rc;
        });
    };
    TInput.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        if (this.jEdit.val() != this.Text) {
            if (this.DisplayAsText)
                this.jEdit.text(this.Text ? this.Text : this.NullValueText);
            else
                this.jEdit.val(this.Text);
        }
    };
    return TInput;
}(exports.TInputBase));
exports.TInput = TInput;
var TDBInput = /** @class */ (function(_super) {
    __extends(TDBInput, _super);
    /**
        @aOwner     Indicates the component that is responsible for streaming and freeing this component.Onwer must be TContainer
        @renderTo   (Optional) the id of the html element that will be the parent node for this component
        @dataset
        @datafield
    **/
    function TDBInput(aOwner, renderTo, dataset, dataField) {
        var _this = _super.call(this, aOwner, renderTo) || this;
        _this._buttonclickonenter = false;
        if (dataset)
            _this.Dataset = dataset;
        if (dataField)
            _this.DataField = dataField;
        return _this;
    }
    Object.defineProperty(TDBInput.prototype, "Dataset", {
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
                if (this._dataset) {
                    this._dataset.removeEventListener(exports.TDataset.EVENT_DATA_CHANGED, this);
                    this._dataset.removeEventListener(exports.TDataset.EVENT_SELECTION_CHANGED, this);
                    this._dataset.removeEventListener(exports.TDataset.EVENT_STATE_CHANGED, this);
                }
                this._dataset = val;
                if (this._dataset) {
                    this._dataset.registerEventListener(exports.TDataset.EVENT_DATA_CHANGED, this, function() { _this.draw(false); });
                    this._dataset.registerEventListener(exports.TDataset.EVENT_SELECTION_CHANGED, this, function() { _this.draw(false); });
                    this._dataset.registerEventListener(exports.TDataset.EVENT_STATE_CHANGED, this, function() { _this.draw(true); });
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    TDBInput.prototype.isEditable = function() {
        if (this.Dataset == null || this.Dataset.Readonly)
            return false;
        return this.Dataset.Active;
    };
    Object.defineProperty(TDBInput.prototype, "Field", {
        /**
         * Specifies the field from which the edit control displays data.(same as DataField)
         */
        get: function() {
            return this._datafield;
        },
        set: function(val) {
            this.DataField = val;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBInput.prototype, "DataField", {
        /**
         * Specifies the field from which the edit control displays data.(same as Field)
         */
        get: function() {
            return this._datafield;
        },
        set: function(val) {
            if (val != this._datafield) {
                if (this.owner && val && val.split(',').length == 2) {
                    var dsName = val.split(',')[0].toLowerCase();
                    var fldName = val.split(',')[1];
                    var ds;
                    for (var item in this.owner) {
                        if (item && item.toLowerCase() == dsName) {
                            ds = this.owner[item];
                            break;
                        }
                    }
                    if (!ds) {
                        exports.Application.raiseException("data source " + this.owner[val.split(',')[0]] + " was not found");
                        throw "data source " + this.owner[val.split(',')[0]] + " was not found";
                    }
                    this.Dataset = ds;
                    this._datafield = fldName.toUpperCase();
                } else
                    this._datafield = val.toUpperCase();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBInput.prototype, "DataValue", {
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
            this.Dataset.setFieldValue(this.DataField.toString(), val);
            this.drawDelayed(false);
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBInput.prototype, "ImmidiatePost", {
        get: function() {
            return this._immidiatepost;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._immidiatepost) {
                this._immidiatepost = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBInput.prototype, "ButtonClickOnEnter", {
        get: function() {
            return this._buttonclickonenter;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._buttonclickonenter) {
                this._buttonclickonenter = val;
                this.drawDelayed(false);
                //if (this.onChanged) this.onChanged(this);
            }
        },
        enumerable: true,
        configurable: true
    });
    TDBInput.prototype.create = function() {
        var _this = this;
        _super.prototype.create.call(this);
        if (!this.Dataset)
            this.Dataset = this.guessDataset();
        var self = this;
        this.jEdit.on("change paste", function() {
            _this.DataValue = _this.jEdit.val();
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(self); }));
        });
        this.jEdit.keyup(function(event) {
            _this.DataValue = _this.jEdit.val();
            if (_this.onKeyUp != null)
                (exports.tryAndCatch(function() { _this.onKeyUp(event.char); }));
        });
        this.jEdit.keypress(function(event) {
            if (_this.ButtonClickOnEnter && event.charCode == 13 && _this.jBtn)
                _this.jBtn.click();
            var rc = true;
            if (_this.onKeyDown != null)
                (exports.tryAndCatch(function() {
                    var key = event.which;
                    rc = _this.onKeyDown(event.char ? event.char : String.fromCharCode((96 <= key && key <= 105) ? key - 48 : key));
                }));
            return rc;
        });
        if (this.ImmidiatePost)
            this.jEdit.keyup(function() {
                _this.DataValue = _this.jEdit.val();
                if (_this.onChanged != null)
                    (exports.tryAndCatch(function() { _this.onChanged(self); }));
            });
    };
    TDBInput.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        if (reCreate || !this.initialized) {
            _super.prototype.draw.call(this, reCreate);
        }
        if (this.jEdit.val() != this.DataValue) {
            if (this.DisplayAsText)
                this.jEdit.text(this.DataValue ? this.DataValue : this.NullValueText);
            else
                this.jEdit.val(this.DataValue);
        }
    };
    return TDBInput;
}(exports.TInputBase));
exports.TDBInput = TDBInput;
var TDBLabeledText = /** @class */ (function(_super) {
    __extends(TDBLabeledText, _super);
    /**
        @aOwner     Indicates the component that is responsible for streaming and freeing this component.Onwer must be TContainer
        @renderTo   (Optional) the id of the html element that will be the parent node for this component
        @dataset
        @datafield
    **/
    function TDBLabeledText(aOwner, renderTo, dataset, dataField) {
        var _this = _super.call(this, aOwner, renderTo, dataset, dataField) || this;
        _this._textstyle = exports.TextStyle.Default;
        _this.DisplayAsText = true;
        return _this;
    }
    Object.defineProperty(TDBLabeledText.prototype, "TextStyle", {
        get: function() {
            return this._textstyle;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val && typeof val == "string") {
                switch (val.toString().toUpperCase()) {
                    case "H1":
                        val = exports.TextStyle.h1;
                        break;
                    case "H2":
                        val = exports.TextStyle.h2;
                        break;
                    case "H3":
                        val = exports.TextStyle.h3;
                        break;
                    case "H4":
                        val = exports.TextStyle.h4;
                        break;
                    case "H5":
                        val = exports.TextStyle.h5;
                        break;
                    case "H6":
                        val = exports.TextStyle.h6;
                        break;
                    case "LEAD":
                        val = exports.TextStyle.lead;
                        break;
                    case "SMALL":
                        val = exports.TextStyle.small;
                        break;
                    case "STRONG":
                        val = exports.TextStyle.strong;
                        break;
                    default:
                        val = exports.TextStyle.Default;
                        break;
                }
            }
            if (val != this._textstyle) {
                this._textstyle = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    return TDBLabeledText;
}(TDBInput));
exports.TDBLabeledText = TDBLabeledText;
var TInputNumeric = /** @class */ (function(_super) {
    __extends(TInputNumeric, _super);
    /**
    @aOwner     Indicates the component that is responsible for streaming and freeing this component.Onwer must be TContainer
    @renderTo   (Optional) the id of the html element that will be the parent node for this component
    **/
    function TInputNumeric(aOwner, renderTo) {
        var _this = _super.call(this, aOwner, renderTo) || this;
        _this._value = 0;
        _this._maxvalue = 999999999999;
        _this._minvalue = -999999999999;
        _this._step = 1;
        _this._precision = 0;
        _this.TextAlignment = exports.TextAlignment.Right;
        _this.ButtonVisible = true;
        return _this;
    }
    Object.defineProperty(TInputNumeric.prototype, "Value", {
        get: function() {
            if (!this._value)
                return null;
            return parseFloat((parseFloat(this._value.toString())).toFixed(this.Precision));
        },
        set: function(val) {
            val = Number(val);
            if (val != this._value) {
                if (val > this.MaxValue)
                    val = this.MaxValue;
                if (val < this.MinValue)
                    val = this.MinValue;
                this._value = val == "" ? null : val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputNumeric.prototype, "MaxValue", {
        get: function() {
            return this._maxvalue;
        },
        set: function(val) {
            val = Number(val);
            if (val != this._maxvalue) {
                this._maxvalue = val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputNumeric.prototype, "MinValue", {
        get: function() {
            return this._minvalue;
        },
        set: function(val) {
            val = Number(val);
            if (val != this._minvalue) {
                this._minvalue = val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputNumeric.prototype, "Step", {
        get: function() {
            return this._step;
        },
        set: function(val) {
            val = Number(val);
            if (val != this._step) {
                this._step = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputNumeric.prototype, "Precision", {
        get: function() {
            return this._precision;
        },
        set: function(val) {
            val = Number(val);
            if (val != this._precision) {
                this._precision = val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    TInputNumeric.prototype.create = function() {
        var _this = this;
        _super.prototype.create.call(this);
        var self = this;
        if (this.jBtn)
            this.jBtn.hide();
        this.jComponent.attr('data-trigger', "spinner").addClass('spinner');
        //draw the component
        var addon = $("<div>").addClass('add-on');
        var btdown = $("<a>").css('outline', 'none').attr('data-spin', "down").addClass('spin-down').attr('tabindex', '-1').append('<i class="icon-sort-down"/>');
        btdown.off('click').click(function() {
            if (!_this.Enabled)
                return;
            _this.Value -= _this.Step;
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(_this); }));
        });
        var btUp = $("<a>").css('outline', 'none').attr('data-spin', "up").addClass('spin-up').attr('tabindex', '-1').append('<i class="icon-sort-up"/>');
        btUp.off('click').click(function() {
            if (!_this.Enabled)
                return;
            _this.Value += _this.Step;
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(_this); }));
        });
        this.jEdit.css('clear', 'none');
        this.jEdit.change(function() {
            var _value = _this.jEdit.val();
            self.Value = _value;
            if (self.Value != _value)
                _this.jEdit.val(self.Value ? self.Value.toString() : null);
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(_this); }));
        });
        this.jEdit.keyup(function() {
            var _this = this;
            var val = this.value.replace(/[^0-9\.-]/g, '');
            if (val != this.value) {
                this.value = this.value.replace(/[^0-9\.-]/g, '');
                if (this.onChanged != null)
                    (exports.tryAndCatch(function() { _this.onChanged(_this); }));
            }
        });
        if (this.ButtonVisible) {
            addon.css('float', 'right');
            this.jComponent.addClass('input-append');
            addon.append(btUp);
            addon.append(btdown);
            this.jinternalSpan.before(addon);
        }
    };
    TInputNumeric.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        if (this.DisplayAsText)
            this.jEdit.text(this.Value ? this.Value.toString() : this.NullValueText);
        else
            this.jEdit.val(this.Value ? this.Value.toString() : "");
    };
    return TInputNumeric;
}(exports.TInputBase));
exports.TInputNumeric = TInputNumeric;
var TDBInputNumeric = /** @class */ (function(_super) {
    __extends(TDBInputNumeric, _super);
    /**
    @aOwner     Indicates the component that is responsible for streaming and freeing this component.Onwer must be TContainer
    @renderTo   (Optional) the id of the html element that will be the parent node for this component
    **/
    function TDBInputNumeric(aOwner, renderTo) {
        var _this = _super.call(this, aOwner, renderTo) || this;
        _this._step = 1;
        _this._precision = 0;
        _this.TextAlignment = exports.TextAlignment.Right;
        return _this;
    }
    Object.defineProperty(TDBInputNumeric.prototype, "Dataset", {
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
                if (this._dataset) {
                    this._dataset.removeEventListener(exports.TDataset.EVENT_DATA_CHANGED, this);
                    this._dataset.removeEventListener(exports.TDataset.EVENT_SELECTION_CHANGED, this);
                    this._dataset.removeEventListener(exports.TDataset.EVENT_STATE_CHANGED, this);
                }
                this._dataset = val;
                if (this._dataset) {
                    this._dataset.registerEventListener(exports.TDataset.EVENT_DATA_CHANGED, this, function() { _this.draw(false); });
                    this._dataset.registerEventListener(exports.TDataset.EVENT_SELECTION_CHANGED, this, function() { _this.draw(false); });
                    this._dataset.registerEventListener(exports.TDataset.EVENT_STATE_CHANGED, this, function() { _this.draw(true); });
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    TDBInputNumeric.prototype.isEditable = function() {
        if (this.Dataset == null || this.Dataset.Readonly)
            return false;
        return this.Dataset.Active;
    };
    Object.defineProperty(TDBInputNumeric.prototype, "DataField", {
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
    Object.defineProperty(TDBInputNumeric.prototype, "DataValue", {
        get: function() {
            if (this.Dataset == null || this.Dataset.Active == false || this.Dataset.RecordCount <= 0)
                return null;
            if (this.DataField == null || this.DataField.toString() == "")
                return null;
            var rc = parseFloat(this.Dataset.getFieldValue(this._datafield));
            if (rc)
                return rc;
            else
                return 0;
        },
        set: function(val) {
            var val1 = parseFloat(val.toString());
            if (this.Dataset == null || this.Dataset.Active == false)
                return;
            if (this.DataField == null || this.DataField.toString() == "")
                return;
            this.Dataset.setFieldValue(this.DataField.toString(), val1);
            this.draw(false);
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBInputNumeric.prototype, "Step", {
        get: function() {
            return this._step;
        },
        set: function(val) {
            val = Number(val);
            if (val != this._step) {
                this._step = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBInputNumeric.prototype, "Precision", {
        get: function() {
            return this._precision;
        },
        set: function(val) {
            val = Number(val);
            if (val != this._precision) {
                this._precision = val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    TDBInputNumeric.prototype.create = function() {
        var _this = this;
        if (!this.Dataset)
            this.Dataset = this.guessDataset();
        _super.prototype.create.call(this);
        this.jComponent.attr('data-trigger', "spinner").addClass('spinner');
        //draw the component
        var addon = $("<div>").addClass('add-on');
        var btdown = $("<a>").css('outline', 'none').attr('data-spin', "down").addClass('spin-down').attr('tabindex', '-1').append('<i class="icon-sort-down"/>');
        btdown.off('click').click(function() {
            _this.DataValue -= _this.Step;
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(_this); }));
        });
        var btUp = $("<a>").css('outline', 'none').attr('data-spin', "up").addClass('spin-up').attr('tabindex', '-1').append('<i class="icon-sort-up"/>');
        btUp.off('click').click(function() {
            _this.DataValue += _this.Step;
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(_this); }));
        });
        this.jEdit.css('clear', 'none');
        this.jEdit.change(function() {
            _this.DataValue = _this.jEdit.val();
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(_this); }));
        });
        this.jEdit.keyup(function() {
            var _this = this;
            var val = this.value.replace(/[^0-9\.]/g, '');
            if (val != this.value) {
                this.value = this.value.replace(/[^0-9\.]/g, '');
                if (this.onChanged != null)
                    (exports.tryAndCatch(function() { _this.onChanged(_this); }));
            }
        });
        addon.css('float', 'right');
        addon.append(btUp);
        addon.append(btdown);
        this.jinternalSpan.before(addon);
    };
    TDBInputNumeric.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        if (this.DisplayAsText)
            this.jEdit.text(this.DataValue ? this.DataValue.toString() : this.NullValueText);
        else
            this.jEdit.val(this.DataValue ? this.DataValue.toString() : "");
    };
    return TDBInputNumeric;
}(exports.TInputBase));
exports.TDBInputNumeric = TDBInputNumeric;
var TTextArea = /** @class */ (function(_super) {
    __extends(TTextArea, _super);

    function TTextArea() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.textarea = true;
        _this._wrap = false;
        _this._readonly = false;
        _this._rows = 2;
        return _this;
    }
    Object.defineProperty(TTextArea.prototype, "Text", {
        get: function() {
            return this._text;
        },
        set: function(val) {
            if (val != this._text) {
                this._text = val;
                this.draw(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTextArea.prototype, "Wrap", {
        get: function() {
            return this._wrap;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._wrap) {
                this._wrap = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTextArea.prototype, "Readonly", {
        get: function() {
            return this._readonly;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._readonly) {
                this._readonly = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTextArea.prototype, "Rows", {
        get: function() {
            return this._rows;
        },
        set: function(val) {
            if (val != this._rows) {
                this._rows = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TTextArea.prototype.create = function() {
        var _this = this;
        var self = this;
        _super.prototype.create.call(this);
        this.jEdit.on("change paste", function() {
            _this.Text = _this.jEdit.val();
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(self); }));
        });
        this.jEdit.keyup(function(event) {
            _this.Text = _this.jEdit.val();
            if (_this.onKeyUp != null)
                (exports.tryAndCatch(function() { _this.onKeyUp(event.char); }));
        });
    };
    TTextArea.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        if (this.jEdit.val() != this.Text) {
            if (this.DisplayAsText)
                this.jEdit.text(this.Text ? this.Text : this.NullValueText);
            this.jEdit.val(this.Text);
        }
    };
    return TTextArea;
}(exports.TInputBase));
exports.TTextArea = TTextArea;
var TDBTextArea = /** @class */ (function(_super) {
    __extends(TDBTextArea, _super);

    function TDBTextArea() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.textarea = true;
        _this._wrap = false;
        _this._rows = 2;
        return _this;
    }
    Object.defineProperty(TDBTextArea.prototype, "Dataset", {
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
                if (this._dataset) {
                    this._dataset.removeEventListener(exports.TDataset.EVENT_DATA_CHANGED, this);
                    this._dataset.removeEventListener(exports.TDataset.EVENT_SELECTION_CHANGED, this);
                    this._dataset.removeEventListener(exports.TDataset.EVENT_STATE_CHANGED, this);
                }
                this._dataset = val;
                if (this._dataset) {
                    this._dataset.registerEventListener(exports.TDataset.EVENT_DATA_CHANGED, this, function() { _this.draw(false); });
                    this._dataset.registerEventListener(exports.TDataset.EVENT_SELECTION_CHANGED, this, function() { _this.draw(false); });
                    this._dataset.registerEventListener(exports.TDataset.EVENT_STATE_CHANGED, this, function() { _this.validateEnabled(); });
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBTextArea.prototype, "Wrap", {
        get: function() {
            return this._wrap;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._wrap) {
                this._wrap = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBTextArea.prototype, "Rows", {
        get: function() {
            return this._rows;
        },
        set: function(val) {
            if (val != this._rows) {
                this._rows = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TDBTextArea.prototype.validateEnabled = function() {
        if (this.Dataset == null)
            this.Enabled = false;
        else if (this.Dataset.Readonly)
            this.Enabled = false;
        else
            this.Enabled = this.Dataset.Active;
    };
    Object.defineProperty(TDBTextArea.prototype, "DataField", {
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
    Object.defineProperty(TDBTextArea.prototype, "DataValue", {
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
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TDBTextArea.prototype, "ImmidiatePost", {
        get: function() {
            return this._immidiatepost;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._immidiatepost) {
                this._immidiatepost = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TDBTextArea.prototype.create = function() {
        var _this = this;
        _super.prototype.create.call(this);
        var self = this;
        this.jEdit.on("change paste", function() {
            _this.DataValue = _this.jEdit.val();
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(self); }));
        });
        this.jEdit.keyup(function(event) {
            _this.DataValue = _this.jEdit.val();
            if (_this.onKeyUp != null)
                (exports.tryAndCatch(function() { _this.onKeyUp(event.char); }));
        });
        if (this.ImmidiatePost)
            this.jEdit.keyup(function() {
                _this.DataValue = _this.jEdit.val();
                if (_this.onChanged != null)
                    (exports.tryAndCatch(function() { _this.onChanged(self); }));
            });
    };
    TDBTextArea.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        if (reCreate || !this.initialized) {
            this.validateEnabled();
            _super.prototype.draw.call(this, reCreate);
        }
        if (this.jEdit.val() != this.DataValue) {
            if (this.DisplayAsText)
                this.jEdit.text(this.DataValue ? this.DataValue : this.NullValueText);
            else
                this.jEdit.val(this.DataValue);
        }
    };
    return TDBTextArea;
}(exports.TInputBase));
exports.TDBTextArea = TDBTextArea;
var TTypeaHeadItem = /** @class */ (function(_super) {
    __extends(TTypeaHeadItem, _super);

    function TTypeaHeadItem() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    Object.defineProperty(TTypeaHeadItem.prototype, "ForeColorValue", {
        get: function() {
            return this._foreColorValue;
        },
        set: function(val) {
            if (val != this._foreColorValue) {
                if (val && val.trim().length > 0) {
                    var isOk = /^#[0-9A-F]{6}$/i.test(val);
                    if (!isOk) {
                        exports.Application.raiseException("'" + val + "' is not valid hex color string");
                    } else {
                        this._foreColorValue = val;
                    }
                } else {
                    this._foreColorValue = "";
                }
                if (this.OwnerCollection != null)
                    this.OwnerCollection.refresh();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TTypeaHeadItem.prototype, "BackgroundColorValue", {
        get: function() {
            return this._backgroundColorValue;
        },
        set: function(val) {
            if (val != this._backgroundColorValue) {
                if (val && val.trim().length > 0) {
                    var isOk = /^#[0-9A-F]{6}$/i.test(val);
                    if (!isOk) {
                        exports.Application.raiseException("'" + val + "' is not valid hex color string");
                    } else {
                        this._backgroundColorValue = val;
                    }
                } else {
                    this._backgroundColorValue = "";
                }
                if (this.OwnerCollection != null)
                    this.OwnerCollection.refresh();
            }
        },
        enumerable: true,
        configurable: true
    });
    return TTypeaHeadItem;
}(exports.TMenuItem));
exports.TTypeaHeadItem = TTypeaHeadItem;
var TInputTypeaHead = /** @class */ (function(_super) {
    __extends(TInputTypeaHead, _super);

    function TInputTypeaHead() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this._highlightMatchedText = true;
        _this.Items = new exports.TCollection();
        return _this;
    }
    Object.defineProperty(TInputTypeaHead.prototype, "Text", {
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
    Object.defineProperty(TInputTypeaHead.prototype, "HighlightMatchedText", {
        get: function() {
            return this._highlightMatchedText;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._highlightMatchedText) {
                this._highlightMatchedText = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TInputTypeaHead.prototype.createItem = function(text, foreColorValue, backgroundColorValue) {
        var item = new TTypeaHeadItem();
        item.ForeColorValue = foreColorValue;
        item.BackgroundColorValue = backgroundColorValue;
        item.Text = text;
        this.Items.add(item);
    };
    TInputTypeaHead.prototype.create = function() {
        var _this = this;
        var self = this;
        _super.prototype.create.call(this);
        if (this.Items.length() > 0) {
            var strArr = new Array();
            this.Items.forEach(function(item) {
                if (item && item.Text.trim().length > 0) {
                    strArr.push(item.Text.trim());
                }
            });
            //Add the final string elements and the data-source attribute
            var x = {
                minLength: 1,
                source: function(query, process) {
                    return strArr;
                },
                highlighter: function(item) {
                    /*if (allLink != undefined) {
                        if (item == allLink.text) return '<strong>' + item + '</strong>';
                    }*/
                    var query = this.query.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, '\\$&');
                    return item.replace(new RegExp('(' + query + ')', 'ig'), function($1, match) {
                        return '<strong>' + match + '</strong>';
                    });
                },
                updater: function(item) {
                    return item;
                }
            };
            if (strArr.length > 0) {
                this.jEdit.typeahead(x).data('typeahead').render = function(items) {
                    var that = this;
                    items = $(items).map(function(i, item) {
                        i = $(that.options.item).attr('data-value', item);
                        var xhtml = i.find('a').html(that.highlighter(item));
                        xhtml = self.addColorToItem(xhtml, item);
                        return i[0];
                    });
                    items.first().addClass('active');
                    this.$menu.html(items);
                    return this;
                };
            }
        }
        this.jEdit.on("change keyup paste", function() {
            _this.Text = _this.jEdit.val();
            if (_this.onChanged != null)
                (exports.tryAndCatch(function() { _this.onChanged(self); }));
        });
    };
    TInputTypeaHead.prototype.addColorToItem = function(xhtml, item) {
        this.Items.forEach(function(itm) {
            if (itm && itm.Text == item) {
                if (itm.ForeColorValue && itm.ForeColorValue.trim().length > 0) {
                    xhtml = xhtml.css("color", itm.ForeColorValue);
                }
                if (itm.BackgroundColorValue && itm.BackgroundColorValue.trim().length > 0) {
                    xhtml = xhtml.css("background-color", itm.BackgroundColorValue);
                }
            }
        });
        return xhtml;
    };
    TInputTypeaHead.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        if (this.jEdit.val() != this.Text) {
            if (this.DisplayAsText)
                this.jEdit.text(this.Text ? this.Text : this.NullValueText);
            else
                this.jEdit.val(this.Text);
        }
    };
    return TInputTypeaHead;
}(exports.TInputBase));
exports.TInputTypeaHead = TInputTypeaHead;