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
//var VXC = requirejs("./VXComponent");
//var V = require("./VCL");
//var VXU = require("./VXUtils");
var TLabeledBase = /** @class */ (function(_super) {
    __extends(TLabeledBase, _super);

    function TLabeledBase() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this._showCaption = false;
        _this._caption = "";
        _this._captionposition = exports.Application.LocaleSettings.CaptionPosition;
        return _this;
    }
    Object.defineProperty(TLabeledBase.prototype, "CaptionTextColor", {
        get: function() {
            return this._captiotextcolor;
        },
        set: function(val) {
            var isOk = /^#[0-9A-F]{6}$/i.test(val);
            if (!isOk)
                exports.Application.raiseException("'" + val + "' is not valid hex color string");
            else {
                if (val != this._captiotextcolor) {
                    this._captiotextcolor = val;
                    this.drawDelayed(true);
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TLabeledBase.prototype, "ShowCaption", {
        get: function() {
            return this._showCaption;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._showCaption) {
                this._showCaption = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TLabeledBase.prototype, "Caption", {
        get: function() {
            return this._caption;
        },
        set: function(val) {
            if (val != this._caption) {
                this._caption = val;
                this.ShowCaption = true;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TLabeledBase.prototype, "CaptionPosition", {
        get: function() {
            return this._captionposition;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._captionposition) {
                this._captionposition = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TLabeledBase.prototype.create = function() {
        _super.prototype.create.call(this);
        if (this.ShowCaption) {
            this.jLabel = $('<label/>');
            this.jLabel.addClass('control-label');
            if (this.jEdit)
                this.jLabel.attr('for', this.jEdit.attr('id'));
            this.jLabel.text(this.LocalizeText(this.Caption));
            if (this.CaptionTextColor)
                this.jComponent.css('color', this.CaptionTextColor);
            if (this.CaptionPosition == exports.CaptionPosition.TopLeft) {
                this.jComponent.prepend(this.jLabel);
            } else if (this.CaptionPosition == exports.CaptionPosition.TopCenter) {
                this.jLabel.addClass('text-center');
                this.jComponent.prepend(this.jLabel);
            } else if (this.CaptionPosition == exports.CaptionPosition.TopRight) {
                this.jLabel.addClass('text-right');
                this.jComponent.prepend(this.jLabel);
            } else if (this.CaptionPosition == exports.CaptionPosition.BottomLeft) {
                this.jComponent.append(this.jLabel);
            } else if (this.CaptionPosition == exports.CaptionPosition.Right) {
                this.jLabel.addClass('pull-right');
                this.jLabel.css('padding-top', '5px');
                this.jLabel.css('padding-left', '5px');
                this.jComponent.prepend(this.jLabel);
            } else if (this.CaptionPosition == exports.CaptionPosition.Left) {
                this.jLabel.addClass('pull-left');
                this.jLabel.css('padding-top', '5px');
                this.jLabel.css('padding-right', '5px');
                this.jComponent.prepend(this.jLabel);
            } else if (this.CaptionPosition == exports.CaptionPosition.BottomCenter) {
                this.jLabel.addClass('text-center');
                this.jComponent.append(this.jLabel);
            } else if (this.CaptionPosition == exports.CaptionPosition.BottomRight) {
                this.jLabel.addClass('text-right');
                this.jComponent.append(this.jLabel);
            }
        }
    };
    return TLabeledBase;
}(TComponent));
exports.TLabeledBase = TLabeledBase;
var TEditorBase = /** @class */ (function(_super) {
    __extends(TEditorBase, _super);

    function TEditorBase() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this._required = false;
        _this._localizable = false;
        _this._rtl = false;
        _this._helpVisible = false;
        _this._errortext = "";
        _this._helptext = "";
        return _this;
    }
    Object.defineProperty(TEditorBase.prototype, "Required", {
        get: function() {
            return this._required;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._required) {
                this._required = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TEditorBase.prototype, "MinLength", {
        get: function() {
            return this._minlength;
        },
        set: function(val) {
            if (val != this._minlength) {
                this._minlength = Math.floor(val);
                if (this._minlength < 0)
                    this._minlength = 0;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TEditorBase.prototype, "Localizable", {
        /**
         * In order to localize application each page or component of the application has to have Localizable property set true.
         */
        get: function() {
            return this._localizable;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._localizable) {
                this._localizable = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TEditorBase.prototype.textLength = function() {
        var c = this.jEdit.val();
        if (!c)
            return 0;
        if (c)
            c = c.trim();
        return c.length;
    };
    TEditorBase.prototype.isEmpty = function() {
        var c = this.jEdit.val();
        if (!c)
            return true;
        if (c)
            c = c.trim();
        return c == "";
    };
    TEditorBase.prototype.ShowErrorMessage = function(message) {
        this._errortext = message;
        this.HelpVisible = true;
        this.addClass("error");
        this.drawDelayed(true);
    };
    TEditorBase.prototype.HideErrorMessage = function() {
        this.removeClass("error");
        this._errortext = null;
        this.drawDelayed(true);
    };
    Object.defineProperty(TEditorBase.prototype, "Rtl", {
        get: function() {
            return this._rtl;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._rtl) {
                this._rtl = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TEditorBase.prototype, "TabIndex", {
        get: function() {
            return this._tabindex;
        },
        set: function(val) {
            if (val != this._tabindex) {
                this._tabindex = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TEditorBase.prototype, "HelpVisible", {
        get: function() {
            return this._helpVisible;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._helpVisible) {
                this._helpVisible = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TEditorBase.prototype, "HelpText", {
        get: function() {
            return this._helptext;
        },
        set: function(val) {
            if (val != this._helptext) {
                this._helptext = val;
                this.HelpVisible = true;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TEditorBase.prototype.setFocus = function() {
        if (this.jEdit)
            this.jEdit.focus();
    };
    TEditorBase.prototype.create = function() {
        if (this.HelpVisible || this._errortext) {
            this.jHelpLabel = $('<small/>');
            this.jHelpLabel.addClass('help-inline text-center');
            this.jHelpLabel.css('font-size', '12px');
            this.jHelpLabel.css('width', '100%');
            this.jComponent.append(this.jHelpLabel);
            this.jHelpLabel.text(this._errortext ? this._errortext : this.HelpText);
        }
        _super.prototype.create.call(this);
    };
    return TEditorBase;
}(TLabeledBase));
exports.TEditorBase = TEditorBase;
var TInputBase = /** @class */ (function(_super) {
    __extends(TInputBase, _super);

    function TInputBase() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this._inputStyle = exports.InputStyle.Default;
        _this._borderradius = null;
        _this._displayastext = false;
        _this._nullvaluetext = "\xa0";
        _this._labelOverflow = false;
        _this._textaligment = exports.TextAlignment.Left;
        _this._buttonicon = null;
        _this._buttonstyle = exports.ButtonStyle.Default;
        _this._buttonVisible = false;
        _this._buttontext = "";
        return _this;
    }
    Object.defineProperty(TInputBase.prototype, "InputStyle", {
        get: function() {
            return this._inputStyle;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._inputStyle) {
                this._inputStyle = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "BorderRadius", {
        get: function() {
            return this._borderradius;
        },
        set: function(val) {
            if (val != this._borderradius) {
                this._borderradius = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "MaxLength", {
        get: function() {
            return this._maxlength;
        },
        set: function(val) {
            if (val != this._maxlength) {
                this._maxlength = Math.floor(val);
                if (this._maxlength < 0)
                    this._maxlength = 0;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "DisplayAsText", {
        get: function() {
            return this._displayastext;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._displayastext) {
                this._displayastext = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "NullValueText", {
        /**
         * Specified text is displayed when the value of the editor is null and the editor is not focused.
         * The prompt text disappears when the editor receives focus
         */
        get: function() {
            return this._nullvaluetext;
        },
        set: function(val) {
            if (val != this._nullvaluetext) {
                this._nullvaluetext = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "LabelOverflow", {
        get: function() {
            return this._labelOverflow;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._labelOverflow) {
                this._labelOverflow = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TInputBase.prototype.canEdit = function() {
        return true;
    };
    TInputBase.prototype.create = function() {
        var _this = this;
        this.jComponent.empty();
        this.jComponent = VXU.VXUtils.changeJComponentType(this.jComponent, 'div', this.FitToWidth, this.FitToHeight);
        this.jComponent.addClass('control-group');
        if (this.InputStyle == exports.InputStyle.Error)
            this.jComponent.addClass("error");
        else if (this.InputStyle == exports.InputStyle.Warning)
            this.jComponent.addClass("warning");
        if (this.InputStyle == exports.InputStyle.Success)
            this.jComponent.addClass("success");
        if (this.InputStyle == exports.InputStyle.Info)
            this.jComponent.addClass("info");
        if (!this.LabelOverflow)
            this.jComponent.css('overflow', 'hidden');
        this.jinternalSpan = $("<span>").css('display', 'block').css('overflow', 'hidden');
        if (!this._displayastext)
            this.jinternalSpan.css('padding-right', '15px');
        if (this._displayastext) {
            if (this.TextStyle == exports.TextStyle.h1)
                this.jEdit = $('<h1/>').addClass('Label-Input');
            else if (this.TextStyle == exports.TextStyle.h2)
                this.jEdit = $('<h2/>').addClass('Label-Input');
            else if (this.TextStyle == exports.TextStyle.h3)
                this.jEdit = $('<h3/>').addClass('Label-Input');
            else if (this.TextStyle == exports.TextStyle.h4)
                this.jEdit = $('<h4/>').addClass('Label-Input');
            else if (this.TextStyle == exports.TextStyle.h5)
                this.jEdit = $('<h5/>').addClass('Label-Input');
            else if (this.TextStyle == exports.TextStyle.h6)
                this.jEdit = $('<h6/>').addClass('Label-Input');
            else if (this.TextStyle == exports.TextStyle.strong)
                this.jEdit = $('<strong/>').addClass('Label-Input');
            else if (this.TextStyle == exports.TextStyle.lead) {
                this.jEdit = $('<label/>').addClass('Label-Input');
                this.jEdit.addClass('lead');
            } else if (this.TextStyle == exports.TextStyle.small)
                this.jEdit = $('<small/>').addClass('Label-Input');
            else
                this.jEdit = $('<label/>').addClass('Label-Input');
        } else if (this.textarea) {
            this.jEdit = $('<textarea/>').attr('rows', this.Rows).css('resize', 'none');
            this.jEdit.attr("type", 'text');
            if (this.Wrap)
                this.jEdit.attr('Wrap', 'Wrap');
        } else {
            this.jEdit = $('<input/>').css('margin-bottom', '0px');
            this.jEdit.attr("type", 'text');
            if (this.TabIndex)
                this.jEdit.attr('tabindex', this.TabIndex);
        }
        if (this.BorderRadius)
            this.jEdit.css('border-radius', this.BorderRadius + 'px').css('-webkit-border-radius', this.BorderRadius + 'px');
        this.jEdit.attr('id', exports.Application.genGUID()).css('width', '100%');
        if (this._readonly)
            this.jEdit.attr("readonly", "readonly");
        this.jImage = $('<i/>');
        this.jbtnText = $('<span/>');
        if (!this.Enabled || (this.isEditable && !this.isEditable()))
            this.jEdit.attr("disabled", "disabled");
        if (this.Password)
            this.jEdit.attr("type", "Password");
        if (this.TextAlignment == exports.TextAlignment.Left)
            this.jEdit.css('text-align', 'left');
        if (this.TextAlignment == exports.TextAlignment.Right)
            this.jEdit.css('text-align', 'right');
        if (this.TextAlignment == exports.TextAlignment.Center)
            this.jEdit.css('text-align', 'center');
        if (this.MaxLength > 0)
            this.jEdit.attr("maxlength", this.MaxLength);
        if (this.Placeholder != null && !this._displayastext)
            this.jEdit.attr("placeholder", this.Placeholder);
        if (this.Rtl == true)
            this.jEdit.attr("dir", "RTL");
        this.jinternalSpan.append(this.jEdit);
        this.jComponent.addClass('input-append');
        if (this.ButtonVisible && !this._displayastext) {
            this.jBtn = $('<button/>').attr('tab-index', '-1').css('outline', 'none');
            this.jBtn.addClass('btn');
            this.jBtn.attr('type', "button").css('float', 'right');
            switch (this.ButtonStyle) {
                case exports.ButtonStyle.Default:
                    break;
                case exports.ButtonStyle.Primary:
                    this.jBtn.addClass("btn-primary");
                    break;
                case exports.ButtonStyle.Info:
                    this.jBtn.addClass("btn-info");
                    break;
                case exports.ButtonStyle.Success:
                    this.jBtn.addClass("btn-success");
                    break;
                case exports.ButtonStyle.Warning:
                    this.jBtn.addClass("btn-warning");
                    break;
                case exports.ButtonStyle.Danger:
                    this.jBtn.addClass("btn-danger");
                    break;
                case exports.ButtonStyle.Link:
                    this.jBtn.addClass("btn-link");
                    break;
            }
            if (this.ButtonIcon != null) {
                this.jImage.addClass(exports.iconEnumToBootstrapStyle(this.ButtonIcon));
                this.jImage.appendTo(this.jBtn);
                if (this.ButtonText != "")
                    this.jImage.css('margin-right', '6px');
                this.jbtnText.text(this.ButtonText);
            } else if (this.ButtonText == "")
                this.jbtnText.text(".");
            else
                this.jbtnText.text(this.ButtonText);
            if (!this.Enabled)
                this.jBtn.addClass("disabled");
            this.jbtnText.appendTo(this.jBtn);
            this.jBtn.off("click").click(function() {
                if (_this.onButtonClicked != null)
                    (exports.tryAndCatch(function() { _this.onButtonClicked(); }));
                return false;
            });
            this.jComponent.append(this.jBtn);
        }
        this.jComponent.append(this.jinternalSpan);
        _super.prototype.create.call(this);
    };
    Object.defineProperty(TInputBase.prototype, "TextAlignment", {
        get: function() {
            return this._textaligment;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._textaligment) {
                this._textaligment = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "Placeholder", {
        /**
         * The placeholder attribute specifies a short hint that describes the expected value of an input field
         * (e.g. a sample value or a short description of the expected format).
         */
        get: function() {
            return this._placeholder;
        },
        set: function(val) {
            if (val != this._placeholder) {
                this._placeholder = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "Password", {
        get: function() {
            return this._password;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._password) {
                this._password = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "ButtonIcon", {
        get: function() {
            return this._buttonicon;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._buttonicon) {
                this._buttonicon = val;
                this.ButtonVisible = true;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "ButtonStyle", {
        get: function() {
            return this._buttonstyle;
        },
        set: function(val) {
            val = exports.convertaAnyToNumber(val);
            if (val != this._buttonstyle) {
                this._buttonstyle = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "ButtonVisible", {
        get: function() {
            return this._buttonVisible;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._buttonVisible) {
                this._buttonVisible = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TInputBase.prototype, "ButtonText", {
        get: function() {
            if (this._buttontext == null)
                return "";
            return this._buttontext;
        },
        set: function(val) {
            if (val != this._buttontext) {
                this._buttontext = val;
                this.ButtonVisible = true;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    return TInputBase;
}(TEditorBase));
exports.TInputBase = TInputBase;