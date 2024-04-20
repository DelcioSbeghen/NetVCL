import { TNvBsGridControl, TBsCustomControl } from "./nv.bs.controls.js";
import { TSubProperty, ChangeObjProps, BrowserDateFormat } from "../nv.classes.js";
import { TNvBsGridContainer } from "./nv.bs.containers.js";


export class TNvBsCustomImput extends TNvBsGridControl {
    _DefaultParams(o) {
        o.ClassCss ??= "form-group";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        if (!this.FInput)
            this.FInput = $(document.createElement("input"))
                .addClass("form-control");
        super._CreateParams(o);
        this.FInput.attr("id", this.FId + "_i");
        this.FLabel = $(document.createElement("label"))
            .attr("for", this.FId + "_i")
            .appendTo(this.FEl);
        this.FMaxLength = 0;
        this.FReadOnly = false;
        this.FValue = "";
        this.FDelayedChange = -1;
        this.FChangeTimer;
    }

    _AttachEvents(E) {
        super._AttachEvents(E);
        this.FInput.off("change.nvjs").on("change.nvjs", (e) => { this._DoValueChanged(e) })

    }

    _DoTextChange(T) {
        if (this.FText !== T) {
            this.FLabel.html(T.htmlEscape());
            this.FText = T;
        }
    }

    _UpdateBackground(el) {
        super._UpdateBackground(this.FInput);
    }

    _UpdateShadow(el) {
        super._UpdateShadow(this.FInput);
    }

    _DoUpdateValue() {
        this.FInput.val(this.FValue);
    }

    _DoValueChanged(e) {
        this.FValue = $(e.target).val();
        App.QueueChange(this.FId, "Value", this.FValue);
    }

    _DoEnabledChange() {
        this.FInput.prop("disabled", !this.FEnabled);
    }


    get MaxLength() { return this.FMaxLength }
    set MaxLength(V) {
        if (V !== this.FMaxLength) {
            this.FInput.attr("maxlength", V);
            this.FMaxLength = V;
        }
    }

    get ReadOnly() { return this.FReadOnly }
    set ReadOnly(V) {
        if (V !== this.FReadOnly) {
            this.FInput.prop("readonly", V);
            this.FReadOnly = V;
        }
    }

    get Value() { return this.FValue }
    set Value(V) {
        if (V !== this.FValue) {
            this.FValue = V;
            this._DoUpdateValue();
        }
    }

    get DelayedChange() { return this.FDelayedChange };
    set DelayedChange(V) {
        if (V != this.FDelayedChange) {
            if (this.FDelayedChange > -1)
                this.FInput.off("keyup.nvjs.delayed");
            if (V > -1)
                this.FInput.on("keyup.nvjs.delayed", (e) => this._DoDelayedChange(e));

            this.FDelayedChange = V;
        }
    }

    _DoDelayedChange(e) {
        e.type = "change";
        if (this.FDelayedChange == 0)
            _DoValueChanged(e)
        else {
            clearTimeout(this.FChangeTimer);
            this.FChangeTimer = setTimeout((e, $this) => {
                $this._DoValueChanged(e);
                $this._ProcessEvent(e);
            }, this.FDelayedChange, e, this);
        }
    }

}


export class TNvBsInput extends TNvBsCustomImput {

    _CreateParams(o) {
        super._CreateParams(o);
        this.FInput.attr("type", "text");
        this.FPlaceholder = '';
        //this.Placeholder = o.Placeholder || '';
        this.FInput.appendTo(this.FEl);
    }

    get Placeholder() { return this.FPlaceholder }
    set Placeholder(V) {
        if (V !== this.FPlaceholder) {
            this.FInput.attr("placeholder", this.Placeholder);
            this.FPlaceholder = V;
        }
    }
}

//TODO: remove Litepicker css(non css version) and add css to nv.bs.css 
export class TNvBsInputDate extends TNvBsInput {
    _DefaultParams(o) {
        o.Options ??= {};
        o.Options.dropdowns ??= { "minYear": 1990, "maxYear": null, "months": true, "years": true };
        o.Options.firstDay ??= 0;
        o.Options.format ??= BrowserDateFormat().toUpperCase();
        o.Options.lang ??= navigator.language;
        o.Options.singleMode ??= true;
        o.Options.numberOfColumns ??= o.Options.singleMode ? 1 : 2;
        o.Options.numberOfMonths ??= o.Options.singleMode ? 1 : 2;
        o.Options.resetButton ??= true;
        o.Options.plugins ??= ['mobilefriendly', 'keyboardnav', 'ranges'];
        o.Options.ranges ??= {};


        //o.Options.zIndex = 10; //adjust on set parent???
        //o.Options.css = ["https://cdn.jsdelivr.net/npm/@easepick/core@1.2.1/dist/index.css"];
        //o.Options.plugins = ["RangePlugin"];
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FInput.attr("type", "");
        o.Options.element = this.FInput[0];
        this.FPicker = new Litepicker(o.Options);
        this.FCustomDates = [];
        this.FPicker.on("selected", () => { this.FInput.trigger("change.nvjs") });
    }

    _DoUpdateValue() {
        let dts = this.FValue.split('-').map(item => item.trim());
        if ((dts.length = 2) && (!this.Options.singleMode))
            this.FPicker.setDateRange(dts[0], dts[1])
        else
            this.FPicker.setDate(dts[0])
    }

    get Options() { return this.FPicker.options ?? {} }
    set Options(V) {
        if (V != this.FPicker.options) {
            ChangeObjProps(this.FPicker.options, V);
            //if (this.FPicker.isShown())
            //   this.FPicker.renderAll();
        }
    }
    get CustomDates() { return this.FCustomDates }
    set CustomDates(V) {
        if (V != this.FCustomDates) {
            let custRanges = {}, timeZoneOffset = - new Date().getTimezoneOffset() / 60;
            V.forEach(d => {
                custRanges[d.Name] = [new Date(d.StartDate + 'Z' + timeZoneOffset), new Date(d.EndDate + 'Z' + timeZoneOffset)];
            });

            this.FPicker.options.ranges.customRanges = custRanges;
            this.FCustomDates = V;
        }
    }

}

export class TNvBsDateRange extends TNvBsInputDate {
    _DefaultParams(o) {
        o.Options ??= {};
        o.Options.singleMode ??= false;
        super._DefaultParams(o);
    }

    // get EndDate() { return this.FPicker.getEndDate() }
    // set EndDate(V) {
    //     if (V !== this.EndDate)
    //         this.FPicker.setDateRange(this.FValue, V);
    // }
}





export class TNvBsInputDateTime extends TNvBsInput {
    _CreateParams(o) {
        super._CreateParams(o);
        // this.FInput.attr("type", "datetime-local");
    }
}

export class TNvBsSelect extends TNvBsCustomImput {
    _CreateParams(o) {
        if (!this.FInput)
            this.FInput = $(document.createElement("select"))
                .addClass("form-select");
        super._CreateParams(o);
        //this.FInput.attr("type", "text");
        //this.FPlaceholder = '';
        //this.Placeholder = o.Placeholder || '';
        this.FItems = [];
        this.Items = o.Items || this.FItems;
        this.FInput.appendTo(this.FEl);
    }

    get Items() { return this.FItems }
    set Items(V) {
        if (V != this.FItems) {
            let opts = '';

            V.map(it => {
                opts = opts + '<option'
                    + (it.Selected ? ' selected ' : '')
                    + (it.Disabled ? ' disabled ' : '')
                    + (it.Value == '' ? ' value="" ' : '')
                    + (it.Action ? ' value=""': '')
                    + '>' + it.Value + '</option>';
            });

            this.FInput.html(opts);

            this.FItems = V;
        }
    }

    _DoValueChanged(e) {
        let _Item = this.FItems[this.FInput[0].selectedIndex];
        if (_Item.Action >= 0 ){
            this._DoExecuteAction(_Item.Action);
            this.FInput[0].selectedIndex = -1;
        }  
        else {
            this.FValue = $(e.target).val();
            App.QueueChange(this.FId, "Value", this.FValue);
        }
    }

    _DoExecuteAction(a) {
        let evt = {
            type : "exec-action",
            action: a,
        };
        this._ProcessEvent(evt); 
    }

}

export class TNvBsLookupSelect extends TNvBsSelect {

}

export class TNvBsMemo extends TNvBsCustomImput {
    _CreateParams(o) {
        if (!this.FInput)
            this.FInput = $(document.createElement("textarea"))
                .addClass("form-control");
        super._CreateParams(o);
        //this.FInput.attr("type", "text");
        //this.FPlaceholder = '';
        //this.Placeholder = o.Placeholder || '';
        this.FInput.appendTo(this.FEl);
        //this.FInput.html("<option>1</option><option>2</option><option>3</option><option>4</option><option>5</option>");
    }
}


export class TNvBsRange extends TNvBsCustomImput {
    _CreateParams(o) {
        super._CreateParams(o);
        this.FInput.attr("type", "range");
        //this.FPlaceholder = '';
        //this.Placeholder = o.Placeholder || '';
        this.FInput.appendTo(this.FEl);
        //this.FInput.html("<option>1</option><option>2</option><option>3</option><option>4</option><option>5</option>");
    }
}

export class TNvBsCheckbox extends TNvBsCustomImput {
    _DefaultParams(o) {
        o.ClassCss ??= "form-check";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FInput//
            .attr("type", "checkbox")//
            .attr("class", "form-check-input");
        this.FLabel.attr("class", "form-check-label");
        //this.FPlaceholder = '';
        //this.Placeholder = o.Placeholder || '';
        this.FInput.insertBefore(this.FLabel);
        this.FValueChecked = 'True';
        this.FValueUnChecked = 'False';
        //this.FInput.html("<option>1</option><option>2</option><option>3</option><option>4</option><option>5</option>");
    }

    get ValueChecked() { return this.FValueChecked }
    set ValueChecked(V) {
        if (V != this.FValueChecked) {
            this.FValueChecked = V;
        }
    }

    get ValueUnChecked() { return this.FValueUnChecked }
    set ValueUnChecked(V) {
        if (V != this.FValueUnChecked) {
            this.FValueUnChecked = V;
        }
    }

    _DoValueChanged(e) {
        this.FValue = $(e.target).prop("indeterminate") ? "" : $(e.target).prop("checked") ? this.FValueChecked : this.FValueUnChecked;
        App.QueueChange(this.FId, "Value", this.FValue);
    }

    _DoUpdateValue() {
        if (this.FValue == this.FValueChecked)
            this.FInput.prop("checked", true)
        else if (this.FValue == this.FValueUnCheckedChecked)
            this.FInput.prop("checked", false)
        else
            this.FInput.val(this.FValue);
    }

}

export class TNvBsSwitch extends TNvBsCheckbox {
    _DefaultParams(o) {
        o.ClassCss ??= "form-check form-switch";
        super._DefaultParams(o);
    }
}


export class TNvBsRadioItem extends TSubProperty {
    constructor(C, o) {
        this.C = C;
        super();
    }


    _CreateParams(o) {
        this.FId = o.Id || this.C.Id + "_" + Object.keys(this.C.FItems).length;
        super._CreateParams(o);

        this.C.FItems[this.FId];
        this.FEl = $(document.createElement("div"))//
            .addClass("form-check");
        this.FInput = $(document.createElement("input"))
            .attr("id", this.FId)
            .addClass("form-check-input")
            .attr("type", "radio")
            .attr("name", this.C.FId)
            .attr("value", "")
            .appendTo(this.FEl);
        this.FLabel = $(document.createElement("label"))
            .addClass("form-check-label")
            .attr("for", this.FId)
            .appendTo(this.FEl);

        this.FCaption = '';
        this.FValue = '';
        this.FChecked = false;
        this.FEl.appendTo(this.C.FEl);
    }

    get Id() { return this.FId }
    get Caption() { return this.FCaption }
    get Value() { return this.FValue }
    get Checked() { return this.FChecked }

    set Id(V) {
        if (V != this.FId) {
            this.FEl.attr("id", V);
            this.FLabel.attr("for", V);
            this.FId = V;
        }
    }
    set Caption(V) {
        if (V != this.FCaption) {
            this.FLabel.text(V);
            this.FCaption = V;
        }
    }
    set Value(V) {
        if (V != this.FValue) {
            this.FInput.attr("value", V);
            this.FValue = V;
        }
    }
    set Checked(V) {
        if (V != this.FChecked) {
            this.FInput.attr("checked", V)
            this.FChecked = V;
        }
    }
}

export class TNvBsRadioGroup extends TNvBsGridControl {
    _CreateParams(o) {
        super._CreateParams(o);
        this.FLabel = $(document.createElement("label"))
            .attr("for", this.FId)
            .appendTo(this.FEl);
        this.FCaption = '';
        //this.Caption = o.Caption || '';
        this.FItems = {};
    }

    get Caption() { return this.FCaption }
    get Items() { return this.FItems }
    set Caption(V) {
        if (V !== this.FCaption) {
            this.FLabel.text(V);
            this.FCaption = V;
        }
    }

    AddItem(o) {
        return new TNvBsRadioItem(this, o);
    }

    RemoveItem(I) {
        delete this.FItems[I];
    }
}


export class TNvBSInputAddon extends TBsCustomControl {
    _DefaultParams(o) {
        //o.ClassCss ??= "input-group";
        o.Position ??= "before";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        // this.FPosition = "";
        this.FInputEl = null;
    }

    _DoAdjustPosition() {
        if (this.FInputEl) {
            if (this.FPosition == "before")
                this.FEl.insertBefore(this.FInputEl)
            else if (this.FPosition == "after")
                this.FEl.insertAfter(this.FInputEl)
            else
                this.FEl.detach()
        }
    }

    get Position() { return this.FPosition };
    set Position(V) {
        if (V != this.FPosition) {
            this.FPosition = V;
            this._DoAdjustPosition();
        }
    }

}

export class TNvBsInputAddonText extends TNvBSInputAddon {
    _DefaultParams(o) {
        o.ClassCss ??= "input-group-text";
        o.Tag ??= "span";
        super._DefaultParams(o);
    }

}

export class TNvBsInputAddonIcon extends TNvBsInputAddonText {
}

export class TNvBsInputAddonAction extends TNvBSInputAddon {
    _DefaultParams(o) {
        o.ClassCss ??= "btn";
        o.Tag ??= "button";
        o.Variant ??= "light";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FVariant = "";
    }

    get Variant() { return this.FVariant };
    set Variant(V) {
        if (V != this.FVariant) {
            this.FEl//
                .removeClassRegex("(^|\\b)(btn-(info|primary|secondary|success|danger|warning|dark|light|link)+)(\\b(?!-)|$)")
                .addClass("btn-" + V);
            this.FVariant = V;
        }
    }
}


export class TNvBsInputGroup extends TNvBsGridContainer {
    _DefaultParams(o) {
        // o.ClassCss ??= "input-group";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FGoupDiv = $(document.createElement("div"))//
            .addClass("input-group")//
            .appendTo(this.FEl);
        this.FAddons = [];
        this.FInput = null;
    }

    _DoInsertControl(C) {
        if (C instanceof TNvBsCustomImput) {
            this.FInput = C;
            C.FLabel.prependTo(this.FEl);
            // C.FInput.prependTo(this.FGoupDiv);
            C.FEl.appendTo(this.FGoupDiv);
            this.FAddons.forEach(Addon => {
                Addon.FInputEl = C.El;
                Addon._DoAdjustPosition();
            });
        } else if (C instanceof TNvBSInputAddon) {
            this.FAddons.push(C);
            if (this.FInput) {
                C.FInputEl = this.FInput.FEl;
                C._DoAdjustPosition();
            }
        }
        else if (C)
            App.Logger.Error("InputGroup can't support " + C.constructor.name);
    }

    _DoRemoveControl(C) {
        if (C instanceof TNvBsCustomImput) {
            this.FAddons.forEach(Addon => {
                Addon.FInputEl = null;
                Addon._DoAdjustPosition();
            });
            // TODO(Delcio): fix the Label and Input element parent
            //C.FInput.appendTo(C.FEl);
            C.FLabel.insertBefore(C.FInput);
            super._DoRemoveControl(C);
            this.FInput = null;
        } else if (C instanceof TNvBSInputAddon) {
            this.FAddons.pop(C);
            C.FInputEl = null;
            super._DoRemoveControl(C);
        }
        else if (C)
            App.Logger.Error("InputGroup can't support " + C.constructor.name);
    }
}



TApplication.RegisterClass(TNvBsInput);
TApplication.RegisterClassModule(TNvBsInput, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsInput, './bootstrap.min.css', 'css');

TApplication.RegisterClass(TNvBsSelect);
TApplication.RegisterClassModule(TNvBsSelect, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsSelect, './bootstrap.min.css', 'css');
