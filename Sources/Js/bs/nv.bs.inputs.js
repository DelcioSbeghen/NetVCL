import { TNvBsGridControl } from "./nv.bs.controls.js";
import { TSubProperty } from "../nv.classes.js";


export class TNvBsCustomImput extends TNvBsGridControl {
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
    }

    _AttachEvents(E) {
        super._AttachEvents(E);
        this.FInput.off("change.nvjs").on("change.nvjs",(e) => this._DoValueChanged(e))
    }

    _DoTextChange(T) {
        if (this.FText !== T) {
            this.FLabel.html(T);
            this.FText = T;
        }
    }

    _UpdateBackground(el) {
        super._UpdateBackground(this.FInput);
    }

    _UpdateShadow(el) {
        super._UpdateShadow(this.FInput);
    }

    _DoUpdateValue(){
        this.FInput.val(this.FValue);
    }

    _DoValueChanged(e){
        this.FValue = $(e.target).val();
        App.QueueChange(this.FId,"Value", this.FValue);
    }

    _DoEnabledChange(){
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
    }s

    get Value() { return this.FValue }
    set Value(V) {
        if (V !== this.FValue) {
            this.FValue = V;
            this._DoUpdateValue();
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
        this.AddClass("form-group");
    }

    get Placeholder() { return this.FPlaceholder }
    set Placeholder(V) {
        if (V !== this.FPlaceholder) {
            this.FInput.attr("placeholder", this.Placeholder);
            this.FPlaceholder = V;
        }
    }
}

export class TNvBsSelect extends TNvBsCustomImput {
    _CreateParams(o) {
        if (!this.FInput)
            this.FInput = $(document.createElement("select"))
                .addClass("form-control");
        super._CreateParams(o);
        //this.FInput.attr("type", "text");
        //this.FPlaceholder = '';
        //this.Placeholder = o.Placeholder || '';
        this.FInput.appendTo(this.FEl);
        this.AddClass("form-group");
        this.FInput.html("<option>1</option><option>2</option><option>3</option><option>4</option><option>5</option>");
    }
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
        this.AddClass("form-group");
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
        this.AddClass("form-group");
        //this.FInput.html("<option>1</option><option>2</option><option>3</option><option>4</option><option>5</option>");
    }
}

export class TNvBsCheckbox extends TNvBsCustomImput {
    _CreateParams(o) {
        super._CreateParams(o);
        this.FInput.attr("type", "checkbox");
        //this.FPlaceholder = '';
        //this.Placeholder = o.Placeholder || '';
        this.FInput.appendTo(this.FEl);
        this.AddClass("form-group");
        //this.FInput.html("<option>1</option><option>2</option><option>3</option><option>4</option><option>5</option>");
    }
}

export class TNvBsRadioItem extends TSubProperty {
    constructor(C, o) {
        super();
        this.C = C;
        this.FId = o.Id || C.Id + "_" + Object.keys(C.FItems).length;
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
        this.Caption = o.Caption || this.FCaption;
        this.FValue = '';
        this.Value = o.Value || this.FValue;
        this.FChecked = false;
        this.Checked = o.Checked || this.FChecked;

        this.FEl.appendTo(C.FEl);

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
        this.AddClass("form-group");
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





TApplication.RegisterClass(TNvBsInput);
TApplication.RegisterClassModule(TNvBsInput, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsInput, './bootstrap.min.css', 'css');

TApplication.RegisterClass(TNvBsSelect);
TApplication.RegisterClassModule(TNvBsSelect, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsSelect, './bootstrap.min.css', 'css');
