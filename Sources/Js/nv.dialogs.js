import { Typ } from "./nv.classes.js";
import { TWinControl } from "./nv.controls.js";

export class TNvDialog extends TWinControl {
    _DefaultParams(o) {
        o.ClassCss ??= "dialog";
        o.Tag ??= "dialog";
        o.Position ??= "";
        o.DialogType ??= "info"; //warning|error|info|confirm|custom 
        o.Shadow ??= true;  
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FVisible = false;
        this.FHeader = $(document.createElement("header"))
            .addClass("dialog-header")
            .appendTo(this.FEl);
        this.FElTitle = $(document.createElement("h1"))
            .addClass("dialog-title")
            .appendTo(this.FHeader);
        this.FBody = $(document.createElement("div"))
            .addClass("dialog-body")
            .appendTo(this.FEl);
        this.FFooter = $(document.createElement("div"))
            .addClass("dialog-footer")
            .appendTo(this.FEl);
        this.FDialogType = ""; //warning|error|info|confirm|custom   
    }

    get Title() { return this.FElTitle.html() }
    set Title(V) {
        if (V != this.Title)
            this.FElTitle.html(V);
    }

    _DoTypeChange(V) {
        this.FEl.removeClassStartingWith("dialog-type-");
        if (V !== '')
            this.FEl.addClass("dialog-type-" + V);
    }

    get DialogType() { return this.FDialogType }
    set DialogType(V) {
        if (V !== this.FDialogType) {
            this._DoTypeChange(V);
            this.FDialogType = V;
        }

    }

    get Visible() { return super.Visible };
    set Visible(V) {
        if (V != this.FVisible) {
            if (V)
                this.FEl.prop("open", true)
            else
                this.FEl.removeAttr("open");
            this.FVisible = V;
        }
    }

    CloseModal() {
        this.Visible = false;
        super.CloseModal();
    }


    ShowModal() {
        super.ShowModal();
        this.Visible = true;
    }


}




export class TNvButtonsDialog extends TNvDialog {

    _CreateParams(o) {
        super._CreateParams(o);
        this.FButtons = [];
        this.FButtonsEL = [];
    }


    _DoTextChange(T) {
        if (this.FBody.html() !== T.htmlEscape()) {
            this.FBody.html(T.htmlEscape());
            this.FText = T;
        }
    }


/*     _DoBtnClick(e) {
        for (const btnEl of this.FButtonsEL) {
            if (btnEl[0] == e.target) {
                let event = $.Event("close-modal", {ButtonIndex:this.FButtonsEL.indexOf(btnEl), ModalResult:Typ.mrClose});
                event.ButtonIndex = this.FButtonsEL.indexOf(btnEl);
                this.FEl.triggerHandler( $.Event("close-modal", {}),  Typ.mrCancel));
                break;
            }

        }
    } */


    _RemoveAllButtons() {
        for (const btn of this.FButtonsEL) {
            btn.remove();
        }
        this.FButtonsEL = [];
    }

    _UpdateButtons(V) {
        this._RemoveAllButtons();

        for (const btn of V) {
            let btnEl = $(document.createElement("button"))
                .addClass("dialog-button")
                .html(btn.Caption)
                //.on("click.nvjs", (e) => this._DoBtnClick(e))
                .on("click.nvjs", (e) => this.FEl.trigger("close-modal.nvjs", btn.ModalResult))
                .appendTo(this.FFooter);

            this.FButtonsEL.push(btnEl);
        }
    }



    get Buttons() { return this.FButtons }

    set Buttons(V) {
        if (V != this.FButtons) {
            this._UpdateButtons(V);
            this.FButtons = V;
        }
    }

}




export class TNvMessageDialog extends TNvDialog {

    _CreateParams(o) {
        super._CreateParams(o);
        this.FButton = $(document.createElement("button"))
            .addClass("dialog-button")
            .html("Ok")
            .on("click.nvjs", () => this.FEl.trigger("close-modal.nvjs", Typ.mrCancel))
            .appendTo(this.FFooter);
    }

    _DoTypeChange(V) {
        super._DoTypeChange(V);
        if (this.FDialogType == "confirm") {
            if (this.FCancel) this.FCancel.remove();
            this.FCancel = $(document.createElement("button"))
                .addClass("dialog-button")
                .html("Cancelar")
                .on("click.nvjs", () => this.FEl.trigger("cancel"))
                .appendTo(this.FFooter);
        } else
            if (this.FCancel) this.FCancel.remove();
    }

    _DoTextChange(T) {
        if (this.FBody.html() !== T.htmlEscape()) {
            this.FBody.html(T.htmlEscape());
            this.FText = T;
        }
    }



}