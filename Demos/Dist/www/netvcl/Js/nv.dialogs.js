import { TWinControl } from "./nv.controls.js";

export class TNvDialog extends TWinControl {

    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("dialog");
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
        this.FDialogType = ""; //''|warning|error|info|confirm    
    }


    _Tag() { return "dialog" }


    get Title() { return this.FElTitle.html() }
    set Title(V) {
        if (V != this.Title)
            this.FElTitle.html(V);
    }

    get DialogType(){return this.FDialogType}
    set DialogType(V){
        if (V !== this.FDialogType){
            this.FEl.removeClassStartingWith("dialog-type");
            if (V !== '')
                this.FEl.addClass("dialog-type-" + V);
            this.FDialogType = V;
        }

    }
}


export class TNvMessageDialog extends TNvDialog {

    _CreateParams(o) {
        super._CreateParams(o);
        this.FButton = $(document.createElement("button"))
            .addClass("dialog-button")
            .html("Ok")
            .on("click.nvjs", () => this.FEl.trigger("close"))
            .appendTo(this.FFooter);
    }

    _DoTextChange(T) {
        if (this.FBody.html() !== T) {
            this.FBody.html(T);
            this.FText = T;
        }
    }



}