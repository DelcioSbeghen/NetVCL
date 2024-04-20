import { TNvBsContainer, TNvBsGridContainer } from "./nv.bs.containers.js";
import { TNvBsDropdownMenu } from "./nv.bs.htmlcontrols.js";


export class TNvBsButton extends TNvBsGridContainer {
    _DefaultParams(o) {
        o.Tag ??= "button";
        o.ClassCss ??= "btn";
        o.Role ??= "button";
        o.Variant ??= "primary";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FSpan = $(document.createElement("span"))//
            .appendTo(this.FEl);
        this.FEl.attr("type", "button");
        this.FVariant = "";
        this.FOutline = false;
        this.FTransparent = false;
        this.FSize = ""; //""|sm|lg
        this.FDataToggle = "";
        this.FDataTarget = "";
    }

    _DoTextChange(T) {
        if (this.FText !== T) {
            this.FSpan.html(T.htmlEscape());
            if (T == "")
                this.FSpan.remove()
            else if ((!this.FSpan.parent()) || (this.FSpan.parent().length == 0))
                this.FSpan.appendTo(this.FEl);
            this.FText = T;
        }
    }

    _DoVariantChange(V, O) {
        this.FEl//
            .removeClassRegex("(^|\\b)(btn-(outline-)?(info|primary|secondary|success|danger|warning|dark|light|link)+)(\\b(?!-)|$)")
            .addClass("btn-" + (O ? "outline-":"") + V);
    }

    //get Grids() { return this.FGrids };
    get Variant() { return this.FVariant };
    set Variant(V) {
        if (V != this.FVariant) {
            this._DoVariantChange(V, this.FOutline);
            this.FVariant = V;
        }
    }

    get Outline() { return this.FOutline };
    set OutLine(V) {
        if (V != this.FOutline) {
            this._DoVariantChange(this.FVariant, V);
            this.FOutline = V;
        }
    }

    get Transparent() { return this.FTransparent };
    set Transparent(V) {
        if (V != this.FTransparent) {
            this.FEl.removeClass("btn-transparent");
            if (V) this.FEl.addClass("btn-transparent");
            this.FTransparent = V;
        }
    }



    get Size() { return this.FSize }
    set Size(V) {
        if (V !== this.FSize) {
            this.FEl.removeClassRegex("(^|\\b)(btn-(sm|lg)+)(\\b(?!-)|$)");
            if (V != "")
                this.FEl.addClass("btn-" + V);
            this.FSize = V;
        }
    }

    get DataToggle() { return this.FDataToggle }
    set DataToggle(V) {
        if (V != this.FDataToggle) {
            this.FEl.attr("data-bs-toggle", V);

            this.FDataToggle = V;
        }
    }

    get DataTarget() { return this.FDataTarget }
    set DataTarget(V) {
        if (V != this.FDataTarget) {
            this.FEl.attr("data-bs-target", V);
            this.FEl.attr("aria-controls", V.substring(1));
            this.FDataTarget = V;
        }
    }
}

export class TNvBsButtonGroup extends TNvBsGridContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "btn-group";
        o.Role ??= "group";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FSize = ""; //""|sm|lg
        this.FVertical = false;
    }

    get Size() { return this.FSize }
    set Size(V) {
        if (V !== this.FSize) {
            this.FEl.removeClassRegex("(^|\\b)(btn-group(sm|lg)+)(\\b(?!-)|$)");
            if (V != "")
                this.FEl.addClass("btn-group" + V);
            this.FSize = V;
        }
    }

    get Vertical() { return this.FVertical }
    set Vertical(V) {
        if (V !== this.FVertical) {
            V ? this.FEl.addClass("btn-group-vertical") : this.FEl.removeClass("btn-group-vertical");
        }
    }
}

export class TNvBsButtonToolbar extends TNvBsGridContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "btn-toolbar";
        o.Role ??= "group";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FSize = ""; //""|sm|lg        
    }

    get Size() { return this.FSize }
    set Size(V) {
        if (V !== this.FSize) {
            this.FEl.removeClassRegex("(^|\\b)(btn-group(sm|lg)+)(\\b(?!-)|$)");
            if (V != "")
                this.FEl.addClass("btn-group" + V);
            this.FSize = V;
        }
    }
}


export class TButtonDropdown extends TNvBsButton {
    _DefaultParams(o) {
        o.ClassCss ??= "btn dropdown-toggle";
        o.DataToggle ??= "dropdown";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FEl//
            .attr("aria-expanded", false)//
            .attr("data-bs-auto-close", "outside");

    }

    Toggle() {
        this.FEl.dropdown("toggle");
    }
}

export class TNvBsDropDown extends TNvBsGridContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "dropdown";
        o.Button ??= {};
        o.Menu ??= {};
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FButton = TButtonDropdown.Create(o.Button);
        this.FMenu = TNvBsDropdownMenu.Create(o.Menu);
        this.FButton.Parent = this;
        this.FMenu.Parent = this;
    }

    get Button() { return this.FButton }
    get Menu() { return this.FMenu }

    _DoInsertControl(C) {
        if ((C == this.FButton) || (C == this.FMenu))
            super._DoInsertControl(C) 
        else
            C.El.appendTo(this.FMenu.El);
    }

}




TApplication.RegisterClass(TNvBsButton);
TApplication.RegisterClassModule(TNvBsButton, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsButton, './bootstrap.min.css', 'css');
