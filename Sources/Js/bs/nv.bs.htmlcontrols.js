import { TBsCustomControl, TNvBsGridControl } from "./nv.bs.controls.js"
import { TNvBsContainer } from "./nv.bs.containers.js";


export class TNvBSText extends TNvBsGridControl {
    _CreateParams(o) {
        this.FTag = this.FTag ?? "span";
        super._CreateParams(o);
    }
}


export class TNvBsLink extends TBsCustomControl {
    _CreateParams(o) {
        this.FTag = this.FTag ?? "a";
        super._CreateParams(o);
        this.FLinkText = $(document.createElement("span"))
            .appendTo(this.FEl);
        this.FHref = "#";
        this.HRef = o.HRef || "#";
        this.FColor = ""; //''|primary|secondary|success|danger|warning|info|light|dark|body-emphasis
    }

    get HRef() { return this.FHref }
    set HRef(V) {
        if (V != this.FHref) {
            this.FEl.attr("href", V);
            this.FHref = V;
        }
    }

    get Color() { return this.FColor }
    set Color(V) {
        if (V != this.FColor) {
            this.FEl.removeClassRegex("(^|\\b)(link-(primary|secondary|success|danger|warning|info|light|dark|body-emphasis)+)(\\b(?!-)|$)");
            if (V != "")
                this.FEl.addClass('link-' + V);
        }
    }

    _DoImageChange() {
        super._DoImageChange();
        //override to change functionality
        if (this.FImage != "")
            this.FEl.addClass("icon-link")
        else
            this.FEl.removeClass("icon-link");
    }

    _DoTextChange(T) {
        this.FLinkText.html(T.htmlEscape());
        this.FText = T;
    }

}


export class TNvBsDropdownItemLink extends TNvBsLink {
    _DefaultParams(o) {
        o.ClassCss ??= "dropdown-item";
        super._DefaultParams(o);
    }
}


export class TNvBsCustomDropdownContainer extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "dropdown-menu";
        super._DefaultParams(o);
    }

    _CreateParams(O) {
        super._CreateParams(O);
        this.FAlignStart = '';
        this.FAlignEnd = '';
    }

    _DoSetParent(P) {
        if (this.ParentDefined())
            this.Parent.FEl
                .off("hide.bs.dropdown.nvjs")
                .off("show.bs.dropdown.nvjs");
        super._DoSetParent(P);
        if (this.ParentDefined())
            this.Parent.FEl
                .off("hide.bs.dropdown.nvjs").on("hide.bs.dropdown.nvjs", (e) => { /* e.preventDefault(); */ this.FEl.triggerHandler("hide.bs.dropdown.nvjs") })
                .off("show.bs.dropdown.nvjs").on("show.bs.dropdown.nvjs", (e) => { /* e.preventDefault(); */ this.FEl.triggerHandler("show.bs.dropdown.nvjs") });
    }

    get AlignStart() { return this.FAlignStart }
    set AlignStart(V) {
        if (V !== this.FAlignStart) {
            this.FEl.removeClassRegex("(^|\\b)(dropdown-menu-(start|sm-start|md-start|lg-start|xl-start)+)(\\b(?!-)|$)");
            if (V != '') {
                this.FEl.addClass("dropdown-menu-" + (V == 'sm') ? 'start' : 'start-' + V);
            }
            this.FAlignStart = V;
        }
    }

    get AlignEnd() { return this.FAlignEnd }
    set AlignEnd(V) {
        if (V !== this.FAlignEnd) {
            this.FEl.removeClassRegex("(^|\\b)(dropdown-menu-(end|sm-end|md-end|lg-end|xl-end)+)(\\b(?!-)|$)");
            if (V != '') {
                this.FEl.addClass("dropdown-menu-" + (V == 'sm') ? 'end' : 'end-' + V);
            }
            this.FAlignEnd = V;
        }
    }


}

export class TNvBsDropdownContainer extends TNvBsCustomDropdownContainer {
}

export class TNvBsDropdownMenu extends TNvBsCustomDropdownContainer {

}



