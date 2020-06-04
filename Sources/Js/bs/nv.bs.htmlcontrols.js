import { TBsCustomControl } from "./nv.bs.controls.js"
import { TNvBsContainer } from "./nv.bs.containers.js";


export class TNvBSText extends TBsCustomControl{
    _Tag(){return "span"}
}


export class TNvBsLink extends TBsCustomControl {
    _CreateParams(o) {
        super._CreateParams(o);
        //if (App.FDesign)
        //    this.AddClass("design-div");
        this.FHref = "#";
        this.HRef = o.HRef || "#";
    }

    _Tag(){return "a"}

    get HRef() { return this.FHref }
    set HRef(V) {
        if (V != this.FHref) {
            this.FEl.attr("href", V);
            this.FHref = V;
        }
    }
}




export class TNvBsDropdownItemLink extends TNvBsLink {
    _CreateParams(o) {
        super._CreateParams(o);
        //if (App.FDesign)
        //    this.AddClass("design-div");
        this.AddClass("dropdown-item");
    }
}





export class TNvBsDropdownMenu extends TNvBsContainer {
    _CreateParams(O) {
        super._CreateParams(O);
        this.AddClass("dropdown-menu");
        this.FAlignLeft = '';
        this.FAlignRight = '';
        this.AlignLeft = O.AlignLeft || '';  //''|xs|sm|md|lg|xl
        this.AlignRight = O.AlignRight || '';//''|xs|sm|md|lg|xl
    }

    get LeftAlign() { return this.FAlignLeft }
    set LeftAlign(V) {
        if (V !== this.FAlignLeft) {
            this.FEl.removeClassRegex("(^|\\b)(dropdown-menu-(left|sm-left|md-left|lg-left|xl-left)+)(\\b(?!-)|$)");
            if (V != '') {
                this.FEl.addClass("dropdown-menu-" + (V == 'sm') ? 'left' : 'left-' + V);
            }
            this.FAlignLeft = V;
        }
    }

    get AlignRight() { return this.FAlignRight }
    set AlignRight(V) {
        if (V !== this.FAlignRight) {
            this.FEl.removeClassRegex("(^|\\b)(dropdown-menu-(right|sm-right|md-right|lg-right|xl-right)+)(\\b(?!-)|$)");
            if (V != '') {
                this.FEl.addClass("dropdown-menu-" + (V == 'sm') ? 'right' : 'right-' + V);
            }
            this.FAlignRight = V;
        }
    }
}