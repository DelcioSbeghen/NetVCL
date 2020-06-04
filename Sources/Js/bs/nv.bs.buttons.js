import { TNvBsGridContainer } from "./nv.bs.containers.js";


export class TNvBsButton extends TNvBsGridContainer {

    _CreateParams(o) {
        super._CreateParams(o);
        this.FEl.attr("type", "button");
        this.AddClass("btn btn-primary");
        this.FVariant = "primary";
        this.FSize = ""; //""|sm|lg
        this.FBlock = false;
    }

    _Tag() {
        return "button";
    }
    //get Grids() { return this.FGrids };
    get Variant() { return this.FVariant };
    set Variant(V) {
        if (V != this.FVariant) {
            this.FEl.removeClassRegex("(^|\\b)(btn-(info|primary|secondary|success|danger|warning|dark|light|link)+)(\\b(?!-)|$)")
                .addClass('btn-' + V);
            this.FVariant = V;
        }
    }

    get Size() { return this.FSize }
    set Size(V) {
        if (V !== this.FSize) {
            this.FEl.removeClassRegex("(^|\\b)(btn-(sm|lg)+)(\\b(?!-)|$)");
            if (V != "")
                this.AddClass("btn-" + V);
            this.FSize = V;
        }
    }

    get Block() { return this.FBlock }
    set Block(V) {
        if (V != this.FBlock) {
            if (V)
                this.AddClass("btn-block")
            else
                this.FEl.removeClass("btn-block");
        }
    }
}

export class TNvBsButtonGroup extends TNvBsGridContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("btn-group");
        this.FEl.attr("role", "group");
        this.FSize = ""; //""|sm|lg
        this.FVertical = false;
    }

    get Size() { return this.FSize }
    set Size(V) {
        if (V !== this.FSize) {
            this.FEl.removeClassRegex("(^|\\b)(btn-group(sm|lg)+)(\\b(?!-)|$)");
            if (V != "")
                this.AddClass("btn-group" + V);
            this.FSize = V;
        }
    }

    get Vertical(){return this.FVertical}
    set Vertical(V){
        if (V!==this.FVertical){
         V ? this.AddClass("btn-group-vertical"):this.FEl.removeClass("btn-group-vertical");  
        }
    }
}

export class TNvBsButtonToolbar extends TNvBsGridContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("btn-toolbar");
        this.FEl.attr("role", "group");
        this.FSize = ""; //""|sm|lg        
    }

    get Size() { return this.FSize }
    set Size(V) {
        if (V !== this.FSize) {
            this.FEl.removeClassRegex("(^|\\b)(btn-group(sm|lg)+)(\\b(?!-)|$)");
            if (V != "")
                this.AddClass("btn-group" + V);
            this.FSize = V;
        }
    }
}




TApplication.RegisterClass(TNvBsButton);
TApplication.RegisterClassModule(TNvBsButton, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsButton, './bootstrap.min.css', 'css');
