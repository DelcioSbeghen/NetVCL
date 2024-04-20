import { TWinControl } from "./nv.controls.js";

export class TNvStatusBar extends TWinControl {
    _DefaultParams(o) {
        this.FRenderPosition ??= false;
        o.ClassCss ??= "status-bar";
        super._DefaultParams(o);
    }
}