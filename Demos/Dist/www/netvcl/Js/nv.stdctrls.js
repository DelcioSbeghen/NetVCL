import { TWinControl } from "./nv.controls.js";

export class TNvStatusBar extends TWinControl {
    _CreateParams(o) {
        this.FRenderPosition = false;
        super._CreateParams(o);
        this.AddClass("status-bar");
    }
}