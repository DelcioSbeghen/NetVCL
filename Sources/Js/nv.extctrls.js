import { TControl } from "./nv.controls.js";

export class TImage extends TControl {
    _DefaultParams(o) {
        o.Tag ??= "img";
        super._DefaultParams(o);
    }
    _CreateParams(o) {
        super._CreateParams(o);
        this.FSrc = '';
       // this.Src = o.Src || '';
    }

    set Src(S) {
        if (S != this.FSrc) {
            this.Attr("src", S);
            this.FSrc = S;
        }
    }
}

TApplication.RegisterClass(TImage);
