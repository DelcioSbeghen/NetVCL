import { TControl } from "./nv.controls.js";

export class TImage extends TControl {
    _CreateParams(o) {
        super._CreateParams(o);
        this.FSrc = '';
       // this.Src = o.Src || '';
    }

    _Tag() {
        return "img";
    }

    set Src(S) {
        if (S != this.FSrc) {
            this.Attr("src", S);
            this.FSrc = S;
        }
    }
}

TApplication.RegisterClass(TImage);
