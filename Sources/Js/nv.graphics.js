import { TSubProperty } from "./nv.classes.js";

export class TFont extends TSubProperty{

    _CreateParams(o) {
        super._CreateParams(o);
        this.FColor = '';
        this.FName = '';
        this.FSize = '';
        this.FStyle = [];
    }

    _DoChange(prop, v) {
        var e = $.Event("change");
        e.prop = prop;
        e.value = v;
        $(this).trigger(e);
    }

    set Color(C) {
        if (C != this.FColor) {
            this.FColor = C;
            this._DoChange("color", C);
        }
    }

    set Name(N) {
        if (N != this.FName) {
            this.FName = N;
            this._DoChange("name", N);
        }
    }

    set Size(S) {
        if (S != this.FSize) {
            this.FSize = S;
            this._DoChange("size", S);
        }
    }

    set Style(S) {
        if (!$.arrayCompare(S, this.FStyle)) {
            this.FStyle = S;
            this._DoChange("style", S);
        }
    }
}