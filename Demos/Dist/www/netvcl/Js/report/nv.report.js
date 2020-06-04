import { TWinControl, TControl } from "./../nv.controls.js";
import { TTable } from './nv.table.js';

export class TNvReport extends TWinControl {

    _CreateParams(o) {
        this.FRenderPosition = false;
        super._CreateParams(o);
        this.FRenderPosition = true;
        this.FWidth = '0';
        this.Width = o.Width || "210mm";
    }

    set Width(W) {
        if (W != this.FWidth) {
            this.FEl.width(W);
            this.FWidth = W;
        }
    }
}

export class TNvrPanel extends TWinControl {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("nvr-rectangle");
        this.FRounded = true;
        //this.Rounded = o.Rounded || true;
        this.FShadows = true;
       // this.Shadows = o.Shadows || true;
    }

    set Rounded(R) {
        if (R != this.FRounded) {
            this.FEl.removeClass("nvr-rounded");
            if (R) this.FEl.addClass("nvr-rounded");
            this.FRounded = R;
        }
    }

    get Rounded() {
        return this.FRounded;
    }

    set Shadows(S) {
        if (S != this.FShadows) {
            this.FEl.removeClass("nvr-shadows");
            if (S) this.FEl.addClass("nvr-shadows");
            this.FShadows = S;
        }
    }

    get Shadows() {
        return this.FShadows;
    }
}

export class TNvrText extends TControl {
    _CreateParams(o) {
        super._CreateParams(o);
    }
}

export class TNvrTable extends TTable {
    _CreateParams(o) {
        super._CreateParams(o);
    }
}


TApplication.RegisterClass(TNvReport);
TApplication.RegisterClassModule(TNvReport, './nv.report.css', 'css');
TApplication.RegisterClass(TNvrPanel);
TApplication.RegisterClassModule(TNvrPanel, './nv.report.css', 'css');
TApplication.RegisterClass(TNvrText);
TApplication.RegisterClassModule(TNvrText, './nv.report.css', 'css');
TApplication.RegisterClass(TNvrTable);
TApplication.RegisterClassModule(TNvrTable, './nv.report.css', 'css');
TApplication.RegisterClassModule(TNvrTable, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvrTable, './bootstrap.min.css', 'css');
