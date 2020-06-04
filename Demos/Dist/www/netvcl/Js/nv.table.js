
import { TControl } from './nv.controls.js';

export class TTable extends TControl {
    _CreateParams(o) {
        //this.FRenderPosition = false;
        super._CreateParams(o);
        //this.FTableEl = $(document.createElement("div"));
        //this.FEl.append(this.FTableEl);
        this.FEl.bootstrapTable(o.Table);
    }

    _Tag() {
        return "table";
    }
}

TApplication.RegisterClass(TTable);
TApplication.RegisterClassModule(TTable, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TTable, './bootstrap.min.css', 'css');
