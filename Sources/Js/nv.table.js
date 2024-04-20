
import { TControl } from './nv.controls.js';

export class TTable extends TControl {
    _DefaultParams(o) {
        o.Tag ??= "table";
        super._DefaultParams(o);
    }
    
    _CreateParams(o) {
        super._CreateParams(o);
        //this.FTableEl = $(document.createElement("div"));
        //this.FEl.append(this.FTableEl);
        this.FEl.bootstrapTable(o.Table);
    }
}

TApplication.RegisterClass(TTable);
TApplication.RegisterClassModule(TTable, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TTable, './bootstrap.min.css', 'css');
