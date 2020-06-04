import { TBsWinControl } from "./nv.bs.controls.js";

export class TNvBsTable extends TBsWinControl {
    _CreateParams(o) {
        super._CreateParams(o);
        this.FOptions = {
            columns: [],
            data: []
        };
        this.FActiveRow = -1;
        this.FTable = $("<table/>")//
            .appendTo(this.FEl)
            .bootstrapTable(this.FOptions);
    }

    // _Tag() {
    //     return "table";
    // }

    get ActiveRow() { return this.FActiveRow }
    set ActiveRow(V) {
        if (this.FActiveRow != this.FActiveRow) {
            this.FActiveRow = V;
            //todo Apply to Table (selected if no multiselect)
        }
    }

    get Data() { return this.FOptions.data }
    set Data(V) {
        if (V != this.FOptions.data) {
            this.FOptions.data = V;
            this.FTable.bootstrapTable('load', this.FOptions.data);
        }
    }

    get Options() { return this.FOptions }
    set Options(V) {
        if (V != this.FOptions) {
            V.data = this.FOptions.data;
            this.FOptions = V;
            this.FTable.bootstrapTable('refreshOptions', this.FOptions);
        }
    }

}

export class TNvBsDbTable extends TNvBsTable {

}

