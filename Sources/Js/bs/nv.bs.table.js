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
            .bootstrapTable(this.FOptions)
            .data("control", this)
            .on("click-cell.bs.table", (e, f, v, r) => this._DoClickCell(f, v, r));
    }

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

    _DoClickCell(f, v, r) {
        let evt = {
            type : "click-cell",
            field: f,
            index: r.RowNo,
        };
        this._ProcessEvent(evt);          
    }

    _AttachEvents(E) {
        E.forEach(evt => {
           this.FTable.off(evt + ".nvjs").on(evt + ".nvjs", (e) => this._ProcessEvent(e));
        });
    }

}

window.actionEvents = {
    'click .grid-action': function (e, value, row, i) {
        e.stopPropagation();
        let evt = {
            type : "click-action",
            index:i,
            action:e.currentTarget.name
        };
        this.$el.data("control")._ProcessEvent(evt);
    }
}

window.actionFormatter = function (value, row) {
    let acts = '<div class="btn-group">';
    value.map(act => {
        acts = acts + '<button name="' + act.n + '" class="grid-action btn btn-' + ((act.v && act.v != "") ?  act.v: 'primary');
        if (act.s && act.s != "") acts = acts + ' btn-' + act.s;
        acts = acts + '">' + (act.i || '') +  (act.c || '') + '</button>'
    });
    return acts + '</div>';
}


export class TNvBsDbTable extends TNvBsTable {

}

