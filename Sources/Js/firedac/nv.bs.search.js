import { TNvBsInput } from "../bs/nv.bs.inputs.js";

export class TNvBsSearch extends TNvBsInput {
    _DefaultParams(o) {
        (o.Events ??= []).push("selected");
        super._DefaultParams(o);
    }
    
    _CreateParams(o) {
        super._CreateParams(o);
        this.FDropDown = $("<div/>")
            .addClass("dropdown")
            .css("position", "absolute")
            .css("z-index", 10000)
            .css("background", "white")
            .appendTo(this.FEl);
        this.FResults =  $("<table/>")
        .appendTo(this.FDropDown)
        .on('click.nvjs', 'tr', (e, $this) =>  this._DoSelect(e, $this));      
        this.FKeyInterval;
        this.FResult = {};
    }

    get Result() { return this.FResult }
    set Result(V) {
        if (V != this.FResult) {
            let r = '<tbody> ';
            if (this.FResult && this.Result.Data) {
                let cols = this.Result.Cols;

                this.Result.Data.map(row => {
                    r = r + '<tr data-key="'+ row["Key"]+ '">';
                    cols.map(col => {
                        r = r + '<td>' + row[col.c] + '</td>'
                    });
                    r = r + '</tr>'
                });
            };
            r = r + '</tbody>';
            this.FResults.html(r);

            this.FResult = V;
        }
    }


    _AttachEvents(E) {
        super._AttachEvents(E);
        this.FInput.off("keyup.nvjs").on("keyup.nvjs", (e) => this._DelayKeyEvent(e));
    }

    _DelayKeyEvent(e) {
        clearTimeout(this.FKeyInterval);
        this.FKeyInterval = setTimeout((e, $this) => { $this._ProcessEvent(e) }, 300, e, this);
    }
     
    _DoSelect(e, $this){
       let event = $.Event("selected");
       event.Key = e.target.nodeName == "TD"? e.target.parentElement.dataset.key: e.target.dataset.key;
       this.FEl.triggerHandler(event);
    }



}