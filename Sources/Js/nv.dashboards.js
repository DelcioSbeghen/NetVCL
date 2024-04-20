import { TWinControl } from "./nv.controls.js";

export class TNvDashboard extends TWinControl {
    _DefaultParams(o) {
        o.ClassCss ??= "grid-stack";
        this.FRenderPosition ??= false;
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FOptions = {
            columOpts: {
                breakpointForWindow: true,
                breakpoints: []
            }

        }
        this.FInitialized = false;
    }

    _ChangeParams(o) {
        super._ChangeParams(o);
        this._InitGridStack();
        this.FInitialized = true;
    }

    //_DoInsertControl(C) {
    //   super._DoInsertControl(C);
    // if (C instanceof TNvDashbdItem)
    //     this.FGridStack.makeWidget(C.El[0]);
    // }

    _InitGridStack() {
        this.FGridStack = GridStack.init(this.FOptions, this.FEl[0]);
    }

    _DestroyGridStack() {
        if (this.FGridStack)
            this.FGridStack.destroy(false);
        this.FGridStack = undefined;
    }

    _DoRemoveControl(C) {
        this.FGridStack.removeWidget(C.El[0], false, false);
        super._DoRemoveControl(C);
    }

    _DoSetParent(P) {
        this._DestroyGridStack();
        super._DoSetParent(P);
        if ((this.FInitialized) && (typeof P == 'object' && P !== null))
            this._InitGridStack();
    }

    Reset() {
        this._DestroyGridStack();
        if (this.Parent)
            this._InitGridStack();
    }

    get AcceptWidgets() { return this.FOptions.acceptWidgets ?? false; }
    get Animate() { return this.FOptions.animate ?? true; }
    get CellHeight() { return this.FOptions.cellHeight ?? "auto"; }
    get DisableDrag() { return this.FOptions.disableDrag ?? false; }
    get DisableResize() { return this.FOptions.disableResize ?? false; }
    get Margin() { return this.FOptions.margin ?? 10; }
    get Breakpoints() { return this.FOptions.columOpts.breakpoints ?? []; }
    get Float() { return this.FOptions.float ?? false; }

    set AcceptWidgets(V) {
        if (V !== this.FOptions.acceptWidgets) {
            if (this.FGridStack) this.FGridStack.update(this.FEl, { acceptWidgets: V });
            this.FOptions.acceptWidgets = V;
        }
    }

    set Animate(V) {
        if (V !== this.FOptions.animate) {
            if (this.FGridStack) this.FGridStack.update(this.FEl, { animate: V });
            this.FOptions.animate = V;
        }
    }

    set CellHeight(V) {
        if (V !== this.FOptions.cellHeight) {
            if (this.FGridStack) this.FGridStack.update(this.FEl, { cellHeight: V });
            this.FOptions.cellHeight = V;
        }
    }

    set DisableDrag(V) {
        if (V !== this.FOptions.disableDrag) {
            if (this.FGridStack) this.FGridStack.update(this.FEl, { disableDrag: V });
            this.FOptions.disableDrag = V;
        }
    }

    set DisableResize(V) {
        if (V !== this.FOptions.disableResize) {
            if (this.FGridStack) this.FGridStack.update(this.FEl, { disableResize: V });
            this.FOptions.disableResize = V;
        }
    }

    set Margin(V) {
        if (V !== this.FOptions.margin) {
            if (this.FGridStack) this.FGridStack.update(this.FEl, { margin: V });
            this.FOptions.margin = V;
        }
    }

    set Breakpoints(V) {
        if (V !== this.FOptions.breakpoints) {
            if (this.FGridStack) this.FGridStack.update(this.FEl, { breakpoints: V });
            this.FOptions.breakpoints = V;
        }
    }

    set Float(V) {
        if (V !== this.FOptions.float) {
            if (this.FGridStack) this.FGridStack.update(this.FEl, { float: V });
            this.FOptions.float = V;
        }
    }

    _ProcessEvent(e) {
        if (e.type == "change") {
            let items = e.originalEvent.detail;
            let evt = { type: e.type, changes: [] };
            items.forEach(i => {
                let C = {
                    iId: i.el.id,
                    h: i.h,
                    w: i.w,
                    x: i.x,
                    y: i.y
                };
                evt.changes.push(C);
            });
            super._ProcessEvent(evt);
        } else
            super._ProcessEvent(e);
    }

}



export class TNvDashbdItem extends TWinControl {
    _DefaultParams(o) {
        o.ClassCss ??= "grid-stack-item";
        this.FRenderPosition ??= false;
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FContent = $(document.createElement("div"))
            .addClass("grid-stack-item-content")
            .appendTo(this.FEl)
    }

    _ChangeParams(o) {
        super._ChangeParams(o);
        if ((this.Parent instanceof TNvDashboard) && (this.Parent.FGridStack))
            this.Parent.FGridStack.makeWidget(this.El[0]);
    }

    _CreateElement() {
        super._CreateElement();
        this.FEl.attr("gs-id", this.FId);
    }

    _DoInsertControl(C) {
        C.El.appendTo(this.FContent);
    }

    // Getters
    get AutoPosition() { return this.FEl.attr('gs-auto-position'); }
    get X() { return parseInt(this.FEl.attr('gs-x'), 10); }
    get Y() { return parseInt(this.FEl.attr('gs-y'), 10); }
    get W() { return parseInt(this.FEl.attr('gs-w'), 10); }
    get H() { return parseInt(this.FEl.attr('gs-h'), 10); }
    get MaxW() { return parseInt(this.FEl.attr('gs-max-w'), 10); }
    get MinW() { return parseInt(this.FEl.attr('gs-min-w'), 10); }
    get MaxH() { return parseInt(this.FEl.attr('gs-max-h'), 10); }
    get MinH() { return parseInt(this.FEl.attr('gs-min-h'), 10); }
    get Locked() { return this.FEl.attr('gs-locked') === 'true'; }
    get NoResize() { return this.FEl.attr('gs-no-resize') === 'true'; }
    get NoMove() { return this.FEl.attr('gs-no-move') === 'true'; }
    get SizeToContent() { return this.FEl.attr('gs-size-to-content') === 'true'; }

    // Setters
    set AutoPosition(V) {
        if (V !== this.AutoPosition) {
            this.FEl.attr('gs-auto-position', V);
        }
    }

    set X(V) {
        if (V !== this.X) {
            this.FEl.attr('gs-x', V);
            //se tiver um, precisa do outro, e como 0 é padrão, não vem no json
            if (!this.FEl.attr('gs-y'))
                this.FEl.attr('gs-y', 0);
        }
    }

    set Y(V) {
        if (V !== this.Y) {
            this.FEl.attr('gs-y', V);
            //se tiver um, precisa do outro, e como 0 é padrão, não vem no json
            if (!this.FEl.attr('gs-x'))
                this.FEl.attr('gs-x', 0);
        }
    }

    set W(V) {
        if (V !== this.W) {
            this.FEl.attr('gs-w', V);
        }
    }

    set H(V) {
        if (V !== this.H) {
            this.FEl.attr('gs-h', V);
        }
    }

    set MaxW(V) {
        if (V !== this.MaxW) {
            this.FEl.attr('gs-max-w', V);
        }
    }

    set MinW(V) {
        if (V !== this.MinW) {
            this.FEl.attr('gs-min-w', V);
        }
    }

    set MaxH(V) {
        if (V !== this.MaxH) {
            this.FEl.attr('gs-max-h', V);
        }
    }

    set MinH(V) {
        if (V !== this.MinH) {
            this.FEl.attr('gs-min-h', V);
        }

    }

    set Locked(V) {
        if (V !== this.Locked) {
            this.FEl.attr('gs-locked', V);
        }
    }

    set NoResize(V) {
        if (V !== this.NoResize) {
            this.FEl.attr('gs-no-resize', V);
        }
    }

    set NoMove(V) {
        if (V !== this.NoMove) {
            this.FEl.attr('gs-no-move', V);
        }
    }

    set SizeToContent(V) {
        if (V !== this.SizeToContent) {
            this.FEl.attr('gs-size-to-content', V);
        }
    }
}




