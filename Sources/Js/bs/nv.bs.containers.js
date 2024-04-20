import { TBsWinControl, TBsControl, TBsGrids, TBsBorders, TBsTextProps, TNvBsGridControl, TBsPosition } from "./nv.bs.controls.js";

export class TNvBsContainer extends TBsWinControl {
    _DefaultParams(o) {
        this.FBgPrefix ??= "bg-";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FFade = false;
        this.FCollapse = false;
        this.FBackground = '';
        this.FShadow = "";
        this.FBorder = new TBsBorders(this, o.Border || {});
        this.FTextProps = new TBsTextProps(this, o.TextProps || {});
        this.FPosition = new TBsPosition(this, o.Position || {});
        this.FWidth = ""; //''|'auto'|'25'|'50'|'75'|'100'

        this._CollapseEl()
            .off("shown.bs.collapse.nvjs").on("shown.bs.collapse.nvjs", (e) => {
                this.ProcessEvent(e);
                this.FCollapse = false;
            })
            .off("hidden.bs.collapse.nvjs").on("hidden.bs.collapse.nvjs", (e) => {
                this.ProcessEvent(e);
                this.FCollapse = true;
            });


    }

    get Background() { return this.FBackground }
    get Border() { return this.FBorder }
    get TextProps() { return this.FTextProps }
    get Shadow() { return this.FShadow }
    get Fade() { return this.FFade }
    get Collapse() { return this.FCollapse }
    get Width_() { return this.FWidth };

    set Background(V) {
        if (V != this.FBackground) {
            this.FEl.removeClassStartingWith(this.FBgPrefix);
            if (V !== '')
                this.FEl.addClass(this.FBgPrefix + V);
            this.FBackground = V;
        }
    }

    set Shadow(V) {
        if (V != this.FShadow) {
            this.FEl.removeClassStartingWith("shadow");
            V === 'sm' ? this.FEl.addClass("shadow-sm") :
                V === 'md' ? this.FEl.addClass("shadow") :
                    V === 'lg' ? this.FEl.addClass("shadow-lg") : '';
            this.FShadow = V;
        }
    }

    set Fade(F) {
        if (F !== this.FFade) {
            if (F === true)
                this.FEl.addClass("fade" + this.Visible ? " show" : "")
            else
                this.FEl.removeClass("fade show");
            this.FAnimated = F;
        }
    }

    _CollapseEl() {
        return this.FEl;
    }

    set Collapse(V) {
        if (V !== this.FCollapse) {
            if (V === true)
                this._CollapseEl().collapse("hide")
            else
                this._CollapseEl().collapse("show");
            this.FCollapse = F;
        }
    }
    get BgPrefix() { return this.FBgPrefix }

    set BgPrefix(V) {
        if (V != this.FBgPrefix) {
            this.FBgPrefix = V;
        }
    }

    set Width_(V) {
        if (V != this.FWidth) {
            this.FEl.removeClassRegex("(^|\\b)(w-(auto|25|50|75|100)+)(\\b(?!-)|$)")
            if (V != "")
                this.FEl.addClass("w-" + V);
        }
    }
}



export class TNvBsRow extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "row";
        this.FDesignClass ?? "design design-div"
        super._DefaultParams(o);
    }
}

export class TNvBsFormRow extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "form-row";
        o.DesignClass ?? "design design-div"
        super._DefaultParams(o);
    }
}

export class TNvBsGridContainer extends TNvBsContainer {
    _DefaultParams(o) {
        //o.ClassCss ??= "col"; dont need???
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FGrids = new TBsGrids(this, o.Grids ?? {});
    }

    get Grids() { return this.FGrids }
}

export class TNvBsColumn extends TNvBsGridContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "col";
        this.FDesignClass ?? "design design-div"
        super._DefaultParams(o);
    }
}

export class TNvBsListItem extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "list-group-item";
        o.Tag ??= "li";
        this.FBgPrefix ??= "list-group-item-";
        this.FDesignClass ?? "design design-div"
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FActive = false;

    }

    _DoEnabledChange() {
        if (this.FEnabled)
            this.FEl.removeClass("disabled")
        else
            this.FEl.addClass("disabled");
    }

    get Active() { return this.FActive };
    set Active(V) {
        if (V != this.FActive) {
            this.FEl.removeClass("active");
            if (V)
                this.FEl.addClass("active");
            this.FActive = V;
        }
    }

}


export class TNvBsListGroup extends TNvBsGridContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "list-group";
        o.Tag ??= "ul";
        this.FDesignClass ?? "design design-div"
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FFlush = false;
        this.FHorizontal = false;
        this.FNumbered = false;
    }

    get Flush() { return this.FFlush };
    set Flush(V) {
        if (V != this.FFlush) {
            this.FEl.removeClass("list-group-flush");
            if (V)
                this.FEl.addClass("list-group-flush");
            this.FFlush = V;
        }
    }

    get Horizontal() { return this.FHorizontal };
    set Horizontal(V) {
        if (V != this.FHorizontal) {
            this.FEl.removeClass("list-group-horizontal");
            if (V)
                this.FEl.addClass("list-group-horizontal");
            this.FHorizontal = V;
        }
    }

    get Numbered() { return this.FNumbered };
    set Numbered(V) {
        if (V != this.FNumbered) {
            this.FEl.removeClass("list-group-numbered");
            if (V)
                this.FEl.addClass("list-group-numbered");
            this.FNumbered = V;
        }
    }
}


TApplication.RegisterClass(TNvBsRow);
TApplication.RegisterClassModule(TNvBsRow, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsRow, './bootstrap.min.css', 'css');

TApplication.RegisterClass(TNvBsFormRow);
TApplication.RegisterClassModule(TNvBsFormRow, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsFormRow, './bootstrap.min.css', 'css');

TApplication.RegisterClass(TNvBsColumn);
TApplication.RegisterClassModule(TNvBsColumn, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsColumn, './bootstrap.min.css', 'css');

