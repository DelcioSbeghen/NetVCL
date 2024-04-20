import { TBsWinControl, TBsControl, TBsGrids, TBsBorders, TBsTextProps, TNvBsGridControl } from "./nv.bs.controls.js";

export class TNvBsContainer extends TBsWinControl {
    _CreateParams(o) {
        super._CreateParams(o);
        if (!this.FBgPrefix) this.FBgPrefix = 'bg-';
        if (!this.FBackground) this.FBackground = '';
        //this.Background = o.Background || this.FBackground;
        this.FBorder = new TBsBorders(this, o.Border || {});
        this.FTextProps = new TBsTextProps(this, o.TextProps || {});
        if (!this.FShadow) this.FShadow = '';
        //this.Shadow = o.Shadow || this.FShadow; // ''|small|normal|larger
        if (this.FFade == undefined)
            this.FFade = false;
        if (this.FCollapse == undefined)
            this.FCollapse = false;
        if (!this.FPosition) this.FPosition = '';

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
    get Position() { return this.FPosition }
    get Fade() { return this.FFade }
    get Collapse() { return this.FCollapse }

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
            this.FShadow === 'small' ? this.FEl.AddClass("shadow-sm") :
                this.FShadow === 'normal' ? this.FEl.addClass("shadow") :
                    this.FShadow === 'larger' ? this.FEl.addClass("shadow-lg") : '';
            this.FShadow = V;
        }
    }

    set Position(V) {
        if (V != this.FPosition) {
            this.FEl.removeClassRegex("(^|\\b)((fixed-top|fixed-bottom|sticky-top|position-static|position-relative|position-absolute|position-fixed|position-sticky)+)(\\b(?!-)|$)")
            if (V != "")
                this.FEl.addClass(V);
            this.FPosition = V;
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



}



export class TNvBsRow extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("row");
        if (App.FDesign)
            this.AddClass("design-div");
    }
}

export class TNvBsFormRow extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("form-row");
        if (App.FDesign)
            this.AddClass("design-div");
    }
}

export class TNvBsGridContainer extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("col");
        this.FGrids = new TBsGrids(this, o.Grids || {});
    }

    get Grids() { return this.FGrids }
}

export class TNvBsColumn extends TNvBsGridContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        if (App.FDesign)
            this.AddClass("design-div");
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

