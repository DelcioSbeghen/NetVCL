import { TNvBsGridContainer } from "./nv.bs.containers.js";
import { TBsControl, TNvBsGridControl } from "./nv.bs.controls.js";
import { TNvBsLink } from "./nv.bs.htmlcontrols.js";


function _DoChangeFloatPos(C, floatPos) {
    C.FEl//
        .removeClassRegex("(^|\\b)(position-(static|relative|absolute|fixed|sticky)+)(\\b(?!-)|$)")
        .removeClassStartingWith("start-")
        .removeClassStartingWith("end-")
        .removeClassStartingWith("top-")
        .removeClassStartingWith("bottom-")
        .removeClassStartingWith("translate-");
    switch (floatPos) {
        case "top-left":
            C.FEl.addClass("position-absolute start-0");
            break;
        case "top-center":
            C.FEl.addClass("position-absolute top-0 start-50 translate-middle-x");
            break;
        case "top-right":
            C.FEl.addClass("position-absolute top-0 end-0");
            break;
        case "center-left":
            C.FEl.addClass("position-absolute top-50 start-0 translate-middle-y");
            break;
        case "center":
            C.FEl.addClass("position-absolute top-50 start-50 translate-middle");
            break;
        case "center-right":
            C.FEl.addClass("position-absolute top-50 end-0 translate-middle-y");
            break;
        case "bottom-left":
            C.FEl.addClass("position-absolute bottom-0 start-0");
            break;
        case "bottom-center":
            C.FEl.addClass("position-absolute bottom-0 start-50 translate-middle-x");
            break;
        case "bottom-right":
            C.FEl.addClass("position-absolute bottom-0 end-0");
            break;
        default:
            C._DoPosition(C.FPositon);
    }

}



export class TNvBsAlert extends TNvBsGridContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "alert";
        this.FBgPrefix ??= "alert-";
        o.Background ??= "info";
        o.Fade ??= true;
        o.Role ??= "alert";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        //this.FVariant = "info";
        //this.Variant = o.Variant || "info"; //'primary' | 'secondary' | 'success' | 'danger' | 'warning' | 'info' | 'dark' | 'light' 
        //this.FGrids = new TBsGrids(this, o.Grids || {});
        //this.Animated = o.Animated || this.FAnimated;
        this.FFloatPos = "";
        this.FTimeout = 0;
        this.FShowClose = true;
        this.FVisible = false;
    }

    _ChangeParams(o) {
        super._ChangeParams(o);
        if (this.FShowClose)
            this._ApplyShowClose();
        if (this.FFade)
            this.FEl.addClass("fade");
    }

    // _DoTextChange(T) {
    //     //override to change functionality
    //     // var _Resizer = this.FEl.children("iframe");
    //     // _Resizer.detach();
    //     this.FEl.html(T);
    //     // _Resizer.prependTo(this.FEl);
    //     this.FText = T;
    // }

    _ApplyTimeout() {
        if ((this.FTimeout > 0) && this.Visible)
            setTimeout(() => {
                this.Close()
            }, this.FTimeout);

    }

    _ApplyShowClose() {
        if (this.FShowClose) {
            this.FEl.addClass("alert-dismissible");
            this.FCloseBtn = $(document.createElement("button"))
                .attr("type", "button")
                .addClass("btn-close")
                .attr("data-bs-dismiss", "alert")
                .attr("aria-label", "close")
                .appendTo(this.FEl);
        } else if (this.FCloseBtn) {
            this.FCloseBtn.remove();
            this.FCloseBtn = undefined;
        }
    }

    get Timeout() { return this.FTimeout }
    set Timeout(V) {
        if (V != this.FTimeout) {
            this.FTimeout = V;
            this._ApplyTimeout();
        }
    }

    get ShowClose() { return this.FShowClose }
    set ShowClose(V) {
        if (V !== this.FShowClose) {
            this.FShowClose = V;
            this._ApplyShowClose();
        }
    }

    get Visible() { return super.Visible }
    set Visible(V) {
        super.Visible = V;
        //re-insert DOM element removed by bootstrap
        if (V && ((!this.FEl.parent()) || (this.FEl.parent().length == 0)) && (this.Parent != null)) {
            this.Parent.InsertControl(this);
            App.FResizeObserver.observe(this.FEl[0]);
        };
        if (V) this.FEl.addClass("show");
        this._ApplyTimeout();
    }

    Close() {
        this.FEl.alert("close");
    }

    get FloatPos() { return this.FFloatPos }
    set FloatPos(V) {
        if (V != this.FFloatPos) {
            _DoChangeFloatPos(this, V);
            this.FFloatPos = V;
        }
    }


}

export class TNvBsBadge extends TBsControl {
    _DefaultParams(o) {
        o.ClassCss ??= "badge";
        o.Tag ??= "span";
        this.FBgPrefix ??= "badge-"
        super._DefaultParams(o);
    }

    _CreateParams(O) {
        super._CreateParams(O);
        this.FPill = false;
    }

    get Pill() { return this.FPill }
    set Pill(V) {
        if (V != this.FPill) {
            this.FEl.removeClass("rounded-pill");
            if (V) this.FEl.addClass("rounded-pill");
        }

    }
}

export class TNvBsBadgeLink extends TNvBsLink {
    _DefaultParams(o) {
        o.ClassCss ??= "badge";
        this.FBgPrefix ??= "badge-"
        super._DefaultParams(o);
    }

    _CreateParams(O) {
        super._CreateParams(O);
        this.FPill = false;
    }

    get Pill() { return this.FPill }
    set Pill(V) {
        if (V != this.FPill) {
            this.FEl.removeClass("rounded-pill");
            if (V) this.FEl.addClass("rounded-pill");
        }

    }
}


export class TNvBsToast extends TNvBsAlert {
    _DefaultParams(o) {
        o.ClassCss ??= "toast";
        this.FBgPrefix ??= "bg-";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FEl.attr("data-bs-autohide", "false");
        this.FEl.on("hide.bs.toast.nvjs", (e) => { this.FEl.trigger("close") });
        this.FHeader = $(document.createElement("div"))
            .addClass("toast-header")
            .appendTo(this.FEl);
        this.FElTitle = $(document.createElement("strong"))
            .addClass("me-auto")
            .appendTo(this.FHeader);
        this.FElTitleSmall = $(document.createElement("small"))
            .appendTo(this.FHeader);
        this.FBody = $(document.createElement("div"))
            .addClass("toast-body")
            .appendTo(this.FEl);
        this.FTimeout = 5000;
        this.FShowClose = true;
        this.FTitle = "";
        this.FTitleSmall = "";
    }

    _ApplyShowClose() {
        if (this.FShowClose) {
            this.FEl.attr("data-bs-autohide", false);
            this.FCloseBtn = $(document.createElement("button"))
                .attr("type", "button")
                .addClass("ms-2 mb-1 btn-close")
                .attr("data-bs-dismiss", "toast")
                .attr("aria-label", "Close")
                .html('<span aria-hidden="true">&times;</span>')
                .appendTo(this.FHeader);
        } else if (this.FCloseBtn) {
            this.FCloseBtn.remove();
            this.FCloseBtn = undefined;
            this.FEl.removeAttr("data-bs-autohide");
        }
    }

    _DoImageChange() {
        //override to change functionality
        if (this.FImage != "") {
            this.FElImage = $(this.FImage);
            this.FElImage.addClass("rounded me-2");
            this.FHeader.prepend(this.FElImage);
        } else if (this.FElImage) {
            this.FElImage.remove();
            this.FElImage = undefined;
        }
    }

    _ApplyTimeout() {
        if (this.FVisible)
            this.Show()
        else
            this.Close();
        super._ApplyTimeout();
    }

    _DoTextChange(T) {
        if (this.FText !== T) {
            this.FBody.setTextPreserveChilds(T.htmlEscape());
            this.FText = T;
        }
    }

    get Title() { return this.FTitle }
    set Title(V) {
        if (V !== this.FTitle) {
            this.FElTitle.html(V);
            this.FTitle = V;
        }
    }

    get TitleSmall() { return this.FTitleSmall }
    set TitleSmall(V) {
        if (V !== this.FTitleSmall) {
            this.FElTitleSmall.html(V);
            this.FTitleSmall = V;
        }
    }

    Close() {
        this.FEl.toast("hide");
    }

    Show() {
        this.FEl.toast("show");
    }

}


export class TNvBsSpinner extends TNvBsGridControl {
    _DefaultParams(o) {
        o.ClassCss ??= "spinner-border";
        o.Role ??= "status";
        super._DefaultParams(o);
    }
    
    
    _CreateParams(o) {
        super._CreateParams(o);
        this.FFloatPos = "";
    }


    get FloatPos() { return this.FFloatPos }
    set FloatPos(V) {
        if (V != this.FFloatPos) {
            _DoChangeFloatPos(this, V);
            this.FFloatPos = V;
        }
    }

}

TApplication.RegisterClass(TNvBsAlert);
TApplication.RegisterClassModule(TNvBsAlert, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsAlert, './bootstrap.min.css', 'css');
