import { TNvBsGridContainer } from "./nv.bs.containers.js";


export class TNvBsAlert extends TNvBsGridContainer {
    _CreateParams(o) {
        if (!this.FBgPrefix) this.FBgPrefix = "alert-";
        if (!this.FBackground) this.FBackground = 'info';
        if (this.FFade == undefined) this.FFade = true;
        this.FFloat = false;
        super._CreateParams(o);
        this.AddClass("alert alert-info");
        //this.FVariant = "info";
        //this.Variant = o.Variant || "info"; //'primary' | 'secondary' | 'success' | 'danger' | 'warning' | 'info' | 'dark' | 'light' 
        //this.FGrids = new TBsGrids(this, o.Grids || {});
        if (this.FFade)
            this.AddClass("fade");
        //this.Animated = o.Animated || this.FAnimated;
        this.FTimeout = 0;
        this.FShowClose = true;
        this.FVisible = false;
    }

    _ChangeParams(o) {
        super._ChangeParams(o);
        if (this.FShowClose)
            this._ApplyShowClose();
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
            this.AddClass("alert-dismissible");
            this.FCloseBtn = $(document.createElement("button"))
                .attr("type", "button")
                .addClass("close")
                .attr("data-dismiss", "alert")
                .attr("aria-label", "close")
                .html('<span aria-hidden="true">&times;</span>')
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
        if (V && (!this.FEl.parent()) && (this.Parent != null)) {
            this.Parent.InsertControl(this);
            App.FResizeObserver.observe(this.FEl[0]);
        };
        if (V) this.AddClass("show");
        this._ApplyTimeout();
    }

    Close() {
        this.FEl.alert("Close");
    }

    get Float() { return this.FFloat }
    set Float(V) {
        if (V != this.FFloat) {
            if (V)
                this.AddClass('alert-float')
            else
                this.RemoveClass('alert-float');
        }
    }


}

export class TNvBsToast extends TNvBsAlert {
    _CreateParams(o) {
        if (!this.FBgPrefix) this.FBgPrefix = "bg-";
        if (!this.FBackground) this.FBackground = 'info';
        if (this.FFade == undefined) this.FFade = true;
        super._CreateParams(o);
        this.FEl.removeClass("alert alert-info fade show");
        this.AddClass("toast bg-info");
        this.FEl.attr("data-autohide", "false");
        this.FEl.on("hide.bs.toast.nvjs", (e) => { this.FEl.trigger("close") });
        this.FHeader = $(document.createElement("div"))
            .addClass("toast-header")
            .appendTo(this.FEl);
        this.FElTitle = $(document.createElement("strong"))
            .addClass("mr-auto")
            .appendTo(this.FHeader);
        this.FElTitleSmall = $(document.createElement("small"))
            .appendTo(this.FHeader);
        this.FBody = $(document.createElement("div"))
            .addClass("toast-body")
            .appendTo(this.FEl);
        this.FTimeout = 500;
        this.FShowClose = true;
        this.FTitle = "";
        this.FTitleSmall = "";
     }

    _ApplyShowClose() {
        if (this.FShowClose) {
            this.FEl.attr("data-autohide", false);
            this.FCloseBtn = $(document.createElement("button"))
                .attr("type", "button")
                .addClass("ml-2 mb-1 close")
                .attr("data-dismiss", "toast")
                .attr("aria-label", "Close")
                .html('<span aria-hidden="true">&times;</span>')
                .appendTo(this.FHeader);
        } else if (this.FCloseBtn) {
            this.FCloseBtn.remove();
            this.FCloseBtn = undefined;
            this.FEl.removeAttr("data-autohide");
        }
    }

    _DoImageChange() {
        //override to change functionality
        if (this.FImage != "") {
            this.FElImage = $(this.FImage);
            this.FElImage.addClass("rounded mr-2");
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
            this.FBody.setTextPreserveChilds(T);
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

TApplication.RegisterClass(TNvBsAlert);
TApplication.RegisterClassModule(TNvBsAlert, './bootstrap.min.js', 'js');
TApplication.RegisterClassModule(TNvBsAlert, './bootstrap.min.css', 'css');
