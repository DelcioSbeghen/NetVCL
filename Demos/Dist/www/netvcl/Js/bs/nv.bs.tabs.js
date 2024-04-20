import { TNvBsNav, TBsNavItemLink, TBsNavItemDropdown } from "./nv.bs.navbar.js";
import { TBsWinControl } from "./nv.bs.controls.js";
import { TNvBsGridContainer } from "./nv.bs.containers.js";


export class TNvBsTabControl extends TNvBsGridContainer {
}

export class TNvBsTabs extends TNvBsNav {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("nav-tabs");
        this.FCloseBtn = null;
        this.FEl.attr("role", "tablist");
    }
}


export class TNvBsTabHeaderLink extends TBsNavItemLink {
    _CreateParams(o) {
        super._CreateParams(o);
        this.FLink.attr("data-toggle", "tab").attr("role", "tab");
        this.FShowClose = false;
       // this.ShowClose = o.ShowClose || false;
    }

    _DoCloseBtnClick(e){
        this.FEl.trigger("close");
    }

    get ShowClose() { return this.FShowClose }
    set ShowClose(V) {
        if (V != this.FShowClose) {
            if (V) {
                this.FCloseBtn = $(document.createElement("button"))
                    .addClass("close close-tab")
                    .html("X")
                    .appendTo(this.FLink)
                    .off("click.nvjs").on("click.nvjs",(e) => this._DoCloseBtnClick(e));
            }
            else {
                this.FCloseBtn.remove();
                this.FCloseBtn = null;
            }
            this.FShowClose = V;
        }
    }

    Show() {
        this.FLink.tab("show");
    }

    dispose() {
        this.FLink.tab("dispose");
    }
}

export class TNvBsTabDropdown extends TBsNavItemDropdown {
    //todo Dropdown tab only show popup menu ????

    Show() {
        this.FLink.tab("show");
    }

    dispose() {
        this.FLink.tab("dispose");
    }
}

export class TNvBsTabContent extends TBsWinControl {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("tab-content");
    }
}

export class TNvBsTab extends TBsWinControl {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("tab-pane");
        this.FEl.attr("role", "tabpanel");
    }
}


