import { TNvBsNav, TBsNavItemLink, TBsNavItemDropdown } from "./nv.bs.navbar.js";
import { TBsWinControl } from "./nv.bs.controls.js";
import { TNvBsGridContainer } from "./nv.bs.containers.js";


export class TNvBsTabControl extends TNvBsGridContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.FTabs = TNvBsTabs.Create(o.Tabs ??= {});
        this.FTabs.Parent = this;
        this.FContent = TNvBsTabContent.Create(o.Content ??= {});
        this.FContent.Parent = this;
    }

    get Tabs() { return this.FTabs }
    get Content() { return this.FContent }

    _DoInsertControl(C) {
        if (C instanceof TNvBsTab) {
            C.El.appendTo(this.FContent.El);
            C.TabHeader.El.appendTo(this.FTabs.El);
        }
        else
            super._DoInsertControl(C);
    }

    _DoRemoveControl(C) {
        if (C instanceof TNvBsTab) {
            C.El.detach();
            C.TabHeader.El.detach();
        }
        else
            super._DoInsertControl(C);
    }

}

export class TNvBsTabs extends TNvBsNav {
    _DefaultParams(o) {
        o.ClassCss ??= "nav nav-tabs";
        o.Role ??= "tablist";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FCloseBtn = null;
    }
}


export class TNvBsTabHeaderLink extends TBsNavItemLink {
    _DefaultParams(o) {
        o.Role ??= "presentation";
        super._DefaultParams(o);

    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FLink.FEl.attr("data-bs-toggle", "tab").attr("role", "tab");
        this.FShowClose = false;
        // this.ShowClose = o.ShowClose || false;
    }

    _DoCloseBtnClick(e) {
        this.FEl.trigger("close");
    }

    get ShowClose() { return this.FShowClose }
    set ShowClose(V) {
        if (V != this.FShowClose) {
            if (V) {
                this.FCloseBtn = $(document.createElement("button"))
                    .addClass("btn-close close-tab")
                    .appendTo(this.FLink.FEl)
                    .off("click.nvjs").on("click.nvjs", (e) => this._DoCloseBtnClick(e));
            }
            else {
                this.FCloseBtn.remove();
                this.FCloseBtn = null;
            }
            this.FShowClose = V;
        }
    }

    Show() {
        if (($("#" + this.FLink.Id).length > 0) && ($(this.FLink.HRef).length > 0))
            this.FLink.FEl.tab("show")
        else {//elements outside DOM
            let _PGC = this.FTab.Parent;
            let _Headers = _PGC.FTabs.El.children(".nav-item");
            let _links = _Headers.children(".nav-link");
            let _Active = _links.filter(".active");
            let _Contents = _PGC.FContent.El.children(".tab-pane");
            
            if (_Active == this.FLink) {
                return
            }

            //TODO: Ver se precisamos disparar os eventos manualmente
            _links.removeClass("active");
            _Contents.removeClass("show active");
            this.FLink.AddClassToEl("active");
            _Contents.filter(this.FLink.HRef).addClass("show active");
        }

    }

    dispose() {
        this.FLink.FEl.tab("dispose");
    }
}

export class TNvBsTabDropdown extends TBsNavItemDropdown {
    //todo Dropdown tab only show popup menu ????

    Show() {
        this.FLink.FEl.tab("show");
    }

    dispose() {
        this.FLink.FEl.tab("dispose");
    }
}

export class TNvBsTabContent extends TBsWinControl {
    _DefaultParams(o) {
        o.ClassCss ??= "tab-content";
        super._DefaultParams(o);
    }
}

export class TNvBsTab extends TBsWinControl {
    _DefaultParams(o) {
        o.ClassCss ??= "tab-pane";
        o.Role ??= "tabpanel";
        o.TabHeader.Link.HRef ??= "#" + o.Id;
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FTabHeader = TNvBsTabHeaderLink.Create(o.TabHeader ?? {});
        this.FTabHeader.FTab = this;
        this.FShowClose = false;
        // this.ShowClose = o.ShowClose || false;
    }

    get TabHeader() { return this.FTabHeader }

    _DoInsertControl(C) {
        if (C instanceof TNvBsTabHeaderLink) {
            //Ignore o.Tabheader.Parent
        }
        else
            super._DoInsertControl(C);
    }

}


