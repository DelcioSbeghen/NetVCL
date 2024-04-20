import { TNvBsButton } from "./nv.bs.buttons.js";
import { TNvBsContainer, TNvBsGridContainer } from "./nv.bs.containers.js";
import { TNvBsLink } from "./nv.bs.htmlcontrols.js";
import { TControl } from "../nv.controls.js";



export class TNvBsNav extends TNvBsGridContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "nav";
        o.Tag ??= "ul";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FUnderline = false;
        this.FVertical = false;
    }

    _DoInsertControl(C) {
        super._DoInsertControl(C);
        if (C instanceof TBsNavItemCollapse) {
            if (this.ParentIs(TNvBsSideBar)) {
                if (this.FParent.FHoverItems)
                    C.FLink.FEl
                        .attr("data-bs-toggle", "")
                        .attr("data-bs-target", "#")
                else
                    C.FCollapse.attr("data-bs-parent", "#" + this.FParent.FId)
            }
            else
                C.FCollapse.attr("data-bs-parent", "#" + this.FId)
        }

    }

    get Underline() { return this.FUnderline }
    set Underline(V) {
        if (V !== this.FUnderline) {
            if (V)
                this.FEl.addClass("nav-underline")
            else
                this.FEl.removeClass("nav-underline");
            this.FUnderline = V;
        }
    }

    get Vertical() { return this.FVertical }
    set Vertical(V) {
        if (V != this.FVertical) {
            if (V)
                this.FEl.addClass("flex-column")
            else
                this.FEl.removeClass("flex-column");

            this.FVertical = V;
        }
    }

}

export class TNvBsNavLink extends TNvBsLink {
    _DefaultParams(o) {
        o.ClassCss ??= "nav-link";
        o.Tag ??= "a";
        super._DefaultParams(o);
    }
}


export class TBsNavItem extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "nav-item";
        o.Tag ??= "li";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FLink = TNvBsNavLink.Create(o.Link ?? {});
        this.FLink.Parent = this;
    }

    get Link() { return this.FLink }
}

export class TBsNavItemLink extends TBsNavItem {
    _CreateParams(o) {
        super._CreateParams(o);
        this.FLink.Tag = "a";
        this.FLink.FEl.addClass("nav-link");

    }

    _DoTextChange(T) {
        this.FLink.FEl.setTextPreserveChilds(T.htmlEscape());
        this.FText = T;
    }

}


export class TBsNavItemDropdown extends TBsNavItemLink {
    _DefaultParams(o) {
        o.ClassCss ??= "dropdown";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FLink.FEl
            .addClass("dropdown-toggle")
            .attr("role", "button")
            .attr("data-bs-toggle", "dropdown")
            .attr("aria-haspopup", "true")
            .attr("aria-expanded", "false");
    }
}

export class TBsNavItemCollapse extends TBsNavItemLink {
    _CreateParams(o) {
        super._CreateParams(o);
        this.FLink.FEl
            .attr("role", "button")
            .attr("data-bs-toggle", "collapse")
            .attr("data-bs-target", "#" + this.FId + "_collapse")
            .attr("aria-controls", this.FId + "_collapse")
            .attr("aria-expanded", "false");

        this.FCollapse = $(document.createElement("div"))
            .attr("id", this.FId + "_collapse")
            .addClass("collapse")
            .appendTo(this.FEl);
    }

    _DoInsertControl(C) {
        if ((C instanceof TControl) && (C !== this.FLink))
            C.El.appendTo(this.FCollapse);
        else
            super._DoInsertControl(C);
    }

}


export class TBsNavBarBrand extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "navbar-brand";
        o.Tag ??= "a";
        o.HRef ??= "#";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FHref = "";
    }

    get HRef() { return this.FHref }
    set HRef(V) {
        if (V != this.FHref) {
            this.FEl.attr("href", V);
            this.FHref = V;
        }
    }
}

export class TBsNavBarToggler extends TNvBsButton {
    _DefaultParams(o) {
        o.ClassCss ??= "navbar-toggler";
        o.DataToggle ??= "collapse";
        super._DefaultParams(o);
    }
    _CreateParams(o) {
        super._CreateParams(o);
        this.FEl.attr("aria-expanded", "false");
        //    aria-label="Toggle navigation"
    }

    _ChangeParams(o) {
        super._ChangeParams(o);
        if ((!o.DataTarget) && (this.Parent))
            this.DataTarget = this.Parent.El.children(".navbar-collapse")[o].id;
    }

}


export class TBsNavBarItemLink extends TBsNavItemLink {
    _DefaultParams(o) {
        o.Tag ??= "div";
        super._DefaultParams(o);
    }
}


export class TBsNavBarItemDropdown extends TBsNavItemDropdown {
    _DefaultParams(o) {
        o.Tag ??= "div";
        super._DefaultParams(o);
    }
}



export class TBsNavBarContent extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "collapse navbar-collapse";
        super._DefaultParams(o);
    }
}

export class TBsNavBarNav extends TNvBsNav {
    _DefaultParams(o) {
        o.ClassCss ??= "navbar-nav";
        o.Tag ??= "div";
        super._DefaultParams(o);
    }
}


export class TBsNavBarContainer extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "container-fluid";
        super._DefaultParams(o);
    }
}


export class TNvBsNavBar extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "navbar";
        o.Tag ??= "nav";
        o.Expand ??= "lg"; //sm|md|lg|xl
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FExpand = '';
    }

    get Expand() { return this.FExpand }
    set Expand(V) {
        if (this.FExpand !== V) {
            this.FEl.removeClassStartingWith("navbar-expand-").addClass("navbar-expand-" + V);
            this.FExpand = V;
        }
    }

    get Brand() {
        this.FControls.forEach(Control => {
            if (Control instanceof TBsNavBarBrand) {
                return Control;
            }
        });
        //todo implement same code of pascal for auto create Brand       
    }

    get Content() {
        this.FControls.forEach(Control => {
            if (Control instanceof TBsNavBarContent) {
                return Control;
            }
        });
        //todo implement same code of pascal for auto create Content       
    }
}


export class TNvBsSideBar extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "sidebar";
        o.Expand ??= "lg"; //sm|md|lg|xl|xxl
        o.Full ??= 'xxl';
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FEl
            .on("mouseenter.nvjs", () => this._DoMouseEnter())
            .on("mouseleave.nvjs", () => this._DoMouseLeave())
        this.FExpand = '';
        this.FFull = '';
        this.FHoverItems = false;
        this.FCollapsedItems = [];
    }

    _DoInsertControl(C) {
        super._DoInsertControl(C);
        if (C instanceof TNvBsNav)
            C.FControls.forEach(_Item => {
                if (_Item instanceof TBsNavItemCollapse)
                    _Item.FCollapse.attr("data-bs-parent", "#" + this.FId);
            });
    }

    _DoMouseEnter() {
        if (this.FExpand != "") {
            this.FCollapsedItems.collapse('show');
        }
    }

    _DoMouseLeave() {
        if (this.FExpand != "") {
            this.FCollapsedItems = this.FEl.find(".collapse.show");
            this.FCollapsedItems.collapse('hide');
        }
    }


    get Expand() { return this.FExpand }
    set Expand(V) {
        if (this.FExpand !== V) {
            this.FEl.removeClassStartingWith("sidebar-expand").addClass("sidebar-expand-" + V);
            this.FExpand = V;
        }
    }

    get Full() { return this.FFull }
    set Full(V) {
        if (this.FFull !== V) {
            this.FEl.removeClassStartingWith("sidebar-full-");
            if (V != "")
                this.FEl.addClass("sidebar-full-" + V);
            this.FFull = V;
        }
    }

    get HoverItems() { return this.FHoverItems };
    set HoverItems(V) {
        if (V != this.FHoverItems) {
            this.FEl.removeClass("sidebar-hover");
            if (V)
                this.FEl.addClass("sidebar-hover");
            this.FHoverItems = V;
        }
    }

    get Brand() {
        this.FControls.forEach(Control => {
            if (Control instanceof TBsNavBarBrand) {
                return Control;
            }
        });
        //todo implement same code of pascal for auto create Brand       
    }

    get Content() {
        this.FControls.forEach(Control => {
            if (Control instanceof TBsNavBarContent) {
                return Control;
            }
        });
        //todo implement same code of pascal for auto create Content       
    }







}




