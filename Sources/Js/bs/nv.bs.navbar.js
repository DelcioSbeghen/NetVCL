import { TNvBsContainer, TNvBsGridContainer } from "./nv.bs.containers.js";



export class TNvBsNav extends TNvBsGridContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("nav");
    }
    _Tag() { return "ul" }
}


export class TBsNavItem extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("nav-item");
        //if (App.FDesign)
        //    this.AddClass("design-div");
    }
    _Tag() { return "li" }
}

export class TBsNavItemLink extends TBsNavItem {
    _CreateParams(o) {
        this.FLink = $(document.createElement("a"))
            .addClass("nav-link")
        super._CreateParams(o);
        //if (App.FDesign)
        //    this.AddClass("design-div");
        this.FLink.appendTo(this.FEl);;
        this.FHref = "";
        this.HRef = o.HRef || "#";
    }

    _DoTextChange(T) {
        this.FLink.html(T);
        this.FText = T;
    }

    get HRef() { return this.FHref }
    set HRef(V) {
        if (V != this.FHref) {
            this.FLink.attr("href", V);
            this.FHref = V;
        }
    }
}


export class TBsNavItemDropdown extends TBsNavItemLink {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("dropdown");
        this.FLink
            .addClass("dropdown-toggle")
            .attr("role", "button")
            .attr("data-toggle", "dropdown")
            .attr("aria-haspopup", "true")
            .attr("aria-expanded", "false");
    }
}

export class TBsNavBarBrand extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("navbar-brand");
        //if (App.FDesign)
        //    this.AddClass("design-div");
        this.FHref = "";
        this.HRef = o.HRef || "#";
    }

    _Tag() {
        return "a";
    }

    get HRef() { return this.FHref }
    set HRef(V) {
        if (V != this.FHref) {
            this.FEl.attr("href", V);
            this.FHref = V;
        }
    }
}


export class TBsNavBarItemLink extends TBsNavItemLink {
    _Tag() { return "div" }
 }


export class TBsNavBarItemDropdown extends TBsNavItemDropdown {
    _Tag() { return "div" }
}



export class TBsNavBarContent extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("collapse navbar-collapse");
    }

}


export class TNvBsNavBar extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("navbar");
        //if (App.FDesign)
        //    this.AddClass("design-div");
        this.FExpand = '';
        this.Expand = o.Expand || 'lg'; //sm|md|lg|xl
        this.FColor = '';
        this.Color = o.Color || 'light'; //light|dark
    }

    get Expand() { return this.FExpand }
    set Expand(V) {
        if (this.FExpand !== V) {
            this.FEl.removeClassStartingWith("navbar-expand").addClass("navbar-expand-" + V);
            this.FExpand = V;
        }
    }

    get Color() { return this.FColor }
    set Color(V) {
        if (V !== this.FColor) {
            this.FEl.removeClass("navbar-light navbar-dark").addClass("navbar-" + V);
            this.FColor = V;
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



