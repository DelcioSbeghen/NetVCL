import { TNvBsContainer } from "./nv.bs.containers.js";

export class TNvBsAccordion extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "accordion";
        super._DefaultParams(o);
    }   
    
    _DoInsertControl(C) {
        super._DoInsertControl(C);
        //update data-bs-parent attribute in collapse
        if (C instanceof TNvBsAccordionItem) {
            C.FCollapse.attr("data-bs-parent", "#" + this.FId);
        }
    }

}

export class TNvBsAccordionItem extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "accordion-item";
        super._DefaultParams(o);
    }
    
    _CreateParams(o) {
        super._CreateParams(o);

        this.FHeader = $(document.createElement("div"))
            .addClass("accordion-header")
            .appendTo(this.FEl);

        this.FTitle = $(document.createElement("button"))
            .addClass("accordion-button")
            .attr("type", "button")
            .attr("data-bs-toggle", "collapse")
            .attr("data-bs-target", "#" + this.FId + "_collapse")
            .attr("aria-expanded", "true")
            .attr("aria-controls", this.FId + "_collapse")
            .appendTo(this.FHeader);

        this.FCollapse = $(document.createElement("div"))
            .attr("id", this.FId + "_collapse")
            .addClass("accordion-collapse collapse")
            .appendTo(this.FEl);
        //attr "data-bs-parent" in TNvBsAccordion._D0InsertControl
    }

    _DoImageChange() {
        //override to change functionality
        if (this.FImage != "") {
            this.FElImage = $(this.FImage);
            this.FElImage.addClass("rounded me-2");
            this.FTitle.prepend(this.FElImage);
        } else if (this.FElImage) {
            this.FElImage.remove();
            this.FElImage = undefined;
        }
    }

    _DoTextChange(T) {
        if (this.FText !== T) {
            this.FTitle.setTextPreserveChilds(T.htmlEscape());
            this._DoImageChange();
            this.FText = T;
        }
    }

    _DoInsertControl(C) {
        if (C instanceof TNvBsAccordionBody)
            C.El.appendTo(this.FCollapse);
        else
            super._DoInsertControl(C);
    }

    Show() {
        this.FCollapse.collapse("show");
    }

    Hide() {
        this.FCollapse.collapse("hide");
    }

}

export class TNvBsAccordionBody extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "accordion-body";
        super._DefaultParams(o);
    }
}
