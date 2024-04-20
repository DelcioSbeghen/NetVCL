import { TNvBsGridContainer, TNvBsContainer } from "./nv.bs.containers.js";
import { TNvBsCard, TNvBsCardBody } from "./nv.bs.cards.js";

export class TNvBsAccordion extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("accordion");
    }

    InsertControl(C) {
        super.InsertControl(C);
        //update data-parent attribute in collapse
        if (C instanceof TNvBsAccordionItem){
            C.FCollapse.attr("data-parent", "#" + this.FId);           
        }
    }

}

export class TNvBsAccordionItem extends TNvBsCard {
    _CreateParams(o) {
        super._CreateParams(o);

        this.FHeader = $(document.createElement("div"))
            .addClass("card-header")
            .appendTo(this.FEl);

        this.FTitle = $(document.createElement("h5"))
            .addClass("mb-0")
            .appendTo(this.FHeader);

        this.FTitleLink = $(document.createElement("a"))
            .attr("data-toggle", "collapse")
            .attr("data-target", "#" + this.FId + "_collapse")
            .appendTo(this.FTitle);

        this.FCollapse = $(document.createElement("div"))
            .attr("id", this.FId + "_collapse")
            .addClass("collapse")
            .appendTo(this.FEl);
        //.attr("data-parent", "collapse")
    }

    _DoImageChange() {
        //override to change functionality
        if (this.FImage != "") {
            this.FElImage = $(this.FImage);
            this.FElImage.addClass("rounded mr-2");
            this.FTitleLink.prepend(this.FElImage);
        } else if (this.FElImage) {
            this.FElImage.remove();
            this.FElImage = undefined;
        }
    }

    _DoTextChange(T) {
        if (this.FText !== T) {
            this.FTitleLink.setTextPreserveChilds(T);
            this._DoImageChange();
            this.FText = T;
        }
    }

    InsertControl(C) {
        super.InsertControl(C);
        if (C instanceof TNvBsAccordionBody){
            C.El.appendTo(this.FCollapse);           
        }
    }

    Show(){
        this.FCollapse.collapse("show");
    }
    
}

export class TNvBsAccordionBody extends TNvBsCardBody {

}
