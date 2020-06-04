import { TNvBsGridContainer, TNvBsContainer } from "./nv.bs.containers.js";
import { TBsControl } from "./nv.bs.controls.js";
import { TNvBSText, TNvBsLink } from "./nv.bs.htmlcontrols.js";

export class TNvBsCard extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card");
        if (App.FDesign)
            this.AddClass("design-div");
    }
}

export class TNvBsCardHeader extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-header");
    }
}

export class TNvBsCardBody extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-body");
       }
}

export class TNvBsCardFooter extends TNvBsContainer {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-footer");
    }
}

export class TNvBsCardImage extends TBsControl {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-image-top");
       }
}

export class TNvBsCardTitle extends TNvBSText {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-title");
    }
    _Tag(){return 'h5'}
}

export class TNvBsCardSubtitle extends TNvBSText {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-subtitle");
    }
    _Tag(){return 'h6'}
}

export class TNvBsCardLink extends TNvBsLink {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-link");
    }
}

export class TNvBsCardText extends TNvBSText {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-text");
    }
    _Tag(){return "p"}
}

export class TNvBsCardStats extends TNvBsCard{
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-stats");
    }
}

export class TNvBsCardChart extends TNvBsCard{
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-chart");
    }  
}

export class TNvBsCardTable extends TNvBsCard {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("card-table");
    }
}