import { TNvBsGridContainer, TNvBsContainer } from "./nv.bs.containers.js";
import { TBsControl } from "./nv.bs.controls.js";
import { TNvBSText, TNvBsLink } from "./nv.bs.htmlcontrols.js";




export class TNvBsCard extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "card";
        this.FDesignClass ??= "design design-div";
        super._DefaultParams(o);
    }
}

export class TNvBsCardHeader extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "card-header";
        super._DefaultParams(o);
    }
}

export class TNvBsCardBody extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "card-body";
        super._DefaultParams(o);
    }
}

export class TNvBsCardFooter extends TNvBsContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "card-footer";
        super._DefaultParams(o);
    }
}

export class TNvBsCardImage extends TBsControl {
    _DefaultParams(o) {
        o.ClassCss ??= "card-img-top";
        super._DefaultParams(o);
    }
}

export class TNvBsCardTitle extends TNvBSText {
    _DefaultParams(o) {
        o.ClassCss ??= "card-title";
        o.Tag ??= "h5";
        super._DefaultParams(o);
    }
}

export class TNvBsCardSubtitle extends TNvBSText {
    _DefaultParams(o) {
        o.ClassCss ??= "card-subtitle";
        o.Tag ??= "h6";
        super._DefaultParams(o);
    }
}


export class TNvBsCardLink extends TNvBsLink {
    _DefaultParams(o) {
        o.ClassCss ??= "card-link";
        super._DefaultParams(o);
    }
}

export class TNvBsCardText extends TNvBSText {
    _DefaultParams(o) {
        o.ClassCss ??= "card-text";
        o.Tag ??= "p";
        super._DefaultParams(o);
    }
}

export class TNvBsCardStats extends TNvBsCard {
    _DefaultParams(o) {
        o.ClassCss ??= "card card-stats";
        super._DefaultParams(o);
    }
}

export class TNvBsCardChart extends TNvBsCard {
    _DefaultParams(o) {
        o.ClassCss ??= "card card-chart";
        super._DefaultParams(o);
    }
}

export class TNvBsCardTable extends TNvBsCard {
    _DefaultParams(o) {
        o.ClassCss ??= "card card-table";
        super._DefaultParams(o);
    }
}