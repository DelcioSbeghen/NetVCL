//base for all bootstrap controls
import { TControl, TWinControl } from "./../nv.controls.js";
import { TSubProperty } from "../nv.classes.js";

export class TBSmargins extends TSubProperty {
    constructor(C, T, P, O) {
        super();
        this.FControl = C;
        this.FPrefix = P;
        this.FType = T;
        //defaults
        this.FRight = '';
        this.FBottom = '';
        this.FX = '';
        this.FY = '';
        this.FAll = '';
        this.FTop = '';
        this.FLeft = '';
        //set
        this._CreateParams(O);
    }

    _CreateParams(O) {
        this.Right = O.Right || '';
        this.Bottom = O.Bottom || '';
        this.X = O.X || '';
        this.Y = O.Y || '';
        this.All = O.All || '';
        this.Top = O.Top || '';
        this.Left = O.Left || '';
    }

    get Right() { return this.Right };
    get Bottom() { this.Bottom };
    get X() { this.X };
    get Y() { this.Y };
    get All() { this.All };
    get Top() { this.Top };
    get Left() { this.Left };

    _UpdateClass(prop, side, breakpoint, size){
        this.FControl.FEl.removeClassRegex("(^|\\b)(" + prop +  side + breakpoint + "(auto|0|1|2|3|4|5|6)+)(\\b(?!-)|$)")
               .addClass(prop +  side + breakpoint +  size); 
    }

    set Right(V) {
        if (V != this.FRight) {
            this._UpdateClass(this.FType, 'r-', this.FPrefix, V);
            this.FRight = V;
        }
    }

    set Bottom(V) {
        if (V != this.FBottom) {
            this._UpdateClass(this.FType, 'b-', this.FPrefix, V);
            this.FBottom = V;
        }
    }

    set X(V) {
        if (V != this.FX) {
            this._UpdateClass(this.FType, 'x-', this.FPrefix, V);
            this.FX = V;
        }
    }

    set Y(V) {
        if (V != this.FY) {
            this._UpdateClass(this.FType, 'y-', this.FPrefix, V);
            this.FY = V;
        }
    }

    set All(V) {
        if (V != this.FAll) {
            this._UpdateClass(this.FType, '-', this.FPrefix, V);
            this.FAll = V;
        }
    }

    set Top(V) {
        if (V != this.FTop) {
            this._UpdateClass(this.FType, 't-', this.FPrefix, V);
            this.FTop = V;
        }
    }

    set Left(V) {
        if (V != this.FLeft) {
            this._UpdateClass(this.FType, 'l-', this.FPrefix, V);
            this.FLeft = V;
        }
    }

}


export class TBSGridOptions extends TSubProperty {
    constructor(C, P, O) {
        super();
        this.FControl = C;
        this.FPrefix = P;
        this.FAutoMargin = '';
        this.FDisplay = '';
        this.FDirection = '';
        this.FGrow = '';
        this.FAlignItems = '';
        this.FJustifyContent = '';
        this.FOrder = '';
        this.FShrink = '';
        this.FWrap = '';
        this.FAlignSelf = '';
        this.FFill = '';
        this.FAlignContent = '';
        this.FFloat = '';
        this.FOffset = '';
        this.FSpan = '';
        this.FPaddings = new TBSmargins(C, 'p', this.FPrefix, O.Paddings || {});
        this.FMargins = new TBSmargins(C, 'm', this.FPrefix, O.Margins || {});;
        this._CreateParams(O);
    }

    _CreateParams(O) {
        this.AutoMargin = O.AutoMargin || '';
        this.Display = O.Display || '';
        this.Direction = O.Direction || '';
        this.Grow = O.Grow || '';
        this.AlignItems = O.AlignItems || '';
        this.JustifyContent = O.JustifyContent || '';
        this.Order = O.Order || '';
        this.Shrink = O.Shrink || '';
        this.Wrap = O.Wrap || '';
        this.AlignSelf = O.AlignSelf || '';
        this.Fill = O.Fill || '';
        this.AlignContent = O.AlignContent || '';
        this.Float = O.Float || '';
        this.Offset = O.Offset || '';
        this.Span = O.Span || '';
    }


    get AutoMargin() { return this.FAutoMargin; }
    get Display() { return this.FDisplay; }
    get Direction() { return this.FDirection; }
    get Grow() { return this.FGrow; }
    get AlignItems() { return this.FAlignItems; }
    get JustifyContent() { return this.FJustifyContent; }
    get Order() { return this.FOrder; }
    get Shrink() { return this.FShrink; }
    get Wrap() { return this.FWrap; }
    get AlignSelf() { return this.FAlignSelf; }
    get Fill() { return this.FFill; }
    get AlignContent() { return this.FAlignContent; }
    get Float() { return this.FFloat; }
    get Offset() { return this.FOffset; }
    get Span() { return this.FSpan; }


    set AutoMargin(V) {
        if (V != this.FAutoMargin) {
            this.FControl.FEl.removeClass("ml-auto mr-auto").addClass(V);
            this.FAutoMargin = V;
        }
    }
    set Display(V) {
        if (V != this.FDisplay) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(d-" + this.FPrefix + "(none|inline|inline-block|block|table|table-cell|table-row|flex|inline-flex)+)(\\b(?!-)|$)")
                .addClass("d-" + this.FPrefix + V);
            this.FDisplay = V
        }
    }
    set Direction(V) {
        if (V != this.FDirection) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(flex-" + this.FPrefix + "(row|row-reverse|column|column-reverse|)+)(\\b(?!-)|$)")
            .addClass("flex-" + this.FPrefix + V);
            this.FDirection = V;
        }
    }
    set Grow(V) {
        if (V != this.FGrow) {

            this.FGrow = V;
        }
    }
    set AlignItems(V) {
        if (V != this.FAlignItems) {

            this.FAlignItems = V;
        }
    }
    set JustifyContent(V) {
        if (V != this.FJustifyContent) {

            this.FJustifyContent = V;
        }
    }
    set Order(V) {
        if (V != this.FOrder) {

            this.FOrder = V;
        }
    }
    set Shrink(V) {
        if (V != this.FShrink) {

            this.FShrink = V;
        }
    }
    set Wrap(V) {
        if (V != this.FWrap) {

            this.FWrap = V;
        }
    }
    set AlignSelf(V) {
        if (V != this.FAlignSelf) {

            this.FAlignSelf = V;
        }
    }
    set Fill(V) {
        if (V != this.FFill) {

            this.FFill = V;
        }
    }
    set AlignContent(V) {
        if (V != this.FAlignContent) {

            this.FAlignContent = V;
        }
    }
    set Float(V) {
        if (V != this.FFloat) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(float-" + this.FPrefix + "(none|right|left)+)(\\b(?!-)|$)")
                .addClass("float-" + this.FPrefix + V);
            this.FFloat = V;
        }
    }
    set Offset(V) {
        if (V != this.FOffset) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(offset-" + this.FPrefix + "(0|1|2|3|4|5|6|7|8|9|10|11|12)+)(\\b(?!-)|$)")
                .addClass("offset-" + this.FPrefix + V);
            this.FOffset = V;
        }
    }
    set Span(V) {
        if (V != this.FSpan) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(col-" + this.FPrefix + "(auto|0|1|2|3|4|5|6|7|8|9|10|11|12)+)(\\b(?!-)|$)")
                .addClass("col-" + this.FPrefix + V);
            this.FSpan = V;
        }
    }
}

export class TBSPrintOptions extends TSubProperty {
    constructor(C, O) {
        super();
    }
}


export class TBsGrids extends TSubProperty {
    constructor(C, O) {
        super();
        this.FXS = new TBSGridOptions(C, "", O.XS || {});
        this.FSM = new TBSGridOptions(C, "sm-", O.SM || {});
        this.FMD = new TBSGridOptions(C, "md-", O.MD || {});
        this.FLG = new TBSGridOptions(C, "lg-", O.LG || {});
        this.FXL = new TBSGridOptions(C, "xl-", O.XL || {});
        this.FPrint = new TBSPrintOptions(C, O.Print || {});
    }

    get XS() { return this.FXS; }
    get SM() { return this.FSM; }
    get MD() { return this.FMD; }
    get LG() { return this.FLG; }
    get XL() { return this.FXL; }
    get Print() { return this.FPrint; }
}



export class TBsBorders extends TSubProperty {
    constructor(C, O) {
        super();
        this.C = C;
        this.FColor = '';
        this.Color = O.Color || '';
        this.FVisible = false;
        this.Visible = O.Visible || false;
        this.FTop = true;
        this.Top = O.Top || true;
        this.FRight = true;
        this.Right = O.Right || true;
        this.FBottom = true;
        this.Bottom = O.Bottom || true;
        this.FLeft = true;
        this.Left = O.Left || true;
        //
        this.FRoundType = '';
        this.RoundType = O.RoundType || ''; //''|Top|right|bottom|Left|circle|pill|0
        this.FRoundSize = '';
        this.RoundSize = O.RoundSize || ''; //''|sm|lg
    }

    get Color() { return this.FColor; }
    set Color(V) { if (V != this.FColor) { this.FColor = V; this._UpdateBorders(); } }
    get Visible() { return this.FVisible; }
    set Visible(V) { if (V != this.FVisible) { this.FVisible = V; this._UpdateBorders(); } }
    get Top() { return this.FTop; }
    set Top(V) { if (V != this.FTop) { this.FTop = V; this._UpdateBorders(); } }
    get Right() { return this.FRight; }
    set Right(V) { if (V != this.FRight) { this.FRight = V; this._UpdateBorders(); } }
    get Bottom() { return this.FBottom; }
    set Bottom(V) { if (V != this.FBottom) { this.FBottom = V; this._UpdateBorders(); } }
    get Left() { return this.FLeft; }
    set Left(V) { if (V != this.FLeft) { this.FLeft = V; this._UpdateBorders(); } }
    //
    get RoundType() { return this.FRoundType; }
    set RoundType(V) { if (V != this.FRoundType) { this.FRoundType = V; this._UpdateRounding(); } }
    get RoundSize() { return this.FRoundSize; }
    set RoundSize(V) { if (V != this.FRoundSize) { this.FRoundSize = V; this._UpdateRounding(); } }

    _UpdateBorders() {
        this.C.FEl.removeClassStartingWith("border");
        if (this.FVisible) {
            this.C.FEl.addClass("border");
        }
    }

    _UpdateRounding() {
        this.C.FEl.removeClassStartingWith("rounded");
        if (this.FRound) {
            this.C.FEl.addClass("rounded");
        }
    }
}


export class TBsTextProps extends TSubProperty {
    constructor(C, o) {
        super();
        this.C = C;
        this.FColor = '';
        this.Color = o.Color || '';
        this.FAlign = '';
        this.Align = o.Align || ''; // ''|justify|left|center|right
        this.FAlignSM = '';
        this.AlignSM = o.AlignSM || '';
        this.FAlignMD = '';
        this.AlignMD = o.AlignMD || '';
        this.FAlignLG = '';
        this.AlignLG = o.AlignLG || '';
        this.FAlignXL = '';
        this.AlignXL = o.AlignXL || '';
        this.FWrap = '';
        this.Wrap = o.Wrap || ''; //''|wrap|no-wrap|truncate
        this.FTransform = '';
        this.Transform = o.Transform || ''; //''|lowercase|uppercase|capitalize
        this.FWeight = 'normal';
        this.Weight = o.Weight || 'normal'; //bold|bolder|normal|light|ligher
        this.FItalic = false;
        this.Italic = o.Italic || false;
        this.FMonospace = false;
        this.Monospace = o.Monospace || false;
    }

    get Color() { return this.FColor };
    set Color(V) { if (V != this.FColor) { this.FColor = V; this._UpdateTextProps; } }

    get Align() { return this.FAlign };
    set Align(V) { if (V != this.FAlign) { this.FAlign = V; this._UpdateTextProps; } }
    get AlignSM() { return this.FAlignSM };
    set AlignSM(V) { if (V != this.FAlignSM) { this.FAlignSM = V; this._UpdateTextProps; } }
    get AlignMD() { return this.FAlignMD };
    set AlignMD(V) { if (V != this.FAlignMD) { this.FAlignMD = V; this._UpdateTextProps; } }
    get AlignLG() { return this.FAlignLG };
    set AlignLG(V) { if (V != this.FAlignLG) { this.FAlignLG = V; this._UpdateTextProps; } }
    get AlignXL() { return this.FAlignXL };
    set AlignXL(V) { if (V != this.FAlignXL) { this.FAlignXL = V; this._UpdateTextProps; } }

    get Wrap() { return this.FWrap };
    set Wrap(V) { if (V != this.FWrap) { this.FWrap = V; this._UpdateTextProps; } }

    get Transform() { return this.FTransform };
    set Transform(V) { if (V != this.FTransform) { this.FTransform = V; this._UpdateTextProps; } }

    get Weight() { return this.FWeight };
    set Weight(V) { if (V != this.FWeight) { this.FWeight = V; this._UpdateTextProps; } }

    get Italic() { return this.FItalic };
    set Italic(V) { if (V != this.FItalic) { this.FItalic = V; this._UpdateTextProps; } }

    get Monospace() { return this.FMonospace };
    set Monospace(V) { if (V != this.FMonospace) { this.FMonospace = V; this._UpdateTextProps; } }

    _UpdateTextProps() {
        this.C.FEl.removeClassStartingWith("text-");
    }
}



export class TBsControl extends TControl {
    _CreateParams(o) {
        this.FRenderPosition = false;
        super._CreateParams(o);
    }

}

export class TBsCustomControl extends TBsControl {
    _CreateParams(o) {
        super._CreateParams(o);
        if (!this.FBgPrefix) this.FBgPrefix = 'bg-';
        if (!this.FBackground) this.FBackground = '';
        //this.Background = o.Background || this.FBackground;
        this.FBorder = new TBsBorders(this, o.Border || {});
        this.FTextProps = new TBsTextProps(this, o.TextProps || {});
        if (!this.FShadow) this.FShadow = '';
        //this.Shadow = o.Shadow || this.FShadow; // ''|small|normal|larger
    }

    get Background() { return this.FBackground; }
    get Border() { return this.FBorder; }
    get TextProps() { return this.FTextProps; }
    get Shadow() { return this.FShadow; }

    set Background(V) {
        if (V != this.FBackground) {
            this.FBackground = V;
            this._UpdateBackground(this.FEl);
        }
    }

    set Shadow(V) {
        if (V != this.FShadow) {
            this.FShadow = V;
            this._UpdateShadow(this.FEl);
        }
    }

    _UpdateBackground(el) {
        el.removeClassStartingWith(this.FBgPrefix);
        if (this.FBackground !== '')
            el.addClass(this.FBgPrefix + this.FBackground);
    }

    _UpdateShadow(el) {
        el.removeClassStartingWith("shadow");
        this.FShadow === 'sm' ? el.AddClass("shadow-sm") :
            this.FShadow === 'md' ? el.addClass("shadow") :
                this.FShadow === 'lg' ? el.addClass("shadow-lg") : '';
    }

}

export class TNvBsGridControl extends TBsCustomControl {
    _CreateParams(o) {
        super._CreateParams(o);
        this.AddClass("col");
        this.FGrids = new TBsGrids(this, o.Grids || {});
    }

    get Grids() { return this.FGrids }
}





export class TBsWinControl extends TWinControl {
    _CreateParams(o) {
        this.FRenderPosition = false;
        super._CreateParams(o);
    }
}

export class TNvBsBadge extends TBsControl {
    _CreateParams(O) {
        super._CreateParams(O);
        this.AddClass("badge");
        this.FVariant = 'info';
    //    this.Variant = O.Variant || 'info';
        this.FPill = false;
    //    this.Pill = O.Pill || false;

    }

    _Tag() {
        return "span";
    }

    get Variant() { return this.FVariant; }
    set Variant(V) { //primary|secondary|success|danger|warning|info|light|dark
        if (V != this.FVariant) {
            this.FEl.removeClassRegex("(^|\\b)(badge-(primary|secondary|success|danger|warning|info|light|dark)+)(\\b(?!-)|$)")
                .addClass("badge-" + V);
            this.FVariant = V;

        }
    }

    get Pill() { return this.FPill }
    set Pill(V) {
        if (V != this.FPill) {
            this.removeClass("badge-pill");
            if (V) this.AddClass("badge-pill");
        }

    }
}

export class TNvBsBadgeLink extends TNvBsBadge {
    _CreateParams(O) {
        super._CreateParams(O);
        this.AddClass("badge");
        this.FHref = '#';
    //    this.Href = O.Href || '#';
    }
    _Tag() {
        return "a";
    }
    get Href() { return this.FHref }
    set Href(V) {
        if (V != this.FHref) {
            this.FEl.attr("href", V);
            this.FHref = V;
        }
    }

}