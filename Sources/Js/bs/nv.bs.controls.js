//base for all bootstrap controls
import { TControl, TWinControl } from "./../nv.controls.js";
import { TSubProperty } from "../nv.classes.js";

export class TBSmargins extends TSubProperty {
    static callAfterConstructor = false;
    constructor(C, T, P, o) {
        super(o);
        this.FControl = C;
        this.FPrefix = P;
        this.FType = T;
        this._AfterConstruction(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FEnd = '';
        this.FBottom = '';
        this.FX = '';
        this.FY = '';
        this.FAll = '';
        this.FTop = '';
        this.FStart = '';
    }

    get End() { return this.FEnd };
    get Bottom() { return this.FBottom };
    get X() { return this.FX };
    get Y() { return this.FY };
    get All() { return this.FAll };
    get Top() { return this.FTop };
    get Start() { return this.FStart };

    _UpdateClass(prop, side, breakpoint, size) {
        this.FControl.FEl.removeClassRegex("(^|\\b)(" + prop + side + breakpoint + "(auto|0|1|2|3|4|5|6)+)(\\b(?!-)|$)")
            .addClass(prop + side + breakpoint + size);
    }

    set End(V) {
        if (V != this.FEnd) {
            this._UpdateClass(this.FType, 'e-', this.FPrefix, V);
            this.FEnd = V;
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

    set Start(V) {
        if (V != this.FStart) {
            this._UpdateClass(this.FType, 's-', this.FPrefix, V);
            this.FStart = V;
        }
    }

}


export class TBSGridOptions extends TSubProperty {
    static callAfterConstructor = false;
    constructor(C, P, o) {
        super(o);
        this.FControl = C;
        this.FPrefix = P;
        this._AfterConstruction(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
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
        this.FPaddings = new TBSmargins(this.FControl, 'p', this.FPrefix, o.Paddings || {});
        this.FMargins = new TBSmargins(this.FControl, 'm', this.FPrefix, o.Margins || {});;
        this.FHeight = '';
        this.FViewportPos = '';
    }


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
    get Margins() { return this.FMargins }
    get Paddings() { return this.FPaddings }
    get Height() { return this.FHeight }
    get ViewportPos() { return this.FViewportPos };

    set Display(V) {
        if (V != this.FDisplay) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(d-" + this.FPrefix + "(none|inline|inline-block|block|table|table-cell|table-row|flex|inline-flex|grid|inline-grid)+)(\\b(?!-)|$)")
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
            this.FControl.FEl.removeClassRegex("(^|\\b)(float-" + this.FPrefix + "(none|end|start)+)(\\b(?!-)|$)")
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
    set Height(V) {
        if (V != this.FHeight) {
            let _prefix = this.FPrefix ? "-" + this.FPrefix.trimEnd("-") : "";
            this.FControl.FEl.css("--hg" + _prefix, "")
                .css("--hg" + this.FPrefix, V);
            this.FHeight = V;
        }
    }
    set ViewportPos(V) {
        if (V != this.FViewportPos) {
            this.FControl.FEl.removeClassRegex("(^|\\b)((fixed-|sticky-)" + this.FPrefix + "(top|bottom)+)(\\b(?!-)|$)");
            if (V != "")
                this.FControl.FEl.addClass(V.replace("-", "-" + this.FPrefix));
            this.FViewportPos = V;
        }
    }

}

export class TBSPrintOptions extends TSubProperty {
    constructor(C, o) {
        super(o);
    }
}


export class TBsGrids extends TSubProperty {
    static callAfterConstructor = false;
    constructor(C, o) {
        super(o);
        this.FControl = C;
        this._AfterConstruction(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FXS = new TBSGridOptions(this.FControl, "", o.XS || {});
        this.FSM = new TBSGridOptions(this.FControl, "sm-", o.SM || {});
        this.FMD = new TBSGridOptions(this.FControl, "md-", o.MD || {});
        this.FLG = new TBSGridOptions(this.FControl, "lg-", o.LG || {});
        this.FXL = new TBSGridOptions(this.FControl, "xl-", o.XL || {});
        this.FPrint = new TBSPrintOptions(this.FControl, o.Print || {});
    }

    get XS() { return this.FXS; }
    get SM() { return this.FSM; }
    get MD() { return this.FMD; }
    get LG() { return this.FLG; }
    get XL() { return this.FXL; }
    get Print() { return this.FPrint; }
}



export class TBsBorders extends TSubProperty {
    static callAfterConstructor = false;
    constructor(C, o) {
        super(o);
        this.FControl = C;
        this._AfterConstruction(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FColor = '';
        this.FVisible = false;
        this.FTop = true;
        this.FEnd = true;
        this.FBottom = true;
        this.FStart = true;
        //
        this.FRoundType = '';//''|Top|end|bottom|start|circle|pill|0
        this.FRoundSize = '';//''|0|1|2|3|4|5
    }


    get Color() { return this.FColor; }
    set Color(V) { if (V != this.FColor) { this.FColor = V; this._UpdateBorders(); } }
    get Visible() { return this.FVisible; }
    set Visible(V) { if (V != this.FVisible) { this.FVisible = V; this._UpdateBorders(); } }
    get Top() { return this.FTop; }
    set Top(V) { if (V != this.FTop) { this.FTop = V; this._UpdateBorders(); } }
    get End() { return this.FEnd; }
    set End(V) { if (V != this.FEnd) { this.FEnd = V; this._UpdateBorders(); } }
    get Bottom() { return this.FBottom; }
    set Bottom(V) { if (V != this.FBottom) { this.FBottom = V; this._UpdateBorders(); } }
    get Start() { return this.FStart; }
    set Start(V) { if (V != this.FStart) { this.FStart = V; this._UpdateBorders(); } }
    //
    get RoundType() { return this.FRoundType; }
    set RoundType(V) { if (V != this.FRoundType) { this.FRoundType = V; this._UpdateRounding(); } }
    get RoundSize() { return this.FRoundSize; }
    set RoundSize(V) { if (V != this.FRoundSize) { this.FRoundSize = V; this._UpdateRounding(); } }

    _UpdateBorders() {
        this.FControl.FEl.removeClassStartingWith("border");
        if (this.FVisible) {
            this.FControl.FEl.addClass("border");
        }
    }

    _UpdateRounding() {
        this.FControl.FEl.removeClassStartingWith("rounded");
        if (this.FRound) {
            this.FControl.FEl.addClass("rounded");
        }
    }
}


export class TBsTextProps extends TSubProperty {
    static callAfterConstructor = false;
    constructor(C, o) {
        super(o);
        this.FControl = C;
        this._AfterConstruction(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FColor = '';
        this.FAlign = '';// ''|justify|start|center|end
        this.FAlignSM = '';
        this.FAlignMD = '';
        this.FAlignLG = '';
        this.FAlignXL = '';
        this.FWrap = ''; //''|wrap|no-wrap|truncate
        this.FTransform = '';//''|lowercase|uppercase|capitalize
        this.Transform = o.Transform || '';
        this.FWeight = 'normal';//bold|bolder|normal|light|ligher
        this.FItalic = false;
        this.FMonospace = false;
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
        this.FControl.FEl.removeClassStartingWith("text-");
    }
}


export class TBsPosition extends TSubProperty {
    static callAfterConstructor = false;
    constructor(C, o) {
        super(o);
        this.FControl = C;
        this._AfterConstruction(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FFBottom = ""; //""|0|50|100
        this.FEnd = "";
        this.FStart = "";
        this.FTop = "";
        this.FPosition = ""; //""|static|relative|absolute|fixed|sticky
    }

    get FBottom() { return this.FFBottom };
    set FBottom(V) {
        if (V != this.FFBottom) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(bottom-(0|50|100)+)(\\b(?!-)|$)");
            if (V != "")
                this.FControl.FEl.addClass("bottom-" + V);
            this.FFBottom = V;
        }
    }

    get End() { return this.FEnd };
    set End(V) {
        if (V != this.FEnd) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(end-(0|50|100)+)(\\b(?!-)|$)");
            if (V != "")
                this.FControl.FEl.addClass("end-" + V);
            this.FEnd = V;
        }
    }

    get Start() { return this.FStart };
    set Start(V) {
        if (V != this.FStart) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(start-(0|50|100)+)(\\b(?!-)|$)");
            if (V != "")
                this.FControl.FEl.addClass("start-" + V);
            this.FStart = V;
        }
    }

    get Top() { return this.FTop };
    set Top(V) {
        if (V != this.FTop) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(top-(0|50|100)+)(\\b(?!-)|$)");
            if (V != "")
                this.FControl.FEl.addClass("top-" + V);
            this.FTop = V;
        }
    }

    get Position() { return this.FPosition };
    set Position(V) {
        if (V != this.FPosition) {
            this.FControl.FEl.removeClassRegex("(^|\\b)(position-(static|relative|absolute|fixed|sticky)+)(\\b(?!-)|$)");
            if (V != "")
                this.FControl.FEl.addClass("position-" + V);
            this.FPosition = V;
        }
    }
}



export class TBsControl extends TControl {
    _DefaultParams(o) {
        this.FRenderPosition ??= false;
        this.FBgPrefix ??= "bg-";
        super._DefaultParams(o);
    }
}

export class TBsCustomControl extends TBsControl {
    _CreateParams(o) {
        super._CreateParams(o);
        this.FBackground = "";
        this.FBorder = new TBsBorders(this, o.Border || {});
        this.FTextProps = new TBsTextProps(this, o.TextProps || {});
        this.FPosition = new TBsPosition(this, o.Position || {})
        this.FShadow = "";
        this.FWidth = ""; //''|'auto'|'25'|'50'|'75'|'100'
    }

    get Background() { return this.FBackground; }
    get Border() { return this.FBorder; }
    get TextProps() { return this.FTextProps; }
    get Shadow() { return this.FShadow; }
    get Width_() { return this.FWidth };

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
        this.FShadow === 'sm' ? el.addClass("shadow-sm") :
            this.FShadow === 'md' ? el.addClass("shadow") :
                this.FShadow === 'lg' ? el.addClass("shadow-lg") : '';
    }

    set Width_(V) {
        if (V != this.FWidth) {
            this.FEl.removeClassRegex("(^|\\b)(w-(auto|25|50|75|100)+)(\\b(?!-)|$)")
            if (V != "")
                this.FEl.addClass("w-" + V);
        }
    }

}

export class TNvBsGridControl extends TBsCustomControl {
    _DefaultParams(o) {
        o.ClassCss ??= "col";
        super._DefaultParams(o);
    }
    _CreateParams(o) {
        super._CreateParams(o);
        this.FGrids = new TBsGrids(this, o.Grids || {});
    }

    get Grids() { return this.FGrids }
}



export class TBsWinControl extends TWinControl {
    _DefaultParams(o) {
        this.FRenderPosition ??= false;
        super._DefaultParams(o);
    }
}

