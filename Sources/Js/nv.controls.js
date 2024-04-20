//base for all visual controls
import { TComponent } from './nv.classes.js';
import { TFont } from './nv.graphics.js';

export class TControl extends TComponent {
    constructor(o) {
        super(o);
        this._AttachEvents(o.Events ?? []);
    }

    //override  and set default values before call super
    _DefaultParams(o) {
        o.Tag ??= "div";
        o.ClassCss ??= "";
        o.Role ??= "";
        //Fxxx are non property values
        this.FRenderPosition ??= true;
        o.Position ??= (this.FRenderPosition ? "absolute" : "");
        this.FDesignClass ??= "design";
        // 
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);

        //Create Html Element      
        this.FEl = null;
        //Tag must to be set before create element
        this.FTag ??= o.Tag;
        this._CreateElement();

        //Initialize params with values to fire (or not) changes on set prop values     
        this.FClass = ""
        this.FRole = "";
        this.FPosition = "";
        this.FText = '';
        this.FTop = '';
        this.FLeft = '';
        this.FWidth = '';
        this.FHeight = '';
        this.FParent = null;
        this.FLayout = '';
        this.FOrientaion = '';
        this.FAlignament = '';
        this.FWordWrap = '';
        this.FVisible = true;
        this.FEnabled = true;
        this.FImage = "";
        this.FDName = "";
        this.FFont = new TFont({});
        this.FRenderIndex = 0;
        $(this.FFont).on("change", (e) => { this._DoFontChange(e) });
    }

    _ChangeParams(o) {
        //Force class first of other properties where can change element class
        this.ClassCss = o.ClassCss;
        super._ChangeParams(o);
    }

    get Tag() { return this.FTag }
    set Tag(T) {
        if (T !== this.FTag) {
            if (this.FEl)//tag can be set before creating the element
                this.FEl = this.FEl.replaceTag(T, true, true);
            this.FTag = T;
        }
    }

    _CreateElement() {
        if (this.FEl == null) {
            this.FEl = $(document.createElement(this.FTag)); //do not use JQuery for Create Element
            this.Attr("id", this.FId)
            if (this.FDName)
                this.Attr("d-name", this.FDName);
            // this.FEl.on("elementResize.NVjs", () => this._DoResize());
        }
    }

    Free() {
        if (this.Parent)
            this.Parent = null;

        if (this.FEl) {
            this.FEl.remove();
        }
        super.Free();
    }

    Css(...a) {
        return this.FEl.css(...a);
    }

    Attr(...a) {
        return this.FEl.attr(...a);
    }

    _DoSetParent(P) {

        if (P != this.FParent) {
            if (P == this) throw this.Name && ' cannot parent itself';

            if (this.FParent != null) this.FParent.RemoveControl(this);

            if (P != null) {
                P.InsertControl(this);
                /* if not(csLoading in ComponentState) and not(csDesigning in ComponentState)
                 and not(csDestroying in ComponentState) and not(csFreeNotification in ComponentState) then
                 begin
                 if not(csDesigning in Parent.ComponentState) then
                 ScaleForPPI(GetParentCurrentDpi);
                 end; 
                 UpdateAnchorRules; */

                if (this.FRenderPosition || App.FDesign)
                    App.FResizeObserver.observe(this.FEl[0]);
                // this.FEl.on("elementResize.NVjs", () => this._DoResize());
            };

            this.FParent = P;
        };

    }

    get RenderIndex() { return this.FRenderIndex };
    set RenderIndex(V) {
        if (V != this.FRenderIndex) {
            this.FRenderIndex = V;
            if (this.ParentDefined())
                this.Parent._DoInsertControl(this);
        }
    }


    set Parent(P) {
        //support Id(string) or Object
        if (P === "")
            P = null
        else if ((typeof P !== 'object') && (P !== null)) {
            var _Pstr = P;
            P = App.GetComponentById(P) ?? null;
            if (!P)
                App.AddParentFixUp(this.Id, _Pstr);
        };

        this._DoSetParent(P);
    }

    get Parent() {
        return this.FParent;
    }

    get El() {
        return this.FEl;
    }

    get ClassCss() { return this.FClass }
    set ClassCss(C) {
        if (C != this.FClass) {
            this.FEl//
                .removeClass(this.FClass)//
                .addClass((App.FDesign ? this.FDesignClass + " " : "") + C);
            this.FClass = C;
        }
    }

    // use to only add class to Element, the Component property ClassCss dont be changed;
    //pass html element or Control.ID to E, default is this Html Element 
    AddClassToEl(C, E = this.El) {
        if (typeof E === "string")
            E = App.FComponentList[E].El;
        E.addClass(C);
    }

    // use to only remove class rom Element, the Component property ClassCss dont be changed;
    //pass html element or Control.ID to E, default is this Html Element  
    RemoveClassFromEl(C, E = this.El) {
        if (typeof E === "string")
            E = App.FComponentList[E].El;
        E.removeClass(C);
    }

    set Text(T) {
        if (T != this.FText) {
            this._DoTextChange(T);
        }
    }

    get Text() {
        return this.FText;
    }

    set Role(R) {
        if (R != this.FRole) {
            this.Attr("role", R);
            this.FRole = R;
        }
    }

    ParentDefined() {
        return ((typeof this.FParent == "object") && (this.FParent !== null));
    }

    ParentIs(C) {
        return (this.ParentDefined() && (this.FParent instanceof C))
    }

    get Role() { return this.FRole }

    _DoImageChange() {
        //override to change functionality
        if (this.FImage != "") {
            this.FElImage = $(this.FImage);
            //this.FElImage.addClass("rounded");
            this.FEl.prepend(this.FElImage);
        } else if (this.FElImage) {
            this.FElImage.remove();
            this.FElImage = undefined;
        }
    }


    _DoTextChange(T) {
        //override to change functionality
        if (this.FText !== T) {
            // var _Resizer = this.FEl.children("iframe");
            // _Resizer.detach();
            this.FEl.setTextPreserveChilds(T.htmlEscape());
            // _Resizer.prependTo(this.FEl);
            this.FText = T;
        }
    }

    _DoFontChange(e) {
        switch (e.prop) {
            case "color":
                this.Css("-webkit-text-fill-color", e.value);
                break;
            case "name":
                this.Css("font-family", e.value);
                break;
            case "size":
                this.Css("font-size", e.value);
                break;
            case "style":
                $.isInArray("bold", e.value) ? this.Css("font-weight", "bold") : this.Css("font-weight", "");
                $.isInArray("italic", e.value) ? this.Css("font-style", "italic") : this.Css("font-style", "");
                $.isInArray("underline", e.value) ? this.Css("text-decoration", "underline") : this.Css("text-decoration", "");
                break;
        }
    }

    _DoResize() {
        if (this.ParentDefined()) {
            //position relative to parent "NVControl"
            var P = this.FEl.offset();
            let PP = this.FParent.FEl.offset();
            P.top -= PP.top;
            P.left -= PP.left;
        }
        else
            var P = this.FEl.Position();

        let _Changed = false;

        if (this.FTop !== P.top) {
            _Changed = true;
            this.FTop = P.top;
            App.QueueChange(this.FId, 'Top', P.top);
        };
        if (this.FLeft !== P.left) {
            _Changed = true;
            this.FLeft = P.left;
            App.QueueChange(this.FId, 'Left', P.left);
        };
        if (this.FHeight !== this.FEl.outerHeight()) {
            _Changed = true;
            this.FHeight = this.FEl.outerHeight();
            App.QueueChange(this.FId, 'Height', this.FEl.outerHeight());
        };
        if (this.FWidth !== this.FEl.outerWidth()) {
            _Changed = true;
            this.FWidth = this.FEl.outerWidth();
            App.QueueChange(this.FId, 'Width', this.FEl.outerWidth());
        };

        if (_Changed && App.FDesign)
            App.Server.SendChanges();
    }

    _DoEnabledChange() {
        this.FEl.prop("disabled", !this.FEnabled);
    }

    _DoPosition(P) {
        if (this.FRenderPosition)
            this.Css("position", P);
    }

    set Width(W) {
        if (W != this.FWidth) {
            if (this.FRenderPosition) {
                this.FEl.outerWidth(W);
                if (!App.FDesign)
                    this.FWidth = W;
            }
        }
    }

    get Width() {
        return this.FWidth;
    }

    set Height(H) {
        if (H != this.FHeight) {
            if (this.FRenderPosition)
                this.FEl.outerHeight(H);
            if (!App.FDesign)
                this.FHeight = H;
        }
    }

    get Height() {
        return this.FHeight;
    }

    set Top(T) {
        if (T != this.FTop) {
            if (this.FRenderPosition)
                this.Css("top", T);
            if (!App.FDesign)
                this.FTop = T;
        }
    }

    get Top() {
        return this.FTop;
    }

    set Left(L) {
        if (L != this.FLeft) {
            if (this.FRenderPosition)
                this.Css("left", L);
            if (!App.FDesign)
                this.FLeft = L;
        }
    }

    get Left() {
        return this.FLeft;
    }

    set Position(P) {
        if (P != this.FPosition) {
            this._DoPosition(P);
            this.FPosition = P;
        }
    }

    get Position() { return this.FPosition }

    get Font() {
        return this.FFont;
    }

    set Layout(L) {
        if (L != this.FLayout) {
            switch (L) {
                case "top":
                    this.Css("vertical-align", "");
                    this.Css("line-height", "");
                    break;
                case "center":
                    this.Css("vertical-align", "middle");
                    if ($.isInArray(this.FOrientaion, ["vert", "vert-invert"]))
                        this.Css("line-height", this.FWidth)
                    else
                        this.Css("line-height", this.FHeight);
                    break;
                case "bottom":
                    this.Css("vertical-align", "bottom");
                    if ($.isInArray(this.FOrientaion, ["vert", "vert-invert"]))
                        this.Css("line-height", this.FWidth)
                    else
                        this.Css("line-height", this.FHeight);
                    break;
            }
            this.FLayout = L;
        }
    }

    set Orientaion(O) {
        if (O != this.FOrientaion) {
            switch (O) {
                case "horiz":
                    this.Css("writing-mode", "");
                    break;
                case "vert":
                    this.Css("writing-mode", "vertical-lr");
                    break;
                case "vert-invert":
                    this.Css("writing-mode", "vertical-lr");
                    this.Css("transform", "rotate(180deg)");
                    this.Css("-webkit-transform", "rotate(180deg)");
                    break;
            }
            this.FOrientaion = O;
        }
    }

    set Alignament(A) {
        if (A != this.FAlignament) {
            if (A == "left")
                this.Css("text-align", "")
            else
                this.Css("text-align", A);
            this.FAlignament = A;
        }
    }

    set WordWrap(W) {
        if (W != this.FWordWrap) {
            this.Css("word-wrap", W);
            this.FWordWrap = W;
        }
    }

    get Visible() { return this.FVisible }
    set Visible(V) {
        if (V != this.FVisible) {
            if (V)
                this.FEl.show()
            else
                this.FEl.hide();
            this.FVisible = V;
        }
    }

    get Enabled() { return this.FEnabled }
    set Enabled(V) {
        if (V !== this.FEnabled) {
            this.FEnabled = V;
            this._DoEnabledChange();
        }
    }

    get Image() { return this.FImage }
    set Image(V) {
        if (V !== this.FImage) {
            this.FImage = V;
            this._DoImageChange();
        }
    }

    get DName() { return this.FDName }
    set DName(V) {
        if (V !== this.FDName) {
            this.FDName = V;
            this.Attr("d-name", this.FDName);
        }
    }

    _ProcessEvent(e) {
        let evt;
        if (["keypress", "keyup", "keydown"].includes(e.type)) {
            evt = {
                type: e.type,
                alt: e.altKey,
                ctrl: e.ctrlKey,
                shift: e.shiftKey,
                key: e.key,
                keyCode: e.keyCode,
                value: $(e.target).val()
            }
        } else
            evt = e;
        App.QueueChange(this.FId, evt.type, evt);
        App.Server.SendChanges();
    }



    _AttachEvents(E) {
        if (typeof E == "object")
            E.forEach(evt => {
                this.FEl.off(evt + ".nvjs").on(evt + ".nvjs", (e) => this._ProcessEvent(e));
            })
        else
            this.FEl.off(E + ".nvjs").on(E + ".nvjs", (e) => this._ProcessEvent(e));
    }
}

export class TWinControl extends TControl {

    constructor(o) {
        super(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FControls = [];
    }

    Free() {
        if (this.FModal) {
            this.FModal.remove();
            this.FModal = undefined;
        }
        super.Free();
    }

    CloseModal() {
        if (this.FModal) {
            if (this.FParent)
                this.FParent.InsertControl(this)
            else
                this.El.detach();
            this.FModal.remove();
            this.FModal = undefined;
        }
    }


    ShowModal() {
        if (this.FParent) this.FParent.RemoveControl(this);
        this.FModal = TApplication.ShowModalProc(this.FEl, this.FCaption || this.FText);
        this.FEl.one("close-modal.nvjs", (e, mr) => {
            mr ? e.ModalResult = mr : null;
            this._ProcessEvent(e)
        });
    }

    _DoInsertControl(C) {
        if (C.FRenderIndex > 0) {
            let _Controls = this.FControls.sort((a, b) => b.FRenderIndex - a.FRenderIndex),
                _Insert = false;
            for (const prevEl of _Controls) {
                if (C.FRenderIndex > prevEl.FRenderIndex) {
                    C.FEl.insertAfter(prevEl.FEl);
                    _Insert = true;
                    break;
                }
            }
            /*
                        _Controls.forEach(prevEl => {
                            if (C.FRenderIndex > prevEl.FRenderIndex) {
                                C.FEl.insertAfter(prevEl.FEl);
                                _Insert = true;
                                break;
                            }
                        });*/
            if (!_Insert)
                this.El.append(C.FEl);
        }
        else {
            C.FRenderIndex = this.FControls.length + 1;
            this.El.append(C.FEl);
        }
    }

    InsertControl(C) {
        this.FControls.push(C);
        this._DoInsertControl(C);
    }

    _DoRemoveControl(C) {
        C.El.detach(); //remove without free and remove data_events 
    }

    RemoveControl(C) {
        this.FControls = this.FControls.filter(item => item !== C);
        this._DoRemoveControl(C);
    }

}







