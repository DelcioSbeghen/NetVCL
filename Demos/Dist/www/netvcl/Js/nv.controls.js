//base for all visual controls
import { TComponent } from './nv.classes.js';
import { TFont } from './nv.graphics.js';

export class TControl extends TComponent {
    constructor(o) {
        super(o);
        this._AttachEvents(o.Events || []);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        if (this.FRenderPosition === undefined)
            this.FRenderPosition = true;
        this.FFont = new TFont({});
        $(this.FFont).on("change", (e) => { this._DoFontChange(e) });
        this.FTag = o.Tag || this._Tag();
        this.FEl = null;
        this._CreateElement();
        this.FClass = '';
        this.FText = '';
        this.FRole = '';
        this.FPosition = '';
        this.Position = o.Position || 'absolute';
        this.FTop = '';
        // this.Top = o.Top || '';
        this.FLeft = '';
        // this.Left = o.Left || '';
        this.FWidth = '';
        //  this.Width = o.Width || '';
        this.FHeight = '';
        //this.Height = o.Height || '';
        this.FParent = null;
        if (App.FDesign)
            this.Class = "design" + (o.Class ? " " && o.Class : "")
        else
            this.Class = o.Class || '';
        this.AddClass(o.AddClass || '');
        //this.Text = o.Text || '';
        //this.Role = o.Role || '';
        this.FLayout = '';
        //this.Layout = o.Layout || '';
        this.FOrientaion = '';
        //this.Orientaion = o.Orientaion || '';
        this.FAlignament = '';
        //this.Alignament = o.Alignament || '';
        this.FWordWrap = '';
        //this.WordWrap = o.WordWrap || '';
        //this.Parent = o.Parent || null;
        this.FVisible = true;
        this.FEnabled = true;
        //this.Visible = o.Visible || true;
        this.FImage = "";
    }

    //default element tag
    _Tag() { return "div" }

    get Tag() { return this.FTag }
    set Tag(T) {
        if (T !== this.FTag) {
            this.FEl = this.FEl.replaceTag(T, true, true);
            this.FTag = T;
        }
    }



    _CreateElement() {
        if (this.FEl == null) {
            this.FEl = $(document.createElement(this.FTag)); //do not use JQuery for Create Element
            this.Attr("id", this.FId)
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

    set Parent(P) {
        //support Id(string) or Object
        if (typeof P !== 'object' && P !== null) {
            var _Pstr = P;
            P = App.GetComponentById(P) || null;
            if (!P)
                App.AddParentFixUp(this.Id, _Pstr);
        };

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
                this.FParent = P;
                App.FResizeObserver.observe(this.FEl[0]);
                // this.FEl.on("elementResize.NVjs", () => this._DoResize());
            };
        };
    }

    get Parent() {
        return this.FParent;
    }

    get El() {
        return this.FEl;
    }

    set Class(C) {
        if (C != this.FClass) {
            this.FEl.removeClass().addClass(C);
            this.FClass = C;
        }
    }

    AddClass(C) {
        this.FEl.addClass(C);
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

    _DoImageChange() {
        //override to change functionality
        if (this.FImage != "") {
            this.FElImage = $(this.FImage);
            this.FElImage.addClass("rounded mr-2");
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
            this.FEl.html(T);
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
        if (typeof this.FParent == 'object' && P !== null) {
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

    _DoEnabledChange(){
        this.FEl.prop("disabled", !this.FEnabled);
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
            if (this.FRenderPosition)
                this.Css("position", P);
            this.FLeft = P;
        }
    }

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

    get Enabled(){return this.FEnabled}
    set Enabled(V){
        if (V!==this.FEnabled){
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

    _ProcessEvent(e) {
        App.QueueChange(this.FId, e.type, e);
        App.Server.SendChanges();
    }

    _AttachEvents(E) {
        E.forEach(evt => {
            this.FEl.off(evt + ".nvjs").on(evt + ".nvjs", (e) => this._ProcessEvent(e));
        });
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

    InsertControl(C) {
        this.FControls.push(C);
        this.El.append(C.FEl);
    }

    RemoveControl(C) {
        this.FControls = this.FControls.filter(item => item !== C);
        C.El.detach(); //remove without free and remove data_events 
    }

}






