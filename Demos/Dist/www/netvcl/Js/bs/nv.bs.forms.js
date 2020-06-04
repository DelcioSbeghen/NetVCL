//Bootstrap Form
import { TPage } from "./../nv.forms.js";

export class TBsPage extends TPage {

    _CreateParams(o) {
        super._CreateParams(o);
        //  this.AddClass("container");
    }
}

//TApplication.RegisterClass(TBsPage);
TApplication.PageClass = TBsPage;