import { TNvBsGridContainer, TNvBsContainer } from "./nv.bs.containers.js";


//Mudar para https://mobius1.github.io/Pageable/#page-1

//rewrite to this https://jsfiddle.net/qnpag09w/3/
export class TNvBsScrollFocus extends TNvBsGridContainer {
    _DefaultParams(o) {
        o.ClassCss ??= "scrollfocus";
        super._DefaultParams(o);
    }

    _CreateParams(o) {
        super._CreateParams(o);
        this.FEl.on("scroll.nvjs", this._DoScroll());
    }

    _DoScroll() {
        let scrollTop = this.FEl.scrollTop();
        let anchors = this.FEl.children();

        for (var i = 0; i < anchors.length; i++) {
            $(anchors[i]).removeClass('active');
            $('nav ul li a[href="#' + $(anchors[i]).attr('id') + '"]').removeClass('active');
        };

        for (var i = 0; i <= anchors.length - 1; i++) {
            if ($(anchors[i]).offset().top > this.FEl.height() - $(anchors[i]).height() - (this.FEl.height() - scrollTop)) {
                $(anchors[i]).addClass('active');
                $('nav ul li a[href="#' + $(anchors[i]).attr('id') + '"]').addClass('active');
                break;
            };
        }

    }
}
