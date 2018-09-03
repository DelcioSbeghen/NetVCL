"use strict";
var __extends = (this && this.__extends) || (function() {
    var extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] }
            instanceof Array && function(d, b) { d.__proto__ = b; }) ||
        function(d, b) {
            for (var p in b)
                if (b.hasOwnProperty(p)) d[p] = b[p];
        };
    return function(d, b) {
        extendStatics(d, b);

        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
exports.__esModule = true;
//var V = require("./VCL");
//var VXO = require("./VXObject");
//var VXM = require("./VXMenu");
var TComponent = /** @class */ (function(_super) {
    __extends(TComponent, _super);
    /**
    @aOwner     Indicates the component that is responsible for streaming and freeing this component.Onwer must be TContainer
    @renderTo   (Optional) the id of the html element that will be the parent node for this component
    **/
    function TComponent(aOwner, renderTo) {
        var _this = _super.call(this) || this;
        _this.initialized = false;
        _this._sticktotop = false;
        _this._outline = false;
        _this._fittowidth = false;
        _this._fittoheight = false;
        _this._tooltipplacement = exports.TooltipPlacement.Top;
        _this._visible = true;
        _this._enabled = true;
        _this.__drawdelayed = false;
        _this.__drawdelayedType = false;
        _this.__tmpShowDuration = 0;
        _this.__tmpHideDuration = 0;
        _this.owner = aOwner;
        if (aOwner != null && !aOwner.isContainer) {
            exports.Application.raiseException("only container components can own components");
            throw "only container components can own components";
        }
        if (renderTo == null) {
            _this.jComponent = $("<div>"); //create and empty jComponent
            if (aOwner != null) {
                //in case of tpanel
                if (aOwner.jContent)
                    aOwner.jContent.append(_this.jComponent);
                else
                    aOwner.jComponent.append(_this.jComponent); //other type of container's
            }
            _this.jComponent[0].id = _this.ID;
        } else {
            var comp;
            if (aOwner)
                comp = $(aOwner.jComponent).find("[id=" + renderTo + "]");
            else
                comp = $("body").find("[id=" + renderTo + "]");
            //check for multiple occurrence
            if (comp.length > 1) {
                exports.Application.raiseException("element '" + renderTo + "' appears more than once on page " + aOwner.getClassName());
                throw "element '" + renderTo + "' appears more than once on page " + aOwner.getClassName();
            }
            if (comp.length == 0)
                comp = $(aOwner.jComponent).find("[id=" + aOwner.ID + renderTo + "]");
            if (comp.length != 1) {
                exports.Application.raiseException("Cant find element '" + renderTo + "' on page " + aOwner.getClassName());
                throw "Cant find element '" + renderTo + "' on page " + aOwner.getClassName();
            }
            if (comp.children().length > 0 && !_this.isContainer) {
                exports.Application.raiseException("Error on element '" + renderTo + "'.Only container element can have child elmenet.On page " + aOwner.getClassName());
                throw "Error on element:'" + renderTo + "' only container element can have child elmenet.On page " + aOwner.getClassName();
            }
            _this.jComponent = comp;
            _this.jComponent[0].id = _this.ID;
            _this.jComponent.attr("DATA-ID", renderTo);
        }
        if (aOwner != null)
            aOwner.addComponent(_this);
        _this.addClass(_this.getClassName().toUpperCase());
        return _this;
    }
    TComponent.prototype.onCreate = function() {};
    TComponent.prototype.onShow = function() {};
    /**
    Return the component HTML representation
    **/
    TComponent.prototype.renderHTML = function() {
        this.draw(true);
        return this.jComponent[0].outerHTML;
    };
    TComponent.prototype.checkDataset = function(name) {
        if (name && typeof name == "string" && this.owner) {
            var arrName = name.split('.');
            for (var i = 0; i < arrName.length; i++)
                arrName[i] = arrName[i].trim();
            var parent = this.owner;
            for (var i = 0; i < arrName.length; i++) {
                var rc;
                for (var item in parent) {
                    if (item.toUpperCase() == arrName[i].toUpperCase()) {
                        rc = parent[item];
                        break;
                    }
                }
                if (!rc) {
                    exports.Application.raiseException(name + " dataset was not found on page " + this.owner.getClassName());
                    throw name + " dataset was not found on page " + this.owner.getClassName();
                }
                parent = rc;
            }
            if (!rc) {
                exports.Application.raiseException(name + " dataset was not found on page " + this.owner.getClassName());
                throw name + " dataset was not found on page " + this.owner.getClassName();
            }
            return rc;
        } else
            return name;
    };
    TComponent.prototype.guessDataset = function() {
        var VXD = require("./VXDataset");
        var o = this.owner;
        if (!o)
            return null;
        var rc = new Array();
        if (o.components)
            o.components.forEach(function(item) {
                if (item instanceof VXD.TDataset) {
                    rc.push(item);
                }
            });
        return rc.length == 1 ? rc[0] : null;
    };
    TComponent.prototype.destroy = function() {
        if (this.owner != null) {
            var a = this.owner.components.remove(this);
        }
        this.jComponent.remove();
    };
    TComponent.prototype.LocalizeText = function(sourceString) {
        if (!exports.Application.ActiveLanguage || (!this._localizable))
            return sourceString;
        return exports.Application.getLanguageTranslation(exports.Application.ActiveLanguage, sourceString);
    };
    /**
    Adds the specified class(es) to the component.
    **/
    TComponent.prototype.addClass = function(classStr) {
        this.jComponent.addClass(classStr);
    };
    /**
    Remove a single class, multiple classes, or all classes from component
    **/
    TComponent.prototype.removeClass = function(classStr) {
        this.jComponent.removeClass(classStr);
    };
    TComponent.prototype.popover = function(popupContainer, popoverplacement, title, autoClose, width) {
        var _this = this;
        if (popoverplacement === void 0) { popoverplacement = exports.PopoverPlacement.Bottom; }
        if (autoClose === void 0) { autoClose = 0; }
        if (width === void 0) { width = null; }
        var self = this;
        if (!popupContainer)
            return;
        if (!this.__clickovercontainer || this.__clickovercontainer.ID != popupContainer.ID) {
            this.__clickovercontainer = popupContainer;
            popupContainer.Visible = false;
        }
        this.__clickover = this.jComponent.data('clickover');
        if (!this.__clickover) {
            this.jComponent.clickover({
                html: true,
                content: popupContainer.jComponent,
                title: title,
                placement: popoverplacement != null ? exports.PopoverPlacement[popoverplacement].toLocaleLowerCase() : "right",
                auto_close: autoClose,
                width: width
            });
            this.__clickover = this.jComponent.data('clickover');
            this.__clickover['show']();
        }
        if (popupContainer.Visible) {
            self.closepopup(popupContainer);
        } else {
            popupContainer.Visible = true;
            popupContainer.draw(true);
            this.__clickover.$tip.show();
            this.__clickover.resetPosition();
            $(window).trigger('resize');
            // trigger timeout hide
            if (autoClose > 0) {
                this.__clickovertimer = setTimeout(function() {
                    self.closepopup(popupContainer);
                }, autoClose * 1000);
            }
            popupContainer.jComponent.on(this.__clickover.attr.click_event_ns, function(e) {
                //e.preventDefault();
                e.stopPropagation();
            });
            //wait for the next click
            window.setTimeout(function() {
                $('body').on(_this.__clickover.attr.click_event_ns, function(e) {
                    self.closepopup(popupContainer);
                });
            }, 100);
        }
    };
    TComponent.prototype.closepopup = function(popupContainer) {
        if (this.__clickover) {
            this.__clickover.$tip.hide();
            $('body').off(this.__clickover.attr.click_event_ns);
            popupContainer.jComponent.off(this.__clickover.attr.click_event_ns);
        }
        popupContainer.Visible = false;
        if (this.__clickovertimer)
            clearTimeout(this.__clickovertimer);
    };
    Object.defineProperty(TComponent.prototype, "StickToTop", {
        /**
         component that remain in view as the user scrolls the page
        **/
        get: function() {
            return this._sticktotop;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._sticktotop) {
                this._sticktotop = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "Outline", {
        /**
         An outline is a line that is drawn around elements (outside the borders) to make the element "stand out".
        **/
        get: function() {
            return this._outline;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._outline) {
                this._outline = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "FitToWidth", {
        /*The component will take fulle parent width*/
        get: function() {
            return this._fittowidth;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._fittowidth) {
                this._fittowidth = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "FitToHeight", {
        get: function() {
            return this._fittoheight;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._fittoheight) {
                this._fittoheight = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "Tooltip", {
        /**Tooltips can be attached to any element. When you hover the element with your mouse, the title attribute is displayed in a little box next to the element*/
        get: function() {
            return this._tooltip;
        },
        set: function(val) {
            if (val != this._tooltip) {
                this._tooltip = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "TooltipPlacement", {
        /**Customize the positioning, e.g., to center the tooltip top elements.**/
        get: function() {
            return this._tooltipplacement;
        },
        set: function(val) {
            if (val != this._tooltipplacement) {
                this._tooltipplacement = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "Visible", {
        get: function() {
            return this._visible;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._visible) {
                this._visible = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "Enabled", {
        get: function() {
            return this._enabled;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._enabled) {
                this._enabled = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    /**
     refresh the control on the screen.
    **/
    TComponent.prototype.refresh = function() {
        this.draw(false);
    };
    /**
     Use repaint when the entire control needs to be fully repainted.
    **/
    TComponent.prototype.repaint = function() {
        this.draw(true);
    };
    /**
     * Display the component by fading them to opaque
     */
    TComponent.prototype.fadeIn = function(duration, complete) {
        this.jComponent.fadeIn(duration, function() {
            if (complete != null)
                complete();
        });
    };
    /**
     * Hide the matched elements by fading them to transparent.
     */
    TComponent.prototype.fadeOut = function(duration, complete) {
        this.jComponent.fadeOut(duration, function() {
            if (complete != null)
                complete();
        });
    };
    Object.defineProperty(TComponent.prototype, "MarginLeft", {
        /**
         * The margin clears an area around an component .
         * The margin does not have a background color, and is completely transparent.
         * Sets the left margin of an component
         */
        get: function() { return parseFloat(this.jComponent.css('margin-left')); },
        set: function(pixel) { this.jComponent.css('margin-left', pixel); },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "MarginRight", {
        /**
         * The margin clears an area around an component .
         * The margin does not have a background color, and is completely transparent.
         *  Sets the right margin of an component
         */
        get: function() { return parseFloat(this.jComponent.css('margin-right')); },
        set: function(pixel) { this.jComponent.css('margin-right', pixel); },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "MarginTop", {
        /**
         * The margin clears an area around an component .
         * The margin does not have a background color, and is completely transparent.
         * Sets the top margin of an component
         */
        get: function() { return parseFloat(this.jComponent.css('margin-top')); },
        set: function(pixel) { this.jComponent.css('margin-top', pixel); },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "MarginBottom", {
        /**
         * The margin clears an area around an component .
         * The margin does not have a background color, and is completely transparent.
         * Sets the bottom margin of an component
         */
        get: function() { return parseFloat(this.jComponent.css('margin-bottom')); },
        set: function(pixel) { this.jComponent.css('margin-bottom', pixel); },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "PaddingLeft", {
        /**
         * The padding clears an area around the content .
         * The padding is affected by the background color of the component.
         * Sets the left padding of an component
         */
        get: function() { return parseFloat(this.jComponent.css('padding-left')); },
        set: function(pixel) { this.jComponent.css('padding-left', pixel + "px"); },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "PaddingRight", {
        /**
         * The padding clears an area around the content .
         * The padding is affected by the background color of the component.
         * Sets the right padding of an component
         */
        get: function() { return parseFloat(this.jComponent.css('padding-right')); },
        set: function(pixel) { this.jComponent.css('padding-right', pixel + "px"); },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "PaddingTop", {
        /**
         * The padding clears an area around the content .
         * The padding is affected by the background color of the component.
         * Sets the top padding of an component
         */
        get: function() { return parseFloat(this.jComponent.css('padding-top')); },
        set: function(pixel) { this.jComponent.css('padding-top', pixel + "px"); },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "PaddingBottom", {
        /**
         * The padding clears an area around the content .
         * The padding is affected by the background color of the component.
         * Sets the bottom padding of an component
         */
        get: function() { return parseFloat(this.jComponent.css('padding-bottom')); },
        set: function(pixel) { this.jComponent.css('padding-bottom', pixel + "px"); },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "Width", {
        /**
         * Specifies the width of the component in pixels.
         */
        get: function() { return this.jComponent.width(); },
        set: function(pixel) {
            if (pixel != this.Width)
                this.jComponent.width(pixel);
        },
        enumerable: true,
        configurable: true
    });
    TComponent.prototype.animateResize = function(duration, widthPixel, heightPixel, completeCallBack) {
        if (duration === void 0) { duration = 400; }
        if (!widthPixel && !heightPixel)
            return;
        if (widthPixel && heightPixel)
            this.jComponent.animate({ width: widthPixel, height: heightPixel }, duration, completeCallBack);
        else if (widthPixel)
            this.jComponent.animate({ width: widthPixel }, duration, completeCallBack);
        else if (heightPixel)
            this.jComponent.animate({ height: heightPixel }, duration, completeCallBack);
    };
    Object.defineProperty(TComponent.prototype, "Height", {
        /**
         * Specifies the height of the component in pixels.
         */
        get: function() { return parseFloat(this.jComponent.css('height')); },
        set: function(pixel) {
            if (pixel != this.Height)
                this.jComponent.css('height', pixel);
        },
        enumerable: true,
        configurable: true
    });
    TComponent.prototype.create = function() {
        if (this.Visible)
            this.jComponent.show();
        else
            this.jComponent.hide();
        if (this.Tooltip != null) {
            this.jComponent.attr('data-toggle', 'tooltip');
            this.jComponent.attr('title', this.Tooltip);
            this.jComponent.attr('data-placement', exports.TooltipPlacement[this.TooltipPlacement]);
        } else {
            this.jComponent.removeAttr('data-toggle title data-placement');
        }
    };
    TComponent.prototype.setFoucs = function() {
        this.jComponent.focus();
    };
    TComponent.prototype.drawDelayed = function(reCreate) {
        var _this = this;
        if (reCreate)
            this.__drawdelayedType = true;
        if (this.__drawdelayed)
            return; //alread in
        this.__drawdelayed = true;
        setTimeout(function() {
            try {
                if (_this.__drawdelayed)
                    _this.draw(_this.__drawdelayedType);
            } finally {
                _this.__drawdelayedType = false;
                _this.__drawdelayed = false;
            }
        }, 50);
    };
    TComponent.prototype.draw = function(reCreate) {
        var _this = this;
        var self = this;
        if (!this.jComponent)
            return;
        if (reCreate)
            this.__drawdelayed = false;
        if (this.Visible) {
            this.jComponent.show(self.__tmpShowDuration, function() {
                _this.__tmpShowDuration = 0;
            });
        } else {
            this.jComponent.hide(self.__tmpHideDuration, function() {
                self.__tmpHideDuration = 0;
            });
        }
        if (this.StickToTop)
            this.jComponent.addClass("affix");
        else
            this.jComponent.removeClass("affix");
        if (reCreate || !this.initialized)
            this.create();
        this.initialized = true;
    };
    /**
     * Makes the control invisible.
     * Call Hide to hide a control. Hide sets the Visible property of the control to false.
     * Although a control that is hidden is not visible, its properties and methods are still available.
     */
    TComponent.prototype.hide = function() {
        this.Visible = false;
    };
    TComponent.prototype.HideWithAnimation = function(duration) {
        this.__tmpHideDuration = duration ? duration : 400;
        this.Visible = false;
    };
    TComponent.prototype.show = function() {
        this.Visible = true;
    };
    TComponent.prototype.showWithAnimation = function(duration) {
        this.__tmpShowDuration = duration ? duration : 400;
        this.Visible = true;
    };
    Object.defineProperty(TComponent.prototype, "isContainer", {
        get: function() {
            return false;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TComponent.prototype, "isPage", {
        get: function() {
            return false;
        },
        enumerable: true,
        configurable: true
    });
    TComponent.prototype.parentInitialized = function() {
        if (!this.owner)
            return true;
        return (this.owner).initialized;
    };
    return TComponent;
}(TObject));
exports.TComponent = TComponent;
var TPopupmenuComponent = /** @class */ (function(_super) {
    __extends(TPopupmenuComponent, _super);

    function TPopupmenuComponent(aOwner, renderTo) {
        var _this = _super.call(this, aOwner, renderTo) || this;
        _this.menuItems = new VXM.TMenuItemCollection();
        _this._showmenucaret = true;
        _this.menuItems.onChanged = function() {
            _this.drawDelayed(true);
        };
        return _this;
    }
    TPopupmenuComponent.prototype.createMenuItem = function(text, onClicked) {
        var menuItem = new VXM.TMenuItem();
        menuItem.Text = text;
        menuItem.onClicked = onClicked;
        this.menuItems.add(menuItem);
        return menuItem;
    };
    TPopupmenuComponent.prototype.create = function() {
        _super.prototype.create.call(this);
        this.reBuildMenu();
    };
    TPopupmenuComponent.prototype.showMenuDropdown = function() {
        this.jComponent.addClass("open");
    };
    TPopupmenuComponent.prototype.hideMenuDropdown = function() {
        this.jComponent.removeClass("open");
    };
    Object.defineProperty(TPopupmenuComponent.prototype, "ShowMenuCaret", {
        /**
         Component that remain in view as the user scrolls the page
        **/
        get: function() {
            return this._showmenucaret;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._showmenucaret) {
                this._showmenucaret = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TPopupmenuComponent.prototype.reBuildMenu = function() {
        this.jComponent.find(".dropdown-menu").empty();
        if (!this.menuItems.length() || !this.Enabled)
            return;
        if (!this.jDropDownTarget)
            return;
        this.jComponent.addClass('dropdown');
        this.jDropDownTarget.attr('data-toggle', "dropdown");
        this.jDropDownTarget.addClass("dropdown-toggle");
        this.menuItems.createmenu('dropdown-menu').appendTo(this.jComponent);
        $('.dropdown-toggle').dropdown();
    };
    return TPopupmenuComponent;
}(TComponent));
exports.TPopupmenuComponent = TPopupmenuComponent;
var TControl = /** @class */ (function(_super) {
    __extends(TControl, _super);

    function TControl() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    TControl.prototype.create = function() {
        var _this = this;
        this.jComponent.off("click").click(function() {
            if (_this.onClicked != null)
                (exports.tryAndCatch(function() { _this.onClicked(); }));
            return false;
        });
        _super.prototype.create.call(this);
    };
    TControl.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
    };
    return TControl;
}(TComponent));
exports.TControl = TControl;
var Clickover = function(element, options) {
    // local init
    this.cinit('clickover', element, options);
    //this.clickery();
};
Clickover.prototype = $.extend({}, $.fn.popover.Constructor.prototype, {
    constructor: Clickover,
    cinit: function(type, element, options) {
        this.attr = {};
        // choose random attrs instead of timestamp ones
        this.attr.me = ((Math.random() * 10) + "").replace(/\D/g, '');
        this.attr.click_event_ns = "click." + this.attr.me + " touchstart." + this.attr.me;
        if (!options)
            options = {};
        options.trigger = 'manual';
        // call parent
        this.init(type, element, options);
        // setup our own handlers
        //this.$element.on('click', this.options.selector, $.proxy(this.clickery, this));
        // soon add click hanlder to body to close this element
        // will need custom handler inside here
    },
    clickery: function(e) {
        // clickery isn't only run by event handlers can be called by timeout or manually
        // only run our click handler and  
        // need to stop progration or body click handler would fire right away
        if (e) {
            e.preventDefault();
            e.stopPropagation();
        }
        // set popover's tip 'id' for greater control of rendering or css rules
        this.options.tip_id && this.tip().attr('id', this.options.tip_id);
        // add a custom class
        this.options.class_name && this.tip().addClass(this.options.class_name);
        // we could override this to provide show and hide hooks 
        //this[this.isShown() ? 'hide' : 'show']();
        // if shown add global click closer
        if (this.isShown()) {
            this.$element.css('dispaly', 'block').addClass('in');
            var that = this;
            // close on global request, exclude clicks inside clickover
            this.options.global_close &&
                $('body').on(this.attr.click_event_ns, function(e) {
                    if (!that.tip().has(e.target).length) {
                        that.clickery();
                    }
                });
            this.options.esc_close && $(document).bind('keyup.clickery', function(e) {
                if (e.keyCode == 27) {
                    that.clickery();
                }
                return;
            });
            // first check for others that might be open
            // wanted to use 'click' but might accidently trigger other custom click handlers
            // on clickover elements 
            !this.options.allow_multiple &&
                $('[data-clickover-open=1]').each(function() {
                    $(this).data('clickover') && $(this).data('clickover').clickery();
                });
            // help us track elements w/ open clickovers using html5
            this.$element.attr('data-clickover-open', 1);
            // if element has close button then make that work, like to
            // add option close_selector
            this.tip().on('click', '[data-dismiss="clickover"]', $.proxy(this.clickery, this));
            // trigger timeout hide
            if (this.options.auto_close && this.options.auto_close > 0) {
                this.attr.tid = setTimeout($.proxy(this.clickery, this), this.options.auto_close);
            }
            // provide callback hooks for post shown event
            typeof this.options.onShown == 'function' && this.options.onShown.call(this);
            this.$element.trigger('shown');
        } else {
            this.$element.css('dispaly', 'none').removeClass('in');
            this.$element.removeAttr('data-clickover-open');
            this.options.esc_close && $(document).unbind('keyup.clickery');
            $('body').off(this.attr.click_event_ns);
            if (typeof this.attr.tid == "number") {
                clearTimeout(this.attr.tid);
                delete this.attr.tid;
            }
            this.$element.trigger('hidden');
        }
    },
    isShown: function() {
        return this.tip().hasClass('in');
    },
    resetPosition: function() {
        var $tip, inside, pos, actualWidth, actualHeight, placement, tp;
        if (this.hasContent() && this.enabled) {
            $tip = this.tip();
            placement = typeof this.options.placement == 'function' ?
                this.options.placement.call(this, $tip[0], this.$element[0]) :
                this.options.placement;
            inside = /in/.test(placement);
            pos = this.getPosition(inside);
            pos = this.getPosition(inside);
            pos.top = this.$element[0].offsetTop;
            pos.left = this.$element[0].offsetLeft;
            actualWidth = this.options.width ? this.options.width : $tip[0].offsetWidth;
            actualHeight = $tip[0].offsetHeight;
            var maxleft = $(window).width() - actualWidth;
            var lft = Math.min(maxleft, pos.left + pos.width / 2 - actualWidth / 2);
            //lft = Math.max(lft, 0);
            switch (inside ? placement.split(' ')[1] : placement) {
                case 'bottom':
                    tp = { top: pos.top + pos.height, left: lft };
                    break;
                case 'top':
                    tp = { top: pos.top - actualHeight, left: lft };
                    break;
                case 'left':
                    tp = { top: pos.top + pos.height / 2 - actualHeight / 2, left: pos.left - actualWidth };
                    break;
                case 'right':
                    tp = { top: pos.top + pos.height / 2 - actualHeight / 2, left: pos.left + pos.width };
                    break;
            }
            $tip.css(tp);
        }
    }
});
/* plugin definition */
/* stolen from bootstrap tooltip.js */
$.fn.clickover = function(option) {
    return this.each(function() {
        var $this = $(this),
            data = $this.data('clickover'),
            options = typeof option == 'object' && option;
        if (!data)
            $this.data('clickover', (data = new Clickover(this, options)));
        if (typeof option == 'string')
            data[option]();
    });
};
$.fn.clickover.Constructor = Clickover;
// these defaults are passed directly to parent classes
$.fn.clickover.defaults = $.extend({}, $.fn.popover.defaults, {
    trigger: 'manual',
    auto_close: 0,
    global_close: 1,
    esc_close: 1,
    onShown: null,
    onHidden: null,
    width: null,
    height: null,
    tip_id: null,
    class_name: 'clickover',
    allow_multiple: 0 /* enable to allow for multiple clickovers to be open at the same time */
});