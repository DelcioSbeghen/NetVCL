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
/// <reference path="Scripts/jquery.d.ts" />
//var V = require("./VCL");
//var VXO = require("./VXObject");
//var VXC = require("./VXComponent");
//var VXU = require("./VXUtils");
//var VXB = require("./VXInputBase");
var TContainer = /** @class */ (function(_super) {
    __extends(TContainer, _super);
    /**
    @aOwner     Indicates the component that is responsible for streaming and freeing this component.Onwer must be TContainer
    @renderTo   (Optional) the id of the html element that will be the parent node of the component
    **/
    function TContainer(aOwner, renderTo) {
        var _this = _super.call(this, aOwner, renderTo) || this;
        /**
        Lists all components owned by the component.
        Use Components to access any of the components owned by this component, such as the components owned by a page
        **/
        _this.components = new TCollection();
        _this._backgroundopacity = 1;
        _this._shadow = exports.ShadowOptions.None;
        if (!_this.__HTML__)
            _this.__HTML__ = _this.getContanierHTML();
        if (_this.__HTML__) {
            $(_this.jComponent).html(_this.__HTML__);
            if (_this.__HTML__.toUpperCase().indexOf('<exports.') >= 0) {
                var VArr = $(_this.jComponent).find('*').each(function(index, elem) {
                    if (elem.tagName && elem.tagName.indexOf("exports.") == 0) {
                        var elemId = elem.getAttribute('id');
                        if (elemId && elemId.length < 1) {
                            exports.Application.raiseException(elem.tagName + " component must have an ID");
                            throw elem.tagName + " component must have an ID";
                        }
                        if (elemId && _this[elemId]) {
                            exports.Application.raiseException("Bad id for element " + elem.tagName + "-property '" + elemId + "' already exists in " + _this.getClassName());
                            throw "Bad id for element " + elem.tagName + "-property '" + elem.tagName + "' already exists in " + _this.getClassName();
                        }
                        var comp = exports.createComponentByElement(elem, _this);
                        if (!comp) {
                            exports.Application.raiseException(elem.tagName + " component was not found");
                            throw elem.tagName + " component was not found";
                        } else if (elemId)
                            _this[elemId] = comp;
                    }
                });
            }
            if (exports.Application.getBootstrapVersion() == 3) {
                //check for bootstrap 2 old code
                $(_this.jComponent).find('.row-fluid').each(function(index, elem) {
                    $(elem).removeClass('row-fluid').addClass('row');
                });
                $(_this.jComponent).find("*[class^='span'").each(function(index, elem) {
                    if ($(elem).hasClass('span1'))
                        $(elem).removeClass('span1').addClass('col-md-1');
                    else if ($(elem).hasClass('span2'))
                        $(elem).removeClass('span2').addClass('col-md-2');
                    else if ($(elem).hasClass('span3'))
                        $(elem).removeClass('span3').addClass('col-md-3');
                    else if ($(elem).hasClass('span4'))
                        $(elem).removeClass('span4').addClass('col-md-4');
                    else if ($(elem).hasClass('span5'))
                        $(elem).removeClass('span5').addClass('col-md-5');
                    else if ($(elem).hasClass('span6'))
                        $(elem).removeClass('span6').addClass('col-md-6');
                    else if ($(elem).hasClass('span7'))
                        $(elem).removeClass('span7').addClass('col-md-7');
                    else if ($(elem).hasClass('span8'))
                        $(elem).removeClass('span8').addClass('col-md-8');
                    else if ($(elem).hasClass('span9'))
                        $(elem).removeClass('span9').addClass('col-md-9');
                    else if ($(elem).hasClass('span10'))
                        $(elem).removeClass('span10').addClass('col-md-10');
                    else if ($(elem).hasClass('span11'))
                        $(elem).removeClass('span11').addClass('col-md-11');
                    else if ($(elem).hasClass('span12'))
                        $(elem).removeClass('span12').addClass('col-md-12');
                });
                $(_this.jComponent).find("*[class^='offset'").each(function(index, elem) {
                    if ($(elem).hasClass('offset1'))
                        $(elem).removeClass('offset1').addClass('col-md-offset-1');
                    else if ($(elem).hasClass('offset2'))
                        $(elem).removeClass('offset2').addClass('col-md-offset-2');
                    else if ($(elem).hasClass('offset3'))
                        $(elem).removeClass('offset3').addClass('col-md-offset-3');
                    else if ($(elem).hasClass('offset4'))
                        $(elem).removeClass('offset4').addClass('col-md-offset-4');
                    else if ($(elem).hasClass('offset5'))
                        $(elem).removeClass('offset5').addClass('col-md-offset-5');
                    else if ($(elem).hasClass('offset6'))
                        $(elem).removeClass('offset6').addClass('col-md-offset-6');
                    else if ($(elem).hasClass('offset7'))
                        $(elem).removeClass('offset7').addClass('col-md-offset-7');
                    else if ($(elem).hasClass('offset8'))
                        $(elem).removeClass('offset8').addClass('col-md-offset-8');
                    else if ($(elem).hasClass('offset9'))
                        $(elem).removeClass('offset9').addClass('col-md-offset-9');
                    else if ($(elem).hasClass('offset10'))
                        $(elem).removeClass('offset10').addClass('col-md-offset-10');
                    else if ($(elem).hasClass('offset11'))
                        $(elem).removeClass('offset11').addClass('col-md-offset-11');
                    else if ($(elem).hasClass('offset12'))
                        $(elem).removeClass('offset12').addClass('col-md-offset-12');
                });
            }
            //translate the page
            if (exports.Application.ActiveLanguage) {
                var trnsElem = $(_this.jComponent).find('[data-localizable]');
                trnsElem.each(function(index, elem) {
                    if (!elem.childElementCount)
                        elem.innerHTML = exports.Application.getLanguageTranslation(exports.Application.ActiveLanguage, elem.innerHTML);
                });
            }
        }
        if (_this.onCreate != null)
            (exports.tryAndCatch(function() { _this.onCreate(); }));
        _this.jComponent.off("click").click(function(e) {
            if (_this.onClicked != null)
                (exports.tryAndCatch(function() { _this.onClicked(_this); }));
        });
        _this.jComponent.off("mouseover").mouseover(function() {
            if (_this.onMouseOver != null)
                (exports.tryAndCatch(function() { _this.onMouseOver(_this); }));
        });
        _this.jComponent.off("mouseout").mouseout(function() {
            if (_this.onMouseOut != null)
                (exports.tryAndCatch(function() { _this.onMouseOut(_this); }));
        });
        _this.jComponent.off("mouseenter").mouseenter(function() {
            if (_this.onMouseEnter != null)
                (exports.tryAndCatch(function() { _this.onMouseEnter(_this); }));
        });
        _this.jComponent.off("mouseleave").mouseleave(function() {
            if (_this.onMouseLeave != null)
                (exports.tryAndCatch(function() { _this.onMouseLeave(_this); }));
        });
        return _this;
    }
    TContainer.__setClassPath = function(path) {
        if (!path)
            return;
        var paths = path.split('/');
        if (paths.length == 1)
            this.__classPath = "";
        else
            this.__classPath = paths.slice(0, paths.length - 1).join('/');
    };
    TContainer.getClassPath = function() {
        return this.__classPath ? this.__classPath : "";
    };
    Object.defineProperty(TContainer.prototype, "BackgroundImageURL", {
        get: function() {
            return this._backgroundimageurl;
        },
        set: function(val) {
            if (val != this._backgroundimageurl) {
                this._backgroundimageurl = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TContainer.prototype, "Overflow", {
        /**The overflow property specifies what happens if content overflows an element's box**/
        get: function() {
            return this._overflow;
        },
        set: function(val) {
            if (val != this._overflow) {
                this._overflow = val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TContainer.prototype, "Overflow_X", {
        /**The overflow property specifies what happens if content overflows an element's box**/
        get: function() {
            return this._overflow_x;
        },
        set: function(val) {
            if (val != this._overflow_x) {
                this._overflow_x = val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TContainer.prototype, "Overflow_Y", {
        /**The overflow property specifies what happens if content overflows an element's box**/
        get: function() {
            return this._overflow_y;
        },
        set: function(val) {
            if (val != this._overflow_y) {
                this._overflow_y = val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TContainer.prototype, "BackgroundColor", {
        /**The background-color property sets the background color of an element.**/
        get: function() {
            return this._backgroundcolor;
        },
        set: function(val) {
            if (exports.Application.checkColorString(val)) {
                if (val != this._backgroundcolor) {
                    this._backgroundcolor = val;
                    this.drawDelayed(false);
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TContainer.prototype, "BackgroundColorOpacity", {
        get: function() {
            return this._backgroundopacity;
        },
        set: function(val) {
            if (val != this._backgroundopacity) {
                this._backgroundopacity = val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    TContainer.prototype.addComponent = function(component) {
        this.components.add(component);
    };
    TContainer.prototype.getContanierHTML = function() {
        return this.__HTML__;
    };
    /**
     Check all input for validation - return true if everything is OK
    **/
    TContainer.prototype.ValidateInputs = function() {
        return this.validateContainer(this.components);
    };
    TContainer.prototype.validateContainer = function(components) {
        var _this = this;
        var containerValid = true;
        components.forEach(function(item) {
            if (item instanceof VXB.TEditorBase) {
                var itm = item;
                var userDefMessage = null;
                if (itm.onValidate) {
                    userDefMessage = itm.onValidate(_this);
                    if (userDefMessage) {
                        itm.ShowErrorMessage(userDefMessage);
                        containerValid = false;
                    }
                }
                if (!userDefMessage && itm.Required && itm.isEmpty()) {
                    itm.ShowErrorMessage(exports.Application.LocaleSettings.MSG_This_value_is_required);
                    containerValid = false;
                } else if (!userDefMessage && itm.MinLength > 0 && itm.textLength() < itm.MinLength) {
                    itm.ShowErrorMessage(exports.Application.LocaleSettings.MSG_This_value_is_not_minimum.replace('%s', itm.MinLength.toString()));
                    containerValid = false;
                } else if (!userDefMessage) {
                    itm.HideErrorMessage();
                }
            } else if (item instanceof TContainer) {
                if (!_this.validateContainer(item.components))
                    containerValid = false;
            }
        });
        return containerValid;
    };
    TContainer.prototype.removeShadow = function() {
        this.jComponent.removeClass('jquery-shadow-raised jquery-perspective jquery-shadow jquery-shadow-lifted');
        this.jComponent.removeClass('jquery-shadow-sides jquery-shadow-sides-vt-2 jquery-shadow-sides-vt-1 jquery-shadow-sides-hz-1 jquery-shadow-sides-hz-2');
    };
    TContainer.prototype.draw = function(reCreate, drawChilds) {
        if (drawChilds === void 0) { drawChilds = true; }
        //if (!this.parentInitialized()) return;
        _super.prototype.draw.call(this, reCreate);
        if (this.BackgroundColor) {
            if (!this.BackgroundColorOpacity)
                this.jComponent.css('background-color', this.BackgroundColor);
            else
                this.jComponent.css('background-color', exports.Application.hexColorToRGB(this.BackgroundColor, this.BackgroundColorOpacity));
        }
        if (this.Overflow) {
            if (this.Overflow == exports.Overflow.Visible)
                this.jComponent.css('overflow', 'visible');
            else if (this.Overflow == exports.Overflow.Hidden)
                this.jComponent.css('overflow', 'hidden');
            else if (this.Overflow == exports.Overflow.Scroll)
                this.jComponent.css('overflow', 'scroll');
            else if (this.Overflow == exports.Overflow.Auto)
                this.jComponent.css('overflow', 'auto');
        }
        if (this.Overflow_X) {
            if (this.Overflow_X == exports.Overflow_X.Visible)
                this.jComponent.css('overflow-x', 'visible');
            else if (this.Overflow_X == exports.Overflow_X.Hidden)
                this.jComponent.css('overflow-x', 'hidden');
            else if (this.Overflow_X == exports.Overflow_X.Scroll)
                this.jComponent.css('overflow-x', 'scroll');
            else if (this.Overflow_X == exports.Overflow_X.Auto)
                this.jComponent.css('overflow-x', 'auto');
        }
        if (this.Overflow_Y) {
            if (this.Overflow_Y == exports.Overflow_Y.Visible)
                this.jComponent.css('overflow-y', 'visible');
            else if (this.Overflow_Y == exports.Overflow_Y.Hidden)
                this.jComponent.css('overflow-y', 'hidden');
            else if (this.Overflow_Y == exports.Overflow_Y.Scroll)
                this.jComponent.css('overflow-y', 'scroll');
            else if (this.Overflow_Y == exports.Overflow_Y.Auto)
                this.jComponent.css('overflow-y', 'auto');
        }
        this.removeShadow();
        if (this.ShadowOptions == exports.ShadowOptions.Perspective) {
            this.jComponent.addClass('jquery-shadow jquery-perspective');
        } else if (this.ShadowOptions == exports.ShadowOptions.Raised) {
            this.jComponent.addClass('jquery-shadow jquery-shadow-raised');
        } else if (this.ShadowOptions == exports.ShadowOptions.Lifted) {
            this.jComponent.addClass('jquery-shadow jquery-shadow-lifted');
        } else if (this.ShadowOptions == exports.ShadowOptions.Side_hz_1) {
            this.jComponent.addClass('jquery-shadow jquery-shadow-sides jquery-shadow-sides-hz-1');
        } else if (this.ShadowOptions == exports.ShadowOptions.Side_hz_2) {
            this.jComponent.addClass('jquery-shadow jquery-shadow-sides jquery-shadow-sides-hz-2');
        } else if (this.ShadowOptions == exports.ShadowOptions.Side_vt_1) {
            this.jComponent.addClass('jquery-shadow jquery-shadow-sides jquery-shadow-sides-vt-1');
        } else if (this.ShadowOptions == exports.ShadowOptions.Side_vt_2) {
            this.jComponent.addClass('jquery-shadow jquery-shadow-sides jquery-shadow-sides-vt-2');
        }
        if (this.BackgroundImageURL != null && this.BackgroundImageURL.length > 0) {
            this.jComponent.css('background-image', 'url(' + this.BackgroundImageURL + ')').css('background-size', 'cover').css('background-repeat', 'no-repeat');
        }
        if (drawChilds) {
            this.components.forEach(function(item) {
                if (item instanceof TComponent)
                    item.draw(reCreate);
                return true;
            });
        }
    };
    TContainer.prototype.hide = function() {
        if (this.__popoverFrom)
            this.__popoverFrom.popover(this);
        _super.prototype.hide.call(this);
    };
    TContainer.prototype.show = function() {
        _super.prototype.show.call(this);
    };
    Object.defineProperty(TContainer.prototype, "isContainer", {
        get: function() {
            return true;
        },
        enumerable: true,
        configurable: true
    });
    /** add component with class="row" to the container
    @returns    TBootstrapRow component
    **/
    TContainer.prototype.createBootstrapRow = function() {
        return new TBootstrapRow(this);
    };
    /** add component with class="row-fluid" to the container
    @returns    createBootstrapRowFluid component
    **/
    TContainer.prototype.createBootstrapRowFluid = function() {
        return new TBootstrapRowFluid(this);
    };
    TContainer.prototype.showLoadingProgressBar = function() {
        if (exports.Application.Global__SPINNER__)
            return;
        var opts = {
            lines: 13,
            length: 20,
            width: 10,
            radius: 30,
            corners: 1,
            rotate: 0,
            direction: 1,
            color: '#000',
            speed: 1,
            trail: 60,
            shadow: false,
            hwaccel: false,
            className: 'spinner',
            zIndex: 2e9,
            top: 'auto',
            left: 'auto' // Left position relative to parent in px
        };
        var jq = $("#progresscerrncnter");
        if (jq.length == 0) {
            jq = $("<div id='progresscerrncnter'>");
            jq.css('position', 'fixed');
            jq.css('top', '50%');
            jq.css('left', '50%');
            jq.css('z-index', '9999');
            $("body").append(jq);
        }
        exports.Application.Global__SPINNER__ = new Spinner(opts).spin(document.getElementById('progresscerrncnter'));
    };
    TContainer.prototype.hideLoadingProgressBar = function() {
        if (exports.Application.Global__SPINNER__) {
            exports.Application.Global__SPINNER__.stop();
            exports.Application.Global__SPINNER__ = null;
            var jq = $("#progresscerrncnter");
            jq.empty();
        }
    };
    TContainer.prototype.addQuery = function(query) {
        if (query == null)
            return;
        if (!query.ShowProgressBar)
            return;
        TContainer.activeQueries.add(query);
        if (TContainer.activeQueries.length() == 1)
            this.showLoadingProgressBar();
    };
    TContainer.prototype.removeQuery = function(query) {
        if (query == null)
            return;
        if (!query.ShowProgressBar)
            return;
        TContainer.activeQueries.remove(query);
        if (TContainer.activeQueries.length() == 0)
            this.hideLoadingProgressBar();
    };
    Object.defineProperty(TContainer.prototype, "ShadowOptions", {
        get: function() {
            return this._shadow;
        },
        set: function(val) {
            if (val != this._shadow) {
                this._shadow = val;
                this.drawDelayed(false);
            }
        },
        enumerable: true,
        configurable: true
    });
    TContainer.activeQueries = new collections.Set();
    return TContainer;
}(TComponent));
exports.TContainer = TContainer;
var TBootstrapRow = /** @class */ (function(_super) {
    __extends(TBootstrapRow, _super);
    /**
    @aOwner     Indicates the component that is responsible for streaming and freeing this component.Onwer must be TContainer
    @renderTo   (Optional) the id of the html element that will be the parent node for this component
    **/
    function TBootstrapRow(aOwner, renderTo) {
        var _this = _super.call(this, aOwner, renderTo) || this;
        _this.jComponent.addClass('row');
        return _this;
    }
    TBootstrapRow.prototype.createBootstrapSpan = function(spanSize, offset) {
        if (offset === void 0) { offset = 0; }
        return new TBootstrapSpan(this, null, spanSize, offset);
    };
    return TBootstrapRow;
}(TContainer));
exports.TBootstrapRow = TBootstrapRow;
var TBootstrapRowFluid = /** @class */ (function(_super) {
    __extends(TBootstrapRowFluid, _super);

    function TBootstrapRowFluid(aOwner, renderTo) {
        var _this = _super.call(this, aOwner, renderTo) || this;
        _this.jComponent.addClass('row-fluid');
        return _this;
    }
    TBootstrapRowFluid.prototype.createBootstrapSpan = function(spanSize, offset) {
        if (offset === void 0) { offset = 0; }
        return new TBootstrapSpan(this, null, spanSize, offset);
    };
    return TBootstrapRowFluid;
}(TContainer));
exports.TBootstrapRowFluid = TBootstrapRowFluid;
var TBootstrapSpan = /** @class */ (function(_super) {
    __extends(TBootstrapSpan, _super);

    function TBootstrapSpan(aOwner, renderTo, spanSize, offset) {
        if (spanSize === void 0) { spanSize = 1; }
        if (offset === void 0) { offset = 0; }
        var _this = _super.call(this, aOwner, renderTo) || this;
        _this.jComponent.addClass('span' + spanSize);
        if (offset > 0)
            _this.jComponent.addClass('offset' + offset);
        return _this;
    }
    return TBootstrapSpan;
}(TContainer));
exports.TBootstrapSpan = TBootstrapSpan;
var TRepeater = /** @class */ (function(_super) {
    __extends(TRepeater, _super);

    function TRepeater() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this._pagesize = 10;
        _this._pagerVisible = true;
        _this._paginationsize = exports.PaginationSize.Default;
        return _this;
    }
    Object.defineProperty(TRepeater.prototype, "PageSize", {
        get: function() {
            return this._pagesize;
        },
        set: function(val) {
            if (val != this._pagesize) {
                this._pagesize = Math.floor(val);
                if (this._pagesize < 1)
                    this._pagesize = 1;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TRepeater.prototype, "CurrentIndex", {
        get: function() {
            return this._currentindex;
        },
        set: function(val) {
            if (val != this._pagesize) {
                this._currentindex = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TRepeater.prototype, "currentItem", {
        get: function() {
            return this._currntItem;
        },
        set: function(val) {
            if (val != this._currntItem) {
                this.currentItem = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TRepeater.prototype.drawItems = function() {
        this.jContent.empty(); //clear all subcomponents
        this.jPagination.empty(); //clear all subcomponents
        if (!this.CurrentIndex)
            this.CurrentIndex = 0;
        var startPage = Math.floor(this.CurrentIndex / this.PageSize);
        for (var i = 0; i < this.PageSize; i++) {
            var idx = startPage * this.PageSize + i;
            if (this.onGetItem) {
                var ctrl = this.onGetItem(idx);
                var block = this.createBootstrapRowFluid();
            } else {
                exports.Application.raiseException("TRepeater must provide a component with onGetItem(index : number)");
                return;
            }
        }
        var ul = $("<ul>");
        this.jPagination.append(ul);
        var prevBtn = $("<li>");
        if (!startPage)
            prevBtn.addClass('disabled');
        ul.append(prevBtn.append($("<a>").addClass('icon-chevron-left')));
        var nextBtn = $("<li>");
        ul.append(nextBtn.append($("<a>").addClass('icon-chevron-right')));
    };
    Object.defineProperty(TRepeater.prototype, "PagerVisible", {
        get: function() {
            return this._pagerVisible;
        },
        set: function(val) {
            val = exports.convertaAnyToBoolean(val);
            if (val != this._pagerVisible) {
                this._pagerVisible = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TRepeater.prototype, "PaginationSize", {
        get: function() {
            return this._paginationsize;
        },
        set: function(val) {
            if (val != this._paginationsize) {
                this._paginationsize = val;
                this.drawDelayed(true);
            }
        },
        enumerable: true,
        configurable: true
    });
    TRepeater.prototype.create = function() {
        this.jComponent.empty(); //clear all subcomponents
        this.jComponent = VXU.VXUtils.changeJComponentType(this.jComponent, 'div', this.FitToWidth, this.FitToHeight);
        this.jContent = $("<div>");
        this.jPagination = $("<div>").addClass('pagination').addClass('pagination-right');
        if (this.PaginationSize == exports.PaginationSize.Large) {
            this.jPagination.addClass('pagination-large');
        } else if (this.PaginationSize == exports.PaginationSize.Small) {
            this.jPagination.addClass('pagination-small');
        } else if (this.PaginationSize == exports.PaginationSize.Mini) {
            this.jPagination.addClass('pagination-mini');
        }
        this.jComponent.append(this.jContent);
        if (this.PagerVisible)
            this.jComponent.append(this.jPagination);
    };
    TRepeater.prototype.draw = function(reCreate) {
        if (!this.parentInitialized())
            return;
        _super.prototype.draw.call(this, reCreate);
        this.drawItems();
    };
    return TRepeater;
}(TContainer));
exports.TRepeater = TRepeater;