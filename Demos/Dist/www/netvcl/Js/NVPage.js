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
/// <reference path="Scripts/require.d.ts" />
/// <reference path="Scripts/jquery.d.ts" />
//var V = require("./VCL");
//var VXCO = require("./VXContainer");
var TPage = /** @class */ (function(_super) {
    __extends(TPage, _super);

    function TPage() {
        var _this = _super.call(this, null, null) || this;
        if (!_this.__HTML__)
            exports.Application.raiseException("Error in " + _this.getClassName() + " - You can't instantiate TPage directly, use exports.Application.loadPage ,exports.Application.createPage or exports.Application.navigateToPage");
        return _this;
    }
    TPage.prototype.show = function(aOwner) {
        var _this = this;
        var renderTo;
        if (aOwner == null)
            renderTo = $("#content");
        else
            renderTo = aOwner.jComponent;
        if (renderTo == null)
            exports.Application.raiseException("Elment was not specfied for page '" + this.getClassName() + "'");
        if (renderTo.length == 0)
            exports.Application.raiseException("Cant find element '" + renderTo.attr('ID') + "' for page '" + this.getClassName() + "'");
        renderTo.empty();
        //on some components (tpanel) the content of the control cab be under jContent
        renderTo.append($(this.jContent ? this.jContent : this.jComponent));
        _super.prototype.draw.call(this, true);
        if (this.onShow != null)
            (exports.tryAndCatch(function() { _this.onShow(); }));
    };
    TPage.prototype.refresh = function() {
        this.show();
    };
    Object.defineProperty(TPage.prototype, "isPage", {
        get: function() {
            return true;
        },
        enumerable: true,
        configurable: true
    });
    return TPage;
}(exports.TContainer));
exports.TPage = TPage;