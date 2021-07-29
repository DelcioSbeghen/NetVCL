//Base
export class TSubProperty {

}

export class TComponent {

    constructor(o) {
        this._CreateParams(o);
        this._ChangeParams(o);
        window.App ? App.AddComponentToList(this) : null;
    }

    _CreateParams(o) {
        this.FId = o.Id;
        this.FName = o.Name || "";
    }

    _ChangeParams(o) {
        ChangeCompProps(this, o);
    }

    Free() {
        window.App ? window.App.RemoveComponentFromList(this) : null;
    }

    set Name(aName) {
        this.FName = aName;
    }

    get Name() {
        return this.FName;
    }

    get Id() {
        return this.FId;
    }

    _DoResize() {
        //null implementation
    }

    static ResolveRequires(ClassReg) {
        if (!ClassReg)
            ClassReg = TApplication.RegisteredClasses[this.name];
        return new Promise(function (resolve, reject) {
            let k;
            for (k of Object.keys(ClassReg.Modules)) {
                var _mod = ClassReg.Modules[k];
                App.AddRequire(_mod.Type, _mod.Url, _mod.Name);
            };
            App._checkRequires().then(() => { resolve(this) })
        })
    }

    static Create(o) {
        return new this(o)
    }

};

export class TNvLogger extends TSubProperty {
    constructor(o) {
        super();
        this.FDebug = o.Debug || false;
        this.FHandler = o.HandlerFunc || null;
        window.onerror = function (msg, url, line, col, error) {
            // Note that col & error are new to the HTML 5 spec and may not be 
            // supported in every browser.  It worked for me in Chrome.
            var extra = !col ? '' : '\ncolumn: ' + col;
            extra += !error ? '' : '\nerror: ' + error;

            // You can view the information in an alert to see things working like this:
            this.Error(msg + "\nurl: " + url + "\nline: " + line + extra);

            return true; //supress default error alert
        };
    }

    Debug(m) {
        if (this.FDebug) {
            console.debug(m);
            if (this.FHandler)
                this.FHandler("Debug:" + m);
        }
    }

    Error(m) {
        console.error(m);
        if (this.FHandler)
            this.FHandler("ERROR:" + m);
        alert("ERROR:" + m);
    }
}

export class TNvReceivedQueue {
    constructor() {
        this.FRecQueue = [];
        this.FProcessing = 0;
    }

    _ProcessQueue() {
        if (this.Count == 0)
            return;
        if (this.FProcessing == 0) {
            this.FProcessing++;
            this._ProcessNext();
            this.FProcessing--;
        }
    }


    _ProcessNext() {
        let js = this.FRecQueue.shift();
        App.ParseJsonNew(js).then(() => {
            if (this.Count > 0)
                this._ProcessNext()
        });
    }


    QueueMessage(C) {
        this.FRecQueue.push(C);
        this._ProcessQueue();
    }

    get Count() {
        return this.FRecQueue.length;
    }

}





//-------------------- For Components decendants ----------------------------------------

export function ChangeCompProps(Obj, Props, ignore = ["Change", "New", "Id", "Events"]) {
    Object.keys(Props).forEach(PropName => {
        //Ignore "Change", "New" and "Id" Props
        if ($.isInArray(PropName, ignore))
            return
        ChangeCompProp(Obj, PropName, Props[PropName], ignore);
    });
}


function ChangeCompProp(Obj, Prop, Value) {
    //execute calls to component functions
    if (Prop === "Call") {
        Value.forEach(call => {
            if (Obj[call["function"]] !== undefined)
                Obj[call["function"]](call["params"]);
        });
        //process SubComponent Props
    } else if (Obj[Prop] instanceof TSubProperty && Value !== null) {
        var _SubObj = Obj[Prop];
        ChangeCompProps(_SubObj, Value)
        //process Prop
    } else if (Obj[Prop] !== undefined)
        Obj[Prop] = Value;
};

//-------------------  For Objects  --------------------------

export function ChangeObjProps(Obj, Props, ignore = []) {
    Object.keys(Props).forEach(PropName => {
        ChangeObjProp(Obj, PropName, Props[PropName], ignore);
    });
}


function ChangeObjProp(Obj, Prop, Value, ignore) {
    //Ignore "Change", "New" and "Id" Props
    if ($.isInArray(Prop, ignore)) {
        return
        //process Array Props
    } else if ($.isArray(Value)) {
        Obj[Prop] = Value;
        //process SubObj Props
    } else if (Value instanceof Object && Value !== null) {
        var _SubObj = Obj[Prop];
        ChangeObjProps(_SubObj, Value);
        //process Prop
    } else //if (Obj[Prop] !== undefined)
        Obj[Prop] = Value;
};


/* ***********************************************************
extend jquery to add removeClassStartingWith function to remove element classes starting with
************************************************************ */

$.fn.removeClassStartingWith = function (filter) {
    $(this).removeClass(function (index, className) {
        return (className.match(new RegExp("\\S*" + filter + "\\S*", 'g')) || []).join(' ')
    });
    return this;
};

$.fn.removeClassRegex = function (filter) {
    $(this).removeClass(function (index, className) {
        return (className.match(new RegExp(filter, 'g')) || []).join(' ')
        // \\W(d-(none|inline|inline-block|block|table|table-cell|table-row|flex|inline-flex)+)\\b       
    });
    return this;
};




/* ***********************************************************
extend jquery to add insertAtIndex function to insert element at index
ex: $("body").insertAtIndex("<div>test</div)", 2);
$("body").insertAtIndex(el, 3);
************************************************************ */

$.fn.insertAtIndex = function (elements, index) {
    var children = this.children();
    if (index >= children.length) {
        this.append(elements);
        return this;
    }
    var before = children.eq(index);
    $(elements).insertBefore(before);
    return this;
};

/* ***********************************************************
extend jquery to add isInArray to check if value is present in array
************************************************************ */

$.isInArray = function (item, array) {
    return !!~$.inArray(item, array);
};

/* ***********************************************************
extend jquery to add arrayRemove to remove an item from array and return this item
************************************************************ */
$.arrayRemove = function (item, array) {
    if ($.inArray(item, array))
        return arr.splice($.inArray(item, array), 1);
};


/**
 * Compare two arrays if they are equal even if they have different order.
 *
 * @link https://stackoverflow.com/a/7726509
 */
jQuery.extend({
    /**
     * @param {array} a
     *   First array to compare.
     * @param {array} b
     *   Second array to compare.
     * @return {boolean}
     *   True if both arrays are equal, otherwise false.
     */
    arrayCompare: function (a, b) {
        return $(a).not(b).get().length === 0 && $(b).not(a).get().length === 0;
    }
});


$.getFileName = function (url) {
    //this removes the anchor at the end, if there is one
    url = url.substring(0, (url.indexOf("#") == -1) ? url.length : url.indexOf("#"));
    //this removes the query after the file name, if there is one
    url = url.substring(0, (url.indexOf("?") == -1) ? url.length : url.indexOf("?"));
    //this removes everything before the last slash in the path
    url = url.substring(url.lastIndexOf("/") + 1, url.length);
    //return
    return url;
};

$.getFileExt = function (url) {
    //this get only filename from path
    url = $.getFileName(url);
    //this removes everything before the last . in the path
    url = url.substring(url.lastIndexOf(".") + 1, url.length);
    //return
    return url;
};




/*!
Copyright (c) 2018 Jed Watson.
Licensed under the MIT License (MIT), see
http://jedwatson.github.io/classnames
*/

/* ! 

The classNames function takes any number of arguments which can be a string or object. 
The argument 'foo' is short for {foo: true}. If the value of the key is falsy, it won't be included in the output.

classNames('foo', 'bar'); // => 'foo bar'
      classNames('foo', { bar: true }); // => 'foo bar'
      classNames({ foo: true }, { bar: true }); // => 'foo bar'
      classNames({ foo: true, bar: true }); // => 'foo bar'

      // lots of arguments of various types
      classNames('foo', { bar: true, duck: false }, 'baz', { quux: true }) // => 'foo bar baz quux'

      // other falsy values are just ignored
      classNames(null, false, 'bar', undefined, 0, 1, { baz: null }, ''); // => 'bar 1'
Arrays will be recursively flattened as per the rules above:

var arr = ['b', { c: true, d: false }];
      classNames('a', arr); // => 'a b c' 
     
*/

/* global define */

(function () {
    'use strict';

    var hasOwn = {}.hasOwnProperty;

    function classNames() {
        var classes = [];

        for (var i = 0; i < arguments.length; i++) {
            var arg = arguments[i];
            if (!arg) continue;

            var argType = typeof arg;

            if (argType === 'string' || argType === 'number') {
                classes.push(arg);
            } else if (Array.isArray(arg) && arg.length) {
                var inner = classNames.apply(null, arg);
                if (inner) {
                    classes.push(inner);
                }
            } else if (argType === 'object') {
                for (var key in arg) {
                    if (hasOwn.call(arg, key) && arg[key]) {
                        classes.push(key);
                    }
                }
            }
        }

        return classes.join(' ');
    }

    if (typeof module !== 'undefined' && module.exports) {
        classNames.default = classNames;
        module.exports = classNames;
    } else if (typeof define === 'function' && typeof define.amd === 'object' && define.amd) {
        // register as 'classnames', consistent with npm package name
        define('classnames', [], function () {
            return classNames;
        });
    } else {
        window.classNames = classNames;
    }
}());


/* **********   JQuery replaceTag Plugin ****************
https://stackoverflow.com/questions/918792/use-jquery-to-change-an-html-tag

replaceTag(<tagName>, [withDataAndEvents], [withDataAndEvents])
Arguments:

tagName: String --The tag name e.g. "div", "span", etc.
withDataAndEvents: Boolean --"A Boolean indicating whether event handlers should be copied along with the elements. As of jQuery 1.4, element data will be copied as well." info
deepWithDataAndEvents: Boolean --A Boolean indicating whether event handlers and data for all children of the cloned element should be copied. By default its value matches the first argument's value (which defaults to false)." info

Returns: A newly created jQuery element
*/

$.extend({
    replaceTag: function (element, tagName, withDataAndEvents, deepWithDataAndEvents) {
        var newTag = $("<" + tagName + ">")[0];
        // From [Stackoverflow: Copy all Attributes](http://stackoverflow.com/a/6753486/2096729)
        $.each(element.attributes, function () {
            newTag.setAttribute(this.name, this.value);
        });
        //$(element).children().clone(withDataAndEvents, deepWithDataAndEvents).appendTo(newTag);
        $(element).contents().clone(withDataAndEvents, deepWithDataAndEvents).appendTo(newTag);
        $(element).replaceWith(newTag);
        return newTag;
    }
})
$.fn.extend({
    replaceTag: function (tagName, withDataAndEvents, deepWithDataAndEvents) {
        // Use map to reconstruct the selector with newly created elements
        return this.map(function () {
            return jQuery.replaceTag(this, tagName, withDataAndEvents, deepWithDataAndEvents);
        })
    }
})




