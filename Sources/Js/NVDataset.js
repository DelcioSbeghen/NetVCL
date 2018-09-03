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
//var VXDS = require("./VXServer");
//var VXCO = require("./VXContainer");
var TGenericDataset = /** @class */ (function(_super) {
    __extends(TGenericDataset, _super);

    function TGenericDataset() {
        var _this = _super !== null && _super.apply(this, arguments) || this;
        _this.recordset = [];
        _this._recno = -1;
        _this._setfieldtouppercase = true;
        _this.indataChanged = false;
        /**
         * Disables data display in data bounded components associated with the dataset.
         **/
        _this._enabledControl = true;
        return _this;
    }
    Object.defineProperty(TGenericDataset.prototype, "RecordCount", {
        get: function() {
            if (this.recordset == null)
                return -1;
            else
                return this.recordset.length;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TGenericDataset.prototype, "Recno", {
        get: function() {
            return this._recno;
        },
        set: function(val) {
            if (this.RecordCount < 1)
                val = -1;
            else if (val > (this.RecordCount - 1))
                val = this.RecordCount - 1;
            else if (val < 0)
                val = 0;
            this._recno = val;
        },
        enumerable: true,
        configurable: true
    });
    TGenericDataset.prototype.getFieldValue = function(fieldname) {
        if (fieldname == null)
            return null;
        if (this.Recno == -1)
            return null;
        var keup = this.SetFieldNameToUpperCase ? fieldname.toUpperCase() : fieldname;
        return this.recordset[this.Recno][keup];
    };
    TGenericDataset.prototype.FieldExists = function(fieldname) {
        if (fieldname == null)
            return false;
        if (this.Recno == -1)
            return false;
        var keup = this.SetFieldNameToUpperCase ? fieldname.toUpperCase() : fieldname;
        return keup in this.recordset[this.Recno];
    };
    TGenericDataset.prototype.aggregateFieldSum = function(fieldname) {
        var _this = this;
        var sum = 0;
        this.forEach(function() {
            var a = _this.getFieldValue(fieldname);
            if (!isNaN(a)) {
                sum += parseFloat(a);
            }
        });
        return sum;
    };
    Object.defineProperty(TGenericDataset.prototype, "SetFieldNameToUpperCase", {
        get: function() {
            return this._setfieldtouppercase;
        },
        set: function(val) {
            val = V.convertaAnyToBoolean(val);
            this._setfieldtouppercase = val;
        },
        enumerable: true,
        configurable: true
    });
    TGenericDataset.prototype.find = function(condition) {
        var _this = this;
        var rec = -1;
        this.forEach(function() {
            if (condition() && rec == -1) {
                rec = _this.Recno;
            }
        });
        if (rec != -1) {
            this.Recno = rec;
        }
    };
    TGenericDataset.prototype.aggregateFieldAvg = function(fieldname) {
        var _this = this;
        var sum = 0;
        var cnt = 0;
        this.forEach(function() {
            var a = _this.getFieldValue(fieldname);
            if (!isNaN(a)) {
                sum += parseFloat(a);
                cnt++;
            }
        });
        return cnt > 0 ? sum / cnt : null;
    };
    TGenericDataset.prototype.aggregateFieldMax = function(fieldname) {
        var _this = this;
        var mx = null;
        this.forEach(function() {
            var a = _this.getFieldValue(fieldname);
            if (!isNaN(a)) {
                mx = mx ? Math.max(parseFloat(a), mx) : parseFloat(a);
            }
        });
        return mx;
    };
    TGenericDataset.prototype.aggregateFieldMin = function(fieldname) {
        var _this = this;
        var mx = null;
        this.forEach(function() {
            var a = _this.getFieldValue(fieldname);
            if (!isNaN(a)) {
                mx = mx ? Math.min(parseFloat(a), mx) : parseFloat(a);
            }
        });
        return mx;
    };
    TGenericDataset.prototype.setFieldValue = function(fieldname, value) {
        if (this.Recno == -1)
            return null;
        var a = this.recordset[this.Recno];
        var keup = this.SetFieldNameToUpperCase ? fieldname.toUpperCase() : fieldname;
        a[keup] = value;
        this.dataChanged();
    };
    TGenericDataset.prototype.dataChanged = function() {
        if (this.indataChanged)
            return;
        try {
            this.indataChanged = true;
            if (this._enabledControl) {
                if (this.onDataChanged != null)
                    this.onDataChanged();
                this.triggerEvent(TDataset.EVENT_DATA_CHANGED);
            }
        } finally {
            this.indataChanged = false;
        }
    };
    /**
     *  Iterator function, which can be used to seamlessly iterate over all records
     */
    TGenericDataset.prototype.forEach = function(Callback) {
        this.DisableControls();
        var recIndex = this.Recno;
        for (var i = 0, len = this.recordset.length; i < len; i++) {
            this.Recno = i;
            Callback();
        }
        this.Recno = recIndex;
        this.EnableControls();
    };
    TGenericDataset.prototype.DisableControls = function() {
        this._enabledControl = false;
    };
    /**
     * Enable data display in data bounded components associated with the dataset.
     **/
    TGenericDataset.prototype.EnableControls = function() {
        this._enabledControl = true;
    };
    Object.defineProperty(TGenericDataset.prototype, "Active", {
        get: function() {
            return true;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TGenericDataset.prototype, "Readonly", {
        get: function() {
            return false;
        },
        enumerable: true,
        configurable: true
    });
    /**
     * return an object with the content of the currnt record
     */
    TGenericDataset.prototype.getCurrentRecord = function() {
        if (this.Recno < 0)
            return null;
        return this.recordset[this.Recno];
    };
    /**
     * return a unique indentifier of the record
     */
    TGenericDataset.prototype.getRecordIndex = function() {
        if (this.Recno < 0)
            return -1;
        return this.recordset[this.Recno]["___RECORDID___"];
    };
    /**
     * return the record number of specfix recordIndex
     */
    TGenericDataset.prototype.getRecordIndexRecNo = function(recordIndex) {
        if (!this.recordset)
            return -1;
        var rc = -1;
        for (var i = 0, len = this.recordset.length; i < len; i++) {
            if (this.recordset[i]['___RECORDID___'] == recordIndex) {
                rc = i;
                break;
            }
        }
        return rc;
    };
    TGenericDataset.prototype.getRecords = function(start, end, sortDirection, sortProperty, groupProperty) {
        return this.recordset.slice(start, end + 1);
    };
    TGenericDataset.prototype.getCheckCount = function() {
        var _this = this;
        var cnt = 0;
        this.forEach(function() {
            if (_this.isChecked())
                cnt++;
        });
        return cnt;
    };
    TGenericDataset.prototype.checkRecord = function() {
        if (this.Recno == -1)
            return null;
        this.recordset[this.Recno]['___CHECKED___'] = true;
    };
    TGenericDataset.prototype.uncheckRecord = function() {
        if (this.Recno == -1)
            return null;
        this.recordset[this.Recno]['___CHECKED___'] = false;
    };
    TGenericDataset.prototype.isChecked = function() {
        if (this.recordset[this.Recno]['___CHECKED___'])
            return true;
        return false;
    };
    /**
     * Moves to the next record in the dataset.
     **/
    TGenericDataset.prototype.next = function() {
        if ((this.RecordCount - 1) > this.Recno) {
            this.Recno++;
            return true;
        } else
            return false;
    };
    /**
     * Moves to the previous record in the dataset.
     **/
    TGenericDataset.prototype.prior = function() {
        if (this.RecordCount > 0) {
            this.Recno--;
            return true;
        } else
            return false;
    };
    /**
     * Moves to the first record in the dataset.
     **/
    TGenericDataset.prototype.first = function() {
        if (this.RecordCount > 0) {
            this.Recno = 0;
            return true;
        } else {
            this.Recno = -1;
            return false;
        }
    };
    /**
     * Moves to the last record in the dataset.
     **/
    TGenericDataset.prototype.last = function() {
        if (this.RecordCount > 0) {
            this.Recno = this.RecordCount - 1;
            return true;
        } else {
            this.Recno = -1;
            return false;
        }
    };
    TGenericDataset.EVENT_SELECTION_CHANGED = "rec_select_changed";
    TGenericDataset.EVENT_DATA_CHANGED = "rec_data_changed";
    TGenericDataset.EVENT_STATE_CHANGED = "rec_stated_changed";
    return TGenericDataset;
}(TObject));
exports.TGenericDataset = TGenericDataset;
var TDataset = /** @class */ (function(_super) {
    __extends(TDataset, _super);

    function TDataset() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    return TDataset;
}(TGenericDataset));
exports.TDataset = TDataset;
var TClientDataset = /** @class */ (function(_super) {
    __extends(TClientDataset, _super);

    function TClientDataset(aOwner) {
        var _this = _super.call(this) || this;
        _this._readonly = false;
        _this._showprogressbar = true;
        _this._active = false;
        _this.metadata = [];
        if (aOwner != null && !aOwner.isContainer) {
            V.Application.raiseException("only container components can own datasets");
            throw "only container components can own components";
        }
        _this.owner = aOwner;
        if (aOwner != null)
            aOwner.addComponent(_this);
        return _this;
    }
    /*
     * return a new dataset with distinct values
     */
    TClientDataset.prototype.distinct = function(fields) {
        var _this = this;
        if (!fields || !fields.length || fields.length == 0)
            return null;
        var cds = new TClientDataset(this.owner);
        var distinct = {};
        this.forEach(function() {
            var recVal = "";
            fields.forEach(function(FieldName) { recVal += _this.getFieldValue(FieldName) + "~~"; });
            if (distinct[recVal] == null) {
                var rec = {};
                fields.forEach(function(FieldName) { rec[FieldName] = _this.getFieldValue(FieldName); });
                cds.appendRecord(rec);
                distinct[recVal] = true;
            }
        });
        return cds;
    };
    TClientDataset.prototype.loadRemoteResults = function(data) {
        var _this = this;
        //replace the dates with JS dates - data is loaded from .net backend
        if (data.META || data.DATA) {
            if (data.META) {
                for (var i = 0, l = data.META.length; i < l; i++) {
                    if (data.META[i].TYPE.toLowerCase() != 'date')
                        continue;
                    for (var j = 0; j < data.DATA.length; j++) {
                        if (data.DATA[j][data.META[i].NAME] != null) {
                            // sometime type Date value can be sent as string
                            var milisecs = (typeof data.DATA[j][data.META[i].NAME] === "number") ? data.DATA[j][data.META[i].NAME] : parseInt(data.DATA[j][data.META[i].NAME]);
                            //data.DATA[j][data.META[i].NAME] = new Date(data.DATA[j][data.META[i].NAME]);
                            data.DATA[j][data.META[i].NAME] = new Date(milisecs);
                        }
                    }
                }
            }
            this.setData(data.DATA);
            this.setMetaData(data.META);
            if (this.onAfterOpen != null)
                (V.tryAndCatch(function() { _this.onAfterOpen(_this); }));
        } else if (data instanceof Array) {
            this.setData(data);
            if (this.onAfterOpen != null)
                (V.tryAndCatch(function() { _this.onAfterOpen(_this); }));
        } else if (data instanceof Object) { //single record data
            this.setData([data]);
            if (this.onAfterOpen != null)
                (V.tryAndCatch(function() { _this.onAfterOpen(_this); }));
        }
    };
    TClientDataset.prototype.openRemoteMethod = function(methodname, param) {
        var _this = this;
        if (this.onBeforeOpen != null)
            (V.tryAndCatch(function() { _this.onBeforeOpen(_this); }));
        var server = new VXDS.TServer();
        if (this.owner != null && (this.owner instanceof VXCO.TContainer)) {
            this.owner.addQuery(this);
        }
        server.send(methodname, param, function(data) {
            _this.loadRemoteResults(data);
            if (_this.owner != null && (_this.owner instanceof VXCO.TContainer)) {
                _this.owner.removeQuery(_this);
            }
        }, function(errorMessage) {
            if (_this.owner != null && (_this.owner instanceof VXCO.TContainer)) {
                _this.owner.removeQuery(_this);
            }
            if (_this.onError != null)
                (V.tryAndCatch(function() { _this.onError(errorMessage); }));
            else
                V.Application.raiseException(errorMessage);
        });
    };
    TClientDataset.prototype.selectionChanged = function() {
        if (this.onSelectionChanged != null)
            this.onSelectionChanged();
        this.triggerEvent(TDataset.EVENT_SELECTION_CHANGED);
    };
    TClientDataset.prototype.stateChanged = function() {
        if (this.onStateChanged != null)
            this.onStateChanged();
        this.triggerEvent(TDataset.EVENT_STATE_CHANGED);
    };
    /**
     * Specifies the callback function of the current filter for a dataset.
     * Use applyFilter to specify a dataset filter. When filtering is applied to a dataset, only those records that meet a filter's conditions are available.
     */
    TClientDataset.prototype.applyFilter = function(filterCallback) {
        if (this.sourceRecordset == null)
            this.sourceRecordset = this.recordset;
        else
            this.recordset = this.sourceRecordset;
        var tempRecordset = [];
        this.DisableControls();
        var recIndex = 0;
        for (var i = 0, len = this.recordset.length; i < len; i++) {
            this.Recno = i;
            if (filterCallback()) {
                var item = jQuery.extend(true, {}, this.recordset[i]);
                tempRecordset.push(item);
                item["___RECORDID___"] = recIndex;
                recIndex++;
            }
        }
        this.recordset = tempRecordset;
        this.EnableControls();
        this.first();
        this.stateChanged();
    };
    /**
     * Clear the filter for a dataset.
     */
    TClientDataset.prototype.clearFilter = function() {
        if (this.sourceRecordset)
            this.recordset = this.sourceRecordset;
        this.sourceRecordset = null;
        this.first();
        this.stateChanged();
    };
    Object.defineProperty(TClientDataset.prototype, "Readonly", {
        get: function() {
            return this._readonly;
        },
        set: function(val) {
            val = V.convertaAnyToBoolean(val);
            if (val != this._readonly) {
                this._readonly = val;
                this.stateChanged();
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TClientDataset.prototype, "ShowProgressBar", {
        get: function() {
            return this._showprogressbar;
        },
        set: function(val) {
            val = V.convertaAnyToBoolean(val);
            if (val != this._showprogressbar) {
                this._showprogressbar = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TClientDataset.prototype, "Recno", {
        get: function() {
            return this._recno;
        },
        set: function(val) {
            if (this.RecordCount < 1)
                val = -1;
            else if (val > (this.RecordCount - 1))
                val = this.RecordCount - 1;
            else if (val < 0)
                val = 0;
            this._recno = val;
            if (this._enabledControl)
                this.selectionChanged();
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(TClientDataset.prototype, "Active", {
        get: function() {
            return this._active;
        },
        set: function(val) {
            val = V.convertaAnyToBoolean(val);
            if (val != this._active) {
                this._active = val;
            }
        },
        enumerable: true,
        configurable: true
    });
    /**
     * Adds a new records to the end of the dataset. the method return the new record number
     * example x.appendRecords([{fileid : 1},{fileid : 2}]);
     */
    TClientDataset.prototype.appendRecords = function(recordsArray, disableEvents) {
        if (disableEvents === void 0) { disableEvents = false; }
        if (!this.Active)
            this.setData([]);
        for (var i = 0; i < recordsArray.length; i++) {
            this.appendRecord(recordsArray[i], true);
        }
        if (this._enabledControl && !disableEvents)
            this.dataChanged();
        return this.RecordCount - 1;
    };
    /**
     * delete the current record
     */
    TClientDataset.prototype.deleteRecord = function() {
        if (!this.Active)
            return;
        if (this.Recno == -1)
            return;
        var rec = this.Recno;
        this.recordset.splice(this.Recno, 1);
        this.Recno = rec;
        if (this._enabledControl)
            this.dataChanged();
    };
    /**
     * Adds a new record to the end of the dataset. the method return the new record number
     */
    TClientDataset.prototype.appendRecord = function(record, disableEvents) {
        if (disableEvents === void 0) { disableEvents = false; }
        if (!this.Active)
            this.setData([]);
        if (record instanceof Array) {
            var recs = record;
            this.appendRecords(recs);
            return;
        }
        var maxid = 0;
        for (var i = 0; i < this.recordset.length; i++) {
            maxid = Math.max(maxid, this.recordset[i].___RECORDID___);
        }
        var obj = {};
        obj.___RECORDID___ = maxid + 1;
        obj.___CHECKED___ = false;
        for (var key in record) {
            var keup = this.SetFieldNameToUpperCase ? key.toUpperCase() : key;
            obj[keup] = record[key];
        }
        this.recordset.push(obj);
        if (this._enabledControl && !disableEvents) {
            this.Recno = this.RecordCount - 1;
            this.dataChanged();
        }
        return this.RecordCount - 1;
    };
    TClientDataset.prototype.setMetaData = function(meta) {
        this.metadata = [];
        if (meta != null) {
            for (var i = 0, len = meta.length; i < len; i++) {
                var obj = {};
                for (var key in meta[i]) {
                    var keup = this.SetFieldNameToUpperCase ? key.toUpperCase() : key;
                    obj[keup] = meta[i][key];
                }
                obj["NAME"] = obj["NAME"].toUpperCase();
                this.metadata.push(obj);
            }
        }
    };
    TClientDataset.prototype.setData = function(data) {
        if (!(data instanceof Array) && (data instanceof Object)) {
            return this.setData([data]);
        }
        if (data.length > 0 && typeof data[1] != "object") {
            V.Application.raiseException("dataset except only object or array of objects");
            return;
        }
        this.recordset = [];
        this.sourceRecordset = null;
        this.tempRecordset = null;
        if (data == null) {
            this._active = false;
            this.Recno = -1;
        } else {
            for (var i = 0, len = data.length; i < len; i++) {
                var obj = {};
                obj.___RECORDID___ = i;
                obj.___CHECKED___ = false;
                for (var key in data[i]) {
                    var keup = this.SetFieldNameToUpperCase ? key.toUpperCase() : key;
                    if (typeof data[i][key] == 'string' && TClientDataset.reISO.exec(data[i][key]))
                        obj[keup] = new Date(data[i][key]);
                    else
                        obj[keup] = data[i][key];
                }
                this.recordset.push(obj);
            }
            this._active = true;
            var newrec = this.RecordCount > 0 ? 0 : -1;
            this.Recno = newrec;
        }
        this.stateChanged();
    };
    TClientDataset.prototype.countTempGroupset = function(grp) {
        if (!grp || grp == "")
            grp = "~@!~#!@#!@#!!@#!@";
        return this.tempGroupset[grp];
    };
    TClientDataset.prototype.sort = function(columnName, ascending, groupname) {
        var _this = this;
        if (ascending === void 0) { ascending = true; }
        if (groupname === void 0) { groupname = null; }
        this.tempRecordset = this.recordset.slice(0);
        this.tempGroupset = [];
        if (groupname)
            this.tempRecordset.forEach(function(item) {
                var grp = item[groupname];
                if (!grp || grp == "") {
                    grp = "~@!~#!@#!@#!!@#!@";
                }
                if (_this.tempGroupset[grp] == null)
                    _this.tempGroupset[grp] = 1;
                else
                    _this.tempGroupset[grp] = _this.tempGroupset[grp] + 1;
            });
        if (ascending) {
            this.tempRecordset.sort(function(a, b) {
                var a1 = "";
                var b1 = "";
                if (groupname) {
                    a1 += a[groupname];
                    b1 += b[groupname];
                    if (a1 > b1)
                        return 1;
                    else if (a1 < b1)
                        return -1;
                }
                if (columnName) {
                    var tmpStr;
                    if (!a[columnName])
                        a1 += "~~";
                    else if (a[columnName].getMonth) {
                        tmpStr = "000000000" + a[columnName].getTime();
                        a1 += "~~" + tmpStr.substr(tmpStr.length - 16);
                    } else if (typeof a[columnName] == "number") {
                        tmpStr = "000000000000" + a[columnName].toFixed(6);
                        a1 += "~~" + tmpStr.substr(tmpStr.length - 18);
                    } else
                        a1 += "~~" + a[columnName];
                    if (!b[columnName])
                        b1 += "~~";
                    else if (b[columnName].getMonth) {
                        tmpStr = "000000000" + b[columnName].getTime();
                        b1 += "~~" + tmpStr.substr(tmpStr.length - 16);
                    } else if (typeof b[columnName] == "number") {
                        tmpStr = "000000000000" + b[columnName].toFixed(6);
                        b1 += "~~" + tmpStr.substr(tmpStr.length - 18);
                    } else
                        b1 += "~~" + b[columnName];
                }
                if (a1 > b1)
                    return 1;
                else if (a1 < b1)
                    return -1;
                else
                    return 0;
            });
        } else {
            this.tempRecordset.sort(function(a, b) {
                var a1 = "";
                var b1 = "";
                if (groupname) {
                    a1 += a[groupname];
                    b1 += b[groupname];
                    if (a1 > b1)
                        return 1;
                    else if (a1 < b1)
                        return -1;
                }
                if (columnName) {
                    var tmpStr;
                    if (!a[columnName])
                        a1 += "~~";
                    else if (a[columnName].getMonth) {
                        tmpStr = "000000000" + a[columnName].getTime();
                        a1 += "~~" + tmpStr.substr(tmpStr.length - 16);
                    } else if (typeof a[columnName] == "number") {
                        tmpStr = "000000000000" + a[columnName].toFixed(6);
                        a1 += "~~" + tmpStr.substr(tmpStr.length - 18);
                    } else
                        a1 += "~~" + a[columnName];
                    if (!b[columnName])
                        b1 += "~~";
                    else if (b[columnName].getMonth) {
                        tmpStr = "000000000" + b[columnName].getTime();
                        b1 += "~~" + tmpStr.substr(tmpStr.length - 16);
                    } else if (typeof b[columnName] == "number") {
                        tmpStr = "000000000000" + b[columnName].toFixed(6);
                        b1 += "~~" + tmpStr.substr(tmpStr.length - 18);
                    } else
                        b1 += "~~" + b[columnName];
                }
                if (a1 < b1)
                    return 1;
                else if (a1 > b1)
                    return -1;
                else
                    return 0;
            });
        }
        this.sortProperty = columnName;
        this.sortDirection = ascending ? "ASC" : "DESC";
        this.groupProperty = groupname;
        this.recordset = this.tempRecordset;
    };
    TClientDataset.prototype.getRecords = function(start, end, sortDirection, sortProperty, groupProperty) {
        if ((sortProperty == null || sortProperty == "") && (groupProperty == null || groupProperty == ""))
            return this.recordset.slice(start, end + 1); //not sort was mentions
        sortProperty = sortProperty.toUpperCase();
        sortDirection = sortDirection.toUpperCase();
        if (this.tempRecordset == null || sortDirection != this.sortDirection ||
            sortProperty != this.sortProperty || groupProperty != this.groupProperty) {
            this.sort(sortProperty, sortDirection == "ASC", groupProperty);
        }
        return this.recordset.slice(start, end + 1);
    };
    /**
     * Data represents the client dataset's local, in-memory copy of its data, encoded as a array of any
     */
    TClientDataset.reISO = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}(?:\.\d*))(?:Z|(\+|-)([\d|:]*))?$/;
    return TClientDataset;
}(TDataset));
exports.TClientDataset = TClientDataset;
var TNestedClientDataset = /** @class */ (function(_super) {
    __extends(TNestedClientDataset, _super);

    function TNestedClientDataset(aOwner, ownerDataset, ownerColumn) {
        var _this = _super.call(this, aOwner) || this;
        _this.owner = aOwner;
        _this.ownerColumn = ownerColumn;
        _this.parentDataset = ownerDataset;
        return _this;
    }
    Object.defineProperty(TNestedClientDataset.prototype, "parentDataset", {
        /**
         * Specifies the dataset that contains the field it represents.
         */
        get: function() {
            return this._parentdataset;
        },
        set: function(val) {
            var _this = this;
            if (val != this._parentdataset) {
                if (this._parentdataset) {
                    this._parentdataset.removeEventListener(TDataset.EVENT_DATA_CHANGED, this);
                    this._parentdataset.removeEventListener(TDataset.EVENT_SELECTION_CHANGED, this);
                    this._parentdataset.removeEventListener(TDataset.EVENT_STATE_CHANGED, this);
                }
                this._parentdataset = val;
                if (this._parentdataset) {
                    this._parentdataset.registerEventListener(TDataset.EVENT_DATA_CHANGED, this, function() { _this.setNestedData(_this._parentdataset.getFieldValue(_this.ownerColumn)); });
                    this._parentdataset.registerEventListener(TDataset.EVENT_SELECTION_CHANGED, this, function() {
                        if (_this._parentRecno != _this._parentdataset.Recno) {
                            _this._parentRecno = _this._parentdataset.Recno;
                            _this.setNestedData(_this._parentdataset.getFieldValue(_this.ownerColumn));
                        }
                    });
                    this._parentdataset.registerEventListener(TDataset.EVENT_STATE_CHANGED, this, function() { _this.setNestedData(_this._parentdataset.getFieldValue(_this.ownerColumn)); });
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    TNestedClientDataset.prototype.setNestedData = function(data) {
        if (!data)
            this.setData(null);
        else if (data instanceof Array) {
            this.setData(data);
        } else if (data instanceof Object) {
            var allObject = true;
            for (var key in data) {
                if (data.hasOwnProperty(key)) {
                    if (!(data instanceof Object)) {
                        allObject = false;
                        break;
                    }
                }
            }
            if (!allObject)
                this.setData([data]);
            else
                this.setData($.map(data, function(value, index) { return [value]; }));
        } else
            this.setData(null);
    };
    return TNestedClientDataset;
}(TClientDataset));
exports.TNestedClientDataset = TNestedClientDataset;