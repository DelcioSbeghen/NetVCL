//Browser Application and Page
import { TWinControl } from "./nv.controls.js";
import { TServer } from "./nv.server.js";
import { RegisterClasses } from "./nv.register.js";
import { TNvLogger, ChangeCompProps } from "./nv.classes.js";

class TApplication {

    constructor(o) {
        // TODO Add Debug handler function to send javascript errors to server
        this.FLogger = new TNvLogger(o.Logger || { Debug: true, HandlerFunc: null });
        this.FComponentList = {}; //Loaded components
        this.FClasses = {}; //loaded classes
        this.FChanges = {};//changes pending of send to server 
        this.FDesign = false; //true is in design mode
        this.FDesigner = null;
        this.FServer = new TServer(o.Port || 80);
        this.FRequiresPending = {}; //o.Requires || {}; //files required 
        this.FRequiresLoded = {};
        this.FFixUpParents = {};
        window.App = this;
        this._CheckLoadedRequires();
        if (o.Requires) { //add initial requires
            o.Requires.forEach(Req => {
                this.AddRequire(Req.Type, Req.Url)
            })
        }
        this.FPage = TApplication.PageClass.Create({
            "Id": "NVRoot",
            "ClassType": "TPage"
        });

        this.FResizeObserver = new ResizeObserver(entries => {
            this._DoObserverResize(entries);
        });
    }

    get Page() { return this.FPage }
    get Server() { return this.FServer }
    get Logger() { return this.FLogger }
    get Url() { return window.location.origin }

    ParseJson(J) {
        //       if (J.Reqs) {
        //           this._ParseRequires(J.Reqs, () => this._ParseComponentRequires(J.Comps));
        //       } else {
        // this._ParseComponentRequires(J.Comps);
        //     }
        if (J.Comps)
            this._ParseComponents(J.Comps);
        this.FixUpParents();
        if (J.Exec) {
            this._ExecuteScrips(J.Exec);
        }



    }


    //Check the ressourses loaded in initial html and add to loadded Requires
    _CheckLoadedRequires() {
        var _Name, _Url, _Type;
        var $this = this;
        $("script").each(function (I, el) {
            _Url = $(el).attr("src");
            if (!_Url)
                return true;
            _Name = $.getFileName(_Url);
            _Type = $(el).attr("type") == "module" ? "jsm" : "js";
            if ($this._AddToLoadedRequireList(_Name, _Type, _Url))
                $(el).attr("id", _Name);
        });

        $("link").each(function (I, el) {
            _Url = $(el).attr("href");
            if (!_Url || $(el).attr("type") != "text/css")
                return true;
            _Name = $.getFileName(_Url);
            if ($this._AddToLoadedRequireList(_Name, "css", _Url))
                $(el).attr("id", _Name);

        });
    }

    _AddToLoadedRequireList(aName, aType, aUrl) {
        if (!this.FRequiresLoded[aName]) {
            this.FRequiresLoded[aName] = {
                Name: aName,
                Type: aType,
                Url: aUrl,
            };
            return true;
        } else
            return false;
    }

    _ParseComponentRequires(C) {
        if (!C) return;
        var _ReqChanged = false;
        C.forEach(element => {
            if (element.New) {
                var _classReg = TApplication.RegisteredClasses[element.New];
                let k;
                for (k of Object.keys(_classReg.Modules)) {
                    var _module = _classReg.Modules[k];
                    var _addedModule = this.FRequiresLoded[_module.Name];
                    if (!_addedModule) {
                        _addedModule = this.FRequiresPending[_module.Name] = {};
                        _addedModule.Name = _module.Name;
                        _addedModule.Type = _module.Type;
                        _addedModule.Url = _module.Url;

                        _ReqChanged = true;
                    };
                };
            };
        });
        if (_ReqChanged) {
            this._checkRequires(() => { this._ParseComponents(C) });
            return false;
        }
        else
            this._ParseComponents(C);
    }

    _checkRequires() {
        var $this = this;
        return new Promise(function (resolve, reject) {
            if (Object.keys($this.FRequiresPending).length === 0) {
                resolve();
            } else {
                let promisses = [];
                let k;
                for (k of Object.keys($this.FRequiresPending)) {
                    var required = $this.FRequiresPending[k];
                    $this._AddToLoadedRequireList(required.Name, required.Type, required.Url);
                    delete $this.FRequiresPending[required.Name];
                    promisses.push(
                        $this._getRequired(required).then(
                            () => { 
                                $this.Logger.Debug('Require Loaded:' + required.Name); 
                            },//resolved
                            (reqFailed) => { //rejected
                                $this.FRequiresPending[reqFailed.Name] = reqFailed;
                                delete $this.FRequiresLoded[reqFailed.Name];
                                $this.Logger.Error('Cant Load Required:' + JSON.stringify(reqFailed));
                            }
                        )
                    );
                };

                Promise.all(promisses).then(() => {
                    $this.Logger.Debug('All Requireds Loaded');
                    resolve();
                });

                // $this._getRequired(required).then(() => {

                //     let _Name = required.Name || $.getFileName(required.Url);
                //     let _Type = required.Type || $.getFileExt(required.Url);
                //     $this._AddToLoadedRequireList(_Name, _Type, required.Url);

                //     delete $this.FRequiresPending[_Name];

                //     $this._checkRequires().then(() => { 
                //         resolve() 
                //     });

                //     // if (Object.keys($this.FRequiresPending).length === 0) 
                //     //     resolve()
                //     //  else
                //     //  _checkRequires().then(() => { resolve()});
                // });
                // // };
            }
        });
    }


    _getRequired(required) {
        var newEl;
        return new Promise(function (resolve, reject) {
            if (required.Type == 'jsm') {
                newEl = document.createElement("script");
                newEl.setAttribute("type", "module");
                if (required.Name) newEl.setAttribute("id", required.Name);
                $(newEl).insertBefore("#last-js");
                $(newEl).on("load", () => resolve(required)); //check for next requires
                newEl.setAttribute("src", required.Url);
            } else if (required.Type == 'js') {
                newEl = document.createElement("script");
                newEl.setAttribute("type", "text/javascript");
                if (required.Name) newEl.setAttribute("id", required.Name);
                $(newEl).insertBefore("#last-js");
                $(newEl).on("load", () => resolve(required)); //check for next requires
                newEl.setAttribute("src", required.Url);
            } else if (required.Type == 'css') {
                newEl = document.createElement("link");
                newEl.setAttribute("rel", "stylesheet");
                newEl.setAttribute("type", "text/css");
                if (required.Name) newEl.setAttribute("id", required.Name);
                $(newEl).insertBefore("#last-css");
                $(newEl).on("load", () => resolve(required)); //check for next requires
                newEl.setAttribute("href", required.Url);
            } else reject(required)
        });
    }

    _ParseComponents(Comps) {
        if (Comps.length === 0)
            return; //avoid loop

        var _Comp = Comps.shift(); //remove first component from list

        if (_Comp.New) {
            //First check if component already exists(Delphi ReRender Method)
            if (this.FComponentList[_Comp.New]) {
                this.ChangeComponent(_Comp);
                this._ParseComponents(Comps);
            } else {
                //create next component only after create this component to avoid errors because
                //next components use this component properties reference (Next.Parent = Actual)
                this.NewComponent(_Comp).then(() => { this._ParseComponents(Comps) });
            }
        }
        else if (_Comp.Change) {
            this.ChangeComponent(_Comp);
            this._ParseComponents(Comps);
        }
        else if (_Comp.Del) {
            this.DeleteComponent(_Comp);
            this._ParseComponents(Comps);
        }
    }


    NewComponent(C) {
        return new Promise(function (resolve, reject) {
            var _Class = App.FClasses[C.New];
            if (_Class) {
                resolve(_Class.Create(C))
            } else {
                //if class not found, load class module and return only after class loaded and component created
                (async () => {
                    var _classReg = TApplication.RegisteredClasses[C.New];
                    //get class module
                    let module = await import(_classReg.Module);
                    _Class = module[C.New];
                    //get other class required files
                    _Class.ResolveRequires(_classReg).then(() => {
                        App.FClasses[C.New] = _Class;
                        //create component
                        resolve(_Class.Create(C));
                    });
                })();
            }
        });
    }

    ChangeComponent(C) {
        //
        if (this.FComponentList[C.Change]) {
            ChangeCompProps(this.FComponentList[C.Change], C);
        }
    }

    //DeleteComponent({Del:Id})
    DeleteComponent(C) {
        if (this.FComponentList[C.Del]) {
            this.FComponentList[C.Del].Free();
        };
    }


    _ExecuteScrips(Scripts) {

        if (Scripts.length === 0)
            return; //avoid loop

        var _Scrpt = Scripts.shift(); //remove first script from list

        if (_Scrpt) {
            //execute next script after this terminated
            this.ExecuteScript(_Scrpt).then(() => { this._ExecuteScrips(Scripts) });
        }
        else if (Scripts.length > 0) //next script
            _ExecuteScrips(Scripts);
    }

    ExecuteScript(S) {
        let $this = this;
        return new Promise(function (resolve, reject) {
            $this.FLogger.Debug("ExecuteScript:" + S);
            try {
                eval(S);
                resolve();
            } catch (e) {
                $this.FLogger.Error(e.message);
                resolve();//reject() ?????
            }
        })
    }

    static RegisterClass(aClass, aModule) {
        var _classReg = TApplication.RegisteredClasses[aClass];
        var $this = this;
        if (!_classReg)
            _classReg = TApplication.RegisteredClasses[aClass] = {
                Module: aModule,
                Modules: {},
                RegisterClassModule: function (aMod, aType) {
                    $this.RegisterClassModule(aClass, aMod, aType);
                    return _classReg;
                }
            };
        return _classReg;
    }

    static RegisterClassModule(aClass, aModule, aType) {
        var _classReg = TApplication.RegisteredClasses[aClass];
        var _ModuleName = $.getFileName(aModule);
        var _Module = _classReg.Modules[_ModuleName];
        if (!_Module)
            _Module = _classReg.Modules[_ModuleName] = {};
        _Module.Name = _ModuleName;
        _Module.Type = aType;
        _Module.Url = aModule;
        return _Module;
    }

    AddComponentToList(C) {
        this.FComponentList[C.Id] = C;
    }

    RemoveComponentFromList(C) {
        this.FComponentList[C.Id] = null;
        delete this.FComponentList[C.Id];
        return this;
    }

    GetComponentById(Id) {
        return this.FComponentList[Id];
    }



    AddRequire(Type, Url, Name = '') {
        if (Name === '')
            Name = $.getFileName(Url);    
        if ((!this.FRequiresLoded[Name]) && (!this.FRequiresPending[Name])) {
            var _req = this.FRequiresPending[Name] = {};
            _req.Type = Type;
            _req.Name = Name;
            _req.Url = Url;
        }
    }

    QueueChange(IdComp, Prop, Value) {
        var _Change = this.FChanges[IdComp];
        if (!_Change)
            _Change = (this.FChanges[IdComp] = {});
        _Change[Prop] = Value;
    }


    AddParentFixUp(Control, Parent) {
        var _Control = this.FFixUpParents[Control];
        if (!_Control)
            _Control = (this.FFixUpParents[Control] = {});
        _Control['Parent'] = Parent;
    }

    FixUpParents(timeOut = 0) {
        var $this = this;
        let K;
        for (K of Object.keys(this.FFixUpParents)) {
            var _Control = $this.GetComponentById(K);
            if (_Control) {
                _Control.Parent = $this.FFixUpParents[K].Parent;
                if (_Control.Parent != null)
                    delete $this.FFixUpParents[K];
            }
        }
        if (Object.keys(this.FFixUpParents).length > 0)
            setTimeout(() => { this.FixUpParents(timeOut + 1000) }, timeOut);
    }

    _DoObserverResize(entries) {
        // var _DoResizeAndChilds = function(C){
        //     C._DoResize();
        //         if(C instanceof TWinControl){
        //           C.FControls.forEach(_child => 
        //             _DoResizeAndChilds(_child) 
        //           );  
        //         }
        // }

        for (let entry of entries) {
            let _comp = this.FComponentList[entry.target.id];
            if (_comp)
                _comp._DoResize();
            // _DoResizeAndChilds(_comp);   
        }
    }
}



window.TApplication = TApplication;
TApplication.RegisteredClasses = {};


export class TPage extends TWinControl {
    constructor(o) {
        super(o);
        this.FHead = $('head');
        this.FBody = $('body');
        this.FBody.insertAtIndex(this.FEl, 0);
    }

    _CreateParams(o) {
        this.FRenderPosition = false;
        super._CreateParams(o);
    }

    get Title() {
        return $(document).attr("title");
    }

    set Title(T) {
        if (this.Title != T) $(document).attr("title", T);
    }

}

export class TNVFrame extends TWinControl {
    constructor(o) {
        super(o);
    }

    _CreateParams(o) {
        this.FRenderPosition = false;
        super._CreateParams(o);
    }
}


RegisterClasses();

//TApplication.RegisterClass(TPage);

if (!TApplication.PageClass) {
    TApplication.PageClass = TPage;
}





