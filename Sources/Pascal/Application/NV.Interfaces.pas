unit NV.Interfaces;

interface

uses
  Classes, DB, NV.Ajax, NV.JSON;

type
  TPropChangeProc = procedure(aJson: TJsonObject) of object;

  // interface for all NV Controls and Containers
  INVBase = interface(IInterfaceComponentReference)
    ['{90AF0C4A-F1DF-4F5C-ABE5-B480F7A8E172}']
    function GetId: string;
    property ID: string read GetId;
  end;

  // interface for all renderable properties and controls
  INVRenderableComponent = interface(INVBase)
    ['{D4268587-8C54-497F-B58C-ABF0B7C763A7}']
    function ControlAjaxJson: TJsonObject;
    // procedure RemoveControlAjaxJson;
    function NeedSendChange: Boolean;
    procedure EnqueueChange(const aName: string; const aProc: TPropChangeProc);
    procedure DequeueChange(const aName: string);
    function Rendered: Boolean;
    procedure ReRender(Now: Boolean = True);
    procedure Invalidate;
  end;

  // interface for all controls
  INvControl = interface(INVRenderableComponent)
    ['{9F339EEB-CF73-47A2-9D0D-EBABCD157E59}']
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    // function Router: TNVRouter;
    // function CanFocus: Boolean;
    // procedure SetFocus;
    // function InDesign: Boolean;
    procedure ProcessRequest(J: TJsonObject);
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  INVPage = interface(INvControl)
    ['{4A4D735D-1130-46F7-9789-0AFDB95D791C}']
  end;

implementation

end.
