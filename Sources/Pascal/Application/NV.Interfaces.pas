unit NV.Interfaces;

interface

uses
  Classes, DB, NV.Ajax, NV.JSON;

type
  // interface for all NV Controls and Containers
  INVBase = interface(IInterfaceComponentReference)
    ['{90AF0C4A-F1DF-4F5C-ABE5-B480F7A8E172}']
    function GetId: string;
    property ID: string read GetId;
  end;

  // interface for all renderable properties and controls
  INVRenderableComponent = interface(INVBase)
    ['{D4268587-8C54-497F-B58C-ABF0B7C763A7}']
    function Ajax: TNvAjax;
    function ControlAjaxJson: TJsonObject;
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

  INvGrid  = interface(INvControl)
  ['{964E20A1-D86B-46D3-9B85-40BBF746225A}']
    procedure LayoutChanged;
  end;

  INvDbGrid = interface(INvGrid)
    ['{EA1D3DE4-EFBA-496D-953E-2F9A748D7CDB}']
    procedure LinkActive(Value: Boolean);
    procedure DataChanged;
    procedure Scroll(Distance: Integer);
    procedure EditingChanged;
    procedure RecordChanged(aField: TField);
    procedure UpdateData;
    function SelectedField: TField;
    function Datalink:TDatalink;
    procedure InvalidateRow(ARow: Longint);
    function GetRow:LongInt;
  end;

  INVPage = interface(INvControl)
    ['{4A4D735D-1130-46F7-9789-0AFDB95D791C}']
  end;

implementation

end.
