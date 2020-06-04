unit NV.Controls;

interface

uses
  Classes, Windows, Messages, SysUtils, Controls, NV.Ajax, NV.Json,
  NV.Interfaces, NV.MergeSort, NV.VCL.Images;

type

  { TODO -oDelcio -cFocus : Implement SetFocus in Controls and WinControls }

  TNvControl = class(TGraphicControl, INvControl, INVRenderableComponent)
  protected
    class function DefaultHtmlTag: string; virtual; // Default Class HtmlTag
    class function SupportImage: Boolean; virtual;  // Activate ImageListLink Support in class
  private
    FId                 : string;
    FOnClick            : TNotifyEvent;
    FOnEnter            : TNotifyEvent;
    FOnExit             : TNotifyEvent;
    FTagHtml            : string;
    FImageListLink      : TNVImageListLink;
    FUpdatingFromRequest: Boolean;
    function ClassType: string;
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetOnEnter(const Value: TNotifyEvent);
    procedure SetOnExit(const Value: TNotifyEvent);
    procedure SetTagHtml(const Value: string);
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetImageListLink(const Value: TNVImageListLink);
    procedure SetPropValueByRequest(aPropName: string; aValue: Variant);
  protected
    FRendered      : Boolean;
    FRenderPosition: Boolean;
    // // Avoid Interface errors in TNVSessionApp.ControlByID after control Released
    // function _AddRef: Integer; stdcall;
    // function _Release: Integer; stdcall;
    //

    function GetText: TCaption; virtual;
    procedure SetText(const Value: TCaption); virtual;
    procedure SetParent(AParent: TWinControl); override;
    function IsCaptionStored: Boolean; virtual;
    function IsTagHtmlStored: Boolean;
    // After component dropped in IDE Designer
    procedure AfterDesignDrop; virtual;
    procedure AddIncludes(Ajax: TNvAjax); virtual;
    function WebClassType: string; virtual;
    function Ajax: TNvAjax; virtual;
    function ControlAjaxJson: TJsonObject;
    function GetID: string;
    function NeedSendChange: Boolean;
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); virtual;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property ImageListLink: TNVImageListLink read FImageListLink write SetImageListLink;
    property Text: TCaption read GetText write SetText;
    property TagHtml: string read FTagHtml write SetTagHtml stored IsTagHtmlStored;
    // events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; virtual;
    procedure DoClick(aEvent: TJsonObject);
    procedure DoEnter(aEvent: TJsonObject);
    procedure DoExit(aEvent: TJsonObject);
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    property OnEnter: TNotifyEvent read FOnEnter write SetOnEnter;
    property OnExit: TNotifyEvent read FOnExit write SetOnExit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Render(Ajax: TNvAjax); virtual;
    procedure Invalidate; override;
    procedure ProcessRequest(J: TJsonObject);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function Rendered: Boolean;
    procedure ReRender(Now: Boolean = True); virtual;
    property ID: string read GetID;
  published
  end;

  TNvWinControl = class(TCustomControl, INvControl, INVRenderableComponent)
  protected
    class function DefaultHtmlTag: string; virtual; // Default Class HtmlTag
    class function SupportImage: Boolean; virtual;  // Activate ImageListLink Support in class
  private
    FId                 : string;
    FOnExit             : TNotifyEvent;
    FOnEnter            : TNotifyEvent;
    FOnClick            : TNotifyEvent;
    FTagHtml            : string;
    FImageListLink      : TNVImageListLink;
    FUpdatingFromRequest: Boolean;
    // procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CnCtlColorStatic(var Msg: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMControlListChange(var Message: TMessage); message CM_CONTROLLISTCHANGE;
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetOnEnter(const Value: TNotifyEvent);
    procedure SetOnExit(const Value: TNotifyEvent);
    procedure SetTagHtml(const Value: string);
    procedure SetImageListLink(const Value: TNVImageListLink);
    procedure SetPropValueByRequest(aPropName: string; aValue: Variant);
  protected
    FRendered       : Boolean;
    FRenderPosition : Boolean;
    FControlsOrdered: TObjectListEx;
    // // Avoid Interface errors in TNVSessionApp.ControlByID after control Released
    // function _AddRef: Integer; stdcall;
    // function _Release: Integer; stdcall;
    //
    function GetText: TCaption; virtual;
    procedure SetText(const Value: TCaption); virtual;
    procedure SetParent(AParent: TWinControl); override;
    function IsCaptionStored: Boolean; virtual;
    function IsTagHtmlStored: Boolean;
    // Occurs after component dropped in IDE Designer
    procedure AfterDesignDrop; virtual;
    procedure AddIncludes(Ajax: TNvAjax); virtual;
    function WebClassType: string; virtual;
    function Ajax: TNvAjax; virtual;
    function ControlAjaxJson: TJsonObject;
    function GetID: string;
    function NeedSendChange: Boolean;
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); virtual;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SortControls; virtual;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property ImageListLink: TNVImageListLink read FImageListLink write SetImageListLink;
    property Text: TCaption read GetText write SetText;
    property TagHtml: string read FTagHtml write SetTagHtml stored IsTagHtmlStored;
    // events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; virtual;
    procedure DoClick(aEvent: TJsonObject);
    procedure DoEnter(aEvent: TJsonObject);
    procedure DoExit(aEvent: TJsonObject);
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    property OnEnter: TNotifyEvent read FOnEnter write SetOnEnter;
    property OnExit: TNotifyEvent read FOnExit write SetOnExit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Render(Ajax: TNvAjax); virtual;
    procedure Invalidate; override;
    procedure ProcessRequest(J: TJsonObject); virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function Rendered: Boolean;
    procedure ReRender(Now: Boolean = True); virtual;
    property ID: string read GetID;
  published
    // property Top;
    // property Left;
    // property Width;
    // property Height;
  end;

  TNVModuleContainer = class;

  // Hook IDesgigner(in DesignTime Package) to INVDesignerHook(in Runtime package)
  INVDesignerHook = interface
    ['{1E431DA5-2BEA-4DE7-A330-CC45FD2FB1EC}']
    function GetRoot: TComponent;
    procedure SelectComponent(Instance: TPersistent); overload;
    function GetComponent(const Name: string): TComponent;
    function CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
      Left, Top, Width, Height: Integer): TComponent;
    procedure Edit(const Component: TComponent);
    procedure Modified;
    function UniqueName(const BaseName: string): string;
    function Page: INVPage;
    property Root: TComponent read GetRoot;
  end;

  TNVModuleContainer = class(TNvWinControl)
  private
    function GetDesigner: INVDesignerHook;
    procedure SetDesigner(const Value: INVDesignerHook);
  protected
    FDesigner: INVDesignerHook;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    property Designer: INVDesignerHook read GetDesigner write SetDesigner;
  end;

  TNvSubProperty = class(TPersistent, INVRenderableComponent)
  private
  protected
    FControl : INVRenderableComponent;
    FPropName: string;
    FPrefix  : string;
    FSuffix  : string;
    procedure InternalRender(aJson: TJsonObject); virtual; abstract;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IInterfaceComponentReference (Not used)
    function GetComponent: TComponent; // (Not used)
    // INVBase (Not used)
    function GetID: string; // (Not used)
    property ID: string read GetID;
    // INVRenderableComponent
    function Ajax: TNvAjax;
    function ControlAjaxJson: TJsonObject; virtual;
    function Rendered: Boolean; virtual;
    procedure ReRender(Now: Boolean = True); virtual;
    procedure Invalidate; virtual;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); virtual;
    procedure Render;
  end;

  // Get an control from ID
function ControlByID(aId: string): INvControl;

implementation

uses
  NV.Utils, NV.VCL.Page, TypInfo, StrUtils, NV.VCL.Forms,
  System.Generics.Collections;

type
  TControlActionLinkHack = class(TControlActionLink);

var
  // Control ID of this session Controls
  LastID: Int64;
  // Instances of controls on this session
  ControlsList: TObjectDictionary<string, INvControl>;

  // Add one Control to list
function _AddControl(aControl: INvControl): String;
begin
  if aControl = nil then
    Exit;
  AtomicIncrement(LastID);
  Result := 'c' + LastID.ToString;
  ControlsList.Add(Result, aControl);
end;

// Remove one control from list
procedure _RemoveControl(aControl: INvControl);
begin
  ControlsList.Remove(aControl.ID);
end;

// Get an control from ID
function ControlByID(aId: string): INvControl;
begin
  if not ControlsList.TryGetValue(aId, Result) then
    Result := nil;
end;

{ TSRPInput }

procedure TNvControl.AddIncludes(Ajax: TNvAjax);
begin
  //
end;

procedure TNvControl.AfterConstruction;
var
  _Designer: INVDesignerHook;
begin
  if (csDesigning in ComponentState) //
    and (Name = '')                  //
    and FindDesigner(Self, _Designer) then
    Name := _Designer.UniqueName(ClassName);
  inherited;
  if (csDesigning in ComponentState) and (Owner <> nil) and
    (csDesignInstance in Owner.ComponentState) and not(csLoading in Owner.ComponentState) then
    AfterDesignDrop;
end;

procedure TNvControl.AfterDesignDrop;
begin
  //
end;

function TNvControl.Ajax: TNvAjax;
begin
  if Owner is TNvPage then
    Result := TNvPage(Owner).Ajax
  else
    Result := FindAjax(Self);
end;

function TNvControl.ClassType: string;
begin
  Result := ClassName;
end;

procedure TNvControl.CMEnabledChanged(var Message: TMessage);
begin
  if NeedSendChange then
    begin
      ControlAjaxJson.B['Enabled'] := Enabled;
      Invalidate;
    end;
end;

procedure TNvControl.CMVisibleChanged(var Message: TMessage);
begin
  if NeedSendChange then
    begin
      ControlAjaxJson.B['Visible'] := (Message.WParam = ord(True));
      Invalidate;
    end;
end;

function TNvControl.ControlAjaxJson: TJsonObject;
begin
  Result := Ajax.GetControlJson(FId);
end;

constructor TNvControl.Create(AOwner: TComponent);
begin
  inherited;
  FTagHtml := DefaultHtmlTag;
  GetID;

  FRenderPosition := True;
  FRendered       := False;
  if SupportImage then
    FImageListLink := TNVImageListLink.Create(Self);
  { To Change Image Json prop name
    FImageListLink.ImagePropName:= 'Image2' }
end;

class function TNvControl.DefaultHtmlTag: string;
begin
  Result := 'div';
end;

destructor TNvControl.Destroy;
begin
  if not FId.IsEmpty then
    begin
      if Ajax <> nil then
        begin
          Ajax.AddDestruction(FId);
          Invalidate;
        end;

      _RemoveControl(Self);
    end;

  if SupportImage then
    FImageListLink.Free;

  inherited;
end;

procedure TNvControl.DoClick(aEvent: TJsonObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TNvControl.DoEnter(aEvent: TJsonObject);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TNvControl.DoExit(aEvent: TJsonObject);
begin
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

function TNvControl.GetID: string;
begin
  if FId.IsEmpty and not(csDestroying in ComponentState) then
    begin
      FId := _AddControl(Self);
    end;
  Result := FId;
end;

function TNvControl.GetText: TCaption;
begin
  Result := inherited Text;
end;

procedure TNvControl.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  //
end;

procedure TNvControl.Invalidate;
// var
// _Dsgn: TNvDesign;
begin
  inherited;
  if GetID.IsEmpty then
    Exit;

  if not(csLoading in ComponentState) and (Ajax <> nil) then
    begin
      if not FRendered then
        Render(Ajax);
      Ajax.Invalidate;
    end;
end;

function TNvControl.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not TControlActionLinkHack(ActionLink).IsCaptionLinked;
end;

function TNvControl.IsTagHtmlStored: Boolean;
begin
  Result := FTagHtml <> DefaultHtmlTag;
end;

function TNvControl.NeedSendChange: Boolean;
begin
  Result := FRendered and not FUpdatingFromRequest;
end;

function TNvControl.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  case IndexStr(AEventName, ['click', 'focus', 'blur']) of
    0:
      begin
        DoClick(aEvent);
        Result := True;
      end;
    1:
      begin
        DoEnter(aEvent);
        Result := True;
      end;
    2:
      begin
        DoExit(aEvent);
        Result := True;
      end;
  else Result := False;
  end;

end;

procedure TNvControl.ProcessRequest(J: TJsonObject);
var
  I: Integer;
begin
  for I := 0 to J.Count - 1 do
    if IsPublishedProp(Self, J.Names[I]) then
      SetPropValueByRequest(J.Names[I], J.Values[J.Names[I]])
    else if ProcessEvent(J.Names[I], J.Items[I].ObjectValue) then
      continue;
end;

procedure TNvControl.Render(Ajax: TNvAjax);
var
  _Json: TJsonObject;
begin
  if not FRendered and not GetID.IsEmpty then
    begin
      AddIncludes(Ajax);
      _Json := Ajax.GetControlJson(FId, True);
      with _Json do
        begin
          S['New'] := WebClassType;
          S['Id']  := FId;
          if FTagHtml <> DefaultHtmlTag then
            S['Tag'] := FTagHtml;
          if Parent is TNvPage then
            S['Parent'] := 'NVRoot'
          else if Parent <> nil then
            S['Parent'] := (Parent as TNvWinControl).ID;
          if FRenderPosition then
            begin
              I['Top']    := Top;
              I['Left']   := Left;
              I['Width']  := Width;
              I['Height'] := Height;
            end;
          if Text <> '' then
            S['Text'] := Text;
          if Not Visible then
            B['Visible'] := False;
          if Not Enabled then
            B['Enabled'] := False;
          if SupportImage and FImageListLink.IsValidImage then
            S[FImageListLink.ImagePropName] := FImageListLink.Render;
          if Assigned(FOnClick) then
            A['Events'].Add('click');
          if Assigned(FOnEnter) then
            A['Events'].Add('focus');
          if Assigned(FOnExit) then
            A['Events'].Add('blur');
        end;
      InternalRender(Ajax, _Json);
      FRendered := True;
    end;
end;

function TNvControl.Rendered: Boolean;
begin
  Result := FRendered;
end;

procedure TNvControl.ReRender(Now: Boolean = True);
begin
  FRendered := False;
  if Now then
    Render(Ajax);
end;

// function TNvControl.Router: TNVRouter;
// begin
// Result := FRouter;
// end;

procedure TNvControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  _Changed: Boolean;
  // _Json   : TJsonObject;
begin
  _Changed := ((ALeft <> Left) or (ATop <> Top) or (AWidth <> Width) or (AHeight <> Height));
  inherited;
  if _Changed and FRenderPosition and NeedSendChange then
    begin
      // _Json := ControlAjaxJson;
      // if _Json <> nil then
      with ControlAjaxJson do
        begin
          I['Top']    := Top;
          I['Left']   := Left;
          I['Width']  := Width;
          I['Height'] := Height;
          Invalidate;
        end;
    end;
end;

procedure TNvControl.SetImageListLink(const Value: TNVImageListLink);
begin
  if FImageListLink <> Value then
    FImageListLink.Assign(Value);
end;

procedure TNvControl.SetOnClick(const Value: TNotifyEvent);
begin
  // if Value <> FOnClick then
  begin
    if Assigned(Value) then
      begin
        // Router.AddRoute('click', DoClick);
        if Rendered then
          ControlAjaxJson.A['Events'].Add('click');
      end;
    FOnClick := Value;
  end;
end;

procedure TNvControl.SetOnEnter(const Value: TNotifyEvent);
begin
  // if Value <> FOnEnter then
  begin
    if Assigned(Value) then
      begin
        // Router.AddRoute('focus', DoEnter);
        if Rendered then
          ControlAjaxJson.A['Events'].Add('focus');
      end;
    FOnEnter := Value;
  end;
end;

procedure TNvControl.SetOnExit(const Value: TNotifyEvent);
begin
  // if Value <> FOnExit then
  begin
    if Assigned(Value) then
      begin
        // Router.AddRoute('blur', DoExit);
        if Rendered then
          ControlAjaxJson.A['Events'].Add('blur');
      end;
    FOnExit := Value;
  end;
end;

procedure TNvControl.SetParent(AParent: TWinControl);
begin
  inherited;

  if csDestroying in ComponentState then
    Exit;

  if FRendered then
    begin
      if Parent = nil then
        ControlAjaxJson.S['Parent'] := ''
      else if Parent is TNvPage then
        ControlAjaxJson.S['Parent'] := 'NVRoot'
      else
        ControlAjaxJson.S['Parent'] := (Parent as TNvWinControl).ID;
    end
  else if Assigned(AParent) and (AParent as TNvWinControl).Rendered then
    ReRender;
end;

procedure TNvControl.SetPropValueByRequest(aPropName: string; aValue: Variant);
begin
  // to avoid setter resend change to browser
  FUpdatingFromRequest := True;
  try
    SetPropValue(Self, aPropName, aValue);
  finally
    FUpdatingFromRequest := False;
  end;
end;

procedure TNvControl.SetTagHtml(const Value: string);
begin
  if Value <> FTagHtml then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Tag'] := Value;
      FTagHtml                   := Value;
      Invalidate;
    end;
end;

procedure TNvControl.SetText(const Value: TCaption);
begin
  if Value <> Text then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Text'] := Value;
      inherited Text              := Value;
      Invalidate;
    end;
end;

class function TNvControl.SupportImage: Boolean;
begin
  Result := False;
end;

function TNvControl.WebClassType: string;
begin
  Result := ClassName;
end;

// function TNvControl._AddRef: Integer;
// begin
// Result := -1;
// end;
//
// function TNvControl._Release: Integer;
// begin
// Result := -1;
// end;

{ TNvWinControl }

procedure TNvWinControl.AddIncludes(Ajax: TNvAjax);
begin
  //
end;

procedure TNvWinControl.AfterConstruction;
var
  _Designer: INVDesignerHook;
begin
  if (csDesigning in ComponentState) //
    and (Name = '')                  //
    and FindDesigner(Self, _Designer) then
    Name := _Designer.UniqueName(ClassName);
  inherited;
  if (csDesigning in ComponentState) and (Owner <> nil) and
    (csDesignInstance in Owner.ComponentState) and not(csLoading in Owner.ComponentState) then
    AfterDesignDrop;
end;

procedure TNvWinControl.AfterDesignDrop;
begin
  //
end;

function TNvWinControl.Ajax: TNvAjax;
begin
  if Owner is TNvPage then
    Result := TNvPage(Owner).Ajax
  else
    Result := FindAjax(Self);
end;

procedure TNvWinControl.CMControlListChange(var Message: TMessage);
begin
  if Boolean(Message.LParam) = True then
    FControlsOrdered.Add(Pointer(Message.WParam))
  else
    FControlsOrdered.Remove(Pointer(message.WParam));
end;

procedure TNvWinControl.CMEnabledChanged(var Message: TMessage);
begin
  if NeedSendChange then
    begin
      ControlAjaxJson.B['Enabled'] := Enabled;
      Invalidate;
    end;
end;

procedure TNvWinControl.CMVisibleChanged(var Message: TMessage);
begin
  if NeedSendChange then
    begin
      ControlAjaxJson.B['Visible'] := (Message.WParam = ord(True));
      Invalidate;
    end;
end;

procedure TNvWinControl.CnCtlColorStatic(var Msg: TWMCtlColorStatic);
begin
  SetBKMode(Msg.ChildDC, TRANSPARENT);
  Msg.Result := GetStockObject(NULL_BRUSH);
end;

function TNvWinControl.ControlAjaxJson: TJsonObject;
begin
  Result := Ajax.GetControlJson(FId);
end;

constructor TNvWinControl.Create(AOwner: TComponent);
begin
  inherited;
  FControlsOrdered             := TObjectListEx.Create;
  FControlsOrdered.OwnsObjects := False;
  FTagHtml                     := DefaultHtmlTag;
  GetID;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csDoubleClicks,
    csParentBackground, csPannable, csGestures];

  FRenderPosition := True;
  FRendered       := False;

  if SupportImage then
    FImageListLink := TNVImageListLink.Create(Self);
  { To Change Image Json prop name
    FImageListLink.ImagePropName:= 'Image2' }
end;

procedure TNvWinControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

// procedure TNvWinControl.CreateRouter;
// begin
// FRouter := TNVRouter.Create;
// end;

class function TNvWinControl.DefaultHtmlTag: string;
begin
  Result := 'div';
end;

destructor TNvWinControl.Destroy;
  procedure DestroyControls(aControl: TControl);
  var
    I         : Integer;
    _Container: TNvWinControl;
  begin
    if aControl is TNvControl then
      Ajax.AddDestruction(TNvControl(aControl).FId)
    else if aControl is TNvWinControl then
      begin
        _Container := TNvWinControl(aControl);

        for I := 0 to _Container.ControlCount - 1 do
          if _Container.Controls[I] is TWinControl then
            DestroyControls(TWinControl(_Container.Controls[I]))
          else if _Container.Controls[I] is TNvControl then
            Ajax.AddDestruction(TNvControl(_Container.Controls[I]).FId);

        Ajax.AddDestruction(_Container.FId);
      end;
  end;

begin
  if not FId.IsEmpty then
    begin
      if Ajax <> nil then
        begin
          DestroyControls(Self);
          Invalidate;
        end;

      _RemoveControl(Self);
    end;
  FControlsOrdered.Free;

  if SupportImage then
    FImageListLink.Free;

  inherited;
end;

procedure TNvWinControl.DoClick(aEvent: TJsonObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

Procedure TNvWinControl.DoEnter(aEvent: TJsonObject);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TNvWinControl.DoExit(aEvent: TJsonObject);
begin
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

function TNvWinControl.GetID: string;
begin
  if FId.IsEmpty and not(csDestroying in ComponentState) then
    begin
      FId := _AddControl(Self);
    end;
  Result := FId;
end;

function TNvWinControl.GetText: TCaption;
begin
  Result := inherited Text;
end;

procedure TNvWinControl.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  //
end;

procedure TNvWinControl.Invalidate;
// var
// _Dsgn: TNvDesign;
begin
  inherited;

  if GetID.IsEmpty then
    Exit;

  if not(csLoading in ComponentState) and (Ajax <> nil) then
    begin
      if not FRendered then
        Render(Ajax);

      Ajax.Invalidate;
    end;
end;

function TNvWinControl.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not TControlActionLinkHack(ActionLink).IsCaptionLinked;
end;

function TNvWinControl.IsTagHtmlStored: Boolean;
begin
  Result := FTagHtml <> DefaultHtmlTag;
end;

function TNvWinControl.NeedSendChange: Boolean;
begin
  Result := FRendered and not FUpdatingFromRequest;
end;

procedure TNvWinControl.Paint;
begin
  SetBKMode(Handle, TRANSPARENT);
  // Canvas.Pen.Style:=psClear;
  // Canvas.Pen.Mode:= pmCopy;
  // Canvas.Pen.Width:= 0;
  // inherited;
  // Canvas.RoundRect(ClientRect, 15,15);
end;

function TNvWinControl.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  case IndexStr(AEventName, ['click', 'focus', 'blur']) of
    0:
      begin
        DoClick(aEvent);
        Result := True;
      end;
    1:
      begin
        DoEnter(aEvent);
        Result := True;
      end;
    2:
      begin
        DoExit(aEvent);
        Result := True;
      end;
  else Result := False;
  end;
end;

procedure TNvWinControl.ProcessRequest(J: TJsonObject);
var
  I       : Integer;
  _Control: INvControl;
begin
  for I := 0 to J.Count - 1 do
    begin
      if (J.Types[J.Names[I]] <> jdtObject) and IsPublishedProp(Self, J.Names[I]) then
        SetPropValueByRequest(J.Names[I], J.Items[I].variantValue)
      else if ProcessEvent(J.Names[I], J.Items[I].ObjectValue) then
        continue
      else
        begin
          _Control := ControlByID(J.Names[I]);
          if _Control <> nil then
            _Control.ProcessRequest(J.O[J.Names[I]]);
        end;
    end;
end;

procedure TNvWinControl.Render(Ajax: TNvAjax);
var
  I    : Integer;
  _Json: TJsonObject;
begin
  SortControls;
  if not FRendered and not GetID.IsEmpty then
    begin
      AddIncludes(Ajax);
      _Json := Ajax.GetControlJson(FId, True);
      with _Json do
        begin
          S['New'] := WebClassType;
          S['Id']  := FId;
          if FTagHtml <> DefaultHtmlTag then
            S['Tag'] := FTagHtml;
          if Parent is TNvPage then
            S['Parent'] := 'NVRoot'
          else if (Parent <> nil) and (Parent is TNvWinControl) then
            S['Parent'] := (Parent as TNvWinControl).ID;
          if FRenderPosition then
            begin
              I['Top']    := Top;
              I['Left']   := Left;
              I['Width']  := Width;
              I['Height'] := Height;
            end;
          if Text <> '' then
            S['Text'] := Text;
          if Not Visible then
            B['Visible'] := False;
          if Not Enabled then
            B['Enabled'] := False;
          if SupportImage and FImageListLink.IsValidImage then
            S[FImageListLink.ImagePropName] := FImageListLink.Render;
          if Assigned(FOnClick) then
            A['Events'].Add('click');
          if Assigned(FOnEnter) then
            A['Events'].Add('focus');
          if Assigned(FOnExit) then
            A['Events'].Add('blur');
        end;
      InternalRender(Ajax, _Json);
      FRendered := True;
    end;
  for I := 0 to FControlsOrdered.Count - 1 do
    begin
      if FControlsOrdered[I] is TNvControl then
        TNvControl(FControlsOrdered[I]).Render(Ajax)
      else if FControlsOrdered[I] is TNvWinControl then
        TNvWinControl(FControlsOrdered[I]).Render(Ajax)
    end;
end;

function TNvWinControl.Rendered: Boolean;
begin
  Result := FRendered;
end;

procedure TNvWinControl.ReRender(Now: Boolean);
var
  I: Integer;
begin
  FRendered := False;

  SortControls;

  for I := 0 to FControlsOrdered.Count - 1 do
    begin
      if FControlsOrdered[I] is TNvControl then
        TNvControl(FControlsOrdered[I]).ReRender(False)
      else if FControlsOrdered[I] is TNvWinControl then
        TNvWinControl(FControlsOrdered[I]).ReRender(False);
    end;

  if Now then
    Render(Ajax);
end;

// function TNvWinControl.Router: TNVRouter;
// begin
// Result := FRouter;
// end;

procedure TNvWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  _Changed: Boolean;
  // _Json   : TJsonObject;
begin
  if (Self is TNVModuleContainer) and FUpdatingFromRequest then
    Exit;

  _Changed := ((ALeft <> Left) or (ATop <> Top) or (AWidth <> Width) or (AHeight <> Height));
  inherited;
  if _Changed and FRenderPosition and NeedSendChange then
    begin
      // _Json := ControlAjaxJson;
      // if _Json <> nil then
      with ControlAjaxJson do
        begin
          I['Top']    := Top;
          I['Left']   := Left;
          I['Width']  := Width;
          I['Height'] := Height;
          Invalidate;
        end;
    end;
end;

procedure TNvWinControl.SetImageListLink(const Value: TNVImageListLink);
begin
  if (FImageListLink <> Value) then
    FImageListLink.Assign(Value);
end;

procedure TNvWinControl.SetOnClick(const Value: TNotifyEvent);
begin
  // if Value <> FOnClick then
  begin
    if Assigned(Value) then
      begin
        // Router.AddRoute('click', DoClick);
        if Rendered then
          ControlAjaxJson.A['Events'].Add('click');
      end;
    FOnClick := Value;
  end;
end;

procedure TNvWinControl.SetOnEnter(const Value: TNotifyEvent);
begin
  // if Value <> FOnEnter then
  begin
    if Assigned(Value) then
      begin
        // Router.AddRoute('focus', DoEnter);
        if Rendered then
          ControlAjaxJson.A['Events'].Add('focus');
      end;
    FOnEnter := Value;
  end;
end;

procedure TNvWinControl.SetOnExit(const Value: TNotifyEvent);
begin
  // if Value <> FOnExit then
  begin
    if Assigned(Value) then
      begin
        // Router.AddRoute('blur', DoExit);
        if Rendered then
          ControlAjaxJson.A['Events'].Add('blur');
      end;
    FOnExit := Value;
  end;
end;

procedure TNvWinControl.SetParent(AParent: TWinControl);
begin
  inherited;

  if (csDestroying in ComponentState) or not(AParent is TNvWinControl) then
    Exit;

  if FRendered then
    begin
      if Parent = nil then
        ControlAjaxJson.S['Parent'] := ''
      else if Parent is TNvPage then
        ControlAjaxJson.S['Parent'] := 'NVRoot'
      else
        ControlAjaxJson.S['Parent'] := (Parent as TNvWinControl).ID;
    end
  else if Assigned(AParent) //
    and (AParent as TNvWinControl).Rendered then
    ReRender;
end;

procedure TNvWinControl.SetPropValueByRequest(aPropName: string; aValue: Variant);
begin
  // to avoid setter resend change to browser
  FUpdatingFromRequest := True;
  try
    SetPropValue(Self, aPropName, aValue);
  finally
    FUpdatingFromRequest := False;
  end;
end;

procedure TNvWinControl.SetTagHtml(const Value: string);
begin
  if Value <> FTagHtml then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Tag'] := Value;
      FTagHtml                   := Value;
      Invalidate;
    end;
end;

procedure TNvWinControl.SetText(const Value: TCaption);
begin
  if Value <> Text then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Text'] := Value;
      inherited Text              := Value;
      Invalidate;
    end;
end;

procedure TNvWinControl.SortControls;
begin
  // only in bootstrap for now
end;

class function TNvWinControl.SupportImage: Boolean;
begin
  Result := False;
end;

function TNvWinControl.WebClassType: string;
begin
  Result := ClassName;
end;

procedure TNvWinControl.WMEraseBkGnd(var Msg: TWMEraseBkGnd);
begin
  // SetBkMode(msg.DC, TRANSPARENT);
  Msg.Result := 1;
end;

// function TNvWinControl._AddRef: Integer;
// begin
// Result := -1;
// end;
//
// function TNvWinControl._Release: Integer;
// begin
// Result := -1;
// end;

// procedure TNvWinControl.WMPaint(var Message: TWMPaint);
// begin
// Message.DC := 0;
// Exit;
// end;

{ TNVModuleContainer }

constructor TNVModuleContainer.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption];
end;

procedure TNVModuleContainer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if (csDesigning in ComponentState) and (Parent = nil) then
    Params.WndParent := Application.Handle;
end;

destructor TNVModuleContainer.Destroy;
begin
  inherited;
end;

procedure TNVModuleContainer.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I             : Integer;
  OwnedComponent: TComponent;
begin
  inherited GetChildren(Proc, Root);
  if Root = Self then
    for I := 0 to ComponentCount - 1 do
      begin
        OwnedComponent := Components[I];
        if not OwnedComponent.HasParent then
          Proc(OwnedComponent);
      end;
end;

function TNVModuleContainer.GetDesigner: INVDesignerHook;
begin
  if not(csDesigning in ComponentState) then
    Result := nil
  else if FDesigner <> nil then
    Result := FDesigner
  else
    begin
      FDesigner := FindDesigner(Self);
      Result    := FDesigner;
    end;
end;

procedure TNVModuleContainer.Invalidate;
begin
  // if (Designer <> nil) and (DesignSession <> Designer.Page.Ajax.DesignSession) then
  // DesignSession := Designer.Page.Ajax.DesignSession as TNVSessionApp;
  inherited;
end;

procedure TNVModuleContainer.SetDesigner(const Value: INVDesignerHook);
begin
  if FDesigner <> Value then
    FDesigner := Value;
end;

{ TNvSubProperty }

function TNvSubProperty.Ajax: TNvAjax;
begin
  if Assigned(FControl) then
    Result := FControl.Ajax
  else
    Result := nil;
end;

function TNvSubProperty.ControlAjaxJson: TJsonObject;
begin
  if Assigned(FControl) then
    Result := FControl.ControlAjaxJson.O[FPropName]
  else
    Result := nil;
end;

constructor TNvSubProperty.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited Create;
  FControl  := aMaster;
  FPropName := aPropName;
  FPrefix   := aPrefix;
  FSuffix   := aSuffix;
end;

function TNvSubProperty.GetComponent: TComponent;
begin
  Result := nil;
end;

function TNvSubProperty.GetID: string;
begin
  Result := '';
end;

procedure TNvSubProperty.Invalidate;
begin
  if Assigned(FControl) then
    FControl.Invalidate;
end;

function TNvSubProperty.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE
end;

procedure TNvSubProperty.Render;
begin
  if Assigned(FControl) then
    InternalRender(ControlAjaxJson);
end;

function TNvSubProperty.Rendered: Boolean;
begin
  if Assigned(FControl) then
    Result := FControl.Rendered
  else
    Result := False;
end;

procedure TNvSubProperty.ReRender(Now: Boolean);
begin
  FControl.ReRender(Now);
end;

function TNvSubProperty._AddRef: Integer;
begin
  Result := -1; // -1 indicates no reference counting is taking place
end;

function TNvSubProperty._Release: Integer;
begin
  Result := -1; // -1 indicates no reference counting is taking place
end;

procedure InitControls;
begin
  ControlsList             := TObjectDictionary<string, INvControl>.Create;
  NV.VCL.Forms.Screen      := TNvScreen.Create(nil);
  NV.VCL.Forms.Application := TNvApplication.Create(nil);
end;

procedure DoneControls;
begin
  FreeAndNil(NV.VCL.Forms.Screen);
  ControlsList.Free;
end;

initialization

InitControls;

finalization

DoneControls;

end.
