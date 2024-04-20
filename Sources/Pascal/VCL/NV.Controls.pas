unit NV.Controls;

interface

uses
  Classes, Windows, Messages, SysUtils, Controls, Generics.Collections, NV.Json,
  NV.Interfaces, NV.MergeSort, NV.VCL.Images;

type

  TNvPropChangeList = TDictionary<string, TPropChangeProc>;

  { TODO -oDelcio -cFocus : Implement SetFocus in Controls and WinControls }

  TNvControl = class(TGraphicControl, INvControl, INVRenderableComponent)
  protected
    // Default Class HtmlTag
    class function DefaultHtmlTag: string; virtual;
    // Activate ImageListLink Support in class
    class function SupportImage: Boolean; virtual;
    // Default Class Css
    class function DefaultClassCss: string; virtual;
    // Default Render Text
    class function DefaultRenderText: Boolean; virtual;
  private
    FId           : string;
    FPropChanges  : TNvPropChangeList;
    FOnClick      : TNotifyEvent;
    FOnEnter      : TNotifyEvent;
    FOnExit       : TNotifyEvent;
    FTagHtml      : string;
    FImageListLink: TNVImageListLink;
    FClassCss     : string;
    FRenderIndex  : Integer;
    FRenderText   : Boolean;
    function ClassType: string;
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetOnEnter(const Value: TNotifyEvent);
    procedure SetOnExit(const Value: TNotifyEvent);
    procedure SetTagHtml(const Value: string);
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetImageListLink(const Value: TNVImageListLink);
    procedure SetPropValueByRequest(aPropName: string; aValue: Variant);
    procedure SetClassCss(const Value: string);
    procedure SetRenderIndex(const Value: Integer);
    procedure SetRenderText(const Value: Boolean);
  protected
    FRendered           : Boolean;
    FUpdatingFromRequest: Boolean;
    // FRenderInvisible    : Boolean;
    FRenderPosition: Boolean;
    FSubPropName   : string;
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetName(const Value: TComponentName); override;
    procedure Loaded; override;
    // // Avoid Interface errors in TNVSessionApp.ControlByID after control Released
    // function _AddRef: Integer; stdcall;
    // function _Release: Integer; stdcall;
    //
    function GetText: TCaption; virtual;
    procedure SetText(const Value: TCaption); virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure SetParent(AParent: TWinControl); override;
    function IsCaptionStored: Boolean; virtual;
    function IsRenderIndexStored: Boolean;
    function IsTagHtmlStored: Boolean;
    function IsNotDefaultClassCss: Boolean;
    function IsTextVisibleStored: Boolean;
    function IsSubComponent: Boolean; inline;
    //
    procedure DoPendingChangesChange(Sender: TObject; const Item: string;
      Action: TCollectionNotification);
    // After component dropped in IDE Designer
    procedure AfterDesignDrop; virtual;
    procedure RenameSubComponents(NewName: string); virtual;
    //
    procedure AddIncludes; virtual;
    function WebClassType: string; virtual;
    function ControlAjaxJson: TJsonObject;
    function GetID: string;
    function NeedSendChange: Boolean; inline;
    procedure EnqueueChange(const aName: string; const aProc: TPropChangeProc); inline;
    procedure DequeueChange(const aName: string); inline;
    // Render props
    procedure InternalRender(Json: TJsonObject); virtual;
    procedure RenderTag(aJson: TJsonObject); dynamic;
    procedure RenderParent(aJson: TJsonObject); dynamic;
    procedure RenderPos(aJson: TJsonObject); dynamic;
    procedure RenderTxt(aJson: TJsonObject); dynamic;
    procedure RenderVisible(aJson: TJsonObject); dynamic;
    procedure RenderEnabled(aJson: TJsonObject); dynamic;
    procedure RenderImage(aJson: TJsonObject); dynamic;
    procedure RenderClassCss(aJson: TJsonObject); dynamic;
    procedure RenderEvents(aJson: TJsonObject); virtual;
    procedure RenderRenderIndex(aJson: TJsonObject); dynamic;
    //
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property CaptionVisible: Boolean read FRenderText write SetRenderText
      stored IsTextVisibleStored;
    property ClassCss: string read FClassCss write SetClassCss stored IsNotDefaultClassCss;
    property ImageListLink: TNVImageListLink read FImageListLink write SetImageListLink;
    property Text: TCaption read GetText write SetText;
    property TextVisible: Boolean read FRenderText write SetRenderText stored IsTextVisibleStored;
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
    procedure Render; virtual;
    procedure RenderChanges;
    procedure Invalidate; override;
    procedure ProcessRequest(J: TJsonObject);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function Rendered: Boolean;
    procedure ReRender(Now: Boolean = True); virtual;
    procedure SetSubComponent(IsSubComponent: Boolean; PropName: string); reintroduce;
    procedure AddClassCss(aClass: string);
    procedure RemoveClassCss(aClass: string);
    property ID: string read GetID;
  published
    property Left stored FRenderPosition;
    property Top stored FRenderPosition;
    property Width stored FRenderPosition;
    property Height stored FRenderPosition;
    property RenderIndex: Integer read FRenderIndex write SetRenderIndex stored IsRenderIndexStored;
  end;

  TNvWinControl = class(TCustomControl, INvControl, INVRenderableComponent)
  protected
    // Default Class HtmlTag
    class function DefaultHtmlTag: string; virtual;
    // Activate ImageListLink Support in class
    class function SupportImage: Boolean; virtual;
    // Default Class Css
    class function DefaultClassCss: string; virtual;
    // Default Render Text
    class function DefaultRenderText: Boolean; virtual;
  private
    FId           : string;
    FPropChanges  : TNvPropChangeList;
    FOnExit       : TNotifyEvent;
    FOnEnter      : TNotifyEvent;
    FOnClick      : TNotifyEvent;
    FTagHtml      : string;
    FImageListLink: TNVImageListLink;
    FClassCss     : string;
    FRenderIndex  : Integer;
    FModalResult  : TModalResult;
    FInModal      : Boolean;
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
    procedure SetClassCss(const Value: string);
    procedure SetRenderIndex(const Value: Integer);
    procedure SetModalResult(const Value: TModalResult);
    procedure SetRenderText(const Value: Boolean);
  protected
    FRendered           : Boolean;
    FUpdatingFromRequest: Boolean;
    // FRenderInvisible    : Boolean;
    FRenderPosition : Boolean;
    FControlsOrdered: TObjectListEx;
    FRenderText     : Boolean;
    FSubPropName    : string;
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetName(const Value: TComponentName); override;
    procedure Loaded; override;
    // // Avoid Interface errors in TNVSessionApp.ControlByID after control Released
    // function _AddRef: Integer; stdcall;
    // function _Release: Integer; stdcall;
    //
    function GetText: TCaption; virtual;
    procedure SetText(const Value: TCaption); virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure SetParent(AParent: TWinControl); override;
    function IsCaptionStored: Boolean; virtual;
    function IsRenderIndexStored: Boolean;
    function IsTagHtmlStored: Boolean;
    function IsNotDefaultClassCss: Boolean;
    function IsTextVisibleStored: Boolean;
    function IsSubComponent: Boolean; inline;
    //
    procedure DoPendingChangesChange(Sender: TObject; const Item: string;
      Action: TCollectionNotification);
    // Occurs after component dropped in IDE Designer
    procedure AfterDesignDrop; virtual;
    procedure RenameSubComponents(NewName: string); virtual;
    //
    procedure AddIncludes; virtual;
    function WebClassType: string; virtual;
    function ControlAjaxJson: TJsonObject;
    function GetID: string;
    function NeedSendChange: Boolean; inline;
    procedure EnqueueChange(const aName: string; const aProc: TPropChangeProc); inline;
    procedure DequeueChange(const aName: string); inline;
    // Render props
    procedure InternalRender(Json: TJsonObject); virtual;
    procedure RenderTag(aJson: TJsonObject); dynamic;
    procedure RenderParent(aJson: TJsonObject); dynamic;
    procedure RenderPos(aJson: TJsonObject); dynamic;
    procedure RenderTxt(aJson: TJsonObject); dynamic;
    procedure RenderVisible(aJson: TJsonObject); dynamic;
    procedure RenderEnabled(aJson: TJsonObject); dynamic;
    procedure RenderImage(aJson: TJsonObject); dynamic;
    procedure RenderClassCss(aJson: TJsonObject); dynamic;
    procedure RenderEvents(aJson: TJsonObject); virtual;
    procedure RenderRenderIndex(aJson: TJsonObject); dynamic;
    //
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SortControls; virtual;
    function ShowModal: Integer; virtual;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property CaptionVisible: Boolean read FRenderText write SetRenderText
      stored IsTextVisibleStored;
    property ClassCss: string read FClassCss write SetClassCss stored IsNotDefaultClassCss;
    property ImageListLink: TNVImageListLink read FImageListLink write SetImageListLink;
    property Text: TCaption read GetText write SetText;
    property TextVisible: Boolean read FRenderText write SetRenderText stored IsTextVisibleStored;
    property InModal: Boolean read FInModal;
    property TagHtml: string read FTagHtml write SetTagHtml stored IsTagHtmlStored;
    // events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; virtual;
    procedure DoClick(aEvent: TJsonObject);
    procedure DoEnter(aEvent: TJsonObject);
    procedure DoExit(aEvent: TJsonObject);
    procedure DoCloseModal(aEvent: TJsonObject); virtual;
    procedure Click; override;
    property ModalResult: TModalResult read FModalResult write SetModalResult;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    property OnEnter: TNotifyEvent read FOnEnter write SetOnEnter;
    property OnExit: TNotifyEvent read FOnExit write SetOnExit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Render; virtual;
    procedure Invalidate; override;
    procedure ProcessRequest(J: TJsonObject); virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function Rendered: Boolean;
    procedure RenderChanges;
    procedure ReRender(Now: Boolean = True); virtual;
    procedure SetSubComponent(IsSubComponent: Boolean; PropName: string); reintroduce;
    procedure AddClassCss(aClass: string);
    procedure RemoveClassCss(aClass: string);
    property ID: string read GetID;
  published
    property Left stored FRenderPosition;
    property Top stored FRenderPosition;
    property Width stored FRenderPosition;
    property Height stored FRenderPosition;
    property RenderIndex: Integer read FRenderIndex write SetRenderIndex stored IsRenderIndexStored;
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
    procedure ResetDesignPage;
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
    FPropChanges: TNvPropChangeList;
  protected
    FControl : INVRenderableComponent;
    FPropName: string;
    FPrefix  : string;
    FSuffix  : string;
    procedure DoPendingChangesChange(Sender: TObject; const Item: string;
      Action: TCollectionNotification);

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
    function ControlAjaxJson: TJsonObject; virtual;
    procedure RemoveControlAjaxJson; virtual;
    function NeedSendChange: Boolean; inline;
    procedure EnqueueChange(const aName: string; const aProc: TPropChangeProc); inline;
    procedure DequeueChange(const aName: string); inline;
    function Rendered: Boolean; virtual;
    procedure ReRender(Now: Boolean = True); virtual;
    procedure Invalidate; virtual;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); virtual;
    destructor Destroy; override;
    procedure Render;
    procedure RenderChanges(aJson: TJsonObject);
  end;

  // Get an control from ID
function ControlByID(aId: string): INvControl;

implementation

uses
  NV.Utils, NV.VCL.Page, TypInfo, StrUtils, NV.VCL.Forms,
  System.Generics.Collections, NV.VCL.ActnList;

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

function RenderOrderSort(AItem1: Pointer; AItem2: Pointer): integer;
var
  LTop1, LLeft1, LTop2, LLeft2, LIdx1, LIdx2: integer;
begin
  if TComponent(AItem1) is TNvControl then
    LIdx1 := TNvControl(AItem1).RenderIndex
  else if TComponent(AItem1) is TNvWinControl then
    LIdx1 := TNvWinControl(AItem1).RenderIndex
  else
    LIdx1 := -1;

  if TComponent(AItem2) is TNvControl then
    LIdx2 := TNvControl(AItem2).RenderIndex
  else if TComponent(AItem2) is TNvWinControl then
    LIdx2 := TNvWinControl(AItem2).RenderIndex
  else
    LIdx2 := -1;

  Result := LIdx1 - LIdx2;

  if Result = 0 then
    begin
      if TComponent(AItem1) is TControl then
        begin
          LTop1  := TControl(AItem1).Top;
          LLeft1 := TControl(AItem1).Left;
          LIdx1  := TControl(AItem1).ComponentIndex;
        end
      else
        begin
          LTop1  := -1;
          LLeft1 := -1;
          LIdx1  := -1;
        end;
      if TComponent(AItem2) is TControl then
        begin
          LTop2  := TControl(AItem2).Top;
          LLeft2 := TControl(AItem2).Left;
          LIdx2  := TControl(AItem2).ComponentIndex;
        end
      else
        begin
          LTop2  := -1;
          LLeft2 := -1;
          LIdx2  := -1;
        end;

      Result := LTop1 - LTop2;
      if Abs(Result) < 3 then
        Result := LLeft1 - LLeft2;

      if Result = 0 then
        Result := LIdx1 - LIdx2;
    end;

end;

{ TNvControl }

procedure TNvControl.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TNvCustomAction then
    with TNvCustomAction(Sender) do
      begin
        if not CheckDefaults or (Self.Caption = '') or (Self.Caption = Self.Name) then
          Self.Caption := Caption;
        if not CheckDefaults or (Self.Enabled = True) then
          Self.Enabled := Enabled;
        if not CheckDefaults or (Self.Hint = '') then
          Self.Hint := Hint;
        if not CheckDefaults or (Self.Visible = True) then
          Self.Visible := Visible;
        if not CheckDefaults or (@Self.OnClick = nil) then
          Self.OnClick := OnExecute;
        if not CheckDefaults or (Self.ImageListLink <> nil) and (not Self.ImageListLink.IsValidImage)
        then
          Self.ImageListLink := ImageListLink;
      end;
end;

procedure TNvControl.AddClassCss(aClass: string);
begin
  ClassCss := AddWords(FClassCss, aClass);
end;

procedure TNvControl.AddIncludes;
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
end;

procedure TNvControl.AfterDesignDrop;
begin
  //
end;

function TNvControl.ClassType: string;
begin
  Result := ClassName;
end;

procedure TNvControl.CMEnabledChanged(var Message: TMessage);
begin
  EnqueueChange('Enabled', RenderEnabled);
  Invalidate;
end;

procedure TNvControl.CMVisibleChanged(var Message: TMessage);
begin
  // if NeedSendChange then
  // begin
  // ControlAjaxJson.B['Visible'] := (Message.WParam = ord(True));
  // Invalidate;
  // end
  // else if (Not FRenderInvisible and Not FRendered and (Message.WParam = ord(True))) then

  EnqueueChange('Visible', RenderVisible);
  Invalidate;
end;

function TNvControl.ControlAjaxJson: TJsonObject;
var
  _Owner: INvControl;
begin
  if IsSubComponent then
    begin
      if csSubComponent in Owner.ComponentStyle then
        Result := (Owner as INvControl).ControlAjaxJson.O[FSubPropName]
      else
        begin
          _Owner := (Owner as INvControl);
          Result := Screen.Ajax.GetControlJson(_Owner.Id, not _Owner.Rendered).O[FSubPropName];
        end;
    end
  else
    Result := Screen.Ajax.GetControlJson(FId);
end;

constructor TNvControl.Create(AOwner: TComponent);
begin
  inherited;
  FPropChanges             := TNvPropChangeList.Create;
  FPropChanges.OnKeyNotify := DoPendingChangesChange;
  FTagHtml                 := DefaultHtmlTag;
  GetID;

  FRenderPosition := True;
  FRenderText     := DefaultRenderText;
  FRendered       := False;
  if SupportImage then
    FImageListLink := TNVImageListLink.Create(Self);
  { To Change Image Json prop name
    FImageListLink.ImagePropName:= 'Image2' }
  FClassCss := DefaultClassCss;
end;

class function TNvControl.DefaultClassCss: string;
begin
  Result := '';
end;

class function TNvControl.DefaultHtmlTag: string;
begin
  Result := 'div';
end;

class function TNvControl.DefaultRenderText: Boolean;
begin
  Result := False;
end;

procedure TNvControl.DefineProperties(Filer: TFiler);
begin
  // inherited; customize;
  if FRenderPosition then
    inherited;
end;

procedure TNvControl.DequeueChange(const aName: string);
begin
  FPropChanges.Remove(aName);
end;

destructor TNvControl.Destroy;
begin
  if not FId.IsEmpty then
    begin
      if (Not Application.Terminated) and (Screen.Ajax <> nil) then
        begin
          Screen.Ajax.AddDestruction(FId);
          FRendered := True;
          FPropChanges.Clear;
          if Not IsSubComponent then
            Invalidate;
        end;

      _RemoveControl(Self);
    end;

  if SupportImage then
    FImageListLink.Free;

  FPropChanges.Free;
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

procedure TNvControl.DoPendingChangesChange(Sender: TObject; const Item: string;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded: Screen.Ajax.ChangeList.Add(Self);
    cnRemoved:
      if FPropChanges.Count = 0 then
        Screen.Ajax.ChangeList.Remove(Self);
  end;
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

procedure TNvControl.InternalRender(Json: TJsonObject);
begin
  //
end;

procedure TNvControl.Invalidate;
begin
  if csDesigning in ComponentState then
    inherited;

  if IsSubComponent and Not FRendered and (Owner as INvControl).Rendered then
    Render
  else if FRendered and (FPropChanges.Count > 0) then
    Screen.Ajax.Invalidate;
end;

function TNvControl.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not TControlActionLinkHack(ActionLink).IsCaptionLinked;
end;

function TNvControl.IsNotDefaultClassCss: Boolean;
begin
  Result := FClassCss <> DefaultClassCss;
end;

function TNvControl.IsRenderIndexStored: Boolean;
begin
  Result := (FRenderIndex <> 0) and not IsSubComponent;
end;

function TNvControl.IsSubComponent: Boolean;
begin
  Result := csSubComponent in FComponentStyle;
end;

function TNvControl.IsTagHtmlStored: Boolean;
begin
  Result := FTagHtml <> DefaultHtmlTag;
end;

function TNvControl.IsTextVisibleStored: Boolean;
begin
  Result := FRenderText <> DefaultRenderText;
end;

procedure TNvControl.Loaded;
begin
  inherited;
  if not FRendered and (Parent <> nil) and (Parent is TNvWinControl) { designer } and
    (Parent as TNvWinControl).Rendered then
    Render;
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

procedure TNvControl.EnqueueChange(const aName: string; const aProc: TPropChangeProc);
begin
  if NeedSendChange and not FPropChanges.ContainsKey(aName) then
    FPropChanges.Add(aName, aProc);
end;

procedure TNvControl.RemoveClassCss(aClass: string);
begin
  ClassCss := RemoveWords(FClassCss, aClass);
end;

procedure TNvControl.RenameSubComponents(NewName: string);
var
  I         : Integer;
  _Component: TComponent;
begin
  for I := 0 to ComponentCount - 1 do
    begin
      _Component := Components[I];
      if (csSubComponent in _Component.ComponentStyle) then
        begin
          if (_Component is TNvControl) and not TNvControl(_Component).FSubPropName.IsEmpty then
            _Component.Name := NewName + '_' + TNvControl(_Component).FSubPropName
          else if (_Component is TNvWinControl) and not TNvWinControl(_Component).FSubPropName.IsEmpty
          then
            _Component.Name := NewName + '_' + TNvWinControl(_Component).FSubPropName
        end;
    end;
end;

procedure TNvControl.Render;
var
  I     : Integer;
  _Json : TJsonObject;
  _Comp : TComponent;
  _Owner: INvControl;
begin
  if not FRendered and not GetID.IsEmpty then
    begin
      AddIncludes;
      if IsSubComponent then
        begin
          if csSubComponent in Owner.ComponentStyle then
            _Json := (Owner as INvControl).ControlAjaxJson.O[FSubPropName]
          else
            begin
              _Owner := (Owner as INvControl);
              _Json  := Screen.Ajax.GetControlJson(_Owner.Id, not _Owner.Rendered).O[FSubPropName];
            end;
        end
      else
        _Json := Screen.Ajax.GetControlJson(FId, True);
      with _Json do
        begin
          S['New'] := WebClassType;
          S['Id']  := FId;
          if DebugHook <> 0 then
            S['DName'] := Name;
          if FTagHtml <> DefaultHtmlTag then
            RenderTag(_Json);
          if FRenderIndex > 0 then
            RenderRenderIndex(_Json);
          { if Parent is TNvPage then
            S['Parent'] := 'NVRoot'
            else } if (Parent <> nil) and not IsSubComponent then
            RenderParent(_Json);
          if FRenderPosition then
            RenderPos(_Json);
          if FRenderText and (Text <> '') then
            RenderTxt(_Json);
          if Not Visible then
            RenderVisible(_Json);
          if Not Enabled then
            RenderEnabled(_Json);
          if SupportImage and FImageListLink.IsVisible then
            RenderImage(_Json);
          if IsNotDefaultClassCss then
            RenderClassCss(_Json);
          RenderEvents(_Json);
        end;

      InternalRender(_Json);

      // SubComponents (manter antes de setar FRendered)
      for I := 0 to ComponentCount - 1 do
        begin
          _Comp := Components[I];

          if csSubComponent in _Comp.ComponentStyle then
            if _Comp.InheritsFrom(TNvControl) then
              TNvControl(_Comp).Render
            else if _Comp is TNvWinControl then
              TNvWinControl(_Comp).Render
        end;

      FRendered := True;
    end;

end;

procedure TNvControl.RenderChanges;
var
  _Json   : TJsonObject;
  I       : Integer;
  _Changes: TArray<TPropChangeProc>;
  _Props  : TArray<string>;
begin
  _Json    := ControlAjaxJson;
  _Changes := FPropChanges.Values.ToArray;
  _Props   := FPropChanges.Keys.ToArray;
  for I    := 0 to Length(_Changes) - 1 do
    begin
      { call } _Changes[I](_Json);
      FPropChanges.Remove(_Props[I]);
    end;

  // FPropChanges.Clear;
  if FPropChanges.Count = 0 then
    Screen.Ajax.ChangeList.Remove(Self);
end;

procedure TNvControl.RenderClassCss(aJson: TJsonObject);
begin
  aJson.S['ClassCss'] := FClassCss;
end;

function TNvControl.Rendered: Boolean;
begin
  Result := FRendered;
end;

procedure TNvControl.RenderEnabled(aJson: TJsonObject);
begin
  aJson.B['Enabled'] := Enabled;
end;

procedure TNvControl.RenderEvents(aJson: TJsonObject);
begin
  if Assigned(FOnClick) then
    aJson.A['Events'].Add('click');
  if Assigned(FOnEnter) then
    aJson.A['Events'].Add('focus');
  if Assigned(FOnExit) then
    aJson.A['Events'].Add('blur');
end;

procedure TNvControl.RenderImage(aJson: TJsonObject);
begin
  aJson.S[FImageListLink.ImagePropName] := FImageListLink.Render;
end;

procedure TNvControl.RenderParent(aJson: TJsonObject);
begin
  if Parent = nil then
    ControlAjaxJson.S['Parent'] := ''
  else
    ControlAjaxJson.S['Parent'] := (Parent as TNvWinControl).ID;
end;

procedure TNvControl.RenderPos(aJson: TJsonObject);
begin
  aJson.I['Top']    := Top;
  aJson.I['Left']   := Left;
  aJson.I['Width']  := Width;
  aJson.I['Height'] := Height;
end;

procedure TNvControl.RenderRenderIndex(aJson: TJsonObject);
begin
  aJson.I['RenderIndex'] := FRenderIndex;
end;

procedure TNvControl.RenderTag(aJson: TJsonObject);
begin
  aJson.S['Tag'] := FTagHtml;
end;

procedure TNvControl.RenderTxt(aJson: TJsonObject);
begin
  aJson.S['Text'] := Text;
end;

procedure TNvControl.RenderVisible(aJson: TJsonObject);
begin
  if not(csDesigning in ComponentState) or (csNoDesignVisible in ControlStyle) then
    aJson.B['Visible'] := Visible;
end;

procedure TNvControl.ReRender(Now: Boolean = True);
var
  I    : Integer;
  _Comp: TComponent;
begin
  FRendered := False;

  // SubComponents
  for I := 0 to ComponentCount - 1 do
    begin
      _Comp := Components[I];

      if csSubComponent in _Comp.ComponentStyle then
        if _Comp.InheritsFrom(TNvControl) then
          TNvControl(_Comp).ReRender(False)
        else if _Comp is TNvWinControl then
          TNvWinControl(_Comp).ReRender(False);
    end;

  if Now then
    Render;
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
  if _Changed and FRenderPosition { and NeedSendChange } then
    begin
      // _Json := ControlAjaxJson;
      // if _Json <> nil then
      EnqueueChange('Position', RenderPos);
      Invalidate;
    end;
end;

procedure TNvControl.SetClassCss(const Value: string);
begin
  if Value <> FClassCss then
    begin
      EnqueueChange('ClassCss', RenderClassCss);
      FClassCss := Value;
      Invalidate;
    end;
end;

procedure TNvControl.SetImageListLink(const Value: TNVImageListLink);
begin
  if FImageListLink <> Value then
    begin
      EnqueueChange('Image', RenderImage);
      FImageListLink.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvControl.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and (Value <> Name) then
    RenameSubComponents(Value);
  inherited;
end;

procedure TNvControl.SetOnClick(const Value: TNotifyEvent);
begin
  EnqueueChange('Events', RenderEvents);
  FOnClick := Value;
  Invalidate;
end;

procedure TNvControl.SetOnEnter(const Value: TNotifyEvent);
begin
  EnqueueChange('Events', RenderEvents);
  FOnEnter := Value;
  Invalidate;
end;

procedure TNvControl.SetOnExit(const Value: TNotifyEvent);
begin
  EnqueueChange('Events', RenderEvents);
  FOnExit := Value;
  Invalidate;
end;

procedure TNvControl.SetParent(AParent: TWinControl);
begin
  if (csDesigning in ComponentState)     //
    and not(csLoading in ComponentState) //
    and (Parent = nil)                   //
    and (AParent <> nil) then
    begin
      inherited;
      AfterDesignDrop;
    end
  else
    inherited;

  if csDestroying in ComponentState then
    Exit;

  if FRendered then
    begin
      EnqueueChange('Parent', RenderParent);
      Invalidate;
    end
  else if Assigned(AParent)                 //
    and (AParent as TNvWinControl).Rendered //
    and not(csLoading in ComponentState) then
    { Re } Render;

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

procedure TNvControl.SetRenderIndex(const Value: Integer);
begin
  if Value <> FRenderIndex then
    begin
      // FRenderIndex := Value;
      //
      // if not(csLoading in ComponentState) and (Parent <> nil) and (Parent is TNvWinControl) then
      // with TNvWinControl(Parent) do
      // begin
      // if Rendered then
      // ReRender(Self.Rendered);
      // end;

      EnqueueChange('RenderIndex', RenderRenderIndex);
      FRenderIndex := Value;
      Invalidate;
    end;
end;

procedure TNvControl.SetRenderText(const Value: Boolean);
begin
  if Value <> FRenderText then
    begin
      if Value then
        EnqueueChange('Text', RenderTxt);

      FRenderText := Value;
      Invalidate;
    end;
end;

procedure TNvControl.SetSubComponent(IsSubComponent: Boolean; PropName: string);
begin
  inherited SetSubComponent(IsSubComponent);
  FSubPropName := PropName;
  if IsSubComponent and (Owner <> nil) and not(csLoading in ComponentState) then
    Name := Owner.Name + '_' + PropName;
end;

procedure TNvControl.SetTagHtml(const Value: string);
begin
  if Value <> FTagHtml then
    begin
      EnqueueChange('Tag', RenderTag);
      FTagHtml := Value;
      Invalidate;
    end;
end;

procedure TNvControl.SetText(const Value: TCaption);
begin
  if Value <> Text then
    begin
      if FRenderText then
        EnqueueChange('Text', RenderTxt);
      inherited Text := Value;
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

procedure TNvWinControl.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TNvCustomAction then
    with TNvCustomAction(Sender) do
      begin
        if not CheckDefaults or (Self.Caption = '') or (Self.Caption = Self.Name) then
          Self.Caption := Caption;
        if not CheckDefaults or (Self.Enabled = True) then
          Self.Enabled := Enabled;
        if not CheckDefaults or (Self.Hint = '') then
          Self.Hint := Hint;
        if not CheckDefaults or (Self.Visible = True) then
          Self.Visible := Visible;
        if not CheckDefaults or (@Self.OnClick = nil) then
          Self.OnClick := OnExecute;

        if not CheckDefaults or (Self.ImageListLink <> nil) and (not Self.ImageListLink.IsValidImage)
        then
          Self.ImageListLink := ImageListLink;

      end;
end;

procedure TNvWinControl.AddClassCss(aClass: string);
begin
  ClassCss := AddWords(FClassCss, aClass);
end;

procedure TNvWinControl.AddIncludes;
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
end;

procedure TNvWinControl.AfterDesignDrop;
begin
  //
end;

procedure TNvWinControl.Click;
begin
  inherited;
  { TODO -oDelcio -cEventos : Ver se poderiamos usar os eventos de TControl, sem ter interferências da VCL }
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TNvWinControl.CMControlListChange(var Message: TMessage);
begin
  if Boolean(Message.LParam) = True then
    begin
      FControlsOrdered.Add(Pointer(Message.WParam));
      if (TObject(Message.WParam) is TNvControl) then
        begin
          with TNvControl(Message.WParam) do
            if (FRenderIndex = 0) //
              or ((csDesigning in Self.ComponentState) and not(csLoading in Self.ComponentState))
            then
              FRenderIndex := FControlsOrdered.Count;
        end
      else if (TObject(Message.WParam) is TNvWinControl) then
        begin
          with TNvWinControl(Message.WParam) do
            if (FRenderIndex = 0) //
              or ((csDesigning in Self.ComponentState) and not(csLoading in Self.ComponentState))
            then
              FRenderIndex := FControlsOrdered.Count;
        end;
    end
  else
    FControlsOrdered.Remove(Pointer(message.WParam));
end;

procedure TNvWinControl.CMEnabledChanged(var Message: TMessage);
begin
  EnqueueChange('Enabled', RenderEnabled);
  Invalidate;
end;

procedure TNvWinControl.CMVisibleChanged(var Message: TMessage);
begin
  EnqueueChange('Visible', RenderVisible);
  Invalidate;
end;

procedure TNvWinControl.CnCtlColorStatic(var Msg: TWMCtlColorStatic);
begin
  SetBKMode(Msg.ChildDC, TRANSPARENT);
  Msg.Result := GetStockObject(NULL_BRUSH);
end;

function TNvWinControl.ControlAjaxJson: TJsonObject;
var
  _Owner: INvControl;
begin
  if IsSubComponent then
    begin
      if csSubComponent in Owner.ComponentStyle then
        Result := (Owner as INvControl).ControlAjaxJson.O[FSubPropName]
      else
        begin
          _Owner := (Owner as INvControl);
          Result := Screen.Ajax.GetControlJson(_Owner.Id, not _Owner.Rendered).O[FSubPropName];
        end;
    end
  else
    Result := Screen.Ajax.GetControlJson(FId);
end;

constructor TNvWinControl.Create(AOwner: TComponent);
begin
  inherited;
  FPropChanges                 := TNvPropChangeList.Create;
  FPropChanges.OnKeyNotify     := DoPendingChangesChange;
  FControlsOrdered             := TObjectListEx.Create;
  FControlsOrdered.OwnsObjects := False;
  FTagHtml                     := DefaultHtmlTag;
  GetID;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csDoubleClicks,
    csParentBackground, csPannable, csGestures];

  FRenderText     := DefaultRenderText;
  FRenderPosition := True;
  FRendered       := False;

  if SupportImage then
    FImageListLink := TNVImageListLink.Create(Self);
  { To Change Image Json prop name
    FImageListLink.ImagePropName:= 'Image2' }
  FClassCss := DefaultClassCss;
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

class function TNvWinControl.DefaultClassCss: string;
begin
  Result := '';
end;

class function TNvWinControl.DefaultHtmlTag: string;
begin
  Result := 'div';
end;

class function TNvWinControl.DefaultRenderText: Boolean;
begin
  Result := False;
end;

procedure TNvWinControl.DefineProperties(Filer: TFiler);
begin
  if FRenderPosition then
    inherited;
end;

procedure TNvWinControl.DequeueChange(const aName: string);
begin
  FPropChanges.Remove(aName);
end;

destructor TNvWinControl.Destroy;
  procedure DestroyControls(aControl: TControl);
  var
    I         : Integer;
    _Container: TNvWinControl;
  begin
    if aControl is TNvControl then
      Screen.Ajax.AddDestruction(TNvControl(aControl).FId)
    else if aControl is TNvWinControl then
      begin
        _Container := TNvWinControl(aControl);

        for I := 0 to _Container.ControlCount - 1 do
          if _Container.Controls[I] is TWinControl then
            DestroyControls(TWinControl(_Container.Controls[I]))
          else if _Container.Controls[I] is TNvControl then
            Screen.Ajax.AddDestruction(TNvControl(_Container.Controls[I]).FId);

        Screen.Ajax.AddDestruction(_Container.FId);
      end;
  end;

begin
  if not FId.IsEmpty then
    begin
      if (not Application.Terminated) and (Screen.Ajax <> nil) then
        begin

          DestroyControls(Self);
          FRendered := True;
          FPropChanges.Clear;
          if Not IsSubComponent then
            Invalidate;
        end;

      _RemoveControl(Self);
    end;
  FControlsOrdered.Free;

  if SupportImage then
    FImageListLink.Free;

  FPropChanges.Free;
  inherited;
end;

procedure TNvWinControl.DoClick(aEvent: TJsonObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TNvWinControl.DoCloseModal(aEvent: TJsonObject);
begin
  if FInModal then
    ModalResult := aEvent.I['ModalResult'];
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

procedure TNvWinControl.DoPendingChangesChange(Sender: TObject; const Item: string;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded: Screen.Ajax.ChangeList.Add(Self);
    cnRemoved:
      if FPropChanges.Count = 0 then
        Screen.Ajax.ChangeList.Remove(Self);
  end;
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

procedure TNvWinControl.InternalRender(Json: TJsonObject);
begin
  //
end;

procedure TNvWinControl.Invalidate;
begin
  if csDesigning in ComponentState then
    inherited;

  if IsSubComponent and Not FRendered and (Owner as INvControl).Rendered then
    Render
  else if FRendered and (FPropChanges.Count > 0) then
    Screen.Ajax.Invalidate;
end;

function TNvWinControl.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not TControlActionLinkHack(ActionLink).IsCaptionLinked;
end;

function TNvWinControl.IsNotDefaultClassCss: Boolean;
begin
  Result := FClassCss <> DefaultClassCss;
end;

function TNvWinControl.IsRenderIndexStored: Boolean;
begin
  Result := (FRenderIndex <> 0) and not IsSubComponent
end;

function TNvWinControl.IsSubComponent: Boolean;
begin
  Result := csSubComponent in FComponentStyle;
end;

function TNvWinControl.IsTagHtmlStored: Boolean;
begin
  Result := FTagHtml <> DefaultHtmlTag;
end;

function TNvWinControl.IsTextVisibleStored: Boolean;
begin
  Result := FRenderText <> DefaultRenderText;
end;

procedure TNvWinControl.Loaded;
begin
  inherited;
  if not FRendered and (Parent <> nil) and (Parent is TNvWinControl) { designer } and
    TNvWinControl(Parent).Rendered then
    Render;
end;

function TNvWinControl.NeedSendChange: Boolean;
begin
  Result := Not Application.Terminated //
    and FRendered                      //
    and not FUpdatingFromRequest;
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
  case IndexStr(AEventName, ['click', 'focus', 'blur', 'close-modal']) of
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
    3:
      begin
        DoCloseModal(aEvent);
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

procedure TNvWinControl.EnqueueChange(const aName: string; const aProc: TPropChangeProc);
begin
  if NeedSendChange and not FPropChanges.ContainsKey(aName) then
    FPropChanges.Add(aName, aProc);
end;

procedure TNvWinControl.RemoveClassCss(aClass: string);
begin
  ClassCss := RemoveWords(FClassCss, aClass);
end;

procedure TNvWinControl.RenameSubComponents(NewName: string);
var
  I         : Integer;
  _Component: TComponent;
begin
  for I := 0 to ComponentCount - 1 do
    begin
      _Component := Components[I];
      if (csSubComponent in _Component.ComponentStyle) then
        begin
          if (_Component is TNvControl) and not TNvControl(_Component).FSubPropName.IsEmpty then
            _Component.Name := NewName + '_' + TNvControl(_Component).FSubPropName
          else if (_Component is TNvWinControl) and not TNvWinControl(_Component).FSubPropName.IsEmpty
          then
            _Component.Name := NewName + '_' + TNvWinControl(_Component).FSubPropName
        end;
    end;
end;

procedure TNvWinControl.Render;
var
  I       : Integer;
  _Json   : TJsonObject;
  _Comp   : TComponent;
  _Control: TObject;
  _Owner  : INvControl;
begin
  SortControls;
  if not FRendered and not GetID.IsEmpty then
    begin
      AddIncludes;
      if IsSubComponent then
        begin
          if csSubComponent in Owner.ComponentStyle then
            _Json := (Owner as INvControl).ControlAjaxJson.O[FSubPropName]
          else
            begin
              _Owner := (Owner as INvControl);
              _Json  := Screen.Ajax.GetControlJson(_Owner.Id, not _Owner.Rendered).O[FSubPropName];
            end;
        end
      else
        _Json := Screen.Ajax.GetControlJson(FId, True);
      with _Json do
        begin
          S['New'] := WebClassType;
          S['Id']  := FId;
          if DebugHook <> 0 then
            S['DName'] := Name;
          if FTagHtml <> DefaultHtmlTag then
            RenderTag(_Json);
          if FRenderIndex > 0 then
            RenderRenderIndex(_Json);
          if (Parent <> nil) and (Parent is TNvWinControl) and not IsSubComponent then
            RenderParent(_Json);
          if FRenderPosition then
            RenderPos(_Json);
          if FRenderText and (Text <> '') then
            RenderTxt(_Json);
          if Not Visible then
            RenderVisible(_Json);
          if Not Enabled then
            RenderEnabled(_Json);
          if SupportImage and FImageListLink.IsVisible then
            RenderImage(_Json);
          if IsNotDefaultClassCss then
            RenderClassCss(_Json);
          RenderEvents(_Json);
        end;

      InternalRender(_Json);

      // SubComponents (manter antes de setar FRendered)
      for I := 0 to ComponentCount - 1 do
        begin
          _Comp := Components[I];

          if csSubComponent in _Comp.ComponentStyle then
            if _Comp.InheritsFrom(TNvControl) then
              TNvControl(_Comp).Render
            else if _Comp is TNvWinControl then
              TNvWinControl(_Comp).Render
        end;

      if FInModal then
        Screen.Ajax.AddCallFunction(Id, 'ShowModal', '');

      FRendered := True;
    end;

  // child controls
  for I := 0 to FControlsOrdered.Count - 1 do
    begin
      { TODO -oDelcio -cSpeedUp : Test if is fast use Interface INVcontrol in FControlsOrdered }
      _Control := FControlsOrdered[I];
      if _Control is TNvControl then
        TNvControl(_Control).Render
      else if _Control is TNvWinControl then
        TNvWinControl(_Control).Render
    end;

end;

procedure TNvWinControl.RenderChanges;
var
  _Json   : TJsonObject;
  I       : Integer;
  _Changes: TArray<TPropChangeProc>;
  _Props  : TArray<string>;
begin
  _Json    := ControlAjaxJson;
  _Changes := FPropChanges.Values.ToArray;
  _Props   := FPropChanges.Keys.ToArray;
  for I    := 0 to Length(_Changes) - 1 do
    begin
      { call } _Changes[I](_Json);
      FPropChanges.Remove(_Props[I]);
    end;

  // FPropChanges.Clear;
  if FPropChanges.Count = 0 then
    Screen.Ajax.ChangeList.Remove(Self);
end;

procedure TNvWinControl.RenderClassCss(aJson: TJsonObject);
begin
  aJson.S['ClassCss'] := FClassCss;
end;

function TNvWinControl.Rendered: Boolean;
begin
  Result := FRendered;
end;

procedure TNvWinControl.RenderEnabled(aJson: TJsonObject);
begin
  aJson.B['Enabled'] := Enabled;
end;

procedure TNvWinControl.RenderEvents(aJson: TJsonObject);
begin
  if Assigned(FOnClick) then
    aJson.A['Events'].Add('click');
  if Assigned(FOnEnter) then
    aJson.A['Events'].Add('focus');
  if Assigned(FOnExit) then
    aJson.A['Events'].Add('blur');
end;

procedure TNvWinControl.RenderImage(aJson: TJsonObject);
begin
  aJson.S[FImageListLink.ImagePropName] := FImageListLink.Render;
end;

procedure TNvWinControl.RenderParent(aJson: TJsonObject);
begin
  if Parent = nil then
    ControlAjaxJson.S['Parent'] := ''
  else
    ControlAjaxJson.S['Parent'] := (Parent as TNvWinControl).ID;
end;

procedure TNvWinControl.RenderPos(aJson: TJsonObject);
begin
  aJson.I['Top']    := Top;
  aJson.I['Left']   := Left;
  aJson.I['Width']  := Width;
  aJson.I['Height'] := Height;
end;

procedure TNvWinControl.RenderRenderIndex(aJson: TJsonObject);
begin
  aJson.I['RenderIndex'] := FRenderIndex;
end;

procedure TNvWinControl.RenderTag(aJson: TJsonObject);
begin
  aJson.S['Tag'] := FTagHtml;
end;

procedure TNvWinControl.RenderTxt(aJson: TJsonObject);
begin
  aJson.S['Text'] := Text;
end;

procedure TNvWinControl.RenderVisible(aJson: TJsonObject);
begin
  if not(csDesigning in ComponentState) or (csNoDesignVisible in ControlStyle) then
    aJson.B['Visible'] := Visible;
end;

procedure TNvWinControl.ReRender(Now: Boolean);
var
  I    : Integer;
  _Comp: TComponent;
begin
  FRendered := False;

  // SubComponents
  for I := 0 to ComponentCount - 1 do
    begin
      _Comp := Components[I];

      if csSubComponent in _Comp.ComponentStyle then
        if _Comp.InheritsFrom(TNvControl) then
          TNvControl(_Comp).ReRender(False)
        else if _Comp is TNvWinControl then
          TNvWinControl(_Comp).ReRender(False);
    end;

  SortControls;

  for I := 0 to FControlsOrdered.Count - 1 do
    begin
      { TODO -oDelcio -cSpeedUp : Test if is fast use Interface INVcontrol in FControlsOrdered }
      if FControlsOrdered[I] is TNvControl then
        TNvControl(FControlsOrdered[I]).ReRender(False)
      else if FControlsOrdered[I] is TNvWinControl then
        TNvWinControl(FControlsOrdered[I]).ReRender(False);
    end;

  if Now then
    Render;
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
  if _Changed and FRenderPosition { and NeedSendChange } then
    begin
      // _Json := ControlAjaxJson;
      // if _Json <> nil then
      EnqueueChange('Position', RenderPos);
      Invalidate;
    end;
end;

procedure TNvWinControl.SetClassCss(const Value: string);
begin
  if Value <> FClassCss then
    begin
      EnqueueChange('ClassCss', RenderClassCss);
      FClassCss := Value;
      Invalidate;
    end;
end;

procedure TNvWinControl.SetImageListLink(const Value: TNVImageListLink);
begin
  if (FImageListLink <> Value) then
    begin
      EnqueueChange('Image', RenderImage);
      FImageListLink.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvWinControl.SetModalResult(const Value: TModalResult);
begin
  if Value <> FModalResult then
    begin
      if FInModal and (Value <> 0) then
        Application.ModalFinished;
      FModalResult := Value;
    end;
end;

procedure TNvWinControl.SetName(const Value: TComponentName);
begin
  if (csDesigning in ComponentState) and (Value <> Name) then
    RenameSubComponents(Value);
  inherited;
end;

procedure TNvWinControl.SetOnClick(const Value: TNotifyEvent);
begin
  EnqueueChange('Events', RenderEvents);
  FOnClick := Value;
  Invalidate;
end;

procedure TNvWinControl.SetOnEnter(const Value: TNotifyEvent);
begin
  EnqueueChange('Events', RenderEvents);
  FOnEnter := Value;
  Invalidate;
end;

procedure TNvWinControl.SetOnExit(const Value: TNotifyEvent);
begin
  EnqueueChange('Events', RenderEvents);
  FOnExit := Value;
  Invalidate;
end;

procedure TNvWinControl.SetParent(AParent: TWinControl);
begin
  if (csDesigning in ComponentState)     //
    and not(csLoading in ComponentState) //
    and (Parent = nil)                   //
    and (AParent <> nil) then
    begin
      inherited;
      AfterDesignDrop;
    end
  else
    inherited;

  if (csDestroying in ComponentState) //
    or ((AParent <> nil) and not(AParent is TNvWinControl)) then
    Exit;

  if FRendered then
    begin
      EnqueueChange('Parent', RenderParent);
      Invalidate;
    end
  else if Assigned(AParent)                 //
    and (AParent as TNvWinControl).Rendered //
    and not(csLoading in ComponentState) then
    { Re } Render;
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

procedure TNvWinControl.SetRenderIndex(const Value: Integer);
begin
  if Value <> FRenderIndex then
    begin
      // FRenderIndex := Value;
      //
      // if not(csLoading in ComponentState) and (Parent <> nil) and (Parent is TNvWinControl) then
      // with TNvWinControl(Parent) do
      // begin
      // if Rendered then
      // ReRender(Self.Rendered);
      // end;
      EnqueueChange('RenderIndex', RenderRenderIndex);
      FRenderIndex := Value;
      Invalidate;

    end;
end;

procedure TNvWinControl.SetRenderText(const Value: Boolean);
begin
  if Value <> FRenderText then
    begin
      if Value then
        EnqueueChange('Text', RenderTxt);
      FRenderText := Value;
      Invalidate;
    end;
end;

procedure TNvWinControl.SetSubComponent(IsSubComponent: Boolean; PropName: string);
begin
  inherited SetSubComponent(IsSubComponent);
  FSubPropName := PropName;
  if IsSubComponent and (Owner <> nil) and not(csLoading in ComponentState) then
    Name := Owner.Name + '_' + PropName;
end;

procedure TNvWinControl.SetTagHtml(const Value: string);
begin
  if Value <> FTagHtml then
    begin
      EnqueueChange('Tag', RenderTag);
      FTagHtml := Value;
      Invalidate;
    end;
end;

procedure TNvWinControl.SetText(const Value: TCaption);
begin
  if Value <> Text then
    begin
      if FRenderText then
        EnqueueChange('Text', RenderTxt);
      inherited Text := Value;
      Invalidate;
    end;
end;

function TNvWinControl.ShowModal: Integer;
begin
  FModalResult := 0;
  FInModal     := True;
  Show;
  Screen.Page.AddModal(Self);
  // Parent = nil or not Parent.Rendered
  if not FRendered then
    Render
  else
    Screen.Ajax.AddCallFunction(Id, 'ShowModal', '');
  Invalidate;
  Application.ModalStarted;
  // Continue After SetModalResult <> 0
  FInModal := False;
  Screen.Page.RemoveModal(Self);
  // Hide; //???
  Screen.Ajax.AddCallFunction(Id, 'CloseModal', '');
  Result := FModalResult;
end;

procedure TNvWinControl.SortControls;
begin
  FControlsOrdered.Sort(RenderOrderSort);
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
  FControl                 := aMaster;
  FPropName                := aPropName;
  FPrefix                  := aPrefix;
  FSuffix                  := aSuffix;
  FPropChanges             := TNvPropChangeList.Create;
  FPropChanges.OnKeyNotify := DoPendingChangesChange;
end;

procedure TNvSubProperty.DequeueChange(const aName: string);
begin
  FPropChanges.Remove(aName);
end;

destructor TNvSubProperty.Destroy;
begin
  FPropChanges.Free;
  inherited;
end;

procedure TNvSubProperty.DoPendingChangesChange(Sender: TObject; const Item: string;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded: FControl.EnqueueChange(FPrefix + FPropName + FSuffix, RenderChanges);
    cnRemoved:
      if FPropChanges.Count = 0 then
        FControl.DequeueChange(FPrefix + FPropName + FSuffix);
  end;
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

function TNvSubProperty.NeedSendChange: Boolean;
begin
  Result := FControl.NeedSendChange;
end;

function TNvSubProperty.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE
end;

procedure TNvSubProperty.EnqueueChange(const aName: string; const aProc: TPropChangeProc);
begin
  if NeedSendChange and not FPropChanges.ContainsKey(aName) then
    FPropChanges.Add(aName, aProc);
end;

procedure TNvSubProperty.RemoveControlAjaxJson;
var
  _I: Integer;
begin
  if Assigned(FControl) then
    begin
      _I := FControl.ControlAjaxJson.IndexOf(FPropName);
      if _I > -1 then
        FControl.ControlAjaxJson.Delete(_I);
    end;
end;

procedure TNvSubProperty.Render;
var
  _Json: TJsonObject;
begin
  if Assigned(FControl) then
    begin
      _Json := ControlAjaxJson;
      InternalRender(_Json);
      if _Json.Count = 0 then
        RemoveControlAjaxJson;
    end;
end;

procedure TNvSubProperty.RenderChanges(aJson: TJsonObject);
var
  _Json   : TJsonObject;
  I       : Integer;
  _Changes: TArray<TPropChangeProc>;
begin
  // ignore Received aJson parameter;
  _Json    := ControlAjaxJson;
  _Changes := FPropChanges.Values.ToArray;
  for I    := 0 to Length(_Changes) - 1 do
    { call } _Changes[I](_Json);

  FPropChanges.Clear;
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
  NV.VCL.Forms.Application := TNvApplication.Create(nil);
  NV.VCL.Forms.Screen      := TNvScreen.Create(nil);
end;

procedure DoneControls;
begin
  if Assigned(NV.VCL.Forms.Application) then
    NV.VCL.Forms.Application.Free;
  FreeAndNil(NV.VCL.Forms.Screen);
  ControlsList.Free;
end;

initialization

InitControls;

finalization

DoneControls;

end.
