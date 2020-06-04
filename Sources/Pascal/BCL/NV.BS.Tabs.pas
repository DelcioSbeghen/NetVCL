unit NV.BS.Tabs;

interface

uses
  Winapi.Messages, Classes, Generics.Collections, NV.BS.Navbar, NV.BS.Controls, NV.BS.Containers,
  NV.JSON, NV.Ajax, NV.Types,
  Vcl.Controls, NV.Vcl.Frame;

type

  TNvBsTabControl = class;
  TNvBsTab        = class;

  TNvBsTabs = class(TNvBsNav)

  end;

  TNvBsTabHeaderLink = class(TBsNavItemLink)
  private
    FTab       : TNvBsTab;
    FShowClose : Boolean;
    FTabControl: TNvBsTabControl;
    procedure SetTab(const Value: TNvBsTab);
    procedure SetShowClose(const Value: Boolean);
    procedure SetHrefFromTab(const Value: TNvBsTab);
  protected
    procedure InternalRender(Ajax: TNvAjax; JSON: TJsonObject); override;
    // events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
    procedure DoClose(aEvent: TJsonObject);
  public
    constructor CreateEx(aTbControl: TNvBsTabControl; aTab: TNvBsTab);
    procedure Render(Ajax: TNvAjax); override;
  published
    property Tab       : TNvBsTab read FTab write SetTab;
    property TabControl: TNvBsTabControl read FTabControl write FTabControl;
    property ShowClose : Boolean read FShowClose write SetShowClose default False;
    property TextProps;
    property Text;
  end;

  TNvBsTab = class(TNvBsWinControl)
  private
    FTabHeader: TNvBsTabHeaderLink;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure SetTabHeader(const Value: TNvBsTabHeaderLink);
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor CreateEx(aTbControl: TNvBsTabControl);
    procedure Show;
  published
    property TabHeader: TNvBsTabHeaderLink read FTabHeader write SetTabHeader;
  end;


  // TNvBsTabDropdown = class(TBsNavItemDropdown)
  // private
  // FTabPane: TNvBsTabPane;
  // procedure SetTabPane(const Value: TNvBsTabPane);
  // published
  // property TabPane: TNvBsTabPane read FTabPane write SetTabPane;
  // end;

  TNvBsTabContent = class(TNvBsWinControl)
  private
    FTabs: TList<TNvBsTab>;
    procedure InsertTab(aTab: TNvBsTab);
    procedure RemoveTab(aTab: TNvBsTab);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;
  end;

  TNvBsTabCloseEvent = procedure(Sender: TObject; Tab: TNvBsTab; var CloseAction: TNVCloseAction)
    of object;

  TNvBsTabControl = class(TNvBsGridContainer)
  private
    FActiveTab            : TNvBsTab;
    FLastActiveTab        : TNvBsTab;
    FDefaultTabCloseAction: TNVCloseAction;
    FOnTabClose           : TNvBsTabCloseEvent;
    function GetContent: TNvBsTabContent;
    function GetTabs: TNvBsTabs;
  protected
    procedure SetActiveTab(const Value: TNvBsTab); virtual;
    procedure DoTabClose(Tab: TNvBsTab; var CloseAction: TNVCloseAction); virtual;
    // Tabs before Content
    procedure SortControls; override;
  public
    constructor Create(AOwner: TComponent); override;
    //
    function AddNewTab: TNvBsTab;
    // function AddNewTabDropdown: TNvBsTabDropdown;
  published
    property ActiveTab : TNvBsTab read FActiveTab write SetActiveTab;
    property Tabs      : TNvBsTabs read GetTabs;          // write SetBrand;
    property Content   : TNvBsTabContent read GetContent; // write SetContent;
    property OnTabClose: TNvBsTabCloseEvent read FOnTabClose write FOnTabClose;
  end;

  TNvBsTDIBeforeNewEvent = procedure(var FrameClass: TNvFrameClass; var CanOpen: Boolean) of object;
  TNvBsTDIAfterNewEvent = procedure(FrameCreated: TNvFrameClass) of object;

  TNvBsTDI = class(TNvBsTabControl)
  private
    FOnBeforeNewTab: TNvBsTDIBeforeNewEvent;
    FOnAfterNewTab : TNvBsTDIAfterNewEvent;
  protected
    function WebClassType: string; override;
    procedure SetActiveTab(const Value: TNvBsTab); override;
    procedure DoTabClose(Tab: TNvBsTab; var CloseAction: TNVCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    function NewTab(AFrameClass: TNvFrameClass; Name: string = ''): TNVBaseFrame;
    function FrameByName(Name: string): TNVBaseFrame;
    function TabByFrame(aFrame: TNVBaseFrame): TNvBsTab;
  published
    property OnBeforeNewTab: TNvBsTDIBeforeNewEvent read FOnBeforeNewTab write FOnBeforeNewTab;
    property OnAfterNewTab : TNvBsTDIAfterNewEvent read FOnAfterNewTab write FOnAfterNewTab;
  end;

implementation

uses
  NV.Utils, NV.BS.HtmlControls, NV.Controls, System.SysUtils;

{ TNvBsTabControl }

// function TNvBsTabControl.AddNewTabDropdown: TNvBsTabDropdown;
// var
// _TabPane : TNvBsTabPane;
// _Dropdown: TNvBsDropdownMenu;
// begin
// _TabPane        := TNvBsTabPane.Create(Owner);
// _TabPane.Parent := Content;
//
// _Dropdown := TNvBsDropdownMenu.Create(Owner);
//
// TBsNavBarItemDropdown.Create(Owner, _Dropdown);
//
// Result         := TNvBsTabDropdown.Create(Owner, _Dropdown);
// Result.TabPane := _TabPane;
// Result.Parent  := Tabs;
// end;

function TNvBsTabControl.AddNewTab: TNvBsTab;
begin
  Result := TNvBsTab.CreateEx(Self);
end;

constructor TNvBsTabControl.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultTabCloseAction := caHide;
end;

procedure TNvBsTabControl.DoTabClose(Tab: TNvBsTab; var CloseAction: TNVCloseAction);
begin
  if Assigned(FOnTabClose) then
    FOnTabClose(Self, Tab, CloseAction);
end;

function TNvBsTabControl.GetContent: TNvBsTabContent;
var
  I: Integer;
begin
  Result := nil;
  if (csLoading in Owner.ComponentState) or (csLoading in ComponentState) or
    (csRecreating in ControlState) then
    Exit;

  for I := 0 to ControlCount - 1 do
    begin
      if Controls[I] is TNvBsTabContent then
        begin
          Result := TNvBsTabContent(Controls[I]);
          Break;
        end;
    end;
  if Result = nil then
    begin
      UpdateRecreatingFlag(True);
      try
        Result        := TNvBsTabContent.Create(Owner);
        Result.Parent := Self;
      finally
        UpdateRecreatingFlag(False);
      end;
    end;

end;

function TNvBsTabControl.GetTabs: TNvBsTabs;
var
  I: Integer;
begin
  Result := nil;
  if (csLoading in Owner.ComponentState) or (csLoading in ComponentState) or
    (csRecreating in ControlState) then
    Exit;

  for I := 0 to ControlCount - 1 do
    begin
      if Controls[I] is TNvBsTabs then
        begin
          Result := TNvBsTabs(Controls[I]);
          Break;
        end;
    end;
  if Result = nil then
    begin
      UpdateRecreatingFlag(True);
      try
        Result        := TNvBsTabs.Create(Owner);
        Result.Parent := Self;
      finally
        UpdateRecreatingFlag(False);
      end;
    end;

end;

procedure TNvBsTabControl.SetActiveTab(const Value: TNvBsTab);
var
  _Tab: TNvBsTab;
begin
  if FActiveTab <> Value then
    begin
      Value.Show;
      // for _Tab in Content.FTabs do
      // _Tab.Visible := (_Tab = Value);
      FActiveTab := Value;
    end;
end;

procedure TNvBsTabControl.SortControls;
var
  I: Integer;
begin
  inherited;
  // Move Tabs to first render element and Content to secound
  for I := 0 to FControlsOrdered.Count - 1 do
    begin
      if FControlsOrdered[I] is TNvBsTabs then
        FControlsOrdered.Move(I, 0)
      else if FControlsOrdered[I] is TNvBsTabContent then
        FControlsOrdered.Move(I, 1);
    end;
end;

{ TNvBsTabLink }

// constructor TNvBsTabLink.Create(AOwner: TComponent);
// begin
// inherited Create(AOwner);
// // FTabPane := aTabPane;
// end;

constructor TNvBsTabHeaderLink.CreateEx(aTbControl: TNvBsTabControl; aTab: TNvBsTab);
begin
  inherited Create(aTbControl.Owner);
  FTabControl := aTbControl;
  Parent      := aTbControl.Tabs;
  Tab         := aTab;
end;

procedure TNvBsTabHeaderLink.DoClose(aEvent: TJsonObject);
var
  _Action: TNVCloseAction;
begin
  if Assigned(FTabControl) then
    begin
      _Action := FTabControl.FDefaultTabCloseAction;
      FTabControl.DoTabClose(Self.Tab, _Action);
    end;

  case _Action of
    caHide:
      begin
        Visible     := False;
        Tab.Visible := False;
      end;
    caFree:
      begin
        Tab.Free;
        Free;
      end;
  end;
end;

procedure TNvBsTabHeaderLink.InternalRender(Ajax: TNvAjax; JSON: TJsonObject);
begin
  inherited;
  JSON.A['Events'].Add('close');
  if FShowClose then
    JSON.B['ShowClose'] := FShowClose;
end;

function TNvBsTabHeaderLink.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  inherited;
  if AEventName = 'close' then
    begin
      DoClose(aEvent);
      Result := True;
    end;
end;

procedure TNvBsTabHeaderLink.Render(Ajax: TNvAjax);
begin
  if csDesigning in ComponentState then
    SetHrefFromTab(FTab);
  inherited;
end;

procedure TNvBsTabHeaderLink.SetHrefFromTab(const Value: TNvBsTab);
begin
  if Value = nil then
    HRef := '#'
  else
    HRef := '#' + Value.ID;
end;

procedure TNvBsTabHeaderLink.SetShowClose(const Value: Boolean);
begin
  if Value <> FShowClose then
    begin
      if NeedSendChange then
        ControlAjaxJson.B['ShowClose'] := Value;
      FShowClose                       := Value;
      Invalidate;
    end;
end;

procedure TNvBsTabHeaderLink.SetTab(const Value: TNvBsTab);
begin
  if FTab <> Value then
    begin
      SetHrefFromTab(Value);
      FTab := Value;
    end;
end;

// { TNvBsTabDropdown }
//
// procedure TNvBsTabDropdown.SetTabPane(const Value: TNvBsTabPane);
// begin
// FTabPane := Value;
// end;

{ TNvBsTabContent }

constructor TNvBsTabContent.Create(AOwner: TComponent);
begin
  inherited;
  FTabs := TList<TNvBsTab>.Create;
end;

destructor TNvBsTabContent.Destroy;
begin
  FTabs.Destroy;
  inherited;
end;

procedure TNvBsTabContent.InsertTab(aTab: TNvBsTab);
begin
  FTabs.Add(aTab);
end;

procedure TNvBsTabContent.RemoveTab(aTab: TNvBsTab);
begin
  FTabs.Remove(aTab);
end;

{ TNvBsTabs }

procedure TNvBsTab.SetParent(AParent: TWinControl);
begin
  if AParent <> Parent then
    begin
      if (Parent <> nil) and (Parent is TNvBsTabContent) then
        TNvBsTabContent(Parent).RemoveTab(Self);
      inherited;
      if (AParent <> nil) and (AParent is TNvBsTabContent) then
        TNvBsTabContent(AParent).InsertTab(Self);
    end
  else
    inherited;
end;

procedure TNvBsTab.SetTabHeader(const Value: TNvBsTabHeaderLink);
begin
  FTabHeader := Value;
end;

procedure TNvBsTab.Show;
begin
  if NeedSendChange then
    Ajax.AddCallFunction(TabHeader.ID, 'Show', '');
end;

{ TNvBsTab }

procedure TNvBsTab.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) and (Message.WParam = ord(True)) then
    SetZOrder(True);
end;

constructor TNvBsTab.CreateEx(aTbControl: TNvBsTabControl);
begin
  inherited Create(aTbControl.Owner);
  Parent     := aTbControl.Content;
  FTabHeader := TNvBsTabHeaderLink.CreateEx(aTbControl, Self);
end;

{ TNvBsTDI }

constructor TNvBsTDI.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultTabCloseAction := caFree;

end;

procedure TNvBsTDI.DoTabClose(Tab: TNvBsTab; var CloseAction: TNVCloseAction);
var
  Frame: TNVBaseFrame;
begin
  Frame := nil;
  try
    if Assigned(Tab) then
      if Tab.ControlCount > 0 then
        if Assigned(Tab.Controls[0]) then
          if Tab.Controls[0] IS TNVBaseFrame then
            Frame := TNVBaseFrame(Tab.Controls[0]);
  except
    on e: Exception do
      begin
        raise Exception.Create('Exception in: TDWTDI.DoInternalTabClose. Exception: ' + e.Message);
      end;
  end;

  if Assigned(Frame) and Assigned(Frame.OnClose) then
    Frame.OnClose(Frame, CloseAction);

  if CloseAction <> caNone then
    inherited DoTabClose(Tab, CloseAction);
end;

function TNvBsTDI.FrameByName(Name: string): TNVBaseFrame;
var
  _Page: TNvBsTab;
begin
  Result := nil;
  if Assigned(Content.FindChildControl('Tab' + Name)) then
    begin
      _Page  := TNvBsTab(Content.FindChildControl('Tab' + Name));
      Result := TNVBaseFrame(_Page.Controls[0]);
    end;
end;

function TNvBsTDI.NewTab(AFrameClass: TNvFrameClass; Name: string): TNVBaseFrame;
var
  _Tab    : TNvBsTab;
  _Frame  : TNVBaseFrame;
  _CanOpen: Boolean;
  _LName  : string;
begin
  _Frame   := nil;
  _CanOpen := True;
  if Assigned(FOnBeforeNewTab) then
    FOnBeforeNewTab(AFrameClass, _CanOpen);

  // Check before open
  if not Assigned(AFrameClass) or (not _CanOpen) then
    Exit;

  // Get Name  - if Name not set, remove "T" from ClassName
  _LName := IIf(Name = '', Copy(AFrameClass.ClassName, 2, AFrameClass.ClassName.Length), Name);

  // check if _Tab is already open
  if Assigned(Content.FindChildControl('Tab' + _LName)) then
    begin
      // activate the _Tab
      ActiveTab := TNvBsTab(Content.FindChildControl('Tab' + _LName));
      _Frame    := (ActiveTab.Controls[0] as AFrameClass);
      // ShowFrame
      _Frame.Show;
      Result := _Frame;
      Exit;
    end;

  try
    // Create the _Tab
    _Tab                     := AddNewTab;
    _Tab.Name                := 'Tab' + _LName;
    _Tab.TabHeader.ShowClose := True;

    // Create the _Frame;
    _Frame := AFrameClass.Create(Owner);
    if Name = '' then
      begin
        if _Frame.Name = '' then
          _Frame.Name := { 'Frame' + } _LName;
      end
    else
      _Frame.Name := _LName;
    _Frame.Parent := _Tab;
    _Frame.Align  := alClient;

    _Frame.Show;

    ActiveTab := _Tab;

    // Set _Tab Caption
    if _Frame.Caption <> '' then
      _Tab.TabHeader.Caption := _Frame.Caption
    else
      _Tab.TabHeader.Caption := _Frame.Name;
  finally
    Result := _Frame;
  end;

end;

procedure TNvBsTDI.SetActiveTab(const Value: TNvBsTab);
var
  Page : TNvBsTab;
  Frame: TNVBaseFrame;
begin
  inherited;
  Page := ActiveTab;
  if Assigned(Page) then
    begin
      try
        Frame := TNVBaseFrame(Page.Controls[0]);
      except
        Frame := nil;
      end;
      if Assigned(Frame) and Assigned(Frame.OnActivate) then
        Frame.OnActivate(Frame);
    end;
end;

function TNvBsTDI.TabByFrame(aFrame: TNVBaseFrame): TNvBsTab;
begin
  Result := nil;
  if Assigned(Content.FindChildControl('Tab' + aFrame.Name)) then
    begin
      Result := TNvBsTab(Content.FindChildControl('Tab' + aFrame.Name));
    end;
end;

function TNvBsTDI.WebClassType: string;
begin
  Result := ClassParent.ClassName;
end;

end.
