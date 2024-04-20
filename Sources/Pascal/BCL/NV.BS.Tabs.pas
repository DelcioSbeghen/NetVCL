unit NV.BS.Tabs;

interface

uses
  Winapi.Messages, Classes, Generics.Collections, NV.BS.Navbar, NV.BS.Controls, NV.BS.Containers,
  NV.JSON, NV.Ajax, NV.Types,
  Vcl.Controls, NV.Vcl.Frame;

type

  TNvBsTabControl = class;
  TNvBsTab        = class;
  TNVCloseAction  = NV.Types.TNVCloseAction;

  TNvBsTabs = class(TNvBsNav)
  protected
    class function DefaultClassCss: string; override;
  end;

  TNvBsTabHeaderLink = class(TBsNavItemLinkBase)
  protected
    class function DefaultClassCss: string; override;
  private
    FShowClose: Boolean;
    procedure SetShowClose(const Value: Boolean);
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
  protected
    procedure InternalRender(JSON: TJsonObject); override;
    procedure RenderShowClose(aJson: TJsonObject); dynamic;
    // events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
    procedure DoClose(aEvent: TJsonObject);
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Tab: TNvBsTab; inline;
  published
    property Link;
    property ShowClose: Boolean read FShowClose write SetShowClose default False;
    property Visible;
  end;

  TNvBsTab = class(TNvBsWinControl)
  protected
    class function DefaultClassCss: string; override;
  private
    FLastActiveTab: TNvBsTab;
    FTabHeader    : TNvBsTabHeaderLink;
    FTabControl   : TNvBsTabControl;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure SetTabControl(const Value: TNvBsTabControl);
  protected
    procedure InternalRender(JSON: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Show;
  published
    property TabHeader : TNvBsTabHeaderLink read FTabHeader;
    property TabControl: TNvBsTabControl read FTabControl write SetTabControl;
    property ClassCss;
  end;


  // TNvBsTabDropdown = class(TBsNavItemDropdown)
  // ClassCss  'nav-item dropdown'
  // private
  // FTabPane: TNvBsTabPane;
  // procedure SetTabPane(const Value: TNvBsTabPane);
  // published
  // property TabPane: TNvBsTabPane read FTabPane write SetTabPane;
  // end;

  TNvBsTabContent = class(TNvBsWinControl)
  protected
    class function DefaultClassCss: string; override;
  private
  public
    property ClassCss;
  end;

  TNvBsTabCloseEvent = procedure(Sender: TObject; Tab: TNvBsTab; var CloseAction: TNVCloseAction)
    of object;

  TNvBsTabControl = class(TNvBsGridContainer)
  private
    FActiveTab            : TNvBsTab;
    FDefaultTabCloseAction: TNVCloseAction;
    FOnTabClose           : TNvBsTabCloseEvent;
    FTabs                 : TNvBsTabs;
    FContent              : TNvBsTabContent;
  protected
    procedure SetActiveTab(const Value: TNvBsTab); virtual;
    procedure DoTabClose(Tab: TNvBsTab; var CloseAction: TNVCloseAction); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    //
    function AddNewTab: TNvBsTab;
    // function AddNewTabDropdown: TNvBsTabDropdown;
  published
    property ActiveTab: TNvBsTab read FActiveTab write SetActiveTab;
    property ClassCss;
    property Content: TNvBsTabContent read FContent;
    property Position;
    property Tabs: TNvBsTabs read FTabs;
    property Width_;
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
  NV.Utils, NV.BS.HtmlControls, NV.Controls, System.SysUtils, Winapi.Windows, NV.VCL.Forms;

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
  Result            := TNvBsTab.Create(Self.Owner);
  Result.TabControl := Self;
end;

constructor TNvBsTabControl.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultTabCloseAction := caHide;
  FTabs                  := TNvBsTabs.Create(Self);
  FTabs.SetSubComponent(True, 'Tabs');
  FContent := TNvBsTabContent.Create(Self);
  FContent.SetSubComponent(True, 'Content');
end;

procedure TNvBsTabControl.DoTabClose(Tab: TNvBsTab; var CloseAction: TNVCloseAction);
begin
  if Assigned(FOnTabClose) then
    FOnTabClose(Self, Tab, CloseAction);
end;

procedure TNvBsTabControl.SetActiveTab(const Value: TNvBsTab);
var
  _Tab: TNvBsTab;
begin
  if FActiveTab <> Value then
    begin

      if Not(csLoading in ComponentState) //
        and Assigned(Value) and not(csDestroying in Value.ComponentState) then
        begin
          Value.FLastActiveTab := FActiveTab;
          Value.Show;
        end;

      FActiveTab := Value;
    end;
end;

{ TNvBsTabHeaderLink }

procedure TNvBsTabHeaderLink.Click;
begin
  inherited;
  if (Tab <> nil) then
    Tab.Show;
end;

procedure TNvBsTabHeaderLink.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  if (csDesigning in ComponentState) then
    Message.Result := HTCLIENT;
end;

constructor TNvBsTabHeaderLink.Create(AOwner: TComponent);
begin
  inherited;
  // FRenderInvisible := True; // Navigation not working if tabHeader invisible not rendered
end;

class function TNvBsTabHeaderLink.DefaultClassCss: string;
begin
  Result := 'nav-link';
end;

procedure TNvBsTabHeaderLink.DoClose(aEvent: TJsonObject);
var
  _Action: TNVCloseAction;
begin
  if Assigned(Tab.TabControl) then
    begin
      _Action := Tab.TabControl.FDefaultTabCloseAction;
      Tab.TabControl.DoTabClose(Self.Tab, _Action);
    end;

  case _Action of
    caHide:
      begin
        Visible                  := False;
        Tab.Visible              := False;
        Tab.TabControl.ActiveTab := Tab.FLastActiveTab;
      end;
    caFree:
      begin
        Tab.TabControl.ActiveTab := Tab.FLastActiveTab;
        Tab.Free;
      end;
  end;
end;

procedure TNvBsTabHeaderLink.InternalRender(JSON: TJsonObject);
begin
  inherited;
  JSON.A['Events'].Add('close');
  if FShowClose then
    RenderShowClose(JSON);
  Link.HRef := '#' + Tab.ID;

  if (Tab.TabControl <> nil) and (Tab.TabControl.ActiveTab = Tab) then
    Screen.Ajax.AddCallFunction(ID, 'Show', '');
end;

function TNvBsTabHeaderLink.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  Result := inherited;
  if AEventName = 'close' then
    begin
      DoClose(aEvent);
      Result := True;
    end;
end;

procedure TNvBsTabHeaderLink.RenderShowClose(aJson: TJsonObject);
begin
  aJson.B['ShowClose'] := FShowClose
end;

procedure TNvBsTabHeaderLink.SetShowClose(const Value: Boolean);
begin
  if Value <> FShowClose then
    begin
      EnqueueChange('ShowClose', RenderShowClose);
      FShowClose := Value;
      Invalidate;
    end;
end;

function TNvBsTabHeaderLink.Tab: TNvBsTab;
begin
  Result := Owner as TNvBsTab;
end;

// { TNvBsTabDropdown }
{ TNvBsTabContent }

class function TNvBsTabContent.DefaultClassCss: string;
begin
  Result := 'tab-content';
end;

{ TNvBsTabs }

procedure TNvBsTab.SetTabControl(const Value: TNvBsTabControl);
begin
  if Value <> FTabControl then
    begin
      FTabControl := Value;
      Parent      := Value;
    end;
end;

procedure TNvBsTab.Show;
begin
  if NeedSendChange then
    Screen.Ajax.AddCallFunction(TabHeader.ID, 'Show', '');
  Invalidate;
end;

{ TNvBsTabs }

class function TNvBsTabs.DefaultClassCss: string;
begin
  Result := 'nav-tabs';
end;

{ TNvBsTab }

procedure TNvBsTab.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) and (Message.WParam = ord(True)) then
    SetZOrder(True);
end;

constructor TNvBsTab.Create(AOwner: TComponent);
begin
  inherited;
  FTabHeader := TNvBsTabHeaderLink.Create(Self);
  FTabHeader.SetSubComponent(True, 'TabHeader');
end;

class function TNvBsTab.DefaultClassCss: string;
begin
  Result := 'tab-pane';
end;

procedure TNvBsTab.InternalRender(JSON: TJsonObject);
begin
  inherited;

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
  _Page  := FindChildControl('Tab' + Name) as TNvBsTab;
  if Assigned(_Page) and (_Page.ControlCount > 0) and (_Page.Controls[0] is TNVBaseFrame) then
    Result := TNVBaseFrame(_Page.Controls[0]);
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
  _Tab := FindChildControl('Tab' + _LName) as TNvBsTab;
  if Assigned(_Tab) then
    begin
      // activate the _Tab
      ActiveTab := _Tab;
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
      _Tab.TabHeader.Link.Text := _Frame.Caption
    else
      _Tab.TabHeader.Link.Text := _Frame.Name;

    if _Frame.ImageListLink.IsValidImage then
      with _Tab.TabHeader.Link.ImageListLink do
        begin
          Height     := _Frame.ImageListLink.Height;
          Width      := _Frame.ImageListLink.Width;
          Images     := _Frame.ImageListLink.Images;
          ImageIndex := _Frame.ImageListLink.ImageIndex;
        end;

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
      if (Page.ControlCount = 1) //
        and (Page.Controls[0] is TNVBaseFrame) then
        Frame := TNVBaseFrame(Page.Controls[0])
      else
        Frame := nil;

      if Assigned(Frame) and Assigned(Frame.OnActivate) then
        Frame.OnActivate(Frame);
    end;
end;

function TNvBsTDI.TabByFrame(aFrame: TNVBaseFrame): TNvBsTab;
begin
  Result := FindChildControl('Tab' + aFrame.Name) as TNvBsTab;
end;

function TNvBsTDI.WebClassType: string;
begin
  Result := ClassParent.ClassName;
end;

end.
