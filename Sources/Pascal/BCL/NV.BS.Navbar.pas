unit NV.BS.Navbar;

interface

uses
  Controls, NV.BS.Containers, NV.BS.Types, NV.Ajax, NV.JSON, System.Classes,
  NV.BS.HtmlControls, NV.BS.Buttons;

type

  TNvBsNav = class(TNvBsGridContainer)
  protected
    class function DefaultHtmlTag: string; override;
    class function DefaultClassCss: string; override;
  private
    FUnderline: Boolean;
    FVertical : Boolean;
    procedure SetUnderline(const Value: Boolean);
    procedure SetVertical(const Value: Boolean);
  protected
    procedure AfterDesignDrop; override;
    procedure InternalRender(JSON: TJsonObject); override;
    procedure RenderUnderline(aJson: TJsonObject);
    procedure RenderVertical(aJson: TJsonObject);
  published
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Grids;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property TextVisible;
    property TagHtml;
    property Underline: Boolean read FUnderline write SetUnderline default False;
    property Vertical : Boolean read FVertical write SetVertical default False;
    property Width_;
  end;

  TBsNavItem = class(TNVBsContainer)
  protected
    class function DefaultHtmlTag: string; override;
    class function SupportImage: Boolean; override;
    class function DefaultClassCss: string; override;
  public
  published

    property ClassCss;

    property TagHtml;
  end;

  TNvBsNavLink = class(TNvBsLink)
  protected
    class function DefaultHtmlTag: string; override;
    class function DefaultClassCss: string; override;
  end;

  TBsNavItemLinkBase = class(TBsNavItem)
  protected
  private

  protected
    FLink: TNvBsNavLink;
  public
    constructor Create(AOwner: TComponent); override;
    property Link: TNvBsNavLink read FLink;
  published

  end;

  TBsNavItemLink = class(TBsNavItemLinkBase)
  published
    property Link;
  end;

  TBsNavItemDropdown = class(TBsNavItemLink)
  protected
    class function DefaultClassCss: string; override;
  private
    FDropDown: TNvBsDropdownMenu;
  public
    constructor Create(AOwner: TComponent; aDropDown: TNvBsDropdownMenu); reintroduce; virtual;
  published
    property DropDown: TNvBsDropdownMenu read FDropDown;
  end;

  TBsNavItemCollapse = class(TBsNavItemLink)
  end;

  TBsNavBarBrand = class(TNVBsContainer)
  protected
    class function DefaultHtmlTag: string; override;
    class function SupportImage: Boolean; override;
    class function DefaultClassCss: string; override;
    class function DefaultRenderText: Boolean; override;
  private
    FHRef: string;
    procedure SetHRef(const Value: string);
  protected
    procedure InternalRender(JSON: TJsonObject); override;
    procedure RenderHRef(aJson: TJsonObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property HRef: string read FHRef write SetHRef;
    property ClassCss;
    property ImageListLink;
    property Text;
    property TextVisible;
  end;

  TBsNavBarToggler = class(TNvBsButton)
  protected
    class function DefaultDataToggle: TNvBsDataToggle; override;
    class function DefaultClassCss: string; override;
  end;

  TBsNavBarItemLink = class(TBsNavItemLink)
  protected
    class function DefaultHtmlTag: string; override;
  end;

  TBsNavBarItemDropdown = class(TBsNavItemDropdown)
  protected
    class function DefaultHtmlTag: string; override;
  end;

  TBsNavBarNav = class(TNvBsNav)
  protected
    class function DefaultHtmlTag: string; override;
    class function DefaultClassCss: string; override;
  end;

  TBsNavBarContent = class(TNVBsContainer)
  protected
    class function DefaultClassCss: string; override;
  private
    function GetNav: TBsNavBarNav;
  published
    property Nav: TBsNavBarNav read GetNav;
    property ClassCss;
  end;

  TBsNavBarContainer = class(TNVBsContainer)
  protected
    class function DefaultClassCss: string; override;
  private
    // function GetNav: TBsNavBarNav;
  published
    // property Nav: TBsNavBarNav read GetNav;
    property ClassCss;
  end;

  TNvBsNavBar = class(TNVBsContainer)
  protected
    class function DefaultHtmlTag: string; override;
    class function DefaultClassCss: string; override;
  private
    FExpand: TBsSize;
    // FBrand  : TBsNavBarBrand;
    // FContent: TBsNavBarContent;
    // FToggler: TBsNavBarToggler;
    procedure SetExpand(const Value: TBsSize);
    // procedure SetBrand(const Value: TBsNavBarBrand);
    // procedure SetContent(const Value: TBsNavBarContent);
    function GetBrand: TBsNavBarBrand;
    function GetContent: TBsNavBarContent;
    function GetToggler: TBsNavBarToggler;
    function GetContainer: TBsNavBarContainer;
  protected
    procedure InternalRender(JSON: TJsonObject); override;
    procedure RenderExpand(aJson: TJsonObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Container: TBsNavBarContainer read GetContainer;
  published
    property Expand : TBsSize read FExpand write SetExpand default bssSM;
    property Brand  : TBsNavBarBrand read GetBrand;     // write SetBrand;
    property Content: TBsNavBarContent read GetContent; // write SetContent;
    property Toggler: TBsNavBarToggler read GetToggler;
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width_;
  end;

  TNvBsSideBar = class(TNVBsContainer)
  protected
    class function DefaultClassCss: string; override;
  private
    FExpand    : TBsSize;
    FFull      : TBsSize;
    FHoverItems: Boolean;
    // FBrand  : TBsNavBarBrand;
    // FContent: TBsNavBarContent;
    // FToggler: TBsNavBarToggler;
    procedure SetExpand(const Value: TBsSize);
    procedure SetFull(const Value: TBsSize);
    procedure SetHoverItems(const Value: Boolean);
    // procedure SetBrand(const Value: TBsNavBarBrand);
    // procedure SetContent(const Value: TBsNavBarContent);
    // function GetBrand: TBsNavBarBrand;
    // function GetContent: TBsNavBarContent;
    // function GetToggler: TBsNavBarToggler;
    // function GetContainer: TBsNavBarContainer;
  protected
    procedure AfterDesignDrop; override;
    procedure InternalRender(JSON: TJsonObject); override;
    procedure RenderExpand(aJson: TJsonObject);
    procedure RenderFull(aJson: TJsonObject);
    procedure RenderHoverItems(aJson: TJsonObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // property Container: TBsNavBarContainer read GetContainer;
  published
    // expand on hover after this size
    property Expand: TBsSize read FExpand write SetExpand default bssSM;
    // full width after this size
    property Full      : TBsSize read FFull write SetFull default bssXL;
    property HoverItems: Boolean read FHoverItems write SetHoverItems default false;
    // property Brand  : TBsNavBarBrand read GetBrand;     // write SetBrand;
    // property Content: TBsNavBarContent read GetContent; // write SetContent;
    // property Toggler: TBsNavBarToggler read GetToggler;
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width_;
  end;

implementation

uses
  SysUtils, NV.Controls;

{ TNvBsNavBar }

constructor TNvBsNavBar.Create(AOwner: TComponent);
begin
  inherited;
  FExpand := bssSM;
  // FBrand   := TBsNavBarBrand.Create(Self);
  // FContent := TBsNavBarContent.Create(Self);
end;

class function TNvBsNavBar.DefaultClassCss: string;
begin
  Result := 'navbar';
end;

class function TNvBsNavBar.DefaultHtmlTag: string;
begin
  Result := 'nav';
end;

destructor TNvBsNavBar.Destroy;
begin
  // FBrand.Free;
  // FContent.Free;
  inherited;
end;

function TNvBsNavBar.GetBrand: TBsNavBarBrand;
var
  I         : integer;
  _Container: TBsNavBarContainer;
begin
  Result := nil;
  if (csLoading in Owner.ComponentState) or (csLoading in ComponentState) or
    (csRecreating in ControlState) then
    Exit;

  _Container := Container; // Optimization;

  for I := 0 to _Container.ControlCount - 1 do
    begin
      if _Container.Controls[I] is TBsNavBarBrand then
        begin
          Result := TBsNavBarBrand(_Container.Controls[I]);
          Break;
        end;
    end;
  if Result = nil then
    begin
      UpdateRecreatingFlag(True);
      try
        Result := TBsNavBarBrand.Create(Owner);

        // if (csDesigning in ComponentState) and (Owner is TNVModuleContainer) and
        // (TNVModuleContainer(Owner).Designer <> nil) then
        // Result.Name := TNVModuleContainer(Owner).Designer.UniqueName(Result.ClassName);

        Result.Parent := _Container;
      finally
        UpdateRecreatingFlag(False);
      end;
    end;
end;

function TNvBsNavBar.GetContainer: TBsNavBarContainer;
var
  I: integer;
begin
  Result := nil;
  if (csLoading in Owner.ComponentState) or (csLoading in ComponentState) or
    (csRecreating in ControlState) then
    Exit;

  for I := 0 to ControlCount - 1 do
    begin
      if Controls[I] is TBsNavBarContainer then
        begin
          Result := TBsNavBarContainer(Controls[I]);
          Break;
        end;
    end;
  if Result = nil then
    begin
      UpdateRecreatingFlag(True);
      try
        Result := TBsNavBarContainer.Create(Owner);

        // if (csDesigning in ComponentState) and (Owner is TNVModuleContainer) and
        // (TNVModuleContainer(Owner).Designer <> nil) then
        // Result.Name := TNVModuleContainer(Owner).Designer.UniqueName(Result.ClassName);

        Result.Parent := Self;
      finally
        UpdateRecreatingFlag(False);
      end;
    end;
end;

function TNvBsNavBar.GetContent: TBsNavBarContent;
var
  I         : integer;
  _Container: TBsNavBarContainer;
begin
  Result := nil;
  if (csLoading in Owner.ComponentState) or (csLoading in ComponentState) or
    (csRecreating in ControlState) then
    Exit;

  _Container := Container; // Optimization;

  for I := 0 to _Container.ControlCount - 1 do
    begin
      if _Container.Controls[I] is TBsNavBarContent then
        begin
          Result := TBsNavBarContent(_Container.Controls[I]);
          Break;
        end;
    end;
  if (Result = nil) and (not(csLoading in ComponentState)) then
    begin
      UpdateRecreatingFlag(True);
      try
        Result := TBsNavBarContent.Create(Owner);

        // if (csDesigning in ComponentState) and (Owner is TNVModuleContainer) and
        // (TNVModuleContainer(Owner).Designer <> nil) then
        // Result.Name := TNVModuleContainer(Owner).Designer.UniqueName(Result.ClassName);

        Result.Parent := _Container;
      finally
        UpdateRecreatingFlag(False);
      end;
    end;
end;

function TNvBsNavBar.GetToggler: TBsNavBarToggler;
var
  I         : integer;
  _Container: TBsNavBarContainer;
begin
  Result := nil;
  if (csLoading in Owner.ComponentState) or (csLoading in ComponentState) or
    (csRecreating in ControlState) then
    Exit;

  _Container := Container; // Optimization;

  for I := 0 to _Container.ControlCount - 1 do
    begin
      if _Container.Controls[I] is TBsNavBarToggler then
        begin
          Result := TBsNavBarToggler(_Container.Controls[I]);
          Break;
        end;
    end;
  if (Result = nil) and (not(csLoading in ComponentState)) then
    begin
      UpdateRecreatingFlag(True);
      try
        Result := TBsNavBarToggler.Create(Owner);

        // if (csDesigning in ComponentState) and (Owner is TNVModuleContainer) and
        // (TNVModuleContainer(Owner).Designer <> nil) then
        // Result.Name := TNVModuleContainer(Owner).Designer.UniqueName(Result.ClassName);

        Result.Parent := _Container;
      finally
        UpdateRecreatingFlag(False);
      end;
    end;
end;

procedure TNvBsNavBar.InternalRender(JSON: TJsonObject);
begin
  inherited;
  if FExpand <> bssXs then
    RenderExpand(JSON);
end;

procedure TNvBsNavBar.RenderExpand(aJson: TJsonObject);
begin
  aJson.S['Expand'] := TBsSizeStr[FExpand]
end;

// procedure TNvBsNavBar.SetBrand(const Value: TBsNavBarBrand);
// begin
// if FBrand <> nil then
// FBrand.Assign(Value);
// end;

// procedure TNvBsNavBar.SetContent(const Value: TBsNavBarContent);
// begin
// if FContent <> nil then
// FContent.Assign(Value);
// end;

procedure TNvBsNavBar.SetExpand(const Value: TBsSize);
begin
  if Value <> FExpand then
    begin
      EnqueueChange('Expand', RenderExpand);
      FExpand := Value;
      Invalidate;
    end;
end;

{ TBsNavBarBrand }

constructor TBsNavBarBrand.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption];
end;

class function TBsNavBarBrand.DefaultClassCss: string;
begin
  Result := 'navbar-brand';
end;

class function TBsNavBarBrand.DefaultHtmlTag: string;
begin
  Result := 'a';
end;

class function TBsNavBarBrand.DefaultRenderText: Boolean;
begin
  Result := True;
end;

procedure TBsNavBarBrand.InternalRender(JSON: TJsonObject);
begin
  inherited;
  if Not FHRef.IsEmpty then
    RenderHRef(JSON);
end;

procedure TBsNavBarBrand.RenderHRef(aJson: TJsonObject);
begin
  aJson.S['HRef'] := FHRef
end;

procedure TBsNavBarBrand.SetHRef(const Value: string);
begin
  if (Value <> HRef) then
    begin
      EnqueueChange('HRef', RenderHRef);
      FHRef := Value;
      Invalidate;
    end;
end;

class function TBsNavBarBrand.SupportImage: Boolean;
begin
  Result := True;
end;

{ TBsNavItemLinkBase }

constructor TBsNavItemLinkBase.Create(AOwner: TComponent);
begin
  inherited;
  FLink := TNvBsNavLink.Create(Self);
  FLink.SetSubComponent(True, 'Link');
end;

{ TBsNavItemDropdown }

constructor TBsNavItemDropdown.Create(AOwner: TComponent; aDropDown: TNvBsDropdownMenu);
begin
  inherited Create(AOwner);
  aDropDown.Parent := Self;
  FDropDown        := aDropDown;
end;

class function TBsNavItemDropdown.DefaultClassCss: string;
begin
  Result := 'nav-item dropdown';
end;

{ TNvBsnav }

procedure TNvBsNav.AfterDesignDrop;
var
  _Control: TControl;
begin
  inherited;
  // Inside Sidebar need to be vertical
  if (Parent <> nil) then
    begin
      _Control := Self.Parent;
      repeat
        if _Control is TNvBsSideBar then
          begin
            Vertical := True;
            Break;
          end;
        _Control := _Control.Parent;
      until (_Control = nil);
    end;
end;

class function TNvBsNav.DefaultClassCss: string;
begin
  Result := 'nav';
end;

class function TNvBsNav.DefaultHtmlTag: string;
begin
  Result := 'ul';
end;

procedure TNvBsNav.InternalRender(JSON: TJsonObject);
begin
  inherited;
  if FUnderline then
    RenderUnderline(JSON);
  if FVertical then
    RenderVertical(JSON);
end;

procedure TNvBsNav.RenderUnderline(aJson: TJsonObject);
begin
  aJson.B['Underline'] := FUnderline
end;

procedure TNvBsNav.RenderVertical(aJson: TJsonObject);
begin
  aJson.B['Vertical'] := FVertical
end;

procedure TNvBsNav.SetUnderline(const Value: Boolean);
begin
  if Value <> FUnderline then
    begin
      EnqueueChange('Underline', RenderUnderline);
      FUnderline := Value;
      Invalidate;
    end;
end;

procedure TNvBsNav.SetVertical(const Value: Boolean);
begin
  if Value <> FVertical then
    begin
      EnqueueChange('Vertical', RenderVertical);
      FVertical := Value;
      Invalidate;
    end;
end;

{ TBsNavItem }

class function TBsNavItem.DefaultClassCss: string;
begin
  Result := 'nav-item';
end;

class function TBsNavItem.DefaultHtmlTag: string;
begin
  Result := 'li';
end;

class function TBsNavItem.SupportImage: Boolean;
begin
  Result := True;
end;

{ TBsNavBarItemLink }

class function TBsNavBarItemLink.DefaultHtmlTag: string;
begin
  Result := 'div';
end;

{ TBsNavBarItemDropdown }

class function TBsNavBarItemDropdown.DefaultHtmlTag: string;
begin
  Result := 'div';
end;

{ TBsNavBarToggler }

class function TBsNavBarToggler.DefaultClassCss: string;
begin
  Result := 'navbar-toggler';
end;

class function TBsNavBarToggler.DefaultDataToggle: TNvBsDataToggle;
begin
  Result := bsdtCollapse;
end;

{ TBsNavBarContent }

class function TBsNavBarContent.DefaultClassCss: string;
begin
  Result := 'collapse navbar-collapse';
end;

function TBsNavBarContent.GetNav: TBsNavBarNav;
var
  I: integer;
begin
  Result := nil;

  if (csLoading in Owner.ComponentState) or (csLoading in ComponentState) or
    (csRecreating in ControlState) then
    Exit;

  for I := 0 to ControlCount - 1 do
    begin
      if Controls[I] is TBsNavBarNav then
        begin
          Result := TBsNavBarNav(Controls[I]);
          Break;
        end;
    end;
  if (Result = nil) and (not(csLoading in ComponentState)) then
    begin
      UpdateRecreatingFlag(True);
      try
        Result := TBsNavBarNav.Create(Owner);

        // if (csDesigning in ComponentState) and (Owner is TNVModuleContainer) and
        // (TNVModuleContainer(Owner).Designer <> nil) then
        // Result.Name := TNVModuleContainer(Owner).Designer.UniqueName(Result.ClassName);

        Result.Parent := Self;
      finally
        UpdateRecreatingFlag(False);
      end;
    end;

end;

{ TBsNavBarNav }

class function TBsNavBarNav.DefaultClassCss: string;
begin
  Result := 'navbar-nav';
end;

class function TBsNavBarNav.DefaultHtmlTag: string;
begin
  Result := 'div';
end;

{ TBsNavBarContainer }

class function TBsNavBarContainer.DefaultClassCss: string;
begin
  Result := 'container-fluid';
end;

{ TNvBsSideBar }

procedure TNvBsSideBar.AfterDesignDrop;
var
  _Nav        : TNvBsNav;
  _MenuItem   : TBsNavItemCollapse;
  _SubMenu    : TNvBsNav;
  _SubMenuItem: TBsNavItemLink;
begin
  inherited;
  _Nav := TNvBsNav.Create(Owner);
  with _Nav do
    begin
      Vertical := True;
      Parent   := Self;
    end;

  _MenuItem := TBsNavItemCollapse.Create(Owner);
  with _MenuItem do
    begin
      Link.Text := 'Menu 1';
      Parent    := _Nav;
    end;

  _SubMenu := TNvBsNav.Create(Owner);
  with _SubMenu do
    begin
      Vertical := True;
      Shadow   := bssNormal;
      Parent   := _MenuItem;
    end;

  _SubMenuItem := TBsNavItemLink.Create(Owner);
  with _SubMenuItem do
    begin
      Link.Text := 'SubMenu 1';
      Parent    := _SubMenu;
    end;

end;

constructor TNvBsSideBar.Create(AOwner: TComponent);
begin
  inherited;
  FExpand := bssSM;
  FFull   := bssXL;
  // Design
  Align   := alCustom;
  Anchors := [];
end;

class function TNvBsSideBar.DefaultClassCss: string;
begin
  Result := 'sidebar';
end;

destructor TNvBsSideBar.Destroy;
begin

  inherited;
end;

procedure TNvBsSideBar.InternalRender(JSON: TJsonObject);
begin
  inherited;
  if FExpand <> bssXs then
    RenderExpand(JSON);
  if FFull <> bssXL then
    RenderFull(JSON);
  if FHoverItems then
    RenderHoverItems(JSON);
end;

procedure TNvBsSideBar.RenderExpand(aJson: TJsonObject);
begin
  aJson.S['Expand'] := TBsSizeStr[FExpand]
end;

procedure TNvBsSideBar.RenderFull(aJson: TJsonObject);
begin
  aJson.S['Full'] := TBsSizeStr[FFull];
end;

procedure TNvBsSideBar.RenderHoverItems(aJson: TJsonObject);
begin
  aJson.B['HoverItems'] := FHoverItems;
end;

procedure TNvBsSideBar.SetExpand(const Value: TBsSize);
begin
  if Value <> FExpand then
    begin
      EnqueueChange('Expand', RenderExpand);
      FExpand := Value;
      Invalidate;
    end;
end;

procedure TNvBsSideBar.SetFull(const Value: TBsSize);
begin
  if Value <> FFull then
    begin
      EnqueueChange('Full', RenderFull);
      FFull := Value;
      Invalidate;
    end;
end;

procedure TNvBsSideBar.SetHoverItems(const Value: Boolean);
begin
  if Value <> FHoverItems then
    begin
      EnqueueChange('HoverItems', RenderHoverItems);
      FHoverItems := Value;
      Invalidate;
    end;
end;

{ TNvBsNavLink }

class function TNvBsNavLink.DefaultClassCss: string;
begin
  Result := 'nav-link';
end;

class function TNvBsNavLink.DefaultHtmlTag: string;
begin
  Result := 'a';
end;

end.
