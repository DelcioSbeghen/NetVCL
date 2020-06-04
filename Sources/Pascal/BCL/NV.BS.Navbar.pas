unit NV.BS.Navbar;

interface

uses
  NV.BS.Containers, NV.BS.Types, NV.Ajax, NV.JSON, System.Classes,
  NV.BS.HtmlControls;

type

  TNvBsNav = class(TNvBsGridContainer)
  protected
    class function DefaultHtmlTag: string; override;
  published
    property Background stored IsNotBgDefault;
    property Border;
    property Grids;
    property Shadow default bssNone;
    property TextProps;
    property TagHtml;
  end;

  TBsNavItem = class(TNVBsContainer)
  protected
    class function DefaultHtmlTag: string; override;
    class function SupportImage: Boolean; override;
  public
  published
    property OnClick;
    property ImageListLink;
    property TagHtml;
  end;

  TBsNavItemLink = class(TBsNavItem)
  private
    FHRef: string;
    procedure SetHRef(const Value: string);
  protected
    procedure InternalRender(Ajax: TNvAjax; JSON: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HRef: string read FHRef write SetHRef;
    property TextProps;
    property Text;
  end;

  TBsNavItemDropdown = class(TBsNavItemLink)
  private
    FDropDown: TNvBsDropdownMenu;
  public
    constructor Create(AOwner: TComponent; aDropDown: TNvBsDropdownMenu); virtual;
  published
    property DropDown: TNvBsDropdownMenu read FDropDown;
  end;

  TBsNavBarBrand = class(TNVBsContainer)
  protected
    class function DefaultHtmlTag: string; override;
    class function SupportImage: Boolean; override;
  private
    FHRef: string;
    procedure SetHRef(const Value: string);
  protected
    procedure InternalRender(Ajax: TNvAjax; JSON: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HRef: string read FHRef write SetHRef;
    property ImageListLink;
    property Text;
  end;

  TBsNavBarItemLink = class(TBsNavItemLink)
  protected
    class function DefaultHtmlTag: string; override;
  end;

  TBsNavBarItemDropdown = class(TBsNavItemDropdown)
  protected
    class function DefaultHtmlTag: string; override;
  end;

  TBsNavBarContent = class(TNVBsContainer)
  private
  protected
  public
  published
  end;

  TNvBsNavBar = class(TNVBsContainer)
  private
    FColor  : TBsNavbarColor;
    FExpand : TBsSize;
    FBrand  : TBsNavBarBrand;
    FContent: TBsNavBarContent;
    procedure SetColor(const Value: TBsNavbarColor);
    procedure SetExpand(const Value: TBsSize);
    procedure SetBrand(const Value: TBsNavBarBrand);
    procedure SetContent(const Value: TBsNavBarContent);
    function GetBrand: TBsNavBarBrand;
    function GetContent: TBsNavBarContent;
  protected
    procedure InternalRender(Ajax: TNvAjax; JSON: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Expand : TBsSize read FExpand write SetExpand default bssSM;
    property Color  : TBsNavbarColor read FColor write SetColor default bsnavcLight;
    property Brand  : TBsNavBarBrand read GetBrand;     // write SetBrand;
    property Content: TBsNavBarContent read GetContent; // write SetContent;
    property Background stored IsNotBgDefault;
    property Border;
    property Shadow default bssNone;
    property TextProps;
  end;

implementation

uses
  SysUtils, Vcl.Controls, NV.Controls;

{ TNvBsNavBar }

constructor TNvBsNavBar.Create(AOwner: TComponent);
begin
  inherited;
  FExpand := bssSM;
  // FBrand   := TBsNavBarBrand.Create(Self);
  // FContent := TBsNavBarContent.Create(Self);
end;

destructor TNvBsNavBar.Destroy;
begin
  FBrand.Free;
  FContent.Free;
  inherited;
end;

function TNvBsNavBar.GetBrand: TBsNavBarBrand;
var
  I: integer;
begin
  Result := nil;
  if (csLoading in Owner.ComponentState) or (csLoading in ComponentState) or
    (csRecreating in ControlState) then
    Exit;

  for I := 0 to ControlCount - 1 do
    begin
      if Controls[I] is TBsNavBarBrand then
        begin
          Result := TBsNavBarBrand(Controls[I]);
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

        Result.Parent := Self;
      finally
        UpdateRecreatingFlag(False);
      end;
    end;
end;

function TNvBsNavBar.GetContent: TBsNavBarContent;
var
  I: integer;
begin
  Result := nil;
  if (csLoading in Owner.ComponentState) or (csLoading in ComponentState) or
    (csRecreating in ControlState) then
    Exit;

  for I := 0 to ControlCount - 1 do
    begin
      if Controls[I] is TBsNavBarContent then
        begin
          Result := TBsNavBarContent(Controls[I]);
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

        Result.Parent := Self;
      finally
        UpdateRecreatingFlag(False);
      end;
    end;
end;

procedure TNvBsNavBar.InternalRender(Ajax: TNvAjax; JSON: TJsonObject);
begin
  inherited;

  if FColor <> bsnavcLight then
    JSON.S['Color'] := TBsNavbarColorStr[FColor];

  if FExpand <> bssXs then
    JSON.S['Expand'] := TBsSizeStr[FExpand];
end;

procedure TNvBsNavBar.SetBrand(const Value: TBsNavBarBrand);
begin
  if FBrand <> nil then
    FBrand.Assign(Value);
end;

procedure TNvBsNavBar.SetColor(const Value: TBsNavbarColor);
begin
  if Value <> FColor then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Color'] := TBsNavbarColorStr[Value];
      FColor                       := Value;
      Invalidate;
    end;
end;

procedure TNvBsNavBar.SetContent(const Value: TBsNavBarContent);
begin
  if FContent <> nil then
    FContent.Assign(Value);
end;

procedure TNvBsNavBar.SetExpand(const Value: TBsSize);
begin
  if Value <> FExpand then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Expand'] := TBsSizeStr[Value];
      FExpand                       := Value;
      Invalidate;
    end;
end;

{ TBsNavBarBrand }

constructor TBsNavBarBrand.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption];
end;

class function TBsNavBarBrand.DefaultHtmlTag: string;
begin
  Result := 'a';
end;

procedure TBsNavBarBrand.InternalRender(Ajax: TNvAjax; JSON: TJsonObject);
begin
  inherited;
  if Not FHRef.IsEmpty then
    JSON.S['HRef'] := FHRef;
end;

procedure TBsNavBarBrand.SetHRef(const Value: string);
begin
  if (Value <> HRef) then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['HRef'] := Value;
      FHRef                       := Value;
      Invalidate;
    end;
end;

class function TBsNavBarBrand.SupportImage: Boolean;
begin
  Result := True;
end;

{ TBsNavItemLink }

constructor TBsNavItemLink.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption];
end;

procedure TBsNavItemLink.InternalRender(Ajax: TNvAjax; JSON: TJsonObject);
begin
  inherited;
  if Not FHRef.IsEmpty then
    JSON.S['HRef'] := FHRef;
end;

procedure TBsNavItemLink.SetHRef(const Value: string);
begin
  if (Value <> HRef) then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['HRef'] := Value;
      FHRef                       := Value;
      Invalidate;
    end;
end;

{ TBsNavItemDropdown }

constructor TBsNavItemDropdown.Create(AOwner: TComponent; aDropDown: TNvBsDropdownMenu);
begin
  inherited Create(AOwner);
  aDropDown.Parent := Self;
  FDropDown        := aDropDown;
end;

{ TNvBsnav }

class function TNvBsNav.DefaultHtmlTag: string;
begin
  Result := 'ul';
end;

{ TBsNavItem }

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

end.
