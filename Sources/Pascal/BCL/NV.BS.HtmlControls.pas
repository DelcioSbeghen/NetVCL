unit NV.BS.HtmlControls;

interface

uses
  NV.Controls, NV.BS.Controls, NV.Ajax, Classes, NV.JSON, NV.BS.Types, NV.Interfaces,
  NV.BS.Containers;

type

  TNvBSText = class(TNvBsGridControl)
  protected
    class function DefaultHtmlTag: string; override;
    class function DefaultRenderText: Boolean; override;
  published
    property Background;
    property Border;
    property ClassCss;
    property Grids;
    property Position;
    property Shadow;
    property TextProps;
    property Text;
    property TextVisible;
    property TagHtml;
    property Width_;
  end;

  TNvBsLink = class(TNvBsCustomControl)
  protected
    class function DefaultHtmlTag: string; override;
    class function SupportImage: Boolean; override;
    class function DefaultRenderText: Boolean; override;
  private
    FHRef : string;
    FColor: TNvBsLinkColor;
    procedure SetHRef(const Value: string);
    procedure SetColor(const Value: TNvBsLinkColor);
  protected
    procedure InternalRender(JSON: TJsonObject); override;
    procedure RenderHref(aJson: TJsonObject);
    procedure RenderColor(aJson: TJsonObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property Color: TNvBsLinkColor read FColor write SetColor default bslcNone;
    property HRef : string read FHRef write SetHRef;
    property ClassCss;
    property ImageListLink;
    property Position;
    property Text;
    property TextProps;
    property TextVisible;
    property Width_;
    property OnClick;
  end;

  TNvBsCustomDropdownContainer = class;

  TNvBsDropdownItemLink = class(TNvBsLink)
  protected
    class function DefaultClassCss: string; override;
  private
    FDropdownMenu: TNvBsCustomDropdownContainer;
  public
    constructor Create(AOwner: TComponent;
      aDropdownContainer: TNvBsCustomDropdownContainer); virtual;
    destructor Destroy; override;
    property DropdownMenu: TNvBsCustomDropdownContainer read FDropdownMenu;
  end;



  // TNvBsDropDownItems = class(TOwnedCollection)
  // private
  // FDropdown: TNvBsCustomDropdownMenu;
  // // procedure WriteDropdown(Writer: TWriter);
  // // procedure ReadDropdown(Reader: TReader);
  // protected
  // // procedure DefintProperties(Filer: TFiler);
  // procedure AssignTo(Dest: TPersistent); override;
  // public
  // constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass;
  // aDropdown: TNvBsCustomDropdownMenu); overload;
  // published
  // property DropDown: TNvBsCustomDropdownMenu read FDropdown write FDropdown;
  // end;

  TNvBsCustomDropdownContainer = class(TNVBsContainer)
  protected
    class function DefaultClassCss: string; override;
  private
    FAlignEnd  : TBsSize;
    FAlignStart: TBsSize;
    FOnShow    : TNotifyEvent;
    FOnHide    : TNotifyEvent;
    // FItems     : TNvBsDropDownItems;
    procedure SetAlignStart(const Value: TBsSize);
    procedure SetAlignEnd(const Value: TBsSize);
    procedure SetOnHide(const Value: TNotifyEvent);
    procedure SetOnShow(const Value: TNotifyEvent);
    // procedure SetItems(const Value: TNvBsDropDownItems);
  protected
    procedure InternalRender(JSON: TJsonObject); override;
    procedure RenderAlignStart(aJson: TJsonObject);
    procedure RenderAlignEnd(aJson: TJsonObject);
    procedure RenderEvents(aJson: TJsonObject); override;
    // Events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
    procedure DoShow(aEvent: TJsonObject);
    procedure DoHide(aEvent: TJsonObject);
    // property Items     : TNvBsDropDownItems read FItems write SetItems;
    property AlignStart: TBsSize read FAlignStart write SetAlignStart;
    property AlignEnd: TBsSize read FAlignEnd write SetAlignEnd;
    property OnShow: TNotifyEvent read FOnShow write SetOnShow;
    property OnHide: TNotifyEvent read FOnHide write SetOnHide;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TNvBsDropdownMenu = class(TNvBsCustomDropdownContainer)
  published
    property AlignStart;
    property AlignEnd;
    property ClassCss;
    property Position;
    property Width_;
    property OnShow;
    property OnHide;
  end;

implementation

uses
  Controls, SysUtils;

{ TNvBsLink }

constructor TNvBsLink.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csSetCaption];
end;

class function TNvBsLink.DefaultHtmlTag: string;
begin
  Result := 'a';
end;

class function TNvBsLink.DefaultRenderText: Boolean;
begin
  Result := True;
end;

procedure TNvBsLink.InternalRender(JSON: TJsonObject);
begin
  inherited;
  if not FHRef.IsEmpty then
    RenderHref(JSON);
  if FColor <> bslcNone then
    RenderColor(JSON);
end;

procedure TNvBsLink.RenderColor(aJson: TJsonObject);
begin
  aJson.S['Color'] := TNvBsLinkColorStr[FColor]
end;

procedure TNvBsLink.RenderHref(aJson: TJsonObject);
begin
  aJson.S['HRef'] := FHRef
end;

procedure TNvBsLink.SetColor(const Value: TNvBsLinkColor);
begin
  if (Value <> FColor) then
    begin
      EnqueueChange('Color', RenderColor);
      FColor := Value;
      Invalidate;
    end;
end;

procedure TNvBsLink.SetHRef(const Value: string);
begin
  if (Value <> HRef) then
    begin
      EnqueueChange('HRef', RenderHref);
      FHRef := Value;
      Invalidate;
    end;
end;

class function TNvBsLink.SupportImage: Boolean;
begin
  Result := True;
end;

{ TNvBsDropDownItems }

constructor TNvBsCustomDropdownContainer.Create;
begin
  inherited;
  // if AOwner is TWinControl then
  // Parent := AOwner as TWinControl;
  // SetSubComponent(True);
  // FItems := TNvBsDropDownItems.Create(Self, TNvBsDropDownItem, Self);

end;

class function TNvBsCustomDropdownContainer.DefaultClassCss: string;
begin
  Result := 'dropdown-menu';
end;

destructor TNvBsCustomDropdownContainer.Destroy;
begin
  inherited;
end;

procedure TNvBsCustomDropdownContainer.DoHide(aEvent: TJsonObject);
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TNvBsCustomDropdownContainer.DoShow(aEvent: TJsonObject);
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TNvBsCustomDropdownContainer.InternalRender(JSON: TJsonObject);
begin
  inherited;
  RenderAlignStart(JSON);
  RenderAlignEnd(JSON);
end;

function TNvBsCustomDropdownContainer.ProcessEvent(AEventName: string; aEvent: TJsonObject)
  : Boolean;
begin
  if AEventName = 'show' then
    begin
      DoShow(aEvent);
      Result := True;
    end
  else if AEventName = 'hide' then
    begin
      DoHide(aEvent);
      Result := True;
    end
  else
    Result := inherited;
end;

procedure TNvBsCustomDropdownContainer.RenderAlignEnd(aJson: TJsonObject);
begin
  aJson.S['AlignEnd'] := TBsSizeStr[FAlignEnd];
end;

procedure TNvBsCustomDropdownContainer.RenderAlignStart(aJson: TJsonObject);
begin
  aJson.S['AlignStart'] := TBsSizeStr[FAlignStart];
end;

procedure TNvBsCustomDropdownContainer.RenderEvents(aJson: TJsonObject);
begin
  inherited;
  if Assigned(FOnShow) then
    aJson.A['Events'].Add('show.bs.dropdown');
  if Assigned(FOnHide) then
    aJson.A['Events'].Add('hide.bs.dropdown');
end;

// procedure TNvBsCustomDropdownContainer.SetItems(const Value: TNvBsDropDownItems);
// begin
// FItems := Value;
// end;

procedure TNvBsCustomDropdownContainer.SetAlignStart(const Value: TBsSize);
begin
  if Value <> FAlignStart then
    begin
      EnqueueChange('AlignStart', RenderAlignStart);
      FAlignStart := Value;
    end;
end;

procedure TNvBsCustomDropdownContainer.SetOnHide(const Value: TNotifyEvent);
begin
  EnqueueChange('Events', RenderEvents);
  FOnHide := Value;
  Invalidate;
end;

procedure TNvBsCustomDropdownContainer.SetOnShow(const Value: TNotifyEvent);
begin
  EnqueueChange('Events', RenderEvents);
  FOnShow := Value;
  Invalidate;
end;

procedure TNvBsCustomDropdownContainer.SetAlignEnd(const Value: TBsSize);
begin
  if Value <> FAlignEnd then
    begin
      EnqueueChange('AlignEnd', RenderAlignEnd);
      FAlignEnd := Value;
    end;
end;

{ TNvBsDropDownItems }

// procedure TNvBsDropDownItems.AssignTo(Dest: TPersistent);
// var
// _Dest: TNvBsDropDownItems;
// begin
// if (Dest <> nil) and (Dest is TNvBsDropDownItems) then
// begin
// _Dest           := (Dest as TNvBsDropDownItems);
// _Dest.FDropdown := FDropdown;
// inherited;
// end
// else
// inherited;
// end;
//
// constructor TNvBsDropDownItems.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass;
// aDropdown: TNvBsCustomDropdownMenu);
// begin
// inherited Create(AOwner, ItemClass);
// FDropdown := aDropdown;
// end;

// procedure TNvBsDropDownItems.DefintProperties(Filer: TFiler);
// begin
// inherited;
// Filer.DefineProperty('DropDown', ReadDropdown, WriteDropdown, FDropdown <> nil);
// end;

// procedure TNvBsDropDownItems.ReadDropdown(Reader: TReader);
// begin
// Reader.Read(FDropdown);
// end;
//
// procedure TNvBsDropDownItems.WriteDropdown(Writer: TWriter);
// begin
// Writer.WriteProperties(FDropdown);
// end;

{ TNvBsDropDownItem }

constructor TNvBsDropdownItemLink.Create(AOwner: TComponent;
  aDropdownContainer: TNvBsCustomDropdownContainer);
begin
  inherited Create(AOwner);
  Parent        := aDropdownContainer;
  FDropdownMenu := aDropdownContainer;
end;

class function TNvBsDropdownItemLink.DefaultClassCss: string;
begin
  Result := 'dropdown-item';
end;

destructor TNvBsDropdownItemLink.Destroy;
begin
  inherited;
end;

{ TNvBSText }

class function TNvBSText.DefaultHtmlTag: string;
begin
  Result := 'span';
end;

class function TNvBSText.DefaultRenderText: Boolean;
begin
  Result := True;
end;

end.
