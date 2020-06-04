unit NV.BS.HtmlControls;

interface

uses
  NV.Controls, NV.BS.Controls, NV.Ajax, System.Classes, NV.JSON, NV.BS.Types, NV.Interfaces,
  NV.BS.Containers;

type

  TNvBSText = class(TNvBsCustomControl)
  protected
    class function DefaultHtmlTag: string; override;
  published
    property Background;
    property Border;
    property Shadow;
    property TextProps;
    property Text;
    property TagHtml;
  end;

  TNvBsLink = class(TNvBsCustomControl)
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
    property OnClick;
  end;

  TNvBsDropdownMenu = class;

  TNvBsDropdownItemLink = class(TNvBsLink)
  private
    FDropdownMenu: TNvBsDropdownMenu;
  public
    constructor Create(AOwner: TComponent; aDropdownMenu: TNvBsDropdownMenu); virtual;
    destructor Destroy; override;
  published
    property DropdownMenu: TNvBsDropdownMenu read FDropdownMenu;
  end;



  // TNvBsDropDownItems = class(TOwnedCollection)
  // private
  // FDropdown: TNvBsDropdownMenu;
  // // procedure WriteDropdown(Writer: TWriter);
  // // procedure ReadDropdown(Reader: TReader);
  // protected
  // // procedure DefintProperties(Filer: TFiler);
  // procedure AssignTo(Dest: TPersistent); override;
  // public
  // constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass;
  // aDropdown: TNvBsDropdownMenu); overload;
  // published
  // property DropDown: TNvBsDropdownMenu read FDropdown write FDropdown;
  // end;

  TNvBsDropdownMenu = class(TNVBsContainer)
  private
    FAlignRight: TBsSize;
    FAlignLeft : TBsSize;
    // FItems     : TNvBsDropDownItems;
    procedure SetAlignLeft(const Value: TBsSize);
    procedure SetAlignRight(const Value: TBsSize);
    // procedure SetItems(const Value: TNvBsDropDownItems);
  protected
    procedure InternalRender(Ajax: TNvAjax; JSON: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // property Items     : TNvBsDropDownItems read FItems write SetItems;
    property AlignLeft : TBsSize read FAlignLeft write SetAlignLeft;
    property AlignRight: TBsSize read FAlignRight write SetAlignRight;
  end;

implementation

uses
  Vcl.Controls, SysUtils;

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

procedure TNvBsLink.InternalRender(Ajax: TNvAjax; JSON: TJsonObject);
begin
  inherited;
  if not FHRef.IsEmpty then
    JSON.S['HRef'] := FHRef;
end;

procedure TNvBsLink.SetHRef(const Value: string);
begin
  if (Value <> HRef) then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['HRef'] := Value;
      FHRef                       := Value;
      Invalidate;
    end;
end;

class function TNvBsLink.SupportImage: Boolean;
begin
  Result := True;
end;

{ TNvBsDropDownItems }

constructor TNvBsDropdownMenu.Create;
begin
  inherited;
  // if AOwner is TWinControl then
  // Parent := AOwner as TWinControl;
  // SetSubComponent(True);
  // FItems := TNvBsDropDownItems.Create(Self, TNvBsDropDownItem, Self);

end;

destructor TNvBsDropdownMenu.Destroy;
begin
  inherited;
end;

procedure TNvBsDropdownMenu.InternalRender(Ajax: TNvAjax; JSON: TJsonObject);
begin
  inherited;
  JSON.S['AlignLeft']  := TBsSizeStr[FAlignLeft];
  JSON.S['AlignRight'] := TBsSizeStr[FAlignRight];
end;

// procedure TNvBsDropdownMenu.SetItems(const Value: TNvBsDropDownItems);
// begin
// FItems := Value;
// end;

procedure TNvBsDropdownMenu.SetAlignLeft(const Value: TBsSize);
begin
  if Value <> FAlignLeft then
    begin
      if Rendered then
        ControlAjaxJson.S['AlignLeft'] := TBsSizeStr[Value];
      FAlignLeft                       := Value;
    end;
end;

procedure TNvBsDropdownMenu.SetAlignRight(const Value: TBsSize);
begin
  if Value <> FAlignRight then
    begin
      if Rendered then
        ControlAjaxJson.S['AlignRight'] := TBsSizeStr[Value];
      FAlignRight                       := Value;
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
// aDropdown: TNvBsDropdownMenu);
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

constructor TNvBsDropdownItemLink.Create(AOwner: TComponent; aDropdownMenu: TNvBsDropdownMenu);
begin
  inherited Create(AOwner);
  Parent        := aDropdownMenu;
  FDropdownMenu := aDropdownMenu;
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

end.
