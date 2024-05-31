unit NV.VCL.Images;

interface

uses
  Classes, NV.Interfaces, Generics.Collections;

type
  TNVImageListLink = class;

  TNvImageItemClass = class of TNvImageItem;

  TNvImageItem = class(TCollectionItem)
  private
    FName: string;
    FHtml: string;
    procedure SetHtml(const Value: string);
    procedure SetName(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function Render: string; virtual;
  public
  published
    property index;
    property Name: string read FName write SetName;
    property Html: string read FHtml write SetHtml;
  end;

  TNvImageCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TNvImageItem;
    procedure SetItem(Index: Integer; const Value: TNvImageItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    property Items[Index: Integer]: TNvImageItem read GetItem write SetItem; default;
  end;

  TNvCustomImageList = class(TComponent)
  protected
    class function DefaultHeight: string; virtual;
    class function DefaultWidth: string; virtual;
  strict private
    FUpdateCount: Integer;
    FLinks      : TList<TNVImageListLink>;
    FChanged    : Boolean;
  private
    function GetLinkCount: Integer;
    function GetLinks(const Index: Integer): TNVImageListLink;
  protected
    procedure DoChange; virtual;
    function GetCount: Integer;
    procedure AddLink(Link: TNVImageListLink);
    procedure DeleteLink(Link: TNVImageListLink);
    property LinkCount: Integer read GetLinkCount;
    property Links[const Index: Integer]: TNVImageListLink read GetLinks;
  private
    FImages  : TNvImageCollection;
    FOnChange: TNotifyEvent;
    FJsLinks : TStringList;
    FCssLinks: TStringList;
    FWidth   : string;
    FHeight  : string;
    procedure SetImages(const Value: TNvImageCollection);
    procedure SetCssLinks(const Value: TStringList);
    procedure SetJsLinks(const Value: TStringList);
    procedure SetHeight(const Value: string);
    procedure SetWidth(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function IsNotDedaultSize: Boolean;
    function ImageItemClass: TNvImageItemClass;
    procedure Updated; override;
    procedure Loaded; override;
    property CssLinks: TStringList read FCssLinks write SetCssLinks;
    property JsLinks: TStringList read FJsLinks write SetJsLinks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Render(Index: Integer; aWidth: string = ''; aHeight: string = ''): string; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Change; virtual;
    property Images: TNvImageCollection read FImages write SetImages;
    property Count: Integer read GetCount;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Height  : string read FHeight write SetHeight stored IsNotDedaultSize;
    property Width   : string read FWidth write SetWidth stored IsNotDedaultSize;
  end;

  TNvSvgImageList = class(TNvCustomImageList)
  protected
    procedure ChangeWidth(var Img: string; aWidth: string);
    procedure ChangeHeight(var Img: string; aHeight: string);
  public
    procedure AddFromFiles(aFiles: TStrings);
    procedure AddFromFile(aFile: string);
    procedure ReplaceFromFile(Index: Integer; aFile: string);
    function Render(Index: Integer; aWidth: string = ''; aHeight: string = ''): string; override;
  published
    property Images;
  end;

  TNVImageListLink = class(TPersistent)
  private
    FImages       : TNvCustomImageList;
    {$IFNDEF FPC} [Weak] {$ENDIF}
    FControl      : INVRenderableComponent;
    FImagePropName: string;
    FImageIndex   : Integer;
    FIgnoreIndex  : Boolean;
    FIgnoreImages : Boolean;
    FOnChange     : TNotifyEvent;
    FWidth        : string;
    FHeight       : string;
    FVisible: Boolean;
    procedure SetImages(const Value: TNvCustomImageList);
    procedure SetImageIndex(const Value: Integer);
    procedure SetHeight(const Value: string);
    procedure SetWidth(const Value: string);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(aControl: INVRenderableComponent); overload;
    destructor Destroy; override;
    procedure Change; virtual;
    function IsValidImage: Boolean;
    function IsVisible:Boolean;
    function Render: string;
    // To Change Image Json prop name
    property ImagePropName: string read FImagePropName write FImagePropName;
    property IgnoreIndex: Boolean read FIgnoreIndex write FIgnoreIndex;
    property IgnoreImages: Boolean read FIgnoreImages write FIgnoreImages;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Images    : TNvCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Height    : string read FHeight write SetHeight;
    property Width     : string read FWidth write SetWidth;
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

implementation

uses
  Windows, SysUtils, RTLConsts, NV.Utils;

{ TNvCustomImageList }

procedure TNvCustomImageList.AddLink(Link: TNVImageListLink);
begin
  if Link <> nil then
    begin
      if FLinks = nil then
        FLinks := TList<TNVImageListLink>.Create;
      if not FLinks.Contains(Link) then
        FLinks.Add(Link);
      Link.FImages := Self;
    end;
end;

procedure TNvCustomImageList.AssignTo(Dest: TPersistent);
var
  I    : Integer;
  _Dest: TNvCustomImageList;
begin
  if Dest is TNvCustomImageList then
    begin
      _Dest         := Dest as TNvCustomImageList;
      _Dest.FWidth  := Width;
      _Dest.FHeight := Height;
      _Dest.BeginUpdate;
      _Dest.Images.Clear;
      for I := 0 to Images.Count - 1 do
        _Dest.Images.Add.Assign(Images[I]);
      _Dest.EndUpdate;
    end
  else
    inherited;
end;

procedure TNvCustomImageList.BeginUpdate;
begin
  if FUpdateCount = 0 then
    Updating;
  Inc(FUpdateCount);
end;

procedure TNvCustomImageList.Change;
begin
  FChanged := True;
  if [csLoading, csDestroying, csUpdating] * ComponentState = [] then
    begin
      DoChange;
      FChanged := False;
    end;
end;

constructor TNvCustomImageList.Create(AOwner: TComponent);
begin
  inherited;
  FImages := TNvImageCollection.Create(Self, ImageItemClass);
  FWidth  := DefaultWidth;
  FHeight := DefaultHeight;
end;

class function TNvCustomImageList.DefaultHeight: string;
begin
  Result := '1em';
end;

class function TNvCustomImageList.DefaultWidth: string;
begin
  Result := '1em';
end;

procedure TNvCustomImageList.DeleteLink(Link: TNVImageListLink);
begin
  if Link <> nil then
    begin
      if FLinks <> nil then
        begin
          FLinks.Remove(Link);
          if FLinks.Count = 0 then
            begin
              FLinks.Free;
              FLinks := nil;
            end;
        end;
      Link.FImages := nil;
    end;
end;

destructor TNvCustomImageList.Destroy;
var
  Link: TNVImageListLink;
begin
  if FLinks <> nil then
    begin
      for Link in FLinks do
        Link.FImages:= nil;

      Flinks.Free;
      FLinks:= nil;
    end;

  FImages.Free;
  inherited;
end;

procedure TNvCustomImageList.DoChange;
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    Links[I].Change;
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNvCustomImageList.EndUpdate;
begin
  if FUpdateCount > 0 then
    begin
      Dec(FUpdateCount);
      if FUpdateCount = 0 then
        Updated;
    end;
end;

function TNvCustomImageList.GetCount: Integer;
begin
  Result := FImages.Count;
end;

function TNvCustomImageList.GetLinkCount: Integer;
begin
  if FLinks <> nil then
    Result := FLinks.Count
  else
    Result := 0;
end;

function TNvCustomImageList.GetLinks(const Index: Integer): TNVImageListLink;
begin
  if (Index < 0) or (Index >= LinkCount) then
    raise EListError.CreateFMT(sArgumentOutOfRange_Index, [Index, LinkCount]);
  Result := FLinks[Index];
end;

function TNvCustomImageList.ImageItemClass: TNvImageItemClass;
begin
  Result := TNvImageItem;
end;

function TNvCustomImageList.IsNotDedaultSize: Boolean;
begin
  Result := (FHeight <> DefaultHeight) or (FWidth <> DefaultWidth);
end;

procedure TNvCustomImageList.Loaded;
begin
  inherited;
  if FChanged then
    Change;
end;

function TNvCustomImageList.Render(Index: Integer; aWidth: string = '';
  aHeight: string = ''): string;
begin
  Result := Images[Index].Render;
end;

procedure TNvCustomImageList.SetCssLinks(const Value: TStringList);
begin
  FCssLinks.Assign(Value);
end;

procedure TNvCustomImageList.SetHeight(const Value: string);
begin
  FHeight := Value;
end;

procedure TNvCustomImageList.SetImages(const Value: TNvImageCollection);
begin
  FImages.Assign(Value);
end;

procedure TNvCustomImageList.SetJsLinks(const Value: TStringList);
begin
  FJsLinks.Assign(Value);
end;

procedure TNvCustomImageList.SetWidth(const Value: string);
begin
  FWidth := Value;
end;

procedure TNvCustomImageList.Updated;
begin
  inherited;
  if FChanged then
    Change;
end;

{ TNvImageItem }

procedure TNvImageItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TNvImageItem then
    begin
      with Dest as TNvImageItem do
        begin
          Name  := Self.Name;
          index := Self.Index;
          Html  := Self.Html;
        end;
    end
  else
    inherited;
end;

function TNvImageItem.Render: string;
begin
  Result := FHtml;
end;

procedure TNvImageItem.SetHtml(const Value: string);
begin
  if (Value <> FHtml) then
    begin
      FHtml := Value;
      Changed(False);
    end;
end;

procedure TNvImageItem.SetName(const Value: string);
begin
  if (Value <> FName) then
    begin
      FName := Value;
      Changed(False);
    end;
end;

{ TNvImageCollection }

function TNvImageCollection.GetItem(Index: Integer): TNvImageItem;
begin
  Result := inherited GetItem(Index) as TNvImageItem;
end;

procedure TNvImageCollection.SetItem(Index: Integer; const Value: TNvImageItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TNvImageCollection.Update(Item: TCollectionItem);
begin
  inherited;
  (Owner as TNvCustomImageList).Change;
end;

{ TNvSvgImageList }

procedure TNvSvgImageList.AddFromFile(aFile: string);
var
  aSvg       : TStringStream;
  _Html, _Aux: string;
begin
  aSvg := TStringStream.Create;
  try
    aSvg.LoadFromFile(aFile);
    aSvg.Position := 0;

    with Images.Add as TNvImageItem do
      begin
        Name := ExtractFileName(aFile);

        // remove original size;
        _Html := aSvg.DataString;
        ChangeWidth(_Html, '');
        ChangeHeight(_Html, '');

        Html := _Html;
      end;

  finally
    aSvg.Free;
  end;
end;

procedure TNvSvgImageList.AddFromFiles(aFiles: TStrings);
var
  I: Integer;
begin
  for I := 0 to aFiles.Count - 1 do
    if FileExists(aFiles[I]) then
      AddFromFile(aFiles[I]);
end;

procedure TNvSvgImageList.ChangeHeight(var Img: string; aHeight: string);
var
  _Aux: string;
begin
  _Aux := ExtractBetweenTags(Img, 'height', '" ');
  if _Aux.IsEmpty then
    Insert(' height="' + aHeight + '" ', Img, Pos('<svg', Img) + 4)
  else
    Img := StringReplace(Img, 'height' + _Aux + '" ', 'height="' + aHeight + '" ', [rfIgnoreCase]);
end;

procedure TNvSvgImageList.ChangeWidth(var Img: string; aWidth: string);
var
  _Aux: string;
begin
  _Aux := ExtractBetweenTags(Img, 'width', '" ');
  if _Aux.IsEmpty then
    Insert(' width="' + aWidth + '" ', Img, Pos('<svg', Img) + 4)
  else
    Img := StringReplace(Img, 'width' + _Aux + '" ', 'width="' + aWidth + '" ', [rfIgnoreCase]);
end;

function TNvSvgImageList.Render(Index: Integer; aWidth: string = ''; aHeight: string = ''): string;
var
  _Height, _Width: string;
begin
  Result := inherited;
  // apply size
  if not aHeight.IsEmpty then
    _Height := aHeight
  else
    _Height := Height;

  if not aWidth.IsEmpty then
    _Width := aWidth
  else
    _Width := Width;

  ChangeWidth(Result, _Width);
  // Result := StringReplace(Result, '<svg', '<svg width="' + Width + '"', [rfIgnoreCase]);

  ChangeHeight(Result, _Height);
  // Result := StringReplace(Result, '<svg', '<svg width="' + Height + '"', [rfIgnoreCase]);
end;

procedure TNvSvgImageList.ReplaceFromFile(Index: Integer; aFile: string);
var
  aSvg       : TStringStream;
  _Html, _Aux: string;
begin
  aSvg := TStringStream.Create;
  try
    aSvg.LoadFromFile(aFile);
    aSvg.Position := 0;

    with Images.Items[Index] do
      begin
        Name := ExtractFileName(aFile);

        // remove original size;
        _Html := aSvg.DataString;
        _Aux  := ExtractBetweenTags(_Html, 'width', '" ');
        if not _Aux.IsEmpty then
          _Html := StringReplace(_Html, 'width' + _Aux + '" ', '', [rfIgnoreCase]);

        _Aux := ExtractBetweenTags(_Html, 'height', '" ');
        if not _Aux.IsEmpty then
          _Html := StringReplace(_Html, 'height' + _Aux + '" ', '', [rfIgnoreCase]);

        Html := _Html;
      end;

  finally
    aSvg.Free;
  end;
end;

{ TNVImageListLink }

procedure TNVImageListLink.AssignTo(Dest: TPersistent);
var
  D: TNVImageListLink;
begin
  if Dest is TNVImageListLink then
    begin
      D               := TNVImageListLink(Dest);
      D.Images        := Images;
      D.ImagePropName := ImagePropName;
      D.IgnoreIndex   := IgnoreIndex;
      D.IgnoreImages  := IgnoreImages;
     // D.OnChange      := OnChange;//Dont assign this
      D.ImageIndex    := ImageIndex;
     // D.Visible:= Visible;//Dont assign this
    end
  else
    inherited;
end;

procedure TNVImageListLink.Change;
begin
  if (FControl <> nil) and FControl.Rendered then
    begin
      FControl.ControlAjaxJson.S[ImagePropName] := Render;
      FControl.Invalidate;
    end;

  if Assigned(OnChange) then
    OnChange(FImages);
end;

constructor TNVImageListLink.Create(aControl: INVRenderableComponent);
begin
  inherited Create;

  FImageIndex    := -1;
  FImagePropName := 'Image';
  FVisible := True;
  FControl := aControl;
end;

destructor TNVImageListLink.Destroy;
begin
  if FImages <> nil then
    FImages.DeleteLink(Self);
  inherited;
end;

function TNVImageListLink.IsValidImage: Boolean;
begin
  Result := (Images <> nil) and (ImageIndex <> -1) and (ImageIndex < Images.Count);
end;

function TNVImageListLink.IsVisible: Boolean;
begin
  Result:= FVisible and IsValidImage;
end;

function TNVImageListLink.Render: string;
begin
  if IsVisible then
    Result := Images.Render(ImageIndex, Width, Height)
  else
    Result := '';
end;

procedure TNVImageListLink.SetHeight(const Value: string);
begin
  if FHeight <> Value then
    begin
      FHeight := Value;
      Change;
    end;
end;

procedure TNVImageListLink.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
    begin
      FImageIndex := Value;
      if not IgnoreIndex then
        Change;
    end;
end;

procedure TNVImageListLink.SetImages(const Value: TNvCustomImageList);
begin
  if Value <> FImages then
    begin
      if FImages <> nil then
        FImages.DeleteLink(Self);
      if Value <> nil then
        Value.AddLink(Self);
      if not IgnoreImages then
        Change;
    end;
end;

procedure TNVImageListLink.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
    begin
      FVisible := Value;
      Change;
    end;
end;

procedure TNVImageListLink.SetWidth(const Value: string);
begin
  if FWidth <> Value then
    begin
      FWidth := Value;
      Change;
    end;
end;

end.
