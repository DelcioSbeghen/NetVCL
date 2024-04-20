unit NV.BS.Containers;

interface

uses
  Classes, NV.BS.Controls, NV.Ajax, NV.Json, NV.BS.Types;

type

  TNvBsContainer = class(TNvBsWinControl)
  protected
    class function BackgroundDefault: TBsBackground; virtual;
    class function FadeDefault: Boolean; virtual;
  private
    FBackground: TBsBackground;
    FShadow    : TBsShadow;
    FBorder    : TBsBorders;
    FTextProps : TBsTextProps;
    FFade      : Boolean;
    FPosition  : TNvBsPosition;
    FWidth     : TBsWidth;
    procedure SetBackground(const Value: TBsBackground);
    procedure SetBorder(const Value: TBsBorders);
    procedure SetShadow(const Value: TBsShadow);
    procedure SetTextProps(const Value: TBsTextProps);
    procedure SetFade(const Value: Boolean);
    procedure SetPosition(const Value: TNvBsPosition);
    procedure SetWidth(const Value: TBsWidth);
  protected
    procedure AddIncludes; override;
    function IsNotBgDefault: Boolean;
    function IsNotFadeDefault: Boolean;
    // render
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderBackground(aJson: TJsonObject); dynamic;
    procedure RenderFade(aJson: TJsonObject); dynamic;
    procedure RenderShadow(aJson: TJsonObject); dynamic;
    procedure RenderWidth(aJson: TJsonObject); dynamic;
    //
    property Background: TBsBackground read FBackground write SetBackground stored IsNotBgDefault;
    property Border: TBsBorders read FBorder write SetBorder;
    property Fade: Boolean read FFade write SetFade stored IsNotFadeDefault;
    // https://getbootstrap.com/docs/5.3/utilities/position/
    property Position: TNvBsPosition read FPosition write SetPosition;
    property Shadow: TBsShadow read FShadow write SetShadow default bssNone;
    property TextProps: TBsTextProps read FTextProps write SetTextProps;
    // https://getbootstrap.com/docs/5.3/utilities/sizing/#relative-to-the-parent
    property Width_: TBsWidth read FWidth write SetWidth default bswdNull;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TNvBsRow = class(TNVBsContainer)
    { TODO -oDelcio -cRows : Implement row-cols-xxx https://getbootstrap.com/docs/4.4/layout/grid/#row-columns }
  protected
    class function DefaultClassCss: string; override;
  published
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width_;
  end;

  TNvBsFormRow = class(TNVBsContainer)
  protected
    class function DefaultClassCss: string; override;
  published
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width_;
  end;

  TNvBsGridContainer = class(TNVBsContainer)
  protected
    class function DefaultClassCss: string; override;
  private
    FGrids: TBSGrids;
    procedure SetGrids(const Value: TBSGrids);
  protected
    procedure InternalRender(Json: TJsonObject); override;
    property Grids: TBSGrids read FGrids write SetGrids;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TNvBsColumn = class(TNvBsGridContainer)
  published
    property Grids;
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width_;
  end;

  TNvBsEmbed = class(TNVBsContainer)
  end;

  TNvBsListItem = class(TNvBsContainer)
  protected
    class function DefaultClassCss: string; override;
    class function DefaultHtmlTag: string; override;
  private
    FActive: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    // render
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderActive(aJson: TJsonObject);
  published
    // https://getbootstrap.com/docs/5.3/components/list-group/#active-items
    property Active: Boolean read FActive write SetActive;
    // https://getbootstrap.com/docs/5.3/components/list-group/#variants
    property Background;
    property ClassCss;
    // https://getbootstrap.com/docs/5.3/components/list-group/#disabled-items
    property Enabled;
  end;

  TNvBsListGroup = class(TNvBsGridContainer)
  protected
    class function DefaultClassCss: string; override;
    class function DefaultHtmlTag: string; override;
  private
    FFlush     : Boolean;
    FNumbered  : Boolean;
    FHorizontal: Boolean;
    procedure SetFlush(const Value: Boolean);
    procedure SetNumbered(const Value: Boolean);
    procedure SetHorizontal(const Value: Boolean);
  protected
    // render
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderFlush(aJson: TJsonObject);
    procedure RenderNumbered(aJson: TJsonObject);
    procedure RenderHorizontal(aJson: TJsonObject);
  published
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Flush: Boolean read FFlush write SetFlush;
    property Grids;
    property Horizontal: Boolean read FHorizontal write SetHorizontal;
    property Numbered  : Boolean read FNumbered write SetNumbered;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width_;
  end;

implementation

uses
  NV.VCL.Forms;

{ TNvBsRow }

{ TNvBsRow }

{ TNvBsGridContainer }

{ TNvBsRow }

class function TNvBsRow.DefaultClassCss: string;
begin
  Result := 'row';
end;

{ TNVBsContainer }

procedure TNVBsContainer.AddIncludes;
begin
  inherited;
  if Screen.Ajax <> nil then
    Screen.Ajax.AddInclude('nv.bs.containers.js', reqModule, '/nv.bs.containers.js');
end;

class function TNVBsContainer.BackgroundDefault: TBsBackground;
begin
  Result := bsbgNone;
end;

constructor TNVBsContainer.Create(AOwner: TComponent);
begin
  inherited;
  FBackground := BackgroundDefault;
  FFade       := FadeDefault;
  FBorder     := TBsBorders.Create(Self as TNvBsWinControl, 'Border');
  FTextProps  := TBsTextProps.Create(Self as TNvBsWinControl, 'TextProps');
  FPosition   := TNvBsPosition.Create(Self as TNvBsWinControl, 'Position')
end;

destructor TNVBsContainer.Destroy;
begin
  FBorder.Free;
  FTextProps.Free;
  FPosition.Free;
  inherited;
end;

class function TNVBsContainer.FadeDefault: Boolean;
begin
  Result := False;
end;

procedure TNVBsContainer.InternalRender(Json: TJsonObject);
begin
  inherited;
  if IsNotBgDefault then
    RenderBackground(Json);
  if IsNotFadeDefault then
    RenderFade(Json);
  if FShadow <> bssNone then
    RenderShadow(Json);
  if FWidth <> bswdNull then
    RenderWidth(Json);
  FPosition.Render;
end;

function TNVBsContainer.IsNotBgDefault: Boolean;
begin
  Result := FBackground <> BackgroundDefault;
end;

function TNVBsContainer.IsNotFadeDefault: Boolean;
begin
  Result := FFade <> FadeDefault;
end;

procedure TNVBsContainer.RenderBackground(aJson: TJsonObject);
begin
  aJson.S['Background'] := TBsBackgroundStr[FBackground];
end;

procedure TNVBsContainer.RenderFade(aJson: TJsonObject);
begin
  aJson.B['Fade'] := FFade;
end;

procedure TNvBsContainer.RenderShadow(aJson: TJsonObject);
begin
  aJson.S['Shadow'] := TBsShadowStr[FShadow];
end;

procedure TNvBsContainer.RenderWidth(aJson: TJsonObject);
begin
  aJson.S['Width_'] := TBsWidthStr[FWidth];
end;

procedure TNVBsContainer.SetBackground(const Value: TBsBackground);
begin
  if Value <> FBackground then
    begin
      EnqueueChange('Background', RenderBackground);
      FBackground := Value;
      Invalidate;
    end;
end;

procedure TNVBsContainer.SetBorder(const Value: TBsBorders);
begin
  FBorder := Value;
end;

procedure TNVBsContainer.SetFade(const Value: Boolean);
begin
  if Value <> FFade then
    begin
      EnqueueChange('Fade', RenderFade);
      FFade := Value;
      Invalidate;
    end;
end;

procedure TNVBsContainer.SetPosition(const Value: TNvBsPosition);
begin
  if Value <> nil then
    FPosition.Assign(Value);
end;

procedure TNVBsContainer.SetShadow(const Value: TBsShadow);
begin
  if Value <> FShadow then
    begin
      EnqueueChange('Shadow', RenderShadow);
      FShadow := Value;
    end;
end;

procedure TNVBsContainer.SetTextProps(const Value: TBsTextProps);
begin
  FTextProps := Value;
end;

procedure TNvBsContainer.SetWidth(const Value: TBsWidth);
begin
  if Value <> FWidth then
    begin
      EnqueueChange('Width_', RenderWidth);
      FWidth := Value;
      Invalidate;
    end;
end;

{ TNvBsGridContainer }

constructor TNvBsGridContainer.Create(AOwner: TComponent);
begin
  inherited;
  FGrids := TBSGrids.Create(Self, 'Grids');
end;

class function TNvBsGridContainer.DefaultClassCss: string;
begin
  Result := 'col';
end;

destructor TNvBsGridContainer.Destroy;
begin
  FGrids.Free;
  inherited;
end;

procedure TNvBsGridContainer.InternalRender(Json: TJsonObject);
begin
  inherited;
  FGrids.Render;
end;

procedure TNvBsGridContainer.SetGrids(const Value: TBSGrids);
begin
  if FGrids <> Value then
    FGrids.Assign(Value);
end;

{ TNvBsFormRow }

class function TNvBsFormRow.DefaultClassCss: string;
begin
  Result := 'form-row';
end;

{ TNvBsListGroup }

class function TNvBsListGroup.DefaultClassCss: string;
begin
  Result := 'list-group';
end;

class function TNvBsListGroup.DefaultHtmlTag: string;
begin
  Result := 'ul';
end;

procedure TNvBsListGroup.InternalRender(Json: TJsonObject);
begin
  inherited;
  if FFlush then
    RenderFlush(Json);
  if FHorizontal then
    RenderHorizontal(Json);
  if FNumbered then
    RenderNumbered(Json);
end;

procedure TNvBsListGroup.RenderFlush(aJson: TJsonObject);
begin
  aJson.B['Flush'] := FFlush;
end;

procedure TNvBsListGroup.RenderHorizontal(aJson: TJsonObject);
begin
  aJson.B['Horizontal'] := FHorizontal;
end;

procedure TNvBsListGroup.RenderNumbered(aJson: TJsonObject);
begin
  aJson.B['Numbered'] := FNumbered;
end;

procedure TNvBsListGroup.SetFlush(const Value: Boolean);
begin
  if Value <> FFlush then
    begin
      EnqueueChange('Flush', RenderFlush);
      FFlush := Value;
      Invalidate;
    end;
end;

procedure TNvBsListGroup.SetHorizontal(const Value: Boolean);
begin
  if Value <> FHorizontal then
    begin
      EnqueueChange('Horizontal', RenderHorizontal);
      FHorizontal := Value;
      Invalidate;
    end;
end;

procedure TNvBsListGroup.SetNumbered(const Value: Boolean);
begin
  if Value <> FNumbered then
    begin
      EnqueueChange('Numbered', RenderNumbered);
      FNumbered := Value;
      Invalidate;
    end;
end;

{ TNvBsListItem }

class function TNvBsListItem.DefaultClassCss: string;
begin
  Result := 'list-group-item';
end;

class function TNvBsListItem.DefaultHtmlTag: string;
begin
  Result := 'li';
end;

procedure TNvBsListItem.InternalRender(Json: TJsonObject);
begin
  inherited;
  if FActive then
    RenderActive(Json);
end;

procedure TNvBsListItem.RenderActive(aJson: TJsonObject);
begin
  aJson.B['Active'] := FActive;
end;

procedure TNvBsListItem.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
    begin
      EnqueueChange('Active', RenderActive);
      FActive := Value;
      Invalidate;
    end;
end;

end.
