unit NV.BS.Containers;

interface

uses
  Classes, NV.BS.Controls, NV.Ajax, NV.Json, NV.BS.Types;

type

  TNVBsContainer = class(TNvBsWinControl)
  protected
    class function BackgroundDefault: TBsBackground; virtual;
    class function FadeDefault: Boolean; virtual;
  private
    FBackground: TBsBackground;
    FShadow    : TBsShadow;
    FBorder    : TBsBorders;
    FTextProps : TBsTextProps;
    FFade      : Boolean;
    procedure SetBackground(const Value: TBsBackground);
    procedure SetBorder(const Value: TBsBorders);
    procedure SetShadow(const Value: TBsShadow);
    procedure SetTextProps(const Value: TBsTextProps);
    procedure SetFade(const Value: Boolean);
  protected
    procedure AddIncludes(Ajax: TNvAjax); override;
    function IsNotBgDefault: Boolean;
    function IsNotFadeDefault: Boolean;
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); override;
    property Background: TBsBackground read FBackground write SetBackground stored IsNotBgDefault;
    property Border: TBsBorders read FBorder write SetBorder;
    property Fade: Boolean read FFade write SetFade stored IsNotFadeDefault;
    property Shadow: TBsShadow read FShadow write SetShadow default bssNone;
    property TextProps: TBsTextProps read FTextProps write SetTextProps;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TNvBsRow = class(TNVBsContainer)
    { TODO -oDelcio -cRows : Implement row-cols-xxx https://getbootstrap.com/docs/4.4/layout/grid/#row-columns }
  published
    property Background stored IsNotBgDefault;
    property Border;
    property Shadow default bssNone;
    property TextProps;
  end;

  TNvBsFormRow = class(TNVBsContainer)
  published
    property Background stored IsNotBgDefault;
    property Border;
    property Shadow default bssNone;
    property TextProps;
  end;

  TNvBsGridContainer = class(TNVBsContainer)
  private
    FGrids: TBSGrids;
    procedure SetGrids(const Value: TBSGrids);
  protected
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); override;
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
    property Shadow default bssNone;
    property TextProps;
  end;

  TNvBsEmbed = class(TNVBsContainer)
  end;

implementation

{ TNvBsRow }

{ TNvBsRow }

{ TNvBsGridContainer }

{ TNVBsContainer }

procedure TNVBsContainer.AddIncludes(Ajax: TNvAjax);
begin
  inherited;
  if Ajax <> nil then
    Ajax.AddInclude('nv.bs.containers.js', reqModule, '/nv.bs.containers.js');
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
end;

destructor TNVBsContainer.Destroy;
begin
  FBorder.Free;
  FTextProps.Free;
  inherited;
end;

class function TNVBsContainer.FadeDefault: Boolean;
begin
  Result := False;
end;

procedure TNVBsContainer.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  inherited;
  if IsNotBgDefault then
    Json.S['Background'] := TBsBackgroundStr[FBackground];
  if IsNotFadeDefault then
    Json.B['Fade'] := FFade;
end;

function TNVBsContainer.IsNotBgDefault: Boolean;
begin
  Result := FBackground <> BackgroundDefault;
end;

function TNVBsContainer.IsNotFadeDefault: Boolean;
begin
  Result := FFade <> FadeDefault;
end;

procedure TNVBsContainer.SetBackground(const Value: TBsBackground);
begin
  if Value <> FBackground then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Background'] := TBsBackgroundStr[Value];
      FBackground                       := Value;
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
      if NeedSendChange then
        ControlAjaxJson.B['Fade'] := Value;
      FFade                       := Value;
      Invalidate;
    end;
end;

procedure TNVBsContainer.SetShadow(const Value: TBsShadow);
begin
  FShadow := Value;
end;

procedure TNVBsContainer.SetTextProps(const Value: TBsTextProps);
begin
  FTextProps := Value;
end;

{ TNvBsGridContainer }

constructor TNvBsGridContainer.Create(AOwner: TComponent);
begin
  inherited;
  FGrids := TBSGrids.Create(Self, 'Grids');
end;

destructor TNvBsGridContainer.Destroy;
begin
  FGrids.Free;
  inherited;
end;

procedure TNvBsGridContainer.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  inherited;
  FGrids.Render;
end;

procedure TNvBsGridContainer.SetGrids(const Value: TBSGrids);
begin
  if FGrids <> Value then
    FGrids.Assign(Value);
end;

end.
