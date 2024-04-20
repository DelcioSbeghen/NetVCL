unit NV.BS.Controls;

{ TODO -oDelcio -cBootstrap : Include .bg-transparent in alll backgrounds }

interface

uses
  Classes, Controls, NV.Controls, NV.Json, NV.BS.Types, NV.Ajax, NV.Interfaces;

type

  TBSGrids = class;

  TBSGridOptions = class;

  TBsMargins = class(TNvSubProperty)
  private
    FGridOption: TBSGridOptions;
    FEnd       : TBsSpacing;
    FBottom    : TBsSpacing;
    FX         : TBsSpacing;
    FY         : TBsSpacing;
    FAll       : TBsSpacing;
    FTop       : TBsSpacing;
    FStart     : TBsSpacing;
    procedure SetAll(const Value: TBsSpacing);
    procedure SetBottom(const Value: TBsSpacing);
    procedure SetStart(const Value: TBsSpacing);
    procedure SetEnd(const Value: TBsSpacing);
    procedure SetTop(const Value: TBsSpacing);
    procedure SetX(const Value: TBsSpacing);
    procedure SetY(const Value: TBsSpacing);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
    procedure RenderAll(aJson: TJsonObject);
    procedure RenderBottom(aJson: TJsonObject);
    procedure RenderStart(aJson: TJsonObject);
    procedure RenderEnd(aJson: TJsonObject);
    procedure RenderTop(aJson: TJsonObject);
    procedure RenderX(aJson: TJsonObject);
    procedure RenderY(aJson: TJsonObject);
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
  published
    property All   : TBsSpacing read FAll write SetAll default bssNull;
    property X     : TBsSpacing read FX write SetX default bssNull;
    property Y     : TBsSpacing read FY write SetY default bssNull;
    property Top   : TBsSpacing read FTop write SetTop default bssNull;
    property Start : TBsSpacing read FStart write SetStart default bssNull;
    property End_  : TBsSpacing read FEnd write SetEnd default bssNull;
    property Bottom: TBsSpacing read FBottom write SetBottom default bssNull;
  end;

  TBSGridOptions = class(TNvSubProperty)
  private
    FDisplay       : TBSDisplay;
    FDirection     : TBSDirection;
    FGrow          : Boolean;
    FPaddings      : TBsMargins;
    FAlignItems    : TBSAlign;
    FJustifyContent: TBSJustifyContent;
    FOrder         : TBs12Range;
    FMargins       : TBsMargins;
    FShrink        : Boolean;
    FWrap          : TBSWrap;
    FAlignSelf     : TBSAlign;
    FFill          : Boolean;
    FAlignContent  : TBsAlignContent;
    FFloat         : TBsFloat;
    FOffset        : TBs12Range;
    FSpan          : TBs12Range;
    FHeight        : string;
    FViewportPos   : TBsViewportPos;
    procedure SetAlignContent(const Value: TBsAlignContent);
    procedure SetAlignItems(const Value: TBSAlign);
    procedure SetAlignSelf(const Value: TBSAlign);
    procedure SetDirection(const Value: TBSDirection);
    procedure SetDisplay(const Value: TBSDisplay);
    procedure SetFill(const Value: Boolean);
    procedure SetFloat(const Value: TBsFloat);
    procedure SetGrow(const Value: Boolean);
    procedure SetJustifyContent(const Value: TBSJustifyContent);
    procedure SetMargins(const Value: TBsMargins);
    procedure SetOffset(const Value: TBs12Range);
    procedure SetOrder(const Value: TBs12Range);
    procedure SetPaddings(const Value: TBsMargins);
    procedure SetShrink(const Value: Boolean);
    procedure SetSpan(const Value: TBs12Range);
    procedure SetWrap(const Value: TBSWrap);
    procedure SetHeight(const Value: string);
    procedure SetViewportPos(const Value: TBsViewportPos);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    // render
    procedure InternalRender(aJson: TJsonObject); override;
    procedure RenderHeight(aJson: TJsonObject);
    procedure RenderViewportPos(aJson: TJsonObject);
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
    destructor Destroy; override;
    function GetClassString(ACustomXsOffset, ACustomSmOffset, ACustomMdOffset,
      ACustomLgOffset: integer): string; overload;
    function GetClassString: string; overload;
    procedure Clear;
  published
    // https://getbootstrap.com/docs/5.3/layout/columns/
    property Span: TBs12Range read FSpan write SetSpan default -1;
    // https://getbootstrap.com/docs/5.3/layout/columns/#offset-classes
    property Offset: TBs12Range read FOffset write SetOffset default -1;
    // https://getbootstrap.com/docs/5.3/utilities/display/
    property Display: TBSDisplay read FDisplay write SetDisplay default bsdNull;
    {
      ****** FLEX NOTE *********
      Flexbox properties only work if display or parent display is Flex
      see https://getbootstrap.com/docs/5.3/utilities/flex/#enable-flex-behaviors
    }
    // https://getbootstrap.com/docs/5.3/utilities/flex/#direction
    property Direction: TBSDirection read FDirection write SetDirection default bsDirNull;
    { TODO -oDelcio -cGridOptions : !!!! Change names to more simple same as bootstrap documentation titles "JustifyContent" to  "HorizontalAlignment" !!!! }
    // https://getbootstrap.com/docs/5.3/utilities/flex/#justify-content
    property JustifyContent: TBSJustifyContent read FJustifyContent write SetJustifyContent
      default bsJustNull;
    // https://getbootstrap.com/docs/5.3/utilities/flex/#align-items
    property AlignItems: TBSAlign read FAlignItems write SetAlignItems default bsaNull;
    // https://getbootstrap.com/docs/5.3/utilities/flex/#align-self
    property AlignSelf: TBSAlign read FAlignSelf write SetAlignSelf default bsaNull;
    // https://getbootstrap.com/docs/5.3/utilities/flex/#fill
    property Fill: Boolean read FFill write SetFill default False;
    // https://getbootstrap.com/docs/5.3/utilities/flex/#grow-and-shrink
    property Grow  : Boolean read FGrow write SetGrow default False;
    property Shrink: Boolean read FShrink write SetShrink default False;
    // https://getbootstrap.com/docs/5.3/utilities/flex/#wrap
    property Wrap: TBSWrap read FWrap write SetWrap default bswNull;
    // https://getbootstrap.com/docs/5.3/utilities/flex/#order
    property Order: TBs12Range read FOrder write SetOrder default -1;
    // https://getbootstrap.com/docs/5.3/utilities/flex/#align-content
    property AlignContent: TBsAlignContent read FAlignContent write SetAlignContent
      default bsacNull;
    { ****** END FLEX NOTE ********* }
    // https://getbootstrap.com/docs/5.3/utilities/float/#overview
    property Float: TBsFloat read FFloat write SetFloat default bsfNull;
    // https://getbootstrap.com/docs/5.3/utilities/spacing/#margin-and-padding
    property Margins : TBsMargins read FMargins write SetMargins;
    property Paddings: TBsMargins read FPaddings write SetPaddings;
    // Heigh is not a bootstrap property, see nv.bs.css
    property Height: string read FHeight write SetHeight;
    // https://getbootstrap.com/docs/5.3/helpers/position/
    property ViewportPos: TBsViewportPos read FViewportPos write SetViewportPos default bsvppNull;
    // Note: ad new propreties to Clear and Assign methods
  end;

  TBSPrintOptions = class(TNvSubProperty)
  private
    FDisplay: TBSDisplay;
    procedure SetDisplay(const Value: TBSDisplay);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
  published
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
    property Display: TBSDisplay read FDisplay write SetDisplay default bsdNull;
  end;

  TBSGrids = class(TNvSubProperty)
  private
    FXS   : TBSGridOptions;
    FLG   : TBSGridOptions;
    FPrint: TBSPrintOptions;
    FMD   : TBSGridOptions;
    FXL   : TBSGridOptions;
    FSM   : TBSGridOptions;
    procedure SetLG(const Value: TBSGridOptions);
    procedure SetMD(const Value: TBSGridOptions);
    procedure SetPrint(const Value: TBSPrintOptions);
    procedure SetSM(const Value: TBSGridOptions);
    procedure SetXL(const Value: TBSGridOptions);
    procedure SetXS(const Value: TBSGridOptions);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
  published
    property XS   : TBSGridOptions read FXS write SetXS;
    property SM   : TBSGridOptions read FSM write SetSM;
    property MD   : TBSGridOptions read FMD write SetMD;
    property LG   : TBSGridOptions read FLG write SetLG;
    property XL   : TBSGridOptions read FXL write SetXL;
    property Print: TBSPrintOptions read FPrint write SetPrint;
  end;

  TBsBorders = class(TNvSubProperty)
  private
    FRoundSize: TBsRoundSize;
    FRight    : Boolean;
    FColor    : TBsBackground;
    FBottom   : Boolean;
    FVisible  : Boolean;
    FTop      : Boolean;
    FLeft     : Boolean;
    FRoundType: TBsRoundType;
    procedure SetBottom(const Value: Boolean);
    procedure SetColor(const Value: TBsBackground);
    procedure SetLeft(const Value: Boolean);
    procedure SetRight(const Value: Boolean);
    procedure SetRoundSize(const Value: TBsRoundSize);
    procedure SetRoundType(const Value: TBsRoundType);
    procedure SetTop(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
  published
    property Color    : TBsBackground read FColor write SetColor default bsbgSecondary;
    property Visible  : Boolean read FVisible write SetVisible default False;
    property Top      : Boolean read FTop write SetTop default True;
    property Right    : Boolean read FRight write SetRight default True;
    property Bottom   : Boolean read FBottom write SetBottom default True;
    property Left     : Boolean read FLeft write SetLeft default True;
    property RoundType: TBsRoundType read FRoundType write SetRoundType default bsrNone;
    property RoundSize: TBsRoundSize read FRoundSize write SetRoundSize default bsrsNormal;
  end;

  TBsTextProps = class(TNvSubProperty)
  private
    FAlign    : TBsTextAlign;
    FMonospace: Boolean;
    FTransform: TBsTextTransform;
    FColor    : TBsBackground;
    FAlignLG  : TBsTextAlign;
    FAlignMD  : TBsTextAlign;
    FWeight   : TBsTextWeight;
    FWrap     : TBsTextWrap;
    FAlignXL  : TBsTextAlign;
    FItalic   : Boolean;
    FAlignSM  : TBsTextAlign;
    procedure SetAlign(const Value: TBsTextAlign);
    procedure SetAlignLG(const Value: TBsTextAlign);
    procedure SetAlignMD(const Value: TBsTextAlign);
    procedure SetAlignSM(const Value: TBsTextAlign);
    procedure SetAlignXL(const Value: TBsTextAlign);
    procedure SetColor(const Value: TBsBackground);
    procedure SetItalic(const Value: Boolean);
    procedure SetMonospace(const Value: Boolean);
    procedure SetTransform(const Value: TBsTextTransform);
    procedure SetWeight(const Value: TBsTextWeight);
    procedure SetWrap(const Value: TBsTextWrap);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
  published
    property Color    : TBsBackground read FColor write SetColor default bsbgNone;
    property Align    : TBsTextAlign read FAlign write SetAlign default bstaNone;
    property AlignSM  : TBsTextAlign read FAlignSM write SetAlignSM default bstaNone;
    property AlignMD  : TBsTextAlign read FAlignMD write SetAlignMD default bstaNone;
    property AlignLG  : TBsTextAlign read FAlignLG write SetAlignLG default bstaNone;
    property AlignXL  : TBsTextAlign read FAlignXL write SetAlignXL default bstaNone;
    property Wrap     : TBsTextWrap read FWrap write SetWrap default bstwNone;
    property Transform: TBsTextTransform read FTransform write SetTransform default bsttNone;
    property Weight   : TBsTextWeight read FWeight write SetWeight default bsttwNormal;
    property Italic   : Boolean read FItalic write SetItalic default False;
    property Monospace: Boolean read FMonospace write SetMonospace default False;
  end;

  TNvBsPosition = class(TNvSubProperty)
  private
    FBottom  : TBsPosValue;
    FEnd_    : TBsPosValue;
    FStart   : TBsPosValue;
    FTop     : TBsPosValue;
    FPosition: TBsDispPosition;
    procedure SetBottom(const Value: TBsPosValue);
    procedure SetEnd_(const Value: TBsPosValue);
    procedure SetPosition(const Value: TBsDispPosition);
    procedure SetStart(const Value: TBsPosValue);
    procedure SetTop(const Value: TBsPosValue);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    //
    procedure InternalRender(aJson: TJsonObject); override;
    procedure RenderPosition(aJson: TJsonObject);
    procedure RenderBottom(aJson: TJsonObject);
    procedure RenderEnd(aJson: TJsonObject);
    procedure RenderStart(aJson: TJsonObject);
    procedure RenderTop(aJson: TJsonObject);
  published
    // https://getbootstrap.com/docs/5.3/utilities/position/
    property Position: TBsDispPosition read FPosition write SetPosition default bsdpNull;
    property Bottom  : TBsPosValue read FBottom write SetBottom default bspvNull;
    property End_    : TBsPosValue read FEnd_ write SetEnd_ default bspvNull;
    property Start   : TBsPosValue read FStart write SetStart default bspvNull;
    property Top     : TBsPosValue read FTop write SetTop default bspvNull;
  end;

  TNvBsControl = class(TNvControl)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TNvBsCustomControl = class(TNvBsControl)
  protected
    class function BackgroundDefault: TBsBackground; virtual;
  private
    FBackground: TBsBackground;
    FShadow    : TBsShadow;
    FBorder    : TBsBorders;
    FTextProps : TBsTextProps;
    FPosition  : TNvBsPosition;
    FWidth     : TBsWidth;
    procedure SetBackground(const Value: TBsBackground);
    procedure SetBorder(const Value: TBsBorders);
    procedure SetShadow(const Value: TBsShadow);
    procedure SetTextProps(const Value: TBsTextProps);
    procedure SetPosition(const Value: TNvBsPosition);
    procedure SetWidth(const Value: TBsWidth);
  protected
    function IsNotBgDefault: Boolean;
    // render
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderBackground(aJson: TJsonObject); dynamic;
    procedure RenderShadow(aJson: TJsonObject); dynamic;
    procedure RenderWidth(aJson: TJsonObject); dynamic;
    //
    property Background: TBsBackground read FBackground write SetBackground stored IsNotBgDefault;
    property Border: TBsBorders read FBorder write SetBorder;
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

  TNvBsGridControl = class(TNvBsCustomControl)
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

  TNvBsWinControl = class(TNvWinControl)
  protected
    // procedure SortControls; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TNVBootstrapSortMethod = (bsrmSortYX, bsrmSortXY);

const
  TBsSpacingStr: array [Low(TBsSpacing) .. High(TBsSpacing)] of string = //
    ('null', 'auto', '0', '1', '2', '3', '4', '5');

var
  // configurations for Grid Render Order
  gNvBoottrapSortMethod   : TNVBootstrapSortMethod = bsrmSortYX;
  gNvBoottrapSortPrecision: integer                = 12;

function BootstrapSort(AItem1: Pointer; AItem2: Pointer): integer;

implementation

function BootstrapSort(AItem1: Pointer; AItem2: Pointer): integer;
var
  LTop1, LLeft1, LTop2, LLeft2, LIdx1, LIdx2: integer;
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

  if gNvBoottrapSortMethod = bsrmSortYX then
    begin
      Result := LTop1 - LTop2;
      if Abs(Result) < gNvBoottrapSortPrecision then
        Result := LLeft1 - LLeft2;
    end
  else
    begin
      Result := LLeft1 - LLeft2;
      if Abs(Result) < gNvBoottrapSortPrecision then
        Result := LTop1 - LTop2;
    end;

  if Result = 0 then
    Result := LIdx1 - LIdx2;

end;

{ TNvBsWinControl }

constructor TNvBsWinControl.Create(AOwner: TComponent);
begin
  inherited;
  FRenderPosition := False;
end;

// procedure TNvBsWinControl.SortControls;
// begin
// inherited;
// //FControlsOrdered.Sort(BootstrapSort);
// end;

{ TNvBsControl }

constructor TNvBsControl.Create(AOwner: TComponent);
begin
  inherited;
  FRenderPosition := False;
end;

{ TBSGridOptions }

procedure TBSGridOptions.AssignTo(Dest: TPersistent);
begin
  if (Dest <> nil) and (Dest is TBSGridOptions) then
    with TBSGridOptions(Dest) do
      begin
        FMargins.Assign(Self.FMargins);
        FPaddings.Assign(Self.FPaddings);
        Span           := Self.FSpan;
        Offset         := Self.FOffset;
        Display        := Self.FDisplay;
        Direction      := Self.FDirection;
        JustifyContent := Self.FJustifyContent;
        AlignItems     := Self.FAlignItems;
        AlignSelf      := Self.FAlignSelf;
        Fill           := Self.FFill;
        Grow           := Self.FGrow;
        Shrink         := Self.FShrink;
        Wrap           := Self.FWrap;
        Order          := Self.FOrder;
        AlignContent   := Self.FAlignContent;
        Float          := Self.FFloat;
        Height         := Self.FHeight;
        ViewportPos    := Self.FViewportPos;
      end
  else
    inherited;
end;

procedure TBSGridOptions.Clear;
begin
  FMargins.Clear;
  FPaddings.Clear;
  Span           := -1;
  Offset         := -1;
  Display        := bsdNull;
  Direction      := bsDirNull;
  JustifyContent := bsJustNull;
  AlignItems     := bsaNull;
  AlignSelf      := bsaNull;
  Fill           := False;
  Grow           := False;
  Shrink         := False;
  Wrap           := bswNull;
  Order          := -1;
  AlignContent   := bsacNull;
  Float          := bsfNull;
  Height         := '';
  ViewportPos    := bsvppNull;
end;

constructor TBSGridOptions.Create;
begin
  inherited;
  FMargins        := TBsMargins.Create(Self, 'Margins');
  FPaddings       := TBsMargins.Create(Self, 'Paddings');
  FSpan           := -1;
  FOffset         := -1;
  FDisplay        := bsdNull;
  FDirection      := bsDirNull;
  FJustifyContent := bsJustNull;
  FAlignItems     := bsaNull;
  FAlignSelf      := bsaNull;
  FFill           := False;
  FGrow           := False;
  FShrink         := False;
  FWrap           := bswNull;
  FOrder          := -1;
  FAlignContent   := bsacNull;
  FFloat          := bsfNull;
end;

destructor TBSGridOptions.Destroy;
begin
  FMargins.Free;
  FPaddings.Free;
  inherited;
end;

function TBSGridOptions.GetClassString(ACustomXsOffset, ACustomSmOffset, ACustomMdOffset,
  ACustomLgOffset: integer): string;
begin

end;

function TBSGridOptions.GetClassString: string;
begin

end;

procedure TBSGridOptions.InternalRender;
begin
  FMargins.Render;
  FPaddings.Render;
  if FSpan > -1 then
    aJson.I['Span'] := FSpan;
  if FOffset > -1 then
    aJson.I['Offset'] := FOffset;
  if FDisplay <> bsdNull then
    aJson.S['Display'] := TBSDisplayStr[FDisplay];
  if FDirection <> bsDirNull then
    aJson.S['Direction'] := TBSDirectionStr[FDirection];
  if FJustifyContent <> bsJustNull then
    aJson.S['JustifyContent'] := TBSJustifyContentStr[FJustifyContent];
  if FAlignItems <> bsaNull then
    aJson.S['AlignItems'] := TBSAlignStr[FAlignItems];
  if FAlignSelf <> bsaNull then
    aJson.S['AlignSelf'] := TBSAlignStr[FAlignSelf];
  if FFill then
    aJson.B['Fill'] := FFill;
  if FGrow then
    aJson.B['Grow'] := FGrow;
  if FShrink then
    aJson.B['Shrink'] := FShrink;
  if FWrap <> bswNull then
    aJson.S['Wrap'] := TBSWrapStr[FWrap];
  if FOrder <> -1 then
    aJson.I['Order'] := FOrder;
  if FAlignContent <> bsacNull then
    aJson.S['AlignContent'] := TBsAlignContentStr[FAlignContent];
  if FFloat <> bsfNull then
    aJson.S['Float'] := TBsFloatStr[FFloat];
  if FHeight <> '' then
    RenderHeight(aJson);
  if FViewportPos <> bsvppNull then
    RenderViewportPos(aJson);
end;

procedure TBSGridOptions.RenderHeight(aJson: TJsonObject);
begin
  aJson.S['Height'] := FHeight;
end;

procedure TBSGridOptions.RenderViewportPos(aJson: TJsonObject);
begin
  aJson.S['ViewportPos'] := TBsViewportPosStr[FViewportPos];
end;

procedure TBSGridOptions.SetAlignContent(const Value: TBsAlignContent);
begin
  if FAlignContent <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['AlignContent'] := TBsAlignContentStr[Value];
      FAlignContent                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetAlignItems(const Value: TBSAlign);
begin
  if FAlignItems <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['AlignItems'] := TBSAlignStr[Value];
      FAlignItems                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetAlignSelf(const Value: TBSAlign);
begin
  if FAlignSelf <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['AlignSelf'] := TBSAlignStr[Value];
      FAlignSelf                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetDirection(const Value: TBSDirection);
begin
  if FDirection <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['Direction'] := TBSDirectionStr[Value];
      FDirection                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetDisplay(const Value: TBSDisplay);
begin
  if FDisplay <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['Display'] := TBSDisplayStr[Value];
      FDisplay                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetFill(const Value: Boolean);
begin
  if FFill <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['Fill'] := Value;
      FFill                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetFloat(const Value: TBsFloat);
begin
  if FFloat <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['Float'] := TBsFloatStr[Value];
      FFloat                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetGrow(const Value: Boolean);
begin
  if FGrow <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['Grow'] := Value;
      FGrow                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetHeight(const Value: string);
begin
  if FHeight <> Value then
    begin
      EnqueueChange('Height', RenderHeight);
      FHeight := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetJustifyContent(const Value: TBSJustifyContent);
begin
  if FJustifyContent <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['JustifyContent'] := TBSJustifyContentStr[Value];
      FJustifyContent                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetMargins(const Value: TBsMargins);
begin
  FMargins := Value;
end;

procedure TBSGridOptions.SetOffset(const Value: TBs12Range);
begin
  if FOffset <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['Offset'] := Value;
      FOffset                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetOrder(const Value: TBs12Range);
begin
  if FOrder <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['Order'] := Value;
      FOrder                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetPaddings(const Value: TBsMargins);
begin
  FPaddings := Value;
end;

procedure TBSGridOptions.SetShrink(const Value: Boolean);
begin
  if FShrink <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['Shrink'] := Value;
      FShrink                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetSpan(const Value: TBs12Range);
begin
  if FSpan <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['Span'] := Value;
      FSpan                       := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetViewportPos(const Value: TBsViewportPos);
begin
  if FViewportPos <> Value then
    begin
      EnqueueChange('ViewportPos', RenderViewportPos);
      FViewportPos := Value;
      Invalidate;
    end;
end;

procedure TBSGridOptions.SetWrap(const Value: TBSWrap);
begin
  if FWrap <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['Wrap'] := TBSWrapStr[Value];
      FWrap                       := Value;
      Invalidate;
    end;
end;

{ TBsMargins }

procedure TBsMargins.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TBsMargins) then
    begin
      FGridOption := TBsMargins(Source).FGridOption;
      End_        := TBsMargins(Source).End_;
      Bottom      := TBsMargins(Source).Bottom;
      X           := TBsMargins(Source).X;
      Y           := TBsMargins(Source).Y;
      All         := TBsMargins(Source).All;
      Top         := TBsMargins(Source).Top;
      Start       := TBsMargins(Source).Start;
    end
  else
    inherited;
end;

procedure TBsMargins.Clear;
begin
  End_   := bssNull;
  Bottom := bssNull;
  X      := bssNull;
  Y      := bssNull;
  All    := bssNull;
  Top    := bssNull;
  Start  := bssNull;
end;

constructor TBsMargins.Create;
begin
  inherited;
  FEnd    := bssNull;
  FBottom := bssNull;
  FX      := bssNull;
  FY      := bssNull;
  FAll    := bssNull;
  FTop    := bssNull;
  FStart  := bssNull;
end;

procedure TBsMargins.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FAll <> bssNull then
    RenderAll(aJson);
  if FBottom <> bssNull then
    RenderBottom(aJson);
  if FStart <> bssNull then
    RenderStart(aJson);
  if FEnd <> bssNull then
    RenderEnd(aJson);
  if FTop <> bssNull then
    RenderTop(aJson);
  if FX <> bssNull then
    RenderX(aJson);
  if FY <> bssNull then
    RenderY(aJson);
end;

procedure TBsMargins.RenderAll(aJson: TJsonObject);
begin
  aJson.S['All'] := TBsSpacingStr[FAll];
end;

procedure TBsMargins.RenderBottom(aJson: TJsonObject);
begin
  aJson.S['Bottom'] := TBsSpacingStr[FBottom];
end;

procedure TBsMargins.RenderEnd(aJson: TJsonObject);
begin
  aJson.S['End'] := TBsSpacingStr[FEnd];
end;

procedure TBsMargins.RenderStart(aJson: TJsonObject);
begin
  aJson.S['Start'] := TBsSpacingStr[FStart];
end;

procedure TBsMargins.RenderTop(aJson: TJsonObject);
begin
  aJson.S['Top'] := TBsSpacingStr[FTop];
end;

procedure TBsMargins.RenderX(aJson: TJsonObject);
begin
  aJson.S['X'] := TBsSpacingStr[FX];
end;

procedure TBsMargins.RenderY(aJson: TJsonObject);
begin
  aJson.S['Y'] := TBsSpacingStr[FY];
end;

procedure TBsMargins.SetAll(const Value: TBsSpacing);
begin
  if Value <> FAll then
    begin
      EnqueueChange('All', RenderAll);
      FAll := Value;
      Invalidate;
    end;
end;

procedure TBsMargins.SetBottom(const Value: TBsSpacing);
begin
  if Value <> FBottom then
    begin
      EnqueueChange('Bottom', RenderBottom);
      FBottom := Value;
      Invalidate;
    end;
end;

procedure TBsMargins.SetStart(const Value: TBsSpacing);
begin
  if Value <> FStart then
    begin
      EnqueueChange('Start', RenderStart);
      FStart := Value;
      Invalidate;
    end;
end;

procedure TBsMargins.SetEnd(const Value: TBsSpacing);
begin
  if Value <> FEnd then
    begin
      EnqueueChange('End', RenderEnd);
      FEnd := Value;
      Invalidate;
    end;
end;

procedure TBsMargins.SetTop(const Value: TBsSpacing);
begin
  if Value <> FTop then
    begin
      EnqueueChange('Top', RenderTop);
      FTop := Value;
      Invalidate;
    end;
end;

procedure TBsMargins.SetX(const Value: TBsSpacing);
begin
  if Value <> FX then
    begin
      EnqueueChange('X', RenderX);
      FX := Value;
      Invalidate;
    end;
end;

procedure TBsMargins.SetY(const Value: TBsSpacing);
begin
  if Value <> FY then
    begin
      EnqueueChange('Y', RenderY);
      FY := Value;
      Invalidate;
    end;
end;

{ TBSGrids }

procedure TBSGrids.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TBSGrids) then
    begin
      FXS.Assign(TBSGrids(Source).FXS);
      FLG.Assign(TBSGrids(Source).FLG);
      FPrint.Assign(TBSGrids(Source).FPrint);
      FMD.Assign(TBSGrids(Source).FMD);
      FXL.Assign(TBSGrids(Source).FXL);
      FSM.Assign(TBSGrids(Source).FSM);
    end
  else
    inherited;
end;

procedure TBSGrids.Clear;
begin
  FXS.Clear;
  FLG.Clear;
  FPrint.Clear;
  FMD.Clear;
  FXL.Clear;
  FSM.Clear;
end;

constructor TBSGrids.Create;
begin
  inherited;
  FXS    := TBSGridOptions.Create(Self, 'XS');
  FLG    := TBSGridOptions.Create(Self, 'LG');
  FPrint := TBSPrintOptions.Create(Self, 'Print');
  FMD    := TBSGridOptions.Create(Self, 'MD');
  FXL    := TBSGridOptions.Create(Self, 'XL');
  FSM    := TBSGridOptions.Create(Self, 'SM');
end;

destructor TBSGrids.Destroy;
begin
  FXS.Free;
  FLG.Free;
  FPrint.Free;
  FMD.Free;
  FXL.Free;
  FSM.Free;
  inherited;
end;

procedure TBSGrids.InternalRender(aJson: TJsonObject);
begin
  inherited;
  FXS.Render;
  FSM.Render;
  FMD.Render;
  FLG.Render;
  FXL.Render;
  FPrint.Render;
end;

procedure TBSGrids.SetLG(const Value: TBSGridOptions);
begin
  if Value <> FLG then
    FLG.Assign(Value);
end;

procedure TBSGrids.SetMD(const Value: TBSGridOptions);
begin
  if Value <> FMD then
    FMD.Assign(Value);
end;

procedure TBSGrids.SetPrint(const Value: TBSPrintOptions);
begin
  if Value <> FPrint then
    FPrint.Assign(Value);
end;

procedure TBSGrids.SetSM(const Value: TBSGridOptions);
begin
  if Value <> FSM then
    FSM.Assign(Value);
end;

procedure TBSGrids.SetXL(const Value: TBSGridOptions);
begin
  if Value <> FXL then
    FXL.Assign(Value);
end;

procedure TBSGrids.SetXS(const Value: TBSGridOptions);
begin
  if Value <> FXS then
    FXS.Assign(Value);
end;

{ TBSPrintOptions }

procedure TBSPrintOptions.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TBSPrintOptions) then
    begin
      // FGrid   := TBSPrintOptions(Source).FGrid;
      Display := TBSPrintOptions(Source).FDisplay;
    end
  else
    inherited;
end;

procedure TBSPrintOptions.Clear;
begin

end;

constructor TBSPrintOptions.Create;
begin
  inherited;
  FDisplay := bsdNull;
end;

procedure TBSPrintOptions.InternalRender;
begin

end;

procedure TBSPrintOptions.SetDisplay(const Value: TBSDisplay);
begin
  FDisplay := Value;
end;

{ TNvBsCustomControl }

class function TNvBsCustomControl.BackgroundDefault: TBsBackground;
begin
  Result := bsbgNone;
end;

constructor TNvBsCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  FBackground := BackgroundDefault;
  FBorder     := TBsBorders.Create(Self { as TNvBsCustomControl } , 'Border');
  FTextProps  := TBsTextProps.Create(Self { as TNvBsCustomControl } , 'TextProps');
  FPosition   := TNvBsPosition.Create(Self { as TNvBsCustomControl } , 'Position');
end;

destructor TNvBsCustomControl.Destroy;
begin
  FBorder.Free;
  FTextProps.Free;
  FPosition.Free;
  inherited;
end;

procedure TNvBsCustomControl.InternalRender(Json: TJsonObject);
begin
  inherited;
  if IsNotBgDefault then
    RenderBackground(Json);
  if FShadow <> bssNone then
    RenderShadow(Json);
  if FWidth <> bswdNull then
    RenderWidth(Json);
  FBorder.Render;
  FTextProps.Render;
  FPosition.Render;
end;

function TNvBsCustomControl.IsNotBgDefault: Boolean;
begin
  Result := FBackground <> BackgroundDefault
end;

procedure TNvBsCustomControl.RenderBackground(aJson: TJsonObject);
begin
  aJson.S['Background'] := TBsBackgroundStr[FBackground];
end;

procedure TNvBsCustomControl.RenderShadow(aJson: TJsonObject);
begin
  aJson.S['Shadow'] := TBsShadowStr[FShadow];
end;

procedure TNvBsCustomControl.RenderWidth(aJson: TJsonObject);
begin
  aJson.S['Width_'] := TBsWidthStr[FWidth];
end;

procedure TNvBsCustomControl.SetBackground(const Value: TBsBackground);
begin
  if Value <> FBackground then
    begin
      EnqueueChange('Background', RenderBackground);
      FBackground := Value;
      Invalidate;
    end;
end;

procedure TNvBsCustomControl.SetBorder(const Value: TBsBorders);
begin
  FBorder := Value;
end;

procedure TNvBsCustomControl.SetPosition(const Value: TNvBsPosition);
begin
  if Value <> nil then
    FPosition.Assign(Value);
end;

procedure TNvBsCustomControl.SetShadow(const Value: TBsShadow);
begin
  if Value <> FShadow then
    begin
      EnqueueChange('Shadow', RenderShadow);
      FShadow := Value;
    end;
end;

procedure TNvBsCustomControl.SetTextProps(const Value: TBsTextProps);
begin
  FTextProps := Value;
end;

procedure TNvBsCustomControl.SetWidth(const Value: TBsWidth);
begin
  if Value <> FWidth then
    begin
      EnqueueChange('Width_', RenderWidth);
      FWidth := Value;
      Invalidate;
    end;
end;

{ TNvBsGridControl }

constructor TNvBsGridControl.Create(AOwner: TComponent);
begin
  inherited;
  FGrids := TBSGrids.Create(Self, 'Grids');
end;

class function TNvBsGridControl.DefaultClassCss: string;
begin
  Result := 'col';
end;

destructor TNvBsGridControl.Destroy;
begin
  FGrids.Free;
  inherited;
end;

procedure TNvBsGridControl.InternalRender(Json: TJsonObject);
begin
  inherited;
  FGrids.Render;
end;

procedure TNvBsGridControl.SetGrids(const Value: TBSGrids);
begin
  if FGrids <> Value then
    FGrids.Assign(Value);
end;

{ TBsBorders }

constructor TBsBorders.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  FRoundSize := bsrsNormal;
  FTop       := True;
  FRight     := True;
  FBottom    := True;
  FLeft      := True;
  FColor     := bsbgSecondary;
end;

procedure TBsBorders.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if Not FVisible then
    begin
      if FColor <> bsbgNone then
        aJson.S['Color'] := TBsBackgroundStr[FColor];
      if Not FRight then
        aJson.B['Right'] := FRight;
      if Not FTop then
        aJson.B['Top'] := FTop;
      if Not FLeft then
        aJson.B['Left'] := FLeft;
      if Not FBottom then
        aJson.B['Bottom'] := FBottom;

      if FRoundType <> bsrNone then
        aJson.S['RoundType'] := TBsRoundTypeStr[FRoundType];
      if FRoundSize <> bsrsNormal then
        aJson.S['RoundSize'] := TBsRoundSizeStr[FRoundSize];
    end
  else
    aJson.Clear;
end;

procedure TBsBorders.SetBottom(const Value: Boolean);
begin
  FBottom := Value;
end;

procedure TBsBorders.SetColor(const Value: TBsBackground);
begin
  FColor := Value;
end;

procedure TBsBorders.SetLeft(const Value: Boolean);
begin
  FLeft := Value;
end;

procedure TBsBorders.SetRight(const Value: Boolean);
begin
  FRight := Value;
end;

procedure TBsBorders.SetRoundSize(const Value: TBsRoundSize);
begin
  FRoundSize := Value;
end;

procedure TBsBorders.SetRoundType(const Value: TBsRoundType);
begin
  FRoundType := Value;
end;

procedure TBsBorders.SetTop(const Value: Boolean);
begin
  FTop := Value;
end;

procedure TBsBorders.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

{ TBsTextProps }

constructor TBsTextProps.Create;
begin
  inherited;
  FWeight := bsttwNormal;
end;

procedure TBsTextProps.InternalRender;
begin
  if // Default Values
    (FColor = bsbgNone) and (FAlign = bstaNone) and (FAlignSM = bstaNone) and (FAlignMD = bstaNone)
    and (FAlignLG = bstaNone) and (FAlignXL = bstaNone) and (FWrap = bstwNone) and
    (FTransform = bsttNone) and (FWeight = bsttwNormal) and (FItalic = False) and
    (FMonospace = False) then
    Exit;

  if FColor <> bsbgNone then
    aJson.S['Color'] := TBsBackgroundStr[FColor];
  if FAlign <> bstaNone then
    aJson.S['Align'] := TBsTextAlignStr[FAlign];
  if FAlignSM <> bstaNone then
    aJson.S['AlignSM'] := TBsTextAlignStr[FAlignSM];
  if FAlignMD <> bstaNone then
    aJson.S['AlignMD'] := TBsTextAlignStr[FAlignMD];
  if FAlignLG <> bstaNone then
    aJson.S['AlignLG'] := TBsTextAlignStr[FAlignLG];
  if FAlignXL <> bstaNone then
    aJson.S['AlignXL'] := TBsTextAlignStr[FAlignXL];
  if FWrap <> bstwNone then
    aJson.S['Wrap'] := TBsTextWrapStr[FWrap];
  if FTransform <> bsttNone then
    aJson.S['Wrap'] := TBsTextTransformStr[FTransform];
  if FWeight <> bsttwNormal then
    aJson.S['Weight'] := TBsTextWeightStr[FWeight];
end;

procedure TBsTextProps.SetAlign(const Value: TBsTextAlign);
begin
  FAlign := Value;
end;

procedure TBsTextProps.SetAlignLG(const Value: TBsTextAlign);
begin
  FAlignLG := Value;
end;

procedure TBsTextProps.SetAlignMD(const Value: TBsTextAlign);
begin
  FAlignMD := Value;
end;

procedure TBsTextProps.SetAlignSM(const Value: TBsTextAlign);
begin
  FAlignSM := Value;
end;

procedure TBsTextProps.SetAlignXL(const Value: TBsTextAlign);
begin
  FAlignXL := Value;
end;

procedure TBsTextProps.SetColor(const Value: TBsBackground);
begin
  FColor := Value;
end;

procedure TBsTextProps.SetItalic(const Value: Boolean);
begin
  FItalic := Value;
end;

procedure TBsTextProps.SetMonospace(const Value: Boolean);
begin
  FMonospace := Value;
end;

procedure TBsTextProps.SetTransform(const Value: TBsTextTransform);
begin
  FTransform := Value;
end;

procedure TBsTextProps.SetWeight(const Value: TBsTextWeight);
begin
  FWeight := Value;
end;

procedure TBsTextProps.SetWrap(const Value: TBsTextWrap);
begin
  FWrap := Value;
end;

{ TNvBsPosition }

procedure TNvBsPosition.AssignTo(Dest: TPersistent);
begin
  if (Dest <> nil) and (Dest is TNvBsPosition) then
    with TNvBsPosition(Dest) do
      begin
        Bottom   := Self.Bottom;
        End_     := Self.End_;
        Start    := Self.Start;
        Top      := Self.Top;
        Position := Self.Position;
      end
  else
    inherited;

end;

procedure TNvBsPosition.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FPosition <> bsdpNull then
    RenderPosition(aJson);
  if FBottom <> bspvNull then
    RenderBottom(aJson);
  if FEnd_ <> bspvNull then
    RenderEnd(aJson);
  if FStart <> bspvNull then
    RenderStart(aJson);
  if FTop <> bspvNull then
    RenderTop(aJson);
end;

procedure TNvBsPosition.RenderBottom(aJson: TJsonObject);
begin
  aJson.S['Bottom'] := TBsPosValueStr[FBottom];
end;

procedure TNvBsPosition.RenderEnd(aJson: TJsonObject);
begin
  aJson.S['End'] := TBsPosValueStr[FEnd_];
end;

procedure TNvBsPosition.RenderPosition(aJson: TJsonObject);
begin
  aJson.S['Position'] := TBsDispPositionStr[FPosition];
end;

procedure TNvBsPosition.RenderStart(aJson: TJsonObject);
begin
  aJson.S['Start'] := TBsPosValueStr[FStart];
end;

procedure TNvBsPosition.RenderTop(aJson: TJsonObject);
begin
  aJson.S['Top'] := TBsPosValueStr[FTop];
end;

procedure TNvBsPosition.SetBottom(const Value: TBsPosValue);
begin
  if Value <> FBottom then
    begin
      EnqueueChange('Bottom', RenderBottom);
      FBottom := Value;
      Invalidate;
    end;
end;

procedure TNvBsPosition.SetEnd_(const Value: TBsPosValue);
begin
  if Value <> FEnd_ then
    begin
      EnqueueChange('End', RenderEnd);
      FEnd_ := Value;
      Invalidate;
    end;
end;

procedure TNvBsPosition.SetPosition(const Value: TBsDispPosition);
begin
  if Value <> FPosition then
    begin
      EnqueueChange('Position', RenderPosition);
      FPosition := Value;
      Invalidate;
    end;
end;

procedure TNvBsPosition.SetStart(const Value: TBsPosValue);
begin
  if Value <> FStart then
    begin
      EnqueueChange('Start', RenderStart);
      FStart := Value;
      Invalidate;
    end;
end;

procedure TNvBsPosition.SetTop(const Value: TBsPosValue);
begin
  if Value <> FTop then
    begin
      EnqueueChange('Top', RenderTop);
      FTop := Value;
      Invalidate;
    end;
end;

end.
