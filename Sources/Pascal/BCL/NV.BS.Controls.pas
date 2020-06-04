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
    FRight     : TBsSpacing;
    FBottom    : TBsSpacing;
    FX         : TBsSpacing;
    FY         : TBsSpacing;
    FAll       : TBsSpacing;
    FTop       : TBsSpacing;
    FLeft      : TBsSpacing;
    procedure SetAll(const Value: TBsSpacing);
    procedure SetBottom(const Value: TBsSpacing);
    procedure SetLeft(const Value: TBsSpacing);
    procedure SetRight(const Value: TBsSpacing);
    procedure SetTop(const Value: TBsSpacing);
    procedure SetX(const Value: TBsSpacing);
    procedure SetY(const Value: TBsSpacing);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
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
    property Left  : TBsSpacing read FLeft write SetLeft default bssNull;
    property Right : TBsSpacing read FRight write SetRight default bssNull;
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
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetClassString(ACustomXsOffset, ACustomSmOffset, ACustomMdOffset,
      ACustomLgOffset: integer): string; overload;
    function GetClassString: string; overload;
    procedure Clear;
  published
    property Span     : TBs12Range read FSpan write SetSpan default -1;
    property Offset   : TBs12Range read FOffset write SetOffset default -1;
    property Display  : TBSDisplay read FDisplay write SetDisplay default bsdNull;
    property Direction: TBSDirection read FDirection write SetDirection default bsDirNull;
    { TODO -oDelcio -cGridOptions : !!!! Change names to more simple same as bootstrap documentation titles "JustifyContent" to  "HorizontalAlignment" !!!! }
    property JustifyContent: TBSJustifyContent read FJustifyContent write SetJustifyContent
      default bsJustNull;
    property AlignItems  : TBSAlign read FAlignItems write SetAlignItems default bsaNull;
    property AlignSelf   : TBSAlign read FAlignSelf write SetAlignSelf default bsaNull;
    property Fill        : Boolean read FFill write SetFill default False;
    property Grow        : Boolean read FGrow write SetGrow default False;
    property Shrink      : Boolean read FShrink write SetShrink default False;
    property Wrap        : TBSWrap read FWrap write SetWrap default bswNull;
    property Order       : TBs12Range read FOrder write SetOrder default -1;
    property AlignContent: TBsAlignContent read FAlignContent write SetAlignContent
      default bsacNull;
    property Float   : TBsFloat read FFloat write SetFloat default bsfNull;
    property Margins : TBsMargins read FMargins write SetMargins;
    property Paddings: TBsMargins read FPaddings write SetPaddings;
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
    FAutoMargin: TBSAutoMargin;
    FXS        : TBSGridOptions;
    FLG        : TBSGridOptions;
    FPrint     : TBSPrintOptions;
    FMD        : TBSGridOptions;
    FXL        : TBSGridOptions;
    FSM        : TBSGridOptions;
    procedure SetAutoMargin(const Value: TBSAutoMargin);
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
    property AutoMargin: TBSAutoMargin read FAutoMargin write SetAutoMargin default bsamNull;
    property XS        : TBSGridOptions read FXS write SetXS;
    property SM        : TBSGridOptions read FSM write SetSM;
    property MD        : TBSGridOptions read FMD write SetMD;
    property LG        : TBSGridOptions read FLG write SetLG;
    property XL        : TBSGridOptions read FXL write SetXL;
    property Print     : TBSPrintOptions read FPrint write SetPrint;
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
    procedure SetBackground(const Value: TBsBackground);
    procedure SetBorder(const Value: TBsBorders);
    procedure SetShadow(const Value: TBsShadow);
    procedure SetTextProps(const Value: TBsTextProps);
  protected
    function IsNotBgDefault: Boolean;
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); override;
    property Background: TBsBackground read FBackground write SetBackground stored IsNotBgDefault;
    property Border: TBsBorders read FBorder write SetBorder;
    property Shadow: TBsShadow read FShadow write SetShadow default bssNone;
    property TextProps: TBsTextProps read FTextProps write SetTextProps;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TNvBsGridControl = class(TNvBsCustomControl)
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

  TNvBsWinControl = class(TNvWinControl)
  protected
    procedure SortControls; override;
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

procedure TNvBsWinControl.SortControls;
begin
  // inherited;
  FControlsOrdered.Sort(BootstrapSort);
end;

{ TNvBsControl }

constructor TNvBsControl.Create(AOwner: TComponent);
begin
  inherited;
  FRenderPosition := False;
end;

{ TBSGridOptions }

procedure TBSGridOptions.Assign(Source: TPersistent);
begin
  inherited;
  if (Source <> nil) and (Source is TBSGridOptions) then
    begin
      FMargins.Assign(TBSGridOptions(Source).FMargins);
      FPaddings.Assign(TBSGridOptions(Source).FPaddings);
      Span           := TBSGridOptions(Source).FSpan;
      Offset         := TBSGridOptions(Source).FOffset;
      Display        := TBSGridOptions(Source).FDisplay;
      Direction      := TBSGridOptions(Source).FDirection;
      JustifyContent := TBSGridOptions(Source).FJustifyContent;
      AlignItems     := TBSGridOptions(Source).FAlignItems;
      AlignSelf      := TBSGridOptions(Source).FAlignSelf;
      Fill           := TBSGridOptions(Source).FFill;
      Grow           := TBSGridOptions(Source).FGrow;
      Shrink         := TBSGridOptions(Source).FShrink;
      Wrap           := TBSGridOptions(Source).FWrap;
      Order          := TBSGridOptions(Source).FOrder;
      AlignContent   := TBSGridOptions(Source).FAlignContent;
      Float          := TBSGridOptions(Source).FFloat;
    end;
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
  inherited;
  if (Source <> nil) and (Source is TBsMargins) then
    begin
      FGridOption := TBsMargins(Source).FGridOption;
      Right       := TBsMargins(Source).Right;
      Bottom      := TBsMargins(Source).Bottom;
      X           := TBsMargins(Source).X;
      Y           := TBsMargins(Source).Y;
      All         := TBsMargins(Source).All;
      Top         := TBsMargins(Source).Top;
      Left        := TBsMargins(Source).Left;
    end;
end;

procedure TBsMargins.Clear;
begin
  Right  := bssNull;
  Bottom := bssNull;
  X      := bssNull;
  Y      := bssNull;
  All    := bssNull;
  Top    := bssNull;
  Left   := bssNull;
end;

constructor TBsMargins.Create;
begin
  inherited;
  FRight  := bssNull;
  FBottom := bssNull;
  FX      := bssNull;
  FY      := bssNull;
  FAll    := bssNull;
  FTop    := bssNull;
  FLeft   := bssNull;
end;

procedure TBsMargins.InternalRender(aJson: TJsonObject);
begin
  inherited;

end;

procedure TBsMargins.SetAll(const Value: TBsSpacing);
begin
  FAll := Value;
end;

procedure TBsMargins.SetBottom(const Value: TBsSpacing);
begin
  FBottom := Value;
end;

procedure TBsMargins.SetLeft(const Value: TBsSpacing);
begin
  FLeft := Value;
end;

procedure TBsMargins.SetRight(const Value: TBsSpacing);
begin
  FRight := Value;
end;

procedure TBsMargins.SetTop(const Value: TBsSpacing);
begin
  FTop := Value;
end;

procedure TBsMargins.SetX(const Value: TBsSpacing);
begin
  FX := Value;
end;

procedure TBsMargins.SetY(const Value: TBsSpacing);
begin
  FY := Value;
end;

{ TBSGrids }

procedure TBSGrids.Assign(Source: TPersistent);
begin
  inherited;
  if (Source <> nil) and (Source is TBSGrids) then
    begin
      AutoMargin := TBSGrids(Source).FAutoMargin;
      FXS.Assign(TBSGrids(Source).FXS);
      FLG.Assign(TBSGrids(Source).FLG);
      FPrint.Assign(TBSGrids(Source).FPrint);
      FMD.Assign(TBSGrids(Source).FMD);
      FXL.Assign(TBSGrids(Source).FXL);
      FSM.Assign(TBSGrids(Source).FSM);
    end;
end;

procedure TBSGrids.Clear;
begin
  AutoMargin := bsamNull;
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
  FAutoMargin := bsamNull;
  FXS         := TBSGridOptions.Create(Self, 'XS');
  FLG         := TBSGridOptions.Create(Self, 'LG');
  FPrint      := TBSPrintOptions.Create(Self, 'Print');
  FMD         := TBSGridOptions.Create(Self, 'MD');
  FXL         := TBSGridOptions.Create(Self, 'XL');
  FSM         := TBSGridOptions.Create(Self, 'SM');
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
  if FAutoMargin <> bsamNull then
    aJson.S['AutoMargin'] := TBSAutoMarginStr[FAutoMargin];
  FXS.Render;
  FSM.Render;
  FMD.Render;
  FLG.Render;
  FXL.Render;
  FPrint.Render;
end;

procedure TBSGrids.SetAutoMargin(const Value: TBSAutoMargin);
begin
  if FAutoMargin <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['AutoMargin'] := TBSAutoMarginStr[Value];
      FAutoMargin                       := Value;
      Invalidate;
    end;
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
  inherited;
  if (Source <> nil) and (Source is TBSPrintOptions) then
    begin
      // FGrid   := TBSPrintOptions(Source).FGrid;
      Display := TBSPrintOptions(Source).FDisplay;
    end;
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
  FBorder     := TBsBorders.Create(Self as TNvBsCustomControl, 'Border');
  FTextProps  := TBsTextProps.Create(Self as TNvBsCustomControl, 'TextProps');
end;

destructor TNvBsCustomControl.Destroy;
begin
  FBorder.Free;
  FTextProps.Free;
  inherited;
end;

procedure TNvBsCustomControl.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  inherited;
  if IsNotBgDefault then
    Json.S['Background'] := TBsBackgroundStr[FBackground];
  if FShadow <> bssNone then
    Json.S['Shadow'] := TBsShadowStr[FShadow];
  FBorder.Render;
  FTextProps.Render;
end;

function TNvBsCustomControl.IsNotBgDefault: Boolean;
begin
  Result := FBackground <> BackgroundDefault
end;

procedure TNvBsCustomControl.SetBackground(const Value: TBsBackground);
begin
  if Value <> FBackground then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Background'] := TBsBackgroundStr[Value];
      FBackground                       := Value;
      Invalidate;
    end;
end;

procedure TNvBsCustomControl.SetBorder(const Value: TBsBorders);
begin
  FBorder := Value;
end;

procedure TNvBsCustomControl.SetShadow(const Value: TBsShadow);
begin
  FShadow := Value;
end;

procedure TNvBsCustomControl.SetTextProps(const Value: TBsTextProps);
begin
  FTextProps := Value;
end;

{ TNvBsGridControl }

constructor TNvBsGridControl.Create(AOwner: TComponent);
begin
  inherited;
  FGrids := TBSGrids.Create(Self, 'Grids');
end;

destructor TNvBsGridControl.Destroy;
begin
  FGrids.Free;
  inherited;
end;

procedure TNvBsGridControl.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
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

end.
