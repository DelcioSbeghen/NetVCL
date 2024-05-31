unit NV.VCL.Charts;

interface

uses
  Classes, Types, Controls, SysUtils, DB, Graphics, {$IFDEF FPC} RtlConsts, {$ELSE} UIConsts, {$ENDIF} System.UITypes, NV.JSON, NV.VCL.Graphics,
  NV.Controls,
  NV.Ajax, NV.Interfaces;

type

  TNvChartSerieTypes = (dwstLine, dwstArea, dwstBars, dwstHorizBar, dwstDoughut);
  TNvChartPosition   = (dwgTop, dwBottom, dwLeft, dwRight);
  TNvChartAnimType = (dwgaLinear, dwgaInQuad, dwgaOutQuad, dwgaInOutQuad, dwgaInCubic, dwgaOutCubic,
    dwgaInOutCubic, dwgaInQuart, dwgaOutQuart, dwgaInOutQuart, dwgaInQuint, dwgaOutQuint,
    dwgaInOutQuint, dwgaInSine, dwgaOutSine, dwgaInOutSine, dwgaInExpo, dwgaOutExpo, dwgaInOutExpo,
    dwgaInCirc, dwgaOutCirc, dwgaInOutCirc, dwgaInElastic, dwgaOutElastic, dwgaInOutElastic,
    dwgaInBack, dwgaOutBack, dwgaInOutBack, dwgaInBounce, dwgaOutBounce, dwgaInOutBounce);
  TNvChartTooltipMode     = (dwgtPoint, dwgtNearest, dwgtIndex, dwgtDataset, dwgtX, dwgtY);
  TNvChartTooltipPosition = (dwgpAverage, dwgpNearest);
  TNvChartPointStyle = (dwgpsCircle, dwgpsCross, dwgpsCrossRot, dwgpsDash, dwgpsLine, dwgpsRect,
    dwgpsRectRounded, dwgpsRectRot, dwgpsStar, dwgpsTriangle);
  TNvChartFillPosition       = (dwgfZero, dwgfTop, dwgfBottom, dwgfTrue, dwgfFalse);
  TNvChartBorderSkipped      = (dwgbsBottom, dwgbsLeft, dwgbsTop, dwgbsRight);
  TNvChartCubicInterpolation = (dwgciDefault, dwgciMonotone);
  TNvChartSteppedLine        = (dwgslfalse, dwgslTrue, dwgslBefore, dwgslAfter);

const
  TNvChartAnimTypeStr: array [0 .. 30] of string = ('linear', 'easeInQuad', 'easeOutQuad',
    'easeInOutQuad', 'easeInCubic', 'easeOutCubic', 'easeInOutCubic', 'easeInQuart', 'easeOutQuart',
    'easeInOutQuart', 'easeInQuint', 'easeOutQuint', 'easeInOutQuint', 'easeInSine', 'easeOutSine',
    'easeInOutSine', 'easeInExpo', 'easeOutExpo', 'easeInOutExpo', 'easeInCirc', 'easeOutCirc',
    'easeInOutCirc', 'easeInElastic', 'easeOutElastic', 'easeInOutElastic', 'easeInBack',
    'easeOutBack', 'easeInOutBack', 'easeInBounce', 'easeOutBounce', 'easeInOutBounce');

  TNvChartPositionStr: array [low(TNvChartPosition) .. High(TNvChartPosition)
    ] of string = ('top', 'bottom', 'left', 'right');

  TNvChartPointStyleStr: array [low(TNvChartPointStyle) .. High(TNvChartPointStyle)
    ] of string = ('circle', 'cross', 'crossRot', 'dash', 'line', 'rect', 'rectRounded', 'rectRot',
    'star', 'triangle');

  TNvChartBorderSkippedStr: array [low(TNvChartBorderSkipped) .. High(TNvChartBorderSkipped)
    ] of string = ('bottom', 'left', 'top', 'right');

  TNvChartTooltipModeStr: array [low(TNvChartTooltipMode) .. High(TNvChartTooltipMode)
    ] of string = ('point', 'nearest', 'index', 'dataset', 'x', 'y');

  TNvChartCubicInterpolationStr: array [low(TNvChartCubicInterpolation)
    .. High(TNvChartCubicInterpolation)] of string = ('default', 'monotone');

  TNvChartFillPositionStr: array [low(TNvChartFillPosition) .. High(TNvChartFillPosition)
    ] of string = ('zero', 'top', 'bottom', 'true', 'false');

  TNvChartSteppedLineStr: array [low(TNvChartSteppedLine) .. High(TNvChartSteppedLine)
    ] of string = ('false', 'true', 'before', 'after');

type

  INvDBChartSerie = interface(IInterfaceComponentReference)
    ['{A382C350-9F14-4180-AAB2-24C8A2C08E48}']
    procedure SetField(Value: TField);
    function GetField: TField;
    procedure SetDataField(Value: string);
    function GetDataField: string;
    property Field: TField read GetField write SetField;
    property DataField: string read GetDataField write SetDataField;
  end;

  TNvCustomChart     = class;
  TNvChartSerieItem  = class;
  TNvChartSerieClass = class of TNvChartSerieBase;

  TNvChartSerieBase = class(TNvSubProperty)
  private
    // FDataLink: TDWDataLink;
    // FDataSource: TDataSource;
    FbackGroundColor : TAlphaColor;
    FTitle           : string;
    FOnLegendClick   : TNotifyEvent;
    FOnLegendHover   : TNotifyEvent;
    FBorderColor     : TAlphaColor;
    FBorderWidth     : Integer;
    FData            : TStringList;
    FDataField       : string;
    FBackgroundColors: TStringList;
    FSerieItem       : TNvChartSerieItem;
    FReleased        : Boolean;
    // procedure SetDataSource(const Value: TDataSource);
    procedure SetBackGroundColor(const Value: TAlphaColor);
    procedure SetTitle(const Value: string);
    procedure SetOnLegendClick(const Value: TNotifyEvent);
    procedure SetOnLegendHover(const Value: TNotifyEvent);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetData(const Value: TStringList);
    procedure SetBackgroundColors(const Value: TStringList);
  protected
    // procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function ControlAjaxJson: TJsonObject; override;
    // Render Props
    procedure InternalRender(aJson: TJsonObject); override;
    procedure RenderBackGroundColor(aJson: TJsonObject); dynamic;
    procedure RenderTitle(aJson: TJsonObject); dynamic;
    procedure RenderBorderColor(aJson: TJsonObject); dynamic;
    procedure RenderBorderWidth(aJson: TJsonObject); dynamic;
    procedure RenderData(aJson: TJsonObject);
    //
  public
    constructor Create(aMaster: INVRenderableComponent; aSerieItem: TNvChartSerieItem); virtual;
    destructor Destroy; override;
  published
    property BackGroundColor: TAlphaColor read FbackGroundColor write SetBackGroundColor
      default $25000000;
    property BackgroundColors: TStringList read FBackgroundColors write SetBackgroundColors;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default $25000000;
    property BorderWidth  : Integer read FBorderWidth write SetBorderWidth default 3;
    property Data         : TStringList read FData write SetData;
    property Title        : string read FTitle write SetTitle; // label
    property OnLegendClick: TNotifyEvent read FOnLegendClick write SetOnLegendClick;
    property OnLegendHover: TNotifyEvent read FOnLegendHover write SetOnLegendHover;
  end;

  TNvChartSerieLine = class(TNvChartSerieBase)
  private
    FPointHoverBorderWidth    : Integer;
    FBorderDashOffset         : Integer;
    FPointHoverBorderColor    : TAlphaColor;
    FBorderDash               : TDWCanvasLineDashPattern;
    FSpanGaps                 : Boolean;
    FPointHitRadius           : Integer;
    FPointRadius              : Integer;
    FLineTension              : Currency;
    FSteppedLine              : TNvChartSteppedLine;
    FPointHoverRadius         : Integer;
    FShowLine                 : Boolean;
    FCubicInterpolationMode   : TNvChartCubicInterpolation;
    FBorderJoinStyle          : TDWCanvasLineJoin;
    FFill                     : TNvChartFillPosition;
    FPointStyle               : TNvChartPointStyle;
    FPointbackGroundColor     : TAlphaColor;
    FBorderCapStyle           : TDWCanvasLineCap;
    FPointBorderWidth         : Integer;
    FPointHoverBackgroundColor: TAlphaColor;
    FPointBorderColor         : TAlphaColor;
    // FXField: string;
    // FYField: string;
    procedure SetBorderCapStyle(const Value: TDWCanvasLineCap);
    procedure SetBorderDash(const Value: TDWCanvasLineDashPattern);
    procedure SetBorderDashOffset(const Value: Integer);
    procedure SetBorderJoinStyle(const Value: TDWCanvasLineJoin);
    procedure SetCubicInterpolationMode(const Value: TNvChartCubicInterpolation);
    procedure SetFill(const Value: TNvChartFillPosition);
    procedure SetLineTension(const Value: Currency);
    procedure SetPointbackGroundColor(const Value: TAlphaColor);
    procedure SetPointBorderColor(const Value: TAlphaColor);
    procedure SetPointBorderWidth(const Value: Integer);
    procedure SetPointHitRadius(const Value: Integer);
    procedure SetPointHoverBackgroundColor(const Value: TAlphaColor);
    procedure SetPointHoverBorderColor(const Value: TAlphaColor);
    procedure SetPointHoverBorderWidth(const Value: Integer);
    procedure SetPointHoverRadius(const Value: Integer);
    procedure SetPointRadius(const Value: Integer);
    procedure SetPointStyle(const Value: TNvChartPointStyle);
    procedure SetShowLine(const Value: Boolean);
    procedure SetSpanGaps(const Value: Boolean);
    procedure SetSteppedLine(const Value: TNvChartSteppedLine);
    // procedure SetXField(const Value: string);
    // procedure SetYField(const Value: string);
    function IsLineTensionStored: Boolean;
  protected
    procedure InternalRender(aJson: TJsonObject); override;
    procedure RenderBorderCapStyle(aJson: TJsonObject);
    procedure RenderBorderDash(aJson: TJsonObject);
    procedure RenderBorderDashOffset(aJson: TJsonObject);
    procedure RenderBorderJoinStyle(aJson: TJsonObject);
    procedure RenderCubicInterpolationMode(aJson: TJsonObject);
    procedure RenderFill(aJson: TJsonObject);
    procedure RenderLineTension(aJson: TJsonObject);
    procedure RenderPointbackGroundColor(aJson: TJsonObject);
    procedure RenderPointBorderColor(aJson: TJsonObject);
    procedure RenderPointBorderWidth(aJson: TJsonObject);
    procedure RenderPointHitRadius(aJson: TJsonObject);
    procedure RenderPointHoverBackgroundColor(aJson: TJsonObject);
    procedure RenderPointHoverBorderColor(aJson: TJsonObject);
    procedure RenderPointHoverBorderWidth(aJson: TJsonObject);
    procedure RenderPointHoverRadius(aJson: TJsonObject);
    procedure RenderPointRadius(aJson: TJsonObject);
    procedure RenderPointStyle(aJson: TJsonObject);
    procedure RenderShowLine(aJson: TJsonObject);
    procedure RenderSpanGaps(aJson: TJsonObject);
    procedure RenderSteppedLine(aJson: TJsonObject);
  public
    constructor Create(aMaster: INVRenderableComponent; aSerieItem: TNvChartSerieItem); override;
  published
    property BorderDash: TDWCanvasLineDashPattern read FBorderDash write SetBorderDash{$IFNDEF FPC}  default [] {$ENDIF};
    property BorderDashOffset: Integer read FBorderDashOffset write SetBorderDashOffset default 0;
    property BorderCapStyle: TDWCanvasLineCap read FBorderCapStyle write SetBorderCapStyle
      default dwclcButt;
    property BorderJoinStyle: TDWCanvasLineJoin read FBorderJoinStyle write SetBorderJoinStyle
      default dwcljMiter;
    property CubicInterpolationMode: TNvChartCubicInterpolation read FCubicInterpolationMode
      write SetCubicInterpolationMode default dwgciDefault;
    property Fill       : TNvChartFillPosition read FFill write SetFill default dwgfTrue;
    property LineTension: Currency read FLineTension write SetLineTension
      stored IsLineTensionStored;
    property PointbackGroundColor: TAlphaColor read FPointbackGroundColor
      write SetPointbackGroundColor default $25000000;
    property PointBorderColor: TAlphaColor read FPointBorderColor write SetPointBorderColor
      default $25000000;
    property PointBorderWidth: Integer read FPointBorderWidth write SetPointBorderWidth default 1;
    property PointRadius     : Integer read FPointRadius write SetPointRadius default 1;
    property PointStyle      : TNvChartPointStyle read FPointStyle write SetPointStyle
      default dwgpsCircle;
    Property PointHitRadius: Integer read FPointHitRadius write SetPointHitRadius default 1;
    property PointHoverBackgroundColor: TAlphaColor read FPointHoverBackgroundColor
      write SetPointHoverBackgroundColor default $25000000;
    property PointHoverBorderColor: TAlphaColor read FPointHoverBorderColor
      write SetPointHoverBorderColor default $25000000;
    property PointHoverBorderWidth: Integer read FPointHoverBorderWidth
      write SetPointHoverBorderWidth default 1;
    property PointHoverRadius: Integer read FPointHoverRadius write SetPointHoverRadius default 4;
    property ShowLine        : Boolean read FShowLine write SetShowLine default True;
    property SpanGaps        : Boolean read FSpanGaps write SetSpanGaps default True;
    property SteppedLine     : TNvChartSteppedLine read FSteppedLine write SetSteppedLine
      default dwgslfalse;
    // property XField: string read FXField write SetXField;
    // property YField: string read FYField write SetYField;
  end;

  TNvChartSerieArea = class(TNvChartSerieBase)
  private

  published

  end;

  TNvChartSerieDoughNut = class(TNvChartSerieLine)
  private
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  published

  end;

  TNvChartSerieBar = class(TNvChartSerieBase)
  private
    FHorizontal          : Boolean;
    FHoverBorderWidth    : Integer;
    FHoverBorderColor    : TAlphaColor;
    FBorderSkipped       : TNvChartBorderSkipped;
    FHoverbackgroundColor: TAlphaColor;
    procedure SetBorderSkipped(const Value: TNvChartBorderSkipped);
    procedure SetHoverbackgroundColor(const Value: TAlphaColor);
    procedure SetHoverBorderColor(const Value: TAlphaColor);
    procedure SetHoverBorderWidth(const Value: Integer);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
    procedure RenderHoverBorderWidth(aJson: TJsonObject);
    procedure RenderHoverBorderColor(aJson: TJsonObject);
    procedure RenderBorderSkipped(aJson: TJsonObject);
    procedure RenderHoverBackgroundColor(aJson: TJsonObject);
  public
    constructor Create(aMaster: INVRenderableComponent; aSerieItem: TNvChartSerieItem); override;
  published
    property BorderSkipped: TNvChartBorderSkipped read FBorderSkipped write SetBorderSkipped
      default dwgbsBottom;
    property HoverBackgroundColor: TAlphaColor read FHoverbackgroundColor
      write SetHoverbackgroundColor default $25000000;
    property HoverBorderColor: TAlphaColor read FHoverBorderColor write SetHoverBorderColor
      default $25000000;
    property HoverBorderWidth: Integer read FHoverBorderWidth write SetHoverBorderWidth default 1;
  end;

  TNvDbChartSerieLine = class(TNvChartSerieLine, INvDBChartSerie)
  private
    FField    : TField;
    FDataField: string;
  protected
    procedure SetField(Value: TField);
    function GetField: TField;
    procedure SetDataField(Value: string);
    function GetDataField: string;
    property Field: TField read GetField write SetField;
  published
    property DataField: string read GetDataField write SetDataField;
  end;

  TNvDbChartSerieBar = class(TNvChartSerieBar, INvDBChartSerie)
  private
    FField    : TField;
    FDataField: string;
  protected
    procedure SetField(Value: TField);
    function GetField: TField;
    procedure SetDataField(Value: string);
    function GetDataField: string;
    property Field: TField read GetField write SetField;
  published
    property DataField: string read GetDataField write SetDataField;
  end;

  TNvDbChartSerieArea = class(TNvChartSerieArea, INvDBChartSerie)
  private
    FField    : TField;
    FDataField: string;
  protected
    procedure SetField(Value: TField);
    function GetField: TField;
    procedure SetDataField(Value: string);
    function GetDataField: string;
    property Field: TField read GetField write SetField;
  published
    property DataField: string read GetDataField write SetDataField;
  end;

  TNvChartSeries = class;

  TNvChartSerieItem = class(TCollectionItem)
  private
    FSeriesCollection: TNvChartSeries;
    FSerie           : TNvChartSerieBase;
    FSerieType       : TNvChartSerieTypes;
    procedure SetSerie(const Value: TNvChartSerieBase);
    procedure SetSerieType(const Value: TNvChartSerieTypes);
    function GetSerie: TNvChartSerieBase;
    procedure CheckSerie(aSerieType: TNvChartSerieTypes);
  protected
    function SerieTypeToSerieClass(aSerieType: TNvChartSerieTypes): TNvChartSerieClass; virtual;
  public
    constructor Create(Collection: TCollection); override;
  published
    property SerieType: TNvChartSerieTypes read FSerieType write SetSerieType;
    property Serie    : TNvChartSerieBase read GetSerie write SetSerie;
  end;

  TNvChartSeries = class(TOwnedCollection, INVRenderableComponent)
  protected
    FChart: TNvCustomChart;
    { IInterface }
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IInterfaceComponentReference (Not used)
    function GetComponent: TComponent; // (Not used)
    // INVBase (Not used)
    function GetID: string; // (Not used)
    property ID: string read GetID;
    // INVRenderableComponent
    function ControlAjaxJson: TJsonObject;
    function NeedSendChange: Boolean; inline;
    procedure EnqueueChange(const aName: string; const aProc: TPropChangeProc); inline;
    procedure DequeueChange(const aName: string); inline;
    function Rendered: Boolean;
    procedure ReRender(Now: Boolean = True);
    procedure Invalidate; virtual;

  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    procedure Render;
  end;

  TNvDbChartSerieItem = class(TNvChartSerieItem)
  protected
    function SerieTypeToSerieClass(aSerieType: TNvChartSerieTypes): TNvChartSerieClass; override;
  end;

  TNvChartFontOptions = class(TNvSubProperty)
  private
    FFontFamilyProp  : string;
    FFontFamily      : TFontName;
    FFontSizeProp    : string;
    FFontSize        : Integer;
    FFontStyleProp   : string;
    FDefaultFontStyle: TFontStyles;
    FFontStyle       : TFontStyles;
    FFontColorProp   : string;
    FDefaultFontColor: TAlphaColor;
    FFontColor       : TAlphaColor;
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetFontFamily(const Value: TFontName);
    procedure SetFontSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
    function FontStyleToString(aFontStyle: TFontStyles): string;
    function IsNotDefaultFontStyle: Boolean;
    function IsNotDefaultFontColor: Boolean;
  protected
    procedure SetPropNames; virtual;
    procedure SetDefaultFontStyle; virtual;
    procedure SetDefaultFontColor; virtual;
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
  published
    // font size of text
    property FontSize: Integer read FFontSize write SetFontSize default 12;
    // font style of text
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle stored IsNotDefaultFontStyle;
    // Color of text
    property FontColor: TAlphaColor read FFontColor write SetFontColor stored IsNotDefaultFontColor;
    // Font family of legend text.
    property FontFamily: TFontName read FFontFamily write SetFontFamily;
  end;

  TNvChartLegOptions = class(TNvSubProperty)
  private
    FBoxWidth      : Integer;
    FPadding       : Integer;
    FGenerateLabels: string;
    FFilterScript  : string;
    FUsePointStyle : Boolean;
    FFontOptions   : TNvChartFontOptions;
    procedure SetBoxWidth(const Value: Integer);
    procedure SetPadding(const Value: Integer);
    procedure SetGenerateLabels(const Value: string);
    procedure SetFilterScript(const Value: string);
    procedure SetUsePointStyle(const Value: Boolean);
    procedure SetFontOptions(const Value: TNvChartFontOptions);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
    destructor Destroy; override;
  published
    // width of coloured box
    property BoxWidth: Integer read FBoxWidth write SetBoxWidth default 40;
    // font options for legend
    property FontOptions: TNvChartFontOptions read FFontOptions write SetFontOptions;
    // Padding between rows of colored boxes.
    property Padding: Integer read FPadding write SetPadding default 10;
    // Generates legend items for each thing in the legend. Default implementation returns the text + styling for the color box. See Legend Item for details.
    property GenerateLabelsScript: string read FGenerateLabels write SetGenerateLabels;
    // Filters legend items out of the legend. Receives 2 parameters, a Legend Item and the chart data.
    property FilterScript: string read FFilterScript write SetFilterScript;
    // Label style will match corresponding point style (size is based on fontSize, boxWidth is not used in this case).
    property UsePointStyle: Boolean read FUsePointStyle write SetUsePointStyle default False;
  end;

  TNvChartLegend = class(TNvSubProperty)
  private
    FVisible     : Boolean;
    FPosition    : TNvChartPosition;
    FFullWidth   : Boolean;
    FReverse     : Boolean;
    FLabelOptions: TNvChartLegOptions;
    procedure SetVisible(const Value: Boolean);
    procedure SetPosition(const Value: TNvChartPosition);
    procedure SetFullWidth(const Value: Boolean);
    procedure SetReverse(const Value: Boolean);
    procedure SetLabelOptions(const Value: TNvChartLegOptions);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  published
    // is the legend shown
    property Visible: Boolean read FVisible write SetVisible default True; // display
    // Position of the legend
    property Position: TNvChartPosition read FPosition write SetPosition default dwBottom;
    // Marks that this box should take the full width of the canvas (pushing down other boxes). This is unlikely to need to be changed in day-to-day use.
    property FullWidth: Boolean read FFullWidth write SetFullWidth default True;
    // Legend will show datasets in reverse order.
    property Reverse: Boolean read FReverse write SetReverse default False;
    // label options for legend
    property LabelOptions: TNvChartLegOptions read FLabelOptions write SetLabelOptions;
    { TODO 1 -oDELCIO -cIMPLEMENT : OnClick event for  TDWGraphLegend }
    { TODO 1 -oDELCIO -cIMPLEMENT : OnHover event for  TDWGraphLegend }
  end;

  TNvChartAnimation = class(TNvSubProperty)
  private
    FDuration     : Integer;
    FAnimationType: TNvChartAnimType;
    FOnProgress   : TNotifyEvent;
    FOnComplete   : TNotifyEvent;
    procedure SetDuration(const Value: Integer);
    procedure SetAnimationType(const Value: TNvChartAnimType);
    procedure SetOnProgress(const Value: TNotifyEvent);
    procedure SetOnComplete(const Value: TNotifyEvent);
  protected
    procedure DoOnProgress(aParams: TStringList); virtual;
    procedure DoOnComplete(aParams: TStringList); virtual;
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
  published
    property Duration     : Integer read FDuration write SetDuration default 1000;
    property AnimationType: TNvChartAnimType read FAnimationType write SetAnimationType
      default dwgaOutQuart;
    property OnProgress: TNotifyEvent read FOnProgress write SetOnProgress;
    property OnComplete: TNotifyEvent read FOnComplete write SetOnComplete;
  end;

  TNvChartTitleFontOptions = class(TNvChartFontOptions)
  protected
    procedure SetDefaultFontStyle; override;
  end;

  TNvChartTitle = class(TNvSubProperty)
  private
    FVisible    : Boolean;
    FPosition   : TNvChartPosition;
    FPadding    : Integer;
    FText       : string;
    FFontOptions: TNvChartTitleFontOptions;
    procedure SetVisible(const Value: Boolean);
    procedure SetPosition(const Value: TNvChartPosition);
    procedure SetPadding(const Value: Integer);
    procedure SetText(const Value: string);
    procedure SetFontOptions(const Value: TNvChartTitleFontOptions);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
    destructor Destroy; override;
  published
    // is the title shown
    property Visible: Boolean read FVisible write SetVisible default True; // display
    // Position of the title
    property Position: TNvChartPosition read FPosition write SetPosition default dwgTop;
    // font options for title
    property FontOptions: TNvChartTitleFontOptions read FFontOptions write SetFontOptions;
    // Padding between rows of colored boxes.
    property Padding: Integer read FPadding write SetPadding default 10;
    // Title text to display
    property Text: string read FText write SetText;
  end;

  TNvChartTooltipCallBack = class(TNvSubProperty)
  private
    FBeforeLabelScript : string;
    FBeforeBodyScript  : string;
    FFooterScript      : string;
    FLabelColorScript  : string;
    FBeforeTitleScript : string;
    FBeforeFooterScript: string;
    FAfterLabelScript  : string;
    FAfterBodyScript   : string;
    FAfterTitleScript  : string;
    FLabelScript       : string;
    FAfterFooterScript : string;
    FTitleScript       : string;
    procedure SetAfterBodyScript(const Value: string);
    procedure SetAfterFooterScript(const Value: string);
    procedure SetAfterLabelScript(const Value: string);
    procedure SetAfterTitleScript(const Value: string);
    procedure SetBeforeBodyScript(const Value: string);
    procedure SetBeforeFooterScript(const Value: string);
    procedure SetBeforeLabelScript(const Value: string);
    procedure SetBeforeTitleScript(const Value: string);
    procedure SetFooterScript(const Value: string);
    procedure SetLabelColorScript(const Value: string);
    procedure SetLabelScript(const Value: string);
    procedure SetTitleScript(const Value: string);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
  published
    property BeforeTitleScript : string read FBeforeTitleScript write SetBeforeTitleScript;
    property TitleScript       : string read FTitleScript write SetTitleScript;
    property AfterTitleScript  : string read FAfterTitleScript write SetAfterTitleScript;
    property BeforeBodyScript  : string read FBeforeBodyScript write SetBeforeBodyScript;
    property BeforeLabelScript : string read FBeforeLabelScript write SetBeforeLabelScript;
    property LabelScript       : string read FLabelScript write SetLabelScript;
    property LabelColorScript  : string read FLabelColorScript write SetLabelColorScript;
    property AfterLabelScript  : string read FAfterLabelScript write SetAfterLabelScript;
    property AfterBodyScript   : string read FAfterBodyScript write SetAfterBodyScript;
    property BeforeFooterScript: string read FBeforeFooterScript write SetBeforeFooterScript;
    property FooterScript      : string read FFooterScript write SetFooterScript;
    property AfterFooterScript : string read FAfterFooterScript write SetAfterFooterScript;
  end;

  TNvChartTtpTltFontOptions = class(TNvChartFontOptions)
  protected
    procedure SetPropNames; override;
    procedure SetDefaultFontStyle; override;
    procedure SetDefaultFontColor; override;
  end;

  TNvChartTtpBodyFontOptions = class(TNvChartFontOptions)
  protected
    procedure SetPropNames; override;
    procedure SetDefaultFontColor; override;
  end;

  TNvChartTtpFtrFontOptions = class(TNvChartFontOptions)
  protected
    procedure SetPropNames; override;
  end;

  TNvChartTooltip = class(TNvSubProperty)
  private
    FEnabled           : Boolean;
    FCustomScript      : string;
    FMode              : TNvChartTooltipMode;
    FIntersect         : Boolean;
    FPosition          : TNvChartTooltipPosition;
    FCallBackScripts   : TNvChartTooltipCallBack;
    FTitleSpacing      : Integer;
    FFilterScript      : string;
    FItemSortScript    : string;
    FTitleMarginBottom : Integer;
    FTitleFontOptions  : TNvChartTtpTltFontOptions;
    FbackGroundColor   : TAlphaColor;
    FBodyFontOptions   : TNvChartTtpBodyFontOptions;
    FYPadding          : Integer;
    FCaretPadding      : Integer;
    FFooterMarginTop   : Integer;
    FFooterSpacing     : Integer;
    FCaretSize         : Integer;
    FCornerRadius      : Integer;
    FMultiKeyBackground: TAlphaColor;
    FBorderWidth       : Integer;
    FBodySpacing       : Integer;
    FXPadding          : Integer;
    FFooterFontOptions : TNvChartTtpFtrFontOptions;
    FBorderColor       : TAlphaColor;
    FDisplayColors     : Boolean;
    procedure SetCustomScript(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    procedure SetMode(const Value: TNvChartTooltipMode);
    procedure SetIntersect(const Value: Boolean);
    procedure SetPosition(const Value: TNvChartTooltipPosition);
    procedure SetCallBackScripts(const Value: TNvChartTooltipCallBack);
    procedure SetBackGroundColor(const Value: TAlphaColor);
    procedure SetFilterScript(const Value: string);
    procedure SetItemSortScript(const Value: string);
    procedure SetTitleFontOptions(const Value: TNvChartTtpTltFontOptions);
    procedure SetTitleMarginBottom(const Value: Integer);
    procedure SetTitleSpacing(const Value: Integer);
    procedure SetBodyFontOptions(const Value: TNvChartTtpBodyFontOptions);
    procedure SetBodySpacing(const Value: Integer);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetCaretPadding(const Value: Integer);
    procedure SetCaretSize(const Value: Integer);
    procedure SetCornerRadius(const Value: Integer);
    procedure SetFooterFontOptions(const Value: TNvChartTtpFtrFontOptions);
    procedure SetFooterMarginTop(const Value: Integer);
    procedure SetFooterSpacing(const Value: Integer);
    procedure SetMultiKeyBackground(const Value: TAlphaColor);
    procedure SetXPadding(const Value: Integer);
    procedure SetYPadding(const Value: Integer);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetDisplayColors(const Value: Boolean);
    procedure RenderPosition(aJson: TJsonObject);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
    destructor Destroy; override;
  published
    property Enabled     : Boolean read FEnabled write SetEnabled default True;
    property CustomScript: string read FCustomScript write SetCustomScript;
    property Mode        : TNvChartTooltipMode read FMode write SetMode default dwgtNearest;
    property Intersect   : Boolean read FIntersect write SetIntersect default True;
    property Position: TNvChartTooltipPosition read FPosition write SetPosition default dwgpAverage;
    property CallBackScripts: TNvChartTooltipCallBack read FCallBackScripts
      write SetCallBackScripts;
    property ItemSortScript : string read FItemSortScript write SetItemSortScript;
    property FilterScript   : string read FFilterScript write SetFilterScript;
    property BackGroundColor: TAlphaColor read FbackGroundColor write SetBackGroundColor
      default $CC000000;
    property TitleFontOptions: TNvChartTtpTltFontOptions read FTitleFontOptions
      write SetTitleFontOptions;
    property TitleSpacing     : Integer read FTitleSpacing write SetTitleSpacing default 2;
    property TitleMarginBottom: Integer read FTitleMarginBottom write SetTitleMarginBottom
      default 6;
    property BodyFontOptions: TNvChartTtpBodyFontOptions read FBodyFontOptions
      write SetBodyFontOptions;
    property BodySpacing      : Integer read FBodySpacing write SetBodySpacing default 2;
    property FooterFontOptions: TNvChartTtpFtrFontOptions read FFooterFontOptions
      write SetFooterFontOptions;
    property FooterSpacing     : Integer read FFooterSpacing write SetFooterSpacing default 2;
    property FooterMarginTop   : Integer read FFooterMarginTop write SetFooterMarginTop default 6;
    property XPadding          : Integer read FXPadding write SetXPadding default 6;
    property YPadding          : Integer read FYPadding write SetYPadding default 6;
    property CaretPadding      : Integer read FCaretPadding write SetCaretPadding default 2;
    property CaretSize         : Integer read FCaretSize write SetCaretSize default 5;
    property CornerRadius      : Integer read FCornerRadius write SetCornerRadius default 6;
    property MultiKeyBackground: TAlphaColor read FMultiKeyBackground write SetMultiKeyBackground
      default $FFFFFFFF;
    property DisplayColors: Boolean read FDisplayColors write SetDisplayColors default True;
    Property BorderColor  : TAlphaColor read FBorderColor write SetBorderColor default $00000000;
    property BorderWidth  : Integer read FBorderWidth write SetBorderWidth default 0;
  end;

  TNvChartElementPoint = class(TNvSubProperty)
  private
    FRadius          : Integer;
    FBorderColor     : TAlphaColor;
    FHoverBorderWidth: Integer;
    FHitRadius       : Integer;
    FHoverRadius     : Integer;
    FPointStyle      : TNvChartPointStyle;
    FbackGroundColor : TAlphaColor;
    FBorderWidth     : Integer;
    procedure SetRadius(const Value: Integer);
    procedure SetBackGroundColor(const Value: TAlphaColor);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetHitRadius(const Value: Integer);
    procedure SetHoverBorderWidth(const Value: Integer);
    procedure SetHoverRadius(const Value: Integer);
    procedure SetPointStyle(const Value: TNvChartPointStyle);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
    procedure RenderRadius(aJson: TJsonObject);
    procedure RenderPointStyle(aJson: TJsonObject);
    procedure RenderBackgroundColor(aJson: TJsonObject);
    procedure RenderBorderWidth(aJson: TJsonObject);
    procedure RenderBorderColor(aJson: TJsonObject);
    procedure RenderHitRadius(aJson: TJsonObject);
    procedure RenderHoverRadius(aJson: TJsonObject);
    procedure RenderHoverBorderWidth(aJson: TJsonObject);
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
  published
    property Radius    : Integer read FRadius write SetRadius default 3;
    property PointStyle: TNvChartPointStyle read FPointStyle write SetPointStyle
      default dwgpsCircle;
    property BackGroundColor: TAlphaColor read FbackGroundColor write SetBackGroundColor
      default $25000000;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default $25000000;
    property HitRadius  : Integer read FHitRadius write SetHitRadius default 1;
    property HoverRadius: Integer read FHoverRadius write SetHoverRadius default 4;
    property HoverBorderWidth: Integer read FHoverBorderWidth write SetHoverBorderWidth default 1;
  end;

  TNvChartElementLine = class(TNvSubProperty)
  private
    FBorderColor     : TAlphaColor;
    FBorderDashOffset: Integer;
    FBorderDash      : TDWCanvasLineDashPattern;
    FCapBezierPoints : Boolean;
    FTension         : Currency;
    FStepped         : Boolean;
    FBorderJoinStyle : TDWCanvasLineJoin;
    FFill            : TNvChartFillPosition;
    FBorderCapStyle  : TDWCanvasLineCap;
    FbackGroundColor : TAlphaColor;
    FBorderWidth     : Integer;
    procedure SetBackGroundColor(const Value: TAlphaColor);
    procedure SetBorderCapStyle(const Value: TDWCanvasLineCap);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetBorderDash(const Value: TDWCanvasLineDashPattern);
    procedure SetBorderDashOffset(const Value: Integer);
    procedure SetBorderJoinStyle(const Value: TDWCanvasLineJoin);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetCapBezierPoints(const Value: Boolean);
    procedure SetFill(const Value: TNvChartFillPosition);
    procedure SetStepped(const Value: Boolean);
    procedure SetTension(const Value: Currency);
    function IsTensionStored: Boolean;
  protected
    procedure InternalRender(aJson: TJsonObject); override;
    procedure RenderTension(aJson: TJsonObject);
    procedure RenderBackgroundColor(aJson: TJsonObject);
    procedure RenderBorderWidth(aJson: TJsonObject);
    procedure RenderBorderColor(aJson: TJsonObject);
    procedure RenderBorderCapStyle(aJson: TJsonObject);
    procedure RenderBorderDash(aJson: TJsonObject);
    procedure RenderBorderDashOffset(aJson: TJsonObject);
    procedure RenderBorderJoinStyle(aJson: TJsonObject);
    procedure RenderCapBezierPoints(aJson: TJsonObject);
    procedure RenderFill(aJson: TJsonObject);
    procedure RenderStepped(aJson: TJsonObject);
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
  published
    property Tension        : Currency read FTension write SetTension stored IsTensionStored;
    property BackGroundColor: TAlphaColor read FbackGroundColor write SetBackGroundColor
      default $25000000;
    property BorderWidth   : Integer read FBorderWidth write SetBorderWidth default 3;
    property BorderColor   : TAlphaColor read FBorderColor write SetBorderColor default $25000000;
    property BorderCapStyle: TDWCanvasLineCap read FBorderCapStyle write SetBorderCapStyle
      default dwclcButt;
    property BorderDash: TDWCanvasLineDashPattern read FBorderDash write SetBorderDash {$IFNDEF FPC} default [] {$ENDIF};
    property BorderDashOffset: Integer read FBorderDashOffset write SetBorderDashOffset default 0;
    property BorderJoinStyle: TDWCanvasLineJoin read FBorderJoinStyle write SetBorderJoinStyle
      default dwcljMiter;
    property CapBezierPoints: Boolean read FCapBezierPoints write SetCapBezierPoints default True;
    property Fill           : TNvChartFillPosition read FFill write SetFill default dwgfTrue;
    property Stepped        : Boolean read FStepped write SetStepped default False;
  end;

  TNvChartElementRect = class(TNvSubProperty)
  private
    FBorderColor    : TAlphaColor;
    FbackGroundColor: TAlphaColor;
    FBorderSkipped  : TNvChartBorderSkipped;
    FBorderWidth    : Integer;
    procedure SetBackGroundColor(const Value: TAlphaColor);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetBorderSkipped(const Value: TNvChartBorderSkipped);
    procedure SetBorderWidth(const Value: Integer);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
    procedure RenderBackgroundColor(aJson: TJsonObject);
    procedure RenderBorderWidth(aJson: TJsonObject);
    procedure RenderBorderColor(aJson: TJsonObject);
    procedure RenderBorderSkipped(aJson: TJsonObject);
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
  published
    property BackGroundColor: TAlphaColor read FbackGroundColor write SetBackGroundColor
      default $25000000;
    Property BorderWidth  : Integer read FBorderWidth write SetBorderWidth default 0;
    property BorderColor  : TAlphaColor read FBorderColor write SetBorderColor default $25000000;
    property BorderSkipped: TNvChartBorderSkipped read FBorderSkipped write SetBorderSkipped
      default dwgbsBottom;
  end;

  TNvChartElementArc = class(TNvSubProperty)
  private
    FBorderColor    : TAlphaColor;
    FbackGroundColor: TAlphaColor;
    FBorderWidth    : Integer;
    procedure SetBackGroundColor(const Value: TAlphaColor);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetBorderWidth(const Value: Integer);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
    procedure RenderBackgroundColor(aJson: TJsonObject);
    procedure RenderBorderWidth(aJson: TJsonObject);
    procedure RenderBorderColor(aJson: TJsonObject);
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
  published
    property BackGroundColor: TAlphaColor read FbackGroundColor write SetBackGroundColor
      default $25000000;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default $FFFFFFFF;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 2;
  end;

  TNvChartElements = class(TNvSubProperty)
  private
    FPoints    : TNvChartElementPoint;
    FArcs      : TNvChartElementArc;
    FRectangles: TNvChartElementRect;
    FLines     : TNvChartElementLine;
    procedure SetPoints(const Value: TNvChartElementPoint);
    procedure SetArcs(const Value: TNvChartElementArc);
    procedure SetLines(const Value: TNvChartElementLine);
    procedure SetRectangles(const Value: TNvChartElementRect);
  protected
    procedure InternalRender(aJson: TJsonObject); override;
  public
    constructor Create(aMaster: INVRenderableComponent; aPropName: string; aPrefix: string = '';
      aSuffix: string = ''); override;
    destructor Destroy; override;
  published
    property Points    : TNvChartElementPoint read FPoints write SetPoints;
    property Lines     : TNvChartElementLine read FLines write SetLines;
    property Rectangles: TNvChartElementRect read FRectangles write SetRectangles;
    property Arcs      : TNvChartElementArc read FArcs write SetArcs;

  end;

  TNvCustomChart = class(TNvControl)
  private
    FSeries    : TNvChartSeries;
    FLegend    : TNvChartLegend;
    FAnimation : TNvChartAnimation;
    FPadding   : TRect;
    FTitle     : TNvChartTitle;
    FElements  : TNvChartElements;
    FTooltip   : TNvChartTooltip;
    FLabels    : TStringList;
    FResponsive: Boolean;
    procedure SetSeries(const Value: TNvChartSeries);
    procedure SetLegend(const Value: TNvChartLegend);
    procedure SetAnimation(const Value: TNvChartAnimation);
    procedure SetPadding(const Value: TRect);
    procedure SetTitle(const Value: TNvChartTitle);
    procedure SetElements(const Value: TNvChartElements);
    procedure SetTooltip(const Value: TNvChartTooltip);
    procedure SetLabels(const Value: TStringList);
    procedure SetResponsive(const Value: Boolean);
  protected
    FData   : string;
    FOldData: string;
    procedure AddIncludes; override;
    procedure InternalRender(JSON: TJsonObject); override;
    procedure RenderLabels(aJson: TJsonObject);
    procedure RenderLayout(aJson: TJsonObject);
    procedure RenderResponsive(aJson: TJsonObject);
    procedure CreateSeriesCollection; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    property Labels: TStringList read FLabels write SetLabels;
    {$IFDEF FPC}     property Padding   : TRect read FPadding write SetPadding;  {$ENDIF}
  published
    property Series: TNvChartSeries read FSeries write SetSeries;
    property Caption;
    property CaptionVisible;
    property Legend    : TNvChartLegend read FLegend write SetLegend;
    property Animation : TNvChartAnimation read FAnimation write SetAnimation;
    property Title     : TNvChartTitle read FTitle write SetTitle;
    property Elements  : TNvChartElements read FElements write SetElements;
{$IFNDEF FPC}     property Padding   : TRect read FPadding write SetPadding;  {$ENDIF}
    property Tooltip   : TNvChartTooltip read FTooltip write SetTooltip;
    property Responsive: Boolean read FResponsive write SetResponsive default False;
  end;

  TNvChart = class(TNvCustomChart)
  protected
    procedure CreateSeriesCollection; override;
  published
    property Labels;
  end;

  // TDWDBGraph = class(TDWCustomGraph)
  // private
  // FDataLink  : TDWDatalink;
  // FDataSource: TDataSource;
  // FDataLabels: string;
  // procedure SetDataSource(const Value: TDataSource);
  // procedure SetDataLabels(const Value: string);
  // protected
  // procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  // procedure CheckData; virtual;
  // procedure CreateSeriesCollection; override;
  // public
  // constructor Create(AOwner: TComponent); override;
  // destructor Destroy; override;
  // procedure RenderAsync; override;
  // published
  // Property DataSource: TDataSource read FDataSource write SetDataSource;
  // property DataLabels: string read FDataLabels write SetDataLabels;
  // end;

implementation

uses
  NV.Utils, NV.VCL.Forms;

{ TDWGraph }

procedure TNvCustomChart.AddIncludes;
begin
  inherited;
  // if Ajax <> nil then
  // Ajax.AddInclude('nv.charts.js', reqModule, '/nv.charts.js');
end;

constructor TNvCustomChart.Create(AOwner: TComponent);
begin
  inherited;
  FResponsive := False;
  Align       := TAlign.alClient;
  FLabels     := TStringList.Create;
  CreateSeriesCollection; // FSeries    := TOwnedCollection.Create(Self, TDWGraphSerieItem);
  FLegend    := TNvChartLegend.Create(Self, 'Legend');
  FAnimation := TNvChartAnimation.Create(Self, 'Animation');
  FTitle     := TNvChartTitle.Create(Self, 'Title');
  FElements  := TNvChartElements.Create(Self, 'Elements');
  FTooltip   := TNvChartTooltip.Create(Self, 'Tooltips');
end;

destructor TNvCustomChart.Destroy;
begin
  FLabels.Free;
  FTooltip.Free;
  FElements.Free;
  FTitle.Free;
  FAnimation.Free;
  FLegend.Free;
  FSeries.Free;
  inherited;
end;

procedure TNvCustomChart.InternalRender(JSON: TJsonObject);
var
  FirstSeriesType: string;
  AnimationStr   : string;
begin
  inherited;

  // serie  type
  if Series.Count > 0 then
    begin
      case TNvChartSerieItem(Series.Items[0]).SerieType of
        dwstBars: FirstSeriesType     := 'bar';
        dwstHorizBar: FirstSeriesType := 'horizontalBar';
        dwstArea: FirstSeriesType     := 'line';
        dwstLine: FirstSeriesType     := 'line';
        dwstDoughut: FirstSeriesType  := 'doughnut';
      end;
    end;

  JSON.S['Type'] := FirstSeriesType;

  FSeries.Render;

  RenderLabels(JSON);

  // padding
  if (FPadding.Left <> 0) or (FPadding.Right <> 0) //
    or (FPadding.Top <> 0) or (FPadding.Bottom <> 0) then
    RenderLayout(JSON);

  FLegend.Render;
  FAnimation.Render;
  FTitle.Render;
  FElements.Render;
  FTooltip.Render;

  if FResponsive then
    RenderResponsive(JSON);

end;

procedure TNvCustomChart.Invalidate;
begin
  inherited;
end;

procedure TNvCustomChart.RenderLayout(aJson: TJsonObject);
var
  _JLayout: TJsonObject;
begin
  _JLayout := aJson.O['Layout'];

  _JLayout.I['left']   := FPadding.Left;
  _JLayout.I['right']  := FPadding.Right;
  _JLayout.I['top']    := FPadding.Top;
  _JLayout.I['bottom'] := FPadding.Bottom;
end;

procedure TNvCustomChart.RenderResponsive(aJson: TJsonObject);
begin
  aJson.B['Responsive'] := FResponsive
end;

procedure TNvCustomChart.RenderLabels(aJson: TJsonObject);
var
  I       : Integer;
  _JLabels: TJsonArray;
begin
  _JLabels := aJson.A['Labels'];
  // data
  for I := 0 to FLabels.Count - 1 do
    _JLabels.Add(FLabels[I]);
end;

procedure TNvCustomChart.SetAnimation(const Value: TNvChartAnimation);
begin
  if FAnimation <> Value then
    FAnimation.Assign(Value);
end;

procedure TNvCustomChart.SetElements(const Value: TNvChartElements);
begin
  if FElements <> Value then
    FElements.Assign(Value);
end;

procedure TNvCustomChart.SetLabels(const Value: TStringList);
begin
  if FLabels <> Value then
    begin
      FLabels.Assign(Value);
      EnqueueChange('Labels', RenderLabels);
      Invalidate;
    end;
end;

procedure TNvCustomChart.SetLegend(const Value: TNvChartLegend);
begin
  if FLegend <> Value then
    FLegend.Assign(Value);
end;

procedure TNvCustomChart.SetPadding(const Value: TRect);
begin
  if not CompareMem(@FPadding, @Value, SizeOf(FPadding)) then
    begin
      FPadding := Value;
      EnqueueChange('Layout', RenderLayout);
    end;
end;

procedure TNvCustomChart.SetResponsive(const Value: Boolean);
begin
  if FResponsive <> Value then
    begin
      EnqueueChange('Responsive', RenderResponsive);
      FResponsive := Value;
      Invalidate;
    end;
end;

procedure TNvCustomChart.SetSeries(const Value: TNvChartSeries);
begin
  if FSeries <> Value then
    FSeries.Assign(Value);
end;

procedure TNvCustomChart.SetTitle(const Value: TNvChartTitle);
begin
  if FTitle <> Value then
    FTitle := Value;
end;

procedure TNvCustomChart.SetTooltip(const Value: TNvChartTooltip);
begin
  if FTooltip <> Value then
    FTooltip.Assign(Value);
end;

{ TDWGraph }

{ TDWGraph }

procedure TNvChart.CreateSeriesCollection;
begin
  FSeries := TNvChartSeries.Create(Self, TNvChartSerieItem);
end;

{ TDWGraphSerieBase }

function TNvChartSerieBase.ControlAjaxJson: TJsonObject;
var
  _Datasets: TJsonArray;
  _Index   : Integer;
begin
  _Datasets := FControl.ControlAjaxJson.A['datasets'];
  _Index    := FSerieItem.Index;

  while _Datasets.Count <= _Index do
    _Datasets.AddObject;

  Result := _Datasets[_Index].ObjectValue;
end;

constructor TNvChartSerieBase.Create(aMaster: INVRenderableComponent;
  aSerieItem: TNvChartSerieItem);
begin
  inherited Create(aMaster, '');
  FSerieItem := aSerieItem;
  // FDataLink  := nil;
  FbackGroundColor  := $25000000;
  FBorderColor      := $25000000;
  FBorderWidth      := 3;
  FData             := TStringList.Create;
  FBackgroundColors := TStringList.Create;
  if aMaster.Rendered then
    aMaster.ReRender(False);
end;

(* procedure TDWGraphSerieBase.Notification(AComponent: TComponent;
  AOperation: TOperation);
  begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  if FDataSource = AComponent then
  SetDataSource(nil);
  end; *)

destructor TNvChartSerieBase.Destroy;
begin
  FData.Free;
  FBackgroundColors.Free;
  if Rendered then
    ReRender(False);
  inherited;
end;

procedure TNvChartSerieBase.RenderBackGroundColor(aJson: TJsonObject);
var
  I: Integer;
begin
  if FBackgroundColors.Count > 0 then
    begin
      for I := 0 to FBackgroundColors.Count - 1 do
        aJson.A['backgroundColors'].Add(FBackgroundColors[I]);
    end
  else
    aJson.S['backgroundColor'] := AlphaColorToRGBA(FbackGroundColor);
end;

procedure TNvChartSerieBase.RenderBorderColor(aJson: TJsonObject);
begin
  aJson.S['borderColor'] := AlphaColorToRGBA(FBorderColor)
end;

procedure TNvChartSerieBase.RenderBorderWidth(aJson: TJsonObject);
begin
  aJson.I['borderWidth'] := FBorderWidth;
end;

procedure TNvChartSerieBase.RenderData(aJson: TJsonObject);
var
  I: Integer;
begin
  for I := 0 to FData.Count - 1 do
    aJson.A['data'].Add(FData[I]);
end;

procedure TNvChartSerieBase.RenderTitle(aJson: TJsonObject);
begin
  aJson.S['label'] := FTitle;
end;

procedure TNvChartSerieBase.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FTitle <> '' then
    RenderTitle(aJson);
  if (FBackgroundColors.Count > 0) or (FbackGroundColor <> $25000000) then
    RenderbackgroundColor(aJson);
  if FBorderColor <> $25000000 then
    RenderBorderColor(aJson);
  if FBorderWidth <> 3 then
    RenderBorderWidth(aJson);
  RenderData(aJson);
end;

procedure TNvChartSerieBase.SetBackGroundColor(const Value: TAlphaColor);
begin
  if FbackGroundColor <> Value then
    begin
      FbackGroundColor := Value;
      EnqueueChange('BackGroundColor', RenderBackGroundColor);
      Invalidate;
    end;
end;

procedure TNvChartSerieBase.SetBackgroundColors(const Value: TStringList);
begin
  if FBackgroundColors <> Value then
    begin
      FBackgroundColors.Assign(Value);
      EnqueueChange('BackGroundColor', RenderBackGroundColor);
      Invalidate;
    end;
end;

procedure TNvChartSerieBase.SetBorderColor(const Value: TAlphaColor);
begin
  if FBorderColor <> Value then
    begin
      EnqueueChange('BorderColor', RenderBorderColor);
      FBorderColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieBase.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
    begin
      EnqueueChange('BorderWidth', RenderBorderWidth);
      FBorderWidth := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieBase.SetData(const Value: TStringList);
begin
  if FData <> Value then
    begin
      FData.Assign(Value);
      EnqueueChange('Data', RenderData);
      Invalidate;
    end;
end;

(* procedure TDWGraphSerieBase.SetDataSource(const Value: TDataSource);
  begin
  if Value <> FDataSource then
  begin
  FDataSource := Value;
  if Value = nil then
  begin
  //FDataField := '';
  FreeAndNil(FDataLink);
  end
  else
  begin
  if FDataLink = nil then
  FDataLink          := TDWDataLink.Create(FChart);
  FDataLink.DataSource := FDataSource;
  end;
  FChart.Invalidate;
  end;
  end; *)

procedure TNvChartSerieBase.SetOnLegendClick(const Value: TNotifyEvent);
begin
  FOnLegendClick := Value;
  raise Exception.Create('Need to implement OnLegendClick on TDWGraphSerieBase');
end;

procedure TNvChartSerieBase.SetOnLegendHover(const Value: TNotifyEvent);
begin
  FOnLegendHover := Value;
  raise Exception.Create('Need to implement OnLegendHover on TDWGraphSerieBase');
end;

procedure TNvChartSerieBase.SetTitle(const Value: string);
begin
  if Value <> FTitle then
    begin
      EnqueueChange('Title', RenderTitle);
      FTitle := Value;
      Invalidate;
    end;
end;

{ TDWGraphSerieArea }

(* procedure TDWGraphSerieArea.SetXField(const Value: string);
  begin
  if not SameText(Value, FXField) then
  begin
  FXField := Value;
  if Assigned(FChart) then
  FChart.Invalidate;
  end;
  end; *)

(* procedure TDWGraphSerieArea.SetYField(const Value: string);
  begin
  if not SameText(Value, FXField) then
  begin
  FXField := Value;
  if Assigned(FChart) then
  FChart.Invalidate;
  end;
  end; *)

{ TDWGraphSerieItem }

constructor TNvChartSerieItem.Create(Collection: TCollection);
begin
  inherited;
  FSeriesCollection := Collection as TNvChartSeries;
end;

procedure TNvChartSerieItem.CheckSerie(aSerieType: TNvChartSerieTypes);
var
  LSerieClass: TNvChartSerieClass;
  LOldSerie  : TNvChartSerieBase;
begin
  LOldSerie   := nil;;
  LSerieClass := SerieTypeToSerieClass(aSerieType);
  if (not Assigned(FSerie)) or (FSerie.ClassType <> LSerieClass) then
    begin
      // save old serie
      if Assigned(FSerie) then
        LOldSerie := FSerie;
      // create new serie
      FSerie := LSerieClass.Create(FSeriesCollection, Self);
      // copy old propertyes
      if Assigned(LOldSerie) then
        begin
          FSerie.FbackGroundColor := LOldSerie.BackGroundColor;
          FSerie.Title            := LOldSerie.Title;
          // notify IDE designer
          LOldSerie.FReleased := True;
          if Screen.Page.Designer <> nil then
            Screen.Page.Designer.Modified;

          LOldSerie.Free;
        end;

    end;
  if (aSerieType = dwstHorizBar) //
    and Assigned(FSerie)         //
    and (FSerie.ClassType = TNvChartSerieBar) then
    TNvChartSerieBar(FSerie).FHorizontal := True;
end;

function TNvChartSerieItem.GetSerie: TNvChartSerieBase;
begin
  CheckSerie(FSerieType);
  Result := FSerie;
end;

function TNvChartSerieItem.SerieTypeToSerieClass(aSerieType: TNvChartSerieTypes)
  : TNvChartSerieClass;
begin
  case aSerieType of
    dwstLine: Result     := TNvChartSerieLine;
    dwstArea: Result     := TNvChartSerieArea;
    dwstBars: Result     := TNvChartSerieBar;
    dwstHorizBar: Result := TNvChartSerieBar;
    dwstDoughut: Result  := TNvChartSerieDoughNut;
  end;
end;

procedure TNvChartSerieItem.SetSerie(const Value: TNvChartSerieBase);
begin
  if FSerie <> Value then
    FSerie.Assign(Value);
end;

procedure TNvChartSerieItem.SetSerieType(const Value: TNvChartSerieTypes);
begin
  if (FSerieType <> Value) then
    begin
      CheckSerie(Value);
      FSerieType := Value;
    end;
end;

{ TDWGraphLegend }

procedure TNvChartLegend.AfterConstruction;
begin
  inherited;

end;

constructor TNvChartLegend.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  FVisible      := True;
  FPosition     := dwBottom;
  FFullWidth    := True;
  FReverse      := False;
  FLabelOptions := TNvChartLegOptions.Create(Self, 'labels');
end;

destructor TNvChartLegend.Destroy;
begin
  FLabelOptions.Free;
  inherited;
end;

procedure TNvChartLegend.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FVisible <> True then
    aJson.B['display'] := FVisible;
  if FPosition <> dwgTop then
    aJson.S['position'] := TNvChartPositionStr[FPosition];
  if FFullWidth <> True then
    aJson.B['fullWidth'] := FFullWidth;
  if FReverse <> False then
    aJson.B['reverse'] := FReverse;
  //
  FLabelOptions.Render;
end;

procedure TNvChartLegend.SetFullWidth(const Value: Boolean);
begin
  if FFullWidth <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['fullWidth'] := Value;
      FFullWidth                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartLegend.SetLabelOptions(const Value: TNvChartLegOptions);
begin
  if FLabelOptions <> Value then
    begin
      FLabelOptions.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvChartLegend.SetPosition(const Value: TNvChartPosition);
begin
  if FPosition <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['position'] := TNvChartPositionStr[Value];
      FPosition                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartLegend.SetReverse(const Value: Boolean);
begin
  if FReverse <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['reverse'] := Value;
      FReverse                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartLegend.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['display'] := Value;
      FVisible                       := Value;
      Invalidate;
    end;
end;

{ TDWGraphLegOptions }

constructor TNvChartLegOptions.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  // FontOptions put properties in same "labels" master Json object
  FFontOptions    := TNvChartFontOptions.Create(aMaster, 'labels');
  FBoxWidth       := 40;
  FPadding        := 10;
  FGenerateLabels := '';
  FFilterScript   := '';
  FUsePointStyle  := False;
end;

destructor TNvChartLegOptions.Destroy;
begin
  FontOptions.Free;
  inherited;
end;

procedure TNvChartLegOptions.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FBoxWidth <> 40 then
    aJson.I['boxWidth'] := FBoxWidth;
  if FPadding <> 10 then
    aJson.I['padding'] := FPadding;
  if FGenerateLabels <> '' then
    aJson.S['generateLabels'] := FGenerateLabels;
  if FFilterScript <> '' then
    aJson.S['filter'] := FFilterScript;
  if FUsePointStyle <> False then
    aJson.B['usePointStyle'] := FUsePointStyle;

  FFontOptions.Render;
end;

procedure TNvChartLegOptions.SetBoxWidth(const Value: Integer);
begin
  if FBoxWidth <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['boxWidth'] := Value;
      FBoxWidth                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartLegOptions.SetFilterScript(const Value: string);
begin
  FFilterScript := Value;
  if DebugHook <> 0 then
    raise Exception.Create('Need to implement FilterScript on TDWGraphLegOptions');
end;

procedure TNvChartLegOptions.SetFontOptions(const Value: TNvChartFontOptions);
begin
  if FontOptions <> Value then
    begin
      FFontOptions.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvChartLegOptions.SetGenerateLabels(const Value: string);
begin
  FGenerateLabels := Value;
  if DebugHook <> 0 then
    raise Exception.Create('Need to implement GenerateLabels on TDWGraphLegOptions');
end;

procedure TNvChartLegOptions.SetPadding(const Value: Integer);
begin
  if FPadding <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['padding'] := Value;
      FPadding                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartLegOptions.SetUsePointStyle(const Value: Boolean);
begin
  if FUsePointStyle <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['usePointStyle'] := Value;
      FUsePointStyle                       := Value;
      Invalidate;
    end;
end;

{ TDWGraphAnimation }

constructor TNvChartAnimation.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  FDuration      := 1000;
  FAnimationType := dwgaOutQuart;
end;

procedure TNvChartAnimation.DoOnComplete(aParams: TStringList);
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

procedure TNvChartAnimation.DoOnProgress(aParams: TStringList);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

procedure TNvChartAnimation.InternalRender(aJson: TJsonObject);
var
  Aux   : TStrings;
  AuxStr: string;
begin
  if FDuration <> 1000 then
    aJson.I['duration'] := FDuration;
  if FAnimationType <> dwgaOutQuart then
    aJson.S['ease'] := TNvChartAnimTypeStr[Ord(FAnimationType)];

  { TODO -oDelcio -cChartAnimation : Implement Events OnProgress and OnComplete }
  // if Assigned(FOnProgress) then
  // begin
  // AuxStr := DWApplication.RegisterCallBack(FChart, 'OnAnimProgress', DoOnProgress);
  // Result.S['onProgress'] := 'function(ann) {' +
  // 'executeAjaxCallBack("&currentStep="+ann.animationObject.currentStep + "&maxStep="+ann.animationObject.numSteps, null, "'
  // + AuxStr + '");' + '}';
  // end;
  // if Assigned(FOnComplete) then
  // begin
  // AuxStr := DWApplication.RegisterCallBack(FChart, 'OnAnimComplete', DoOnComplete);
  // Result.S['onAnimationComplete'] := 'function(ann) {' +
  // 'executeAjaxCallBack("&maxStep="+ann.animationObject.numSteps, null, "' + AuxStr +
  // '");' + '}';
  // end;

  (*

    Aux:= TStringList.Create;
    try
    Aux.NameValueSeparator := ':';
    Aux.Delimiter:= ',';
    Aux.QuoteChar:= '"';
    if FDuration <> 1000 then
    Aux.Values['duration']:= IntToStr(FDuration);
    if FAnimationType <> dwgaInOutQuart then
    Aux.Values['ease']:= TDWGrAnimTypeSrt[FAnimationType];
    if Assigned(FOnProgress) then
    begin
    AuxStr:= DWApplication.RegisterCallBack(FChart, 'OnAnimProgress', DoOnProgress);
    Aux.Values['onProgress']:= 'function(ann) {'+
    'executeAjaxCallBack("&currentStep="+ann.animationObject.currentStep + "&maxStep="+ann.animationObject.numSteps, null, "'+AuxStr+'");' +
    '}';
    end;
    if Assigned(FOnComplete) then
    begin
    AuxStr:= DWApplication.RegisterCallBack(FChart, 'OnAnimComplete', DoOnComplete);
    Aux.Values['onAnimationComplete']:= 'function(ann) {'+
    'executeAjaxCallBack("&maxStep="+ann.animationObject.numSteps, null, "'+AuxStr+'");' +
    '}';
    end;
    Result:= Aux.DelimitedText;
    finally
    Aux.Free;
    end; *)
end;

procedure TNvChartAnimation.SetAnimationType(const Value: TNvChartAnimType);
begin
  if FAnimationType <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['ease'] := TNvChartAnimTypeStr[Ord(FAnimationType)];
      FAnimationType              := Value;
      Invalidate;
    end;

end;

procedure TNvChartAnimation.SetDuration(const Value: Integer);
begin
  if FDuration <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['duration'] := Value;
      FDuration                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartAnimation.SetOnComplete(const Value: TNotifyEvent);
begin
  FOnComplete := Value;
  if DebugHook <> 0 then
    raise Exception.Create('Need to Implement TDWGraphAnimation.OnComplete');
end;

procedure TNvChartAnimation.SetOnProgress(const Value: TNotifyEvent);
begin
  FOnProgress := Value;
  if DebugHook <> 0 then
    raise Exception.Create('Need to Implement TDWGraphAnimation.OnProgress');
end;

{ TDWGraphTitle }

constructor TNvChartTitle.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  FPadding  := 10;
  FVisible  := True;
  FPosition := dwgTop;
  // fontOptions properties is rendered in master Json
  FFontOptions := TNvChartTitleFontOptions.Create(aMaster, 'Title');
end;

destructor TNvChartTitle.Destroy;
begin
  FFontOptions.Free;
  inherited;
end;

procedure TNvChartTitle.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FVisible <> False then
    aJson.B['display'] := FVisible;
  if FPosition <> dwgTop then
    aJson.S['position'] := TNvChartPositionStr[FPosition];
  if FPadding <> 10 then
    aJson.I['padding'] := FPadding;
  if FText <> '' then
    aJson.S['text'] := FText;

  FFontOptions.Render;
end;

procedure TNvChartTitle.SetFontOptions(const Value: TNvChartTitleFontOptions);
begin
  if FFontOptions <> Value then
    begin
      FFontOptions.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvChartTitle.SetPadding(const Value: Integer);
begin
  if FPadding <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['padding'] := Value;
      FPadding                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTitle.SetPosition(const Value: TNvChartPosition);
begin
  if FPosition <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['position'] := TNvChartPositionStr[Value];
      FPosition                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTitle.SetText(const Value: string);
begin
  if FText <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['text'] := Value;
      FText                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTitle.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['display'] := Value;
      FVisible                       := Value;
      Invalidate;
    end;
end;

{ TDWGraphTooltip }

constructor TNvChartTooltip.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  FCallBackScripts := TNvChartTooltipCallBack.Create(Self, 'callbacks');
  // Render sub properties in master Json
  FTitleFontOptions   := TNvChartTtpTltFontOptions.Create(aMaster, aPropName);
  FBodyFontOptions    := TNvChartTtpBodyFontOptions.Create(aMaster, aPropName);
  FFooterFontOptions  := TNvChartTtpFtrFontOptions.Create(aMaster, aPropName);
  FEnabled            := True;
  FMode               := dwgtNearest;
  FIntersect          := True;
  FPosition           := dwgpAverage;
  FbackGroundColor    := $CC000000;
  FTitleSpacing       := 2;
  FTitleMarginBottom  := 6;
  FBodySpacing        := 2;
  FFooterSpacing      := 2;
  FFooterMarginTop    := 6;
  FXPadding           := 6;
  FYPadding           := 6;
  FCaretPadding       := 2;
  FCaretSize          := 5;
  FCornerRadius       := 6;
  FMultiKeyBackground := $FFFFFFFF;
  FDisplayColors      := True;
  FBorderColor        := $00000000;
  FBorderWidth        := 0;
end;

destructor TNvChartTooltip.Destroy;
begin
  FFooterFontOptions.Free;
  FBodyFontOptions.Free;
  FTitleFontOptions.Free;
  FCallBackScripts.Free;
  inherited;
end;

procedure TNvChartTooltip.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FEnabled <> True then
    aJson.B['enabled'] := FEnabled;
  if FCustomScript <> '' then
    aJson.S['custom'] := FCustomScript;
  if FMode <> dwgtNearest then
    aJson.S['mode'] := TNvChartTooltipModeStr[FMode];
  if FIntersect <> True then
    aJson.B['intersect'] := FIntersect;
  if FPosition <> dwgpAverage then
    RenderPosition(aJson);
  FCallBackScripts.Render;
  // aJson.O['callbacks'] := FCallBackScripts.Render;
  if FItemSortScript <> '' then
    aJson.S['itemSort'] := FItemSortScript;
  if FFilterScript <> '' then
    aJson.S['filter'] := FFilterScript;
  if FbackGroundColor <> $CC000000 then
    aJson.S['backgroundColor'] := AlphaColorToRGBA(FbackGroundColor);
  // title
  FTitleFontOptions.Render;
  // if FTitleFontOptions.FFontSize <> 12 then
  // aJson.I['titleFontSize'] := FTitleFontOptions.FontSize;
  // if FTitleFontOptions.FFontStyle <> [fsBold] then
  // RenderTitleFontStyle(aJson);
  // if FTitleFontOptions.FFontColor <> $FFFFFF then
  // aJson.S['titleFontColor'] := AlphaColorToRGBA(FTitleFontOptions.FFontColor);
  // if FTitleFontOptions.FFontFamily <> '' then
  // aJson.S['titleFontFamily'] := FTitleFontOptions.FFontFamily;
  if FTitleSpacing <> 2 then
    aJson.I['titleSpacing'] := FTitleSpacing;
  if FTitleMarginBottom <> 6 then
    aJson.I['titleMarginBottom'] := FTitleMarginBottom;
  // body
  FBodyFontOptions.Render;
  // if FBodyFontOptions.FFontSize <> 12 then
  // aJson.I['bodyFontSize'] := FBodyFontOptions.FontSize;
  // if FBodyFontOptions.FFontStyle <> [] then
  // RenderBodyFontStyle(aJson);
  // if FBodyFontOptions.FFontColor <> $FFFFFF then
  // aJson.S['bodyFontColor'] := AlphaColorToRGBA(FBodyFontOptions.FFontColor);
  if FBodySpacing <> 2 then
    aJson.I['bodySpacing'] := FBodySpacing;
  // footer
  FFooterFontOptions.Render;
  // if FFooterFontOptions.FFontSize <> 12 then
  // aJson.I['footerFontSize'] := FFooterFontOptions.FontSize;
  // if FFooterFontOptions.FFontStyle <> [fsBold] then
  // RenderFooterFontStyle(aJson);
  // if FFooterFontOptions.FFontColor <> $FFFFFF then
  // aJson.S['footerFontColor'] := AlphaColorToRGBA(FFooterFontOptions.FFontColor);

  if FFooterSpacing <> 2 then
    aJson.I['footerSpacing'] := FFooterSpacing;
  if FFooterMarginTop <> 6 then
    aJson.I['footerMarginTop'] := FFooterMarginTop;
  if FXPadding <> 6 then
    aJson.I['xPadding'] := FXPadding;
  if FYPadding <> 6 then
    aJson.I['yPadding'] := FYPadding;
  if FCaretPadding <> 2 then
    aJson.I['caretPadding'] := FCaretPadding;
  if FCaretSize <> 5 then
    aJson.I['caretSize'] := FCaretSize;
  if FCornerRadius <> 6 then
    aJson.I['cornerRadius'] := FCornerRadius;
  if FMultiKeyBackground <> $FFFFFFFF then
    aJson.S['multiKeybackground'] := AlphaColorToRGBA(FMultiKeyBackground);
  if FDisplayColors <> True then
    aJson.B['displayColors'] := FDisplayColors;
  if FBorderColor <> $00000000 then
    aJson.S['borderColor'] := AlphaColorToRGBA(FBorderColor);
  if FBorderWidth <> 0 then
    aJson.I['borderWidth'] := FBorderWidth;

end;

procedure TNvChartTooltip.RenderPosition(aJson: TJsonObject);
begin
  case FPosition of
    dwgpAverage: aJson.S['position'] := 'average';
    dwgpNearest: aJson.S['position'] := 'nearest';
  end;
end;

procedure TNvChartTooltip.SetBackGroundColor(const Value: TAlphaColor);
begin
  if FbackGroundColor <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['backgroudColor'] := AlphaColorToRGBA(Value);
      FbackGroundColor                      := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetBodyFontOptions(const Value: TNvChartTtpBodyFontOptions);
begin
  if FBodyFontOptions <> Value then
    begin
      FBodyFontOptions.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetBodySpacing(const Value: Integer);
begin
  if FBodySpacing <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['bodySpacing'] := Value;
      FBodySpacing                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetBorderColor(const Value: TAlphaColor);
begin
  if FBorderColor <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['borderColor'] := AlphaColorToRGBA(Value);
      FBorderColor                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['borderWidth'] := Value;
      FBorderWidth                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetCallBackScripts(const Value: TNvChartTooltipCallBack);
begin
  if FCallBackScripts <> Value then
    begin
      FCallBackScripts.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetCaretPadding(const Value: Integer);
begin
  if FCaretPadding <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['caretPadding'] := Value;
      FCaretPadding                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetCaretSize(const Value: Integer);
begin
  if FCaretSize <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['caretSize'] := Value;
      FCaretSize                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetCornerRadius(const Value: Integer);
begin
  if FCornerRadius <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['cornerRadius'] := Value;
      FCornerRadius                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetCustomScript(const Value: string);
begin
  FCustomScript := Value;
  if DebugHook <> 0 then
    raise Exception.Create('Need to implement TDWGraphTooltip.CustomScript');
end;

procedure TNvChartTooltip.SetDisplayColors(const Value: Boolean);
begin
  if FDisplayColors <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['displayColors'] := Value;
      FDisplayColors                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['enabled'] := Value;
      FEnabled                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetFilterScript(const Value: string);
begin
  if FFilterScript <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['filter'] := Value;
      FFilterScript                 := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetFooterFontOptions(const Value: TNvChartTtpFtrFontOptions);
begin
  if FFooterFontOptions <> Value then
    begin
      FFooterFontOptions.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetFooterMarginTop(const Value: Integer);
begin
  if FFooterMarginTop <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['footerMarginTop'] := Value;
      FFooterMarginTop                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetFooterSpacing(const Value: Integer);
begin
  if FFooterSpacing <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['footerSpacing'] := Value;
      FFooterSpacing                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetIntersect(const Value: Boolean);
begin
  if FIntersect <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['intersect'] := Value;
      FIntersect                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetItemSortScript(const Value: string);
begin
  if FItemSortScript <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['itemSort'] := Value;
      FItemSortScript                 := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetMode(const Value: TNvChartTooltipMode);
begin
  if FMode <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['mode'] := TNvChartTooltipModeStr[Value];
      FMode                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetMultiKeyBackground(const Value: TAlphaColor);
begin
  if FMultiKeyBackground <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S['multiKeybackground'] := AlphaColorToRGBA(Value);
      FMultiKeyBackground                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetPosition(const Value: TNvChartTooltipPosition);
begin
  if FPosition <> Value then
    begin
      FPosition := Value;
      if Rendered then
        RenderPosition(ControlAjaxJson);
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetTitleFontOptions(const Value: TNvChartTtpTltFontOptions);
begin
  if FTitleFontOptions <> Value then
    begin
      FTitleFontOptions.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetTitleMarginBottom(const Value: Integer);
begin
  if FTitleMarginBottom <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['titleMarginBottom'] := Value;
      FTitleMarginBottom                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetTitleSpacing(const Value: Integer);
begin
  if FTitleSpacing <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['titleSpacing'] := Value;
      FTitleSpacing                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetXPadding(const Value: Integer);
begin
  if FXPadding <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['xPadding'] := Value;
      FXPadding                       := Value;
      Invalidate;
    end;
end;

procedure TNvChartTooltip.SetYPadding(const Value: Integer);
begin
  if FYPadding <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['yPadding'] := Value;
      FYPadding                       := Value;
      Invalidate;
    end;
end;

{ TDWGraphTooltipCallBack }

constructor TNvChartTooltipCallBack.Create(aMaster: INVRenderableComponent;
  aPropName, aPrefix, aSuffix: string);
begin
  inherited;
end;

procedure TNvChartTooltipCallBack.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FBeforeTitleScript <> '' then
    aJson.S['beforeTitle'] := FBeforeTitleScript;
  if FTitleScript <> '' then
    aJson.S['title'] := FTitleScript;
  if FAfterTitleScript <> '' then
    aJson.S['afterTitle'] := FAfterTitleScript;
  if FBeforeBodyScript <> '' then
    aJson.S['beforeBody'] := FBeforeBodyScript;
  if FBeforeLabelScript <> '' then
    aJson.S['beforeLabel'] := FBeforeLabelScript;
  if FLabelScript <> '' then
    aJson.S['label'] := FLabelScript;
  if FLabelColorScript <> '' then
    aJson.S['labelColor'] := FLabelColorScript;
  if FAfterLabelScript <> '' then
    aJson.S['afterLabel'] := FAfterLabelScript;
  if FAfterBodyScript <> '' then
    aJson.S['afterBody'] := FAfterBodyScript;
  if FBeforeFooterScript <> '' then
    aJson.S['beforeFooter'] := FBeforeFooterScript;
  if FFooterScript <> '' then
    aJson.S['footer'] := FFooterScript;
  if FAfterBodyScript <> '' then
    aJson.S['afterBody'] := FAfterBodyScript;
end;

procedure TNvChartTooltipCallBack.SetAfterBodyScript(const Value: string);
begin
  { TODO -oDelcio -cTNvChartTooltipCallBack : Implement ajax script updates }
  FAfterBodyScript := Value;
end;

procedure TNvChartTooltipCallBack.SetAfterFooterScript(const Value: string);
begin
  FAfterFooterScript := Value;
end;

procedure TNvChartTooltipCallBack.SetAfterLabelScript(const Value: string);
begin
  FAfterLabelScript := Value;
end;

procedure TNvChartTooltipCallBack.SetAfterTitleScript(const Value: string);
begin
  FAfterTitleScript := Value;
end;

procedure TNvChartTooltipCallBack.SetBeforeBodyScript(const Value: string);
begin
  FBeforeBodyScript := Value;
end;

procedure TNvChartTooltipCallBack.SetBeforeFooterScript(const Value: string);
begin
  FBeforeFooterScript := Value;
end;

procedure TNvChartTooltipCallBack.SetBeforeLabelScript(const Value: string);
begin
  FBeforeLabelScript := Value;
end;

procedure TNvChartTooltipCallBack.SetBeforeTitleScript(const Value: string);
begin
  FBeforeTitleScript := Value;
end;

procedure TNvChartTooltipCallBack.SetFooterScript(const Value: string);
begin
  FFooterScript := Value;
end;

procedure TNvChartTooltipCallBack.SetLabelColorScript(const Value: string);
begin
  FLabelColorScript := Value;
end;

procedure TNvChartTooltipCallBack.SetLabelScript(const Value: string);
begin
  FLabelScript := Value;
end;

procedure TNvChartTooltipCallBack.SetTitleScript(const Value: string);
begin
  FTitleScript := Value;
end;

{ TDWGraphFontOptions }

constructor TNvChartFontOptions.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  SetPropNames;
  SetDefaultFontStyle;
  SetDefaultFontColor;
  FFontSize   := 12;
  FFontFamily := '';
  FFontStyle  := FDefaultFontStyle;
  FFontColor  := FDefaultFontColor;
end;

function TNvChartFontOptions.FontStyleToString(aFontStyle: TFontStyles): string;
begin
  if (fsItalic in FFontStyle) and (fsBold in FFontStyle) then
    Result := 'italic bold'
  else if (fsItalic in FFontStyle) then
    Result := 'italic'
  else if (fsBold in FFontStyle) then
    Result := 'bold'
  else
    Result := '';
end;

procedure TNvChartFontOptions.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FFontSize <> 12 then
    aJson.I[FFontSizeProp] := FontSize;
  if IsNotDefaultFontStyle then
    aJson.S[FFontStyleProp] := FontStyleToString(FFontStyle);
  if IsNotDefaultFontColor then
    aJson.S[FFontColorProp] := AlphaColorToRGBA(FFontColor);

  if FFontFamily <> '' then
    aJson.S[FFontFamilyProp] := FFontFamily;
end;

function TNvChartFontOptions.IsNotDefaultFontColor: Boolean;
begin
  Result := FFontColor <> FDefaultFontColor;
end;

function TNvChartFontOptions.IsNotDefaultFontStyle: Boolean;
begin
  Result := FFontStyle <> FDefaultFontStyle;
end;

procedure TNvChartFontOptions.SetDefaultFontColor;
begin
  FDefaultFontColor := $666666;
end;

procedure TNvChartFontOptions.SetDefaultFontStyle;
begin
  FDefaultFontStyle := [];
end;

procedure TNvChartFontOptions.SetFontColor(const Value: TAlphaColor);
begin
  if FFontColor <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S[FFontColorProp] := AlphaColorToRGBA(FFontColor);
      FFontColor                          := Value;
      Invalidate;
    end;
end;

procedure TNvChartFontOptions.SetFontFamily(const Value: TFontName);
begin
  if FFontFamily <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S[FFontFamilyProp] := Value;
      FFontFamily                          := Value;
      Invalidate;
    end;
end;

procedure TNvChartFontOptions.SetFontSize(const Value: Integer);
begin
  if FFontSize <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I[FFontSizeProp] := Value;
      FFontSize                          := Value;
      Invalidate;
    end;
end;

procedure TNvChartFontOptions.SetFontStyle(const Value: TFontStyles);
begin
  if FFontStyle <> Value then
    begin
      if Rendered then
        ControlAjaxJson.S[FFontStyleProp] := FontStyleToString(Value);
      FFontStyle                          := Value;
      Invalidate;
    end;
end;

procedure TNvChartFontOptions.SetPropNames;
begin
  FFontStyleProp  := 'fontStyle';
  FFontSizeProp   := 'fontSize';
  FFontFamilyProp := 'fontFamily';
  FFontColorProp  := 'fontColor';
end;

{ TdwGraphElementPoint }

constructor TNvChartElementPoint.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  FRadius           := 3;
  FPointStyle       := dwgpsCircle;
  FbackGroundColor  := $25000000;
  FBorderWidth      := 1;
  FBorderColor      := $25000000;
  FHitRadius        := 1;
  FHoverRadius      := 4;
  FHoverBorderWidth := 1;
end;

procedure TNvChartElementPoint.InternalRender(aJson: TJsonObject);
begin
  inherited;

  if FRadius <> 3 then
    RenderRadius(aJson);
  if FPointStyle <> dwgpsCircle then
    RenderPointStyle(aJson);
  if FbackGroundColor <> $25000000 then
    RenderBackgroundColor(aJson);
  if FBorderWidth <> 1 then
    RenderBorderWidth(aJson);
  if FBorderColor <> $25000000 then
    RenderBorderColor(aJson);
  if FHitRadius <> 1 then
    RenderHitRadius(aJson);
  if FHoverRadius <> 4 then
    RenderHoverRadius(aJson);
  if FHoverBorderWidth <> 1 then
    RenderHoverBorderWidth(aJson);
end;

procedure TNvChartElementPoint.RenderBackgroundColor(aJson: TJsonObject);
begin
  aJson.S['backgroundColor'] := AlphaColorToRGBA(FbackGroundColor);
end;

procedure TNvChartElementPoint.RenderBorderColor(aJson: TJsonObject);
begin
  aJson.I['borderColor'] := AlphaColorToColor(FBorderColor);
end;

procedure TNvChartElementPoint.RenderBorderWidth(aJson: TJsonObject);
begin
  aJson.I['borderWidth'] := FBorderWidth;
end;

procedure TNvChartElementPoint.RenderHitRadius(aJson: TJsonObject);
begin
  aJson.I['hitRadius'] := FHitRadius;
end;

procedure TNvChartElementPoint.RenderHoverBorderWidth(aJson: TJsonObject);
begin
  aJson.I['hoverBorderWidth'] := FHoverBorderWidth;
end;

procedure TNvChartElementPoint.RenderHoverRadius(aJson: TJsonObject);
begin
  aJson.I['hoverRadius'] := FHoverRadius;
end;

procedure TNvChartElementPoint.RenderPointStyle(aJson: TJsonObject);
begin
  aJson.S['pointStyle'] := TNvChartPointStyleStr[FPointStyle];
end;

procedure TNvChartElementPoint.RenderRadius(aJson: TJsonObject);
begin
  aJson.I['radius'] := FRadius;
end;

procedure TNvChartElementPoint.SetBackGroundColor(const Value: TAlphaColor);
begin
  if Value <> FBackgroundColor then
    begin
      EnqueueChange('BackgroundColor', RenderBackgroundColor);
      FBackgroundColor := Value;
      Invalidate;
    end;

end;

procedure TNvChartElementPoint.SetBorderColor(const Value: TAlphaColor);
begin
  if Value <> FBorderColor then
    begin
      EnqueueChange('BorderColor', RenderBorderColor);
      FBorderColor := Value;
      Invalidate;
    end;

end;

procedure TNvChartElementPoint.SetBorderWidth(const Value: Integer);
begin
  if Value <> FBorderWidth then
    begin
      EnqueueChange('BorderWidth', RenderBorderWidth);
      FBorderWidth := Value;
      Invalidate;
    end;

end;

procedure TNvChartElementPoint.SetHitRadius(const Value: Integer);
begin
  if Value <> FHitRadius then
    begin
      EnqueueChange('HitRadius', RenderHitRadius);
      FHitRadius := Value;
      Invalidate;
    end;

end;

procedure TNvChartElementPoint.SetHoverBorderWidth(const Value: Integer);
begin
  if Value <> FHoverBorderWidth then
    begin
      EnqueueChange('HoverBorderWidth', RenderHoverBorderWidth);
      FHoverBorderWidth := Value;
      Invalidate;
    end;

end;

procedure TNvChartElementPoint.SetHoverRadius(const Value: Integer);
begin
  if Value <> FHoverRadius then
    begin
      EnqueueChange('HoverRadius', RenderHoverRadius);
      FHoverRadius := Value;
      Invalidate;
    end;

end;

procedure TNvChartElementPoint.SetPointStyle(const Value: TNvChartPointStyle);
begin
  if Value <> FPointStyle then
    begin
      EnqueueChange('PointStyle', RenderPointStyle);
      FPointStyle := Value;
      Invalidate;
    end;

end;

procedure TNvChartElementPoint.SetRadius(const Value: Integer);
begin
  if Value <> FRadius then
    begin
      EnqueueChange('Radius', RenderRadius);
      FRadius := Value;
      Invalidate;
    end;
end;

{ TDWGraphElements }

constructor TNvChartElements.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  FPoints     := TNvChartElementPoint.Create(Self, 'point');
  FLines      := TNvChartElementLine.Create(Self, 'line');
  FRectangles := TNvChartElementRect.Create(Self, 'rectangle');
  FArcs       := TNvChartElementArc.Create(Self, 'arc');
end;

destructor TNvChartElements.Destroy;
begin
  FArcs.Free;
  FRectangles.Free;
  FLines.Free;
  FPoints.Free;
  inherited;
end;

procedure TNvChartElements.InternalRender(aJson: TJsonObject);
begin
  inherited;
  FPoints.Render;
  FLines.Render;
  FRectangles.Render;
  FArcs.Render;
end;

procedure TNvChartElements.SetArcs(const Value: TNvChartElementArc);
begin
  if FArcs <> Value then
    begin
      FArcs.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvChartElements.SetLines(const Value: TNvChartElementLine);
begin
  if FLines <> Value then
    begin
      FLines.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvChartElements.SetPoints(const Value: TNvChartElementPoint);
begin
  if FPoints <> Value then
    begin
      FPoints.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvChartElements.SetRectangles(const Value: TNvChartElementRect);
begin
  if FRectangles <> Value then
    begin
      FRectangles.Assign(Value);
      Invalidate;
    end;
end;

{ TDWGraphElementLine }

constructor TNvChartElementLine.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  FTension          := 0.4;
  FbackGroundColor  := $25000000;
  FBorderWidth      := 3;
  FBorderColor      := $25000000;
  FBorderCapStyle   := dwclcButt;
  FBorderDash       := [];
  FBorderDashOffset := 0;
  FBorderJoinStyle  := dwcljMiter;
  FCapBezierPoints  := True;
  FFill             := dwgfTrue;
  FStepped          := False;
end;

procedure TNvChartElementLine.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FTension <> 0.4 then
    RenderTension(aJson);
  if FbackGroundColor <> $25000000 then
    RenderBackgroundColor(aJson);
  if FBorderWidth <> 3 then
    RenderBorderWidth(aJson);
  if FBorderColor <> $25000000 then
    RenderBorderColor(aJson);
  if FBorderCapStyle <> dwclcButt then
    RenderBorderCapStyle(aJson);
  if High(FBorderDash) >= 0 then
    RenderBorderDash(aJSon);
  if FBorderDashOffset <> 0 then
    RenderBorderDashOffset(aJson);
  if FBorderJoinStyle <> dwcljMiter then
    RenderBorderJoinStyle(aJson);
  if FCapBezierPoints <> True then
    RenderCapBezierPoints(aJson);
  if FFill <> dwgfTrue then
    RenderFill(aJson);
  if FStepped <> False then
    RenderStepped(aJson);
end;

function TNvChartElementLine.IsTensionStored: Boolean;
begin
  Result := FTension <> 0.4;
end;

procedure TNvChartElementLine.RenderFill(aJson: TJsonObject);
begin
  aJson.S['fill'] := TNvChartFillPositionStr[FFill];
  // testar pois algusn são booleanos, veja abaixo
  // case FFill of
  // dwgfZero: aJson.S['fill']   := 'zero';
  // dwgfTop: aJson.S['fill']    := 'top';
  // dwgfBottom: aJson.S['fill'] := 'bottom';
  // dwgfTrue: aJson.B['fill']   := True;
  // dwgfFalse: aJson.B['fill']  := False;
  // end;
end;

procedure TNvChartElementLine.RenderBackgroundColor(aJson: TJsonObject);
begin
  aJson.S['backgroudColor'] := AlphaColorToRGBA(FbackGroundColor);
end;

procedure TNvChartElementLine.RenderBorderCapStyle(aJson: TJsonObject);
begin
  aJson.S['borderCapStyle'] := TDWCanvasLineCapStr[FBorderCapStyle];
end;

procedure TNvChartElementLine.RenderBorderColor(aJson: TJsonObject);
begin
  aJson.S['borderColor'] := AlphaColorToRGBA(FBorderColor);
end;

procedure TNvChartElementLine.RenderStepped(aJson: TJsonObject);
begin
  aJson.B['stepped'] := FStepped;
end;

procedure TNvChartElementLine.RenderTension(aJson: TJsonObject);
begin
  aJson.F['tension'] := FTension;
end;

procedure TNvChartElementLine.RenderBorderDash(aJson: TJsonObject);
var
  I: Integer;
begin
  for I := Low(FBorderDash) to High(FBorderDash) do
    aJson.A['borderDash'].Add(FBorderDash[I]);
end;

procedure TNvChartElementLine.RenderBorderDashOffset(aJson: TJsonObject);
begin
  aJson.I['borderDashOffset'] := FBorderDashOffset;
end;

procedure TNvChartElementLine.RenderBorderJoinStyle(aJson: TJsonObject);
begin
  aJson.S['borderjoinStyle'] := TDWCanvasLineJoinStr[FBorderJoinStyle];
end;

procedure TNvChartElementLine.RenderBorderWidth(aJson: TJsonObject);
begin
  aJson.I['borderWidth'] := FBorderWidth;
end;

procedure TNvChartElementLine.RenderCapBezierPoints(aJson: TJsonObject);
begin
  aJson.B['capBezierPoints'] := FCapBezierPoints;
end;

procedure TNvChartElementLine.SetBackGroundColor(const Value: TAlphaColor);
begin
  if Value <> FBackgroundColor then
    begin
      EnqueueChange('BackgroundColor', RenderBackgroundColor);
      FBackgroundColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementLine.SetBorderCapStyle(const Value: TDWCanvasLineCap);
begin
  if Value <> FBorderCapStyle then
    begin
      EnqueueChange('BorderCapStyle', RenderBorderCapStyle);
      FBorderCapStyle := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementLine.SetBorderColor(const Value: TAlphaColor);
begin
  if Value <> FBorderColor then
    begin
      EnqueueChange('BorderColor', RenderBorderColor);
      FBorderColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementLine.SetBorderDash(const Value: TDWCanvasLineDashPattern);
begin
  if FBorderDash <> Value then
    begin
      FBorderDash := Value;
      EnqueueChange('BorderDash', RenderBorderDash);
      Invalidate;
    end;
end;

procedure TNvChartElementLine.SetBorderDashOffset(const Value: Integer);
begin
  if Value <> FBorderDashOffset then
    begin
      EnqueueChange('BorderDashOffset', RenderBorderDashOffset);
      FBorderDashOffset := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementLine.SetBorderJoinStyle(const Value: TDWCanvasLineJoin);
begin
  if Value <> FBorderJoinStyle then
    begin
      EnqueueChange('BorderJoinStyle', RenderBorderJoinStyle);
      FBorderJoinStyle := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementLine.SetBorderWidth(const Value: Integer);
begin
  if Value <> FBorderWidth then
    begin
      EnqueueChange('BorderWidth', RenderBorderWidth);
      FBorderWidth := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementLine.SetCapBezierPoints(const Value: Boolean);
begin
  if Value <> FCapBezierPoints then
    begin
      EnqueueChange('CapBezierPoints', RenderCapBezierPoints);
      FCapBezierPoints := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementLine.SetFill(const Value: TNvChartFillPosition);
begin
  if Value <> FFill then
    begin
      EnqueueChange('Fill', RenderFill);
      FFill := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementLine.SetStepped(const Value: Boolean);
begin
  if Value <> FStepped then
    begin
      EnqueueChange('Stepped', RenderStepped);
      FStepped := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementLine.SetTension(const Value: Currency);
begin
  if Value <> FTension then
    begin
      EnqueueChange('Tension', RenderTension);
      FTension := Value;
      Invalidate;
    end;
end;

{ TDWGraphElementRect }

constructor TNvChartElementRect.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  FbackGroundColor := $25000000;
  FBorderWidth     := 0;
  FBorderColor     := $25000000;
  FBorderSkipped   := dwgbsBottom;
end;

procedure TNvChartElementRect.InternalRender(aJson: TJsonObject);
begin
  inherited;
  // aJson := TJsonObject.Create; Precisa mesmo disso????
  if FbackGroundColor <> $25000000 then
    RenderBackgroundColor(aJson);
  if FBorderWidth <> 0 then
    RenderBorderWidth(aJson);
  if FBorderColor <> $25000000 then
    RenderBorderColor(aJson);
  if FBorderSkipped <> dwgbsBottom then
    RenderBorderSkipped(aJson);
end;

procedure TNvChartElementRect.RenderBackgroundColor(aJson: TJsonObject);
begin
  aJson.S['backgroundColor'] := AlphaColorToRGBA(FbackGroundColor);
end;

procedure TNvChartElementRect.RenderBorderColor(aJson: TJsonObject);
begin
  aJson.S['borderColor'] := AlphaColorToRGBA(FBorderColor);
end;

procedure TNvChartElementRect.RenderBorderSkipped(aJson: TJsonObject);
begin
  aJson.S['borderSkipped'] := TNvChartBorderSkippedStr[FBorderSkipped];
end;

procedure TNvChartElementRect.RenderBorderWidth(aJson: TJsonObject);
begin
  aJson.I['borderWidth'] := FBorderWidth;
end;

procedure TNvChartElementRect.SetBackGroundColor(const Value: TAlphaColor);
begin
  if Value <> FBackgroundColor then
    begin
      EnqueueChange('BackgroundColor', RenderBackgroundColor);
      FBackgroundColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementRect.SetBorderColor(const Value: TAlphaColor);
begin
  if Value <> FBorderColor then
    begin
      EnqueueChange('BorderColor', RenderBorderColor);
      FBorderColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementRect.SetBorderSkipped(const Value: TNvChartBorderSkipped);
begin
  if Value <> FBorderSkipped then
    begin
      EnqueueChange('BorderSkipped', RenderBorderSkipped);
      FBorderSkipped := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementRect.SetBorderWidth(const Value: Integer);
begin
  if Value <> FBorderWidth then
    begin
      EnqueueChange('BorderWidth', RenderBorderWidth);
      FBorderWidth := Value;
      Invalidate;
    end;
end;

{ DWGraphElementArc }

constructor TNvChartElementArc.Create(aMaster: INVRenderableComponent; aPropName: string;
  aPrefix: string = ''; aSuffix: string = '');
begin
  inherited;
  FbackGroundColor := $25000000;
  FBorderColor     := $FFFFFFFF;
  FBorderWidth     := 2;
end;

procedure TNvChartElementArc.InternalRender(aJson: TJsonObject);
begin
  inherited;
  if FbackGroundColor <> $25000000 then
    RenderBackgroundColor(aJson);
  if FBorderWidth <> 2 then
    RenderBorderWidth(aJson);
  if FBorderColor <> $FFFFFFFF then
    RenderBorderColor(aJson);
end;

procedure TNvChartElementArc.RenderBackgroundColor(aJson: TJsonObject);
begin
  aJson.S['backgroundColor'] := AlphaColorToRGBA(FbackGroundColor);
end;

procedure TNvChartElementArc.RenderBorderColor(aJson: TJsonObject);
begin
  aJson.S['borderColor'] := AlphaColorToRGBA(FBorderColor);
end;

procedure TNvChartElementArc.RenderBorderWidth(aJson: TJsonObject);
begin
  aJson.I['borderWidth'] := FBorderWidth;
end;

procedure TNvChartElementArc.SetBackGroundColor(const Value: TAlphaColor);
begin
  if Value <> FBackgroundColor then
    begin
      EnqueueChange('BackgroundColor', RenderBackgroundColor);
      FBackgroundColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementArc.SetBorderColor(const Value: TAlphaColor);
begin
  if Value <> FBorderColor then
    begin
      EnqueueChange('BorderColor', RenderBorderColor);
      FBorderColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartElementArc.SetBorderWidth(const Value: Integer);
begin
  if Value <> FBorderWidth then
    begin
      EnqueueChange('BorderWidth', RenderBorderWidth);
      FBorderWidth := Value;
      Invalidate;
    end;
end;

{ TDWGraphSerieLine }

constructor TNvChartSerieLine.Create(aMaster: INVRenderableComponent;
  aSerieItem: TNvChartSerieItem);
begin
  inherited;
  FBorderDash                := [];
  FBorderDashOffset          := 0;
  FBorderCapStyle            := dwclcButt;
  FBorderJoinStyle           := dwcljMiter;
  FCubicInterpolationMode    := dwgciDefault;
  FFill                      := dwgfTrue;
  FLineTension               := 0.4;
  FPointbackGroundColor      := $25000000;
  FPointBorderColor          := $25000000;
  FPointBorderWidth          := 1;
  FPointRadius               := 1;
  FPointStyle                := dwgpsCircle;
  FPointHitRadius            := 1;
  FPointHoverBackgroundColor := $25000000;
  FPointHoverBorderColor     := $25000000;
  FPointHoverBorderWidth     := 1;
  FPointHoverRadius          := 4;
  FShowLine                  := True;
  FSpanGaps                  := True;
  FSteppedLine               := dwgslfalse;
end;

procedure TNvChartSerieLine.InternalRender(aJson: TJsonObject);
var
  I: Integer;
begin
  inherited;
  if High(FBorderDash) >= 0 then
    RenderBorderDash(aJSon);
  if FBorderDashOffset <> 0 then
    RenderBorderDashOffset(aJson);
  if FBorderCapStyle <> dwclcButt then
    RenderBorderCapStyle(aJson);
  if FBorderJoinStyle <> dwcljMiter then
    RenderBorderJoinStyle(aJson);
  if FCubicInterpolationMode <> dwgciDefault then
    RenderCubicInterpolationMode(aJson);
  if FFill <> dwgfTrue then
    RenderFill(aJson);
  if FLineTension <> 0.4 then
    RenderLineTension(aJson);
  if FPointbackGroundColor <> $25000000 then
    RenderPointbackGroundColor(aJson);
  if FPointBorderColor <> $25000000 then
    RenderPointBorderColor(aJson);
  if FPointBorderWidth <> 1 then
    RenderPointBorderWidth(aJson);
  if FPointRadius <> 1 then
    RenderPointRadius(aJson);
  if FPointStyle <> dwgpsCircle then
    RenderPointStyle(aJson);
  if FPointHitRadius <> 1 then
    RenderPointHitRadius(aJson);
  if FPointHoverBackgroundColor <> $25000000 then
    RenderPointHoverBackgroundColor(aJson);
  if FPointHoverBorderColor <> $25000000 then
    RenderPointHoverBorderColor(aJson);
  if FPointHoverBorderWidth <> 1 then
    RenderPointHoverBorderWidth(aJson);
  if FPointHoverRadius <> 4 then
    RenderPointHoverRadius(aJson);
  if FShowLine <> True then
    RenderShowLine(aJson);
  if FSpanGaps <> True then
    RenderSpanGaps(aJson);
  if FSteppedLine <> dwgslfalse then
    RenderSteppedLine(aJson);

  if FPointHitRadius <> 1 then;
  if FPointHoverBackgroundColor <> $25000000 then;
  if FPointHoverBorderColor <> $25000000 then;
  if FPointHoverBorderWidth <> 1 then

    if FPointHoverRadius <> 4 then;
  if FShowLine <> True then;
  if FSpanGaps <> True then;
  if FSteppedLine <> dwgslfalse then
    begin

    end;
  // FXField: string;
  // FYField: string;
  aJson.S['type'] := 'line';
end;

function TNvChartSerieLine.IsLineTensionStored: Boolean;
begin
  Result := FLineTension <> 0.4;
end;

procedure TNvChartSerieLine.RenderBorderDash(aJson: TJsonObject);
var
  IDx: Integer;
begin
  with aJson.A['borderDash'] do
    begin
      for IDx := Low(FBorderDash) to High(FBorderDash) do
        Add(FBorderDash[IDx]);
    end;
end;

procedure TNvChartSerieLine.RenderBorderDashOffset(aJson: TJsonObject);
begin
  aJson.I['borderDashOffset'] := FBorderDashOffset
end;

procedure TNvChartSerieLine.RenderBorderJoinStyle(aJson: TJsonObject);
begin
  aJson.S['borderjoinStyle'] := TDWCanvasLineJoinStr[FBorderJoinStyle];
end;

procedure TNvChartSerieLine.RenderBorderCapStyle(aJson: TJsonObject);
begin
  aJson.S['borderCapStyle'] := TDWCanvasLineCapStr[FBorderCapStyle];
end;

procedure TNvChartSerieLine.RenderCubicInterpolationMode(aJson: TJsonObject);
begin
  aJson.S['cubicInterpolationMode'] := TNvChartCubicInterpolationStr[FCubicInterpolationMode];
end;

procedure TNvChartSerieLine.RenderFill(aJson: TJsonObject);
begin
  aJson.S['fill'] := TNvChartFillPositionStr[FFill];
end;

procedure TNvChartSerieLine.RenderLineTension(aJson: TJsonObject);
begin
  aJson.F['tension'] := FLineTension;
end;

procedure TNvChartSerieLine.RenderPointbackGroundColor(aJson: TJsonObject);
begin
  aJson.S['pointBackgroundColor'] := AlphaColorToRGBA(FPointbackGroundColor);
end;

procedure TNvChartSerieLine.RenderPointBorderColor(aJson: TJsonObject);
begin
  aJson.S['pointBorderColor'] := AlphaColorToRGBA(FPointBorderColor);
end;

procedure TNvChartSerieLine.RenderPointBorderWidth(aJson: TJsonObject);
begin
  aJson.I['pointBorderWidth'] := FPointBorderWidth
end;

procedure TNvChartSerieLine.RenderPointHitRadius(aJson: TJsonObject);
begin
  aJson.I['pointHitRadius'] := FPointHitRadius;
end;

procedure TNvChartSerieLine.RenderPointHoverBackgroundColor(aJson: TJsonObject);
begin
  aJson.S['pointHoverBackgroundColor'] := AlphaColorToRGBA(FPointHoverBackgroundColor)
end;

procedure TNvChartSerieLine.RenderPointHoverBorderColor(aJson: TJsonObject);
begin
  aJson.S['pointHoverBorderColor'] := AlphaColorToRGBA(FPointHoverBorderColor)
end;

procedure TNvChartSerieLine.RenderPointHoverBorderWidth(aJson: TJsonObject);
begin
  aJson.I['pointHoverBorderWidth'] := FPointHoverBorderWidth;
end;

procedure TNvChartSerieLine.RenderPointHoverRadius(aJson: TJsonObject);
begin
  aJson.I['pointHoverRadius'] := FPointHoverRadius
end;

procedure TNvChartSerieLine.RenderPointRadius(aJson: TJsonObject);
begin
  aJson.I['pointRadius'] := FPointRadius
end;

procedure TNvChartSerieLine.RenderPointStyle(aJson: TJsonObject);
begin
  aJson.S['pointStyle'] := TNvChartPointStyleStr[FPointStyle];
end;

procedure TNvChartSerieLine.RenderShowLine(aJson: TJsonObject);
begin
  aJson.B['showLine'] := FShowLine;
end;

procedure TNvChartSerieLine.RenderSpanGaps(aJson: TJsonObject);
begin
  aJson.B['spanGaps'] := FSpanGaps;
end;

procedure TNvChartSerieLine.RenderSteppedLine(aJson: TJsonObject);
begin
  aJson.S['steppedLine'] := TNvChartSteppedLineStr[FSteppedLine];
  // case FSteppedLine of
  // dwgslfalse: aJson.B['steppedLine']  := False;
  // dwgslTrue: aJson.B['steppedLine']   := True;
  // dwgslBefore: aJson.S['steppedLine'] := 'before';
  // dwgslAfter: aJson.S['steppedLine']  := 'after';
  // end;
end;

procedure TNvChartSerieLine.SetBorderCapStyle(const Value: TDWCanvasLineCap);
begin
  if Value <> FBorderCapStyle then
    begin
      EnqueueChange('BorderCapStyle', RenderBorderCapStyle);
      FBorderCapStyle := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetBorderDash(const Value: TDWCanvasLineDashPattern);
begin
  if Value <> FBorderDash then
    begin
      EnqueueChange('BorderDash', RenderBorderDash);
      FBorderDash := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetBorderDashOffset(const Value: Integer);
begin
  if Value <> FBorderDashOffset then
    begin
      EnqueueChange('BorderDashOffset', RenderBorderDashOffset);
      FBorderDashOffset := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetBorderJoinStyle(const Value: TDWCanvasLineJoin);
begin
  if Value <> FBorderJoinStyle then
    begin
      EnqueueChange('BorderJoinStyle', RenderBorderJoinStyle);
      FBorderJoinStyle := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetCubicInterpolationMode(const Value: TNvChartCubicInterpolation);
begin
  if Value <> FCubicInterpolationMode then
    begin
      EnqueueChange('CubicInterpolationMode', RenderCubicInterpolationMode);
      FCubicInterpolationMode := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetFill(const Value: TNvChartFillPosition);
begin
  if Value <> FFill then
    begin
      EnqueueChange('Fill', RenderFill);
      FFill := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetLineTension(const Value: Currency);
begin
  if Value <> FLineTension then
    begin
      EnqueueChange('LineTension', RenderLineTension);
      FLineTension := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetPointbackGroundColor(const Value: TAlphaColor);
begin
  if Value <> FPointbackGroundColor then
    begin
      EnqueueChange('PointbackGroundColor', RenderPointbackGroundColor);
      FPointbackGroundColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetPointBorderColor(const Value: TAlphaColor);
begin
  if Value <> FPointBorderColor then
    begin
      EnqueueChange('PointBorderColor', RenderPointBorderColor);
      FPointBorderColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetPointBorderWidth(const Value: Integer);
begin
  if Value <> FPointBorderWidth then
    begin
      EnqueueChange('PointBorderWidth', RenderPointBorderWidth);
      FPointBorderWidth := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetPointHitRadius(const Value: Integer);
begin
  if Value <> FPointHitRadius then
    begin
      EnqueueChange('PointHitRadius', RenderPointHitRadius);
      FPointHitRadius := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetPointHoverBackgroundColor(const Value: TAlphaColor);
begin
  if Value <> FPointHoverBackgroundColor then
    begin
      EnqueueChange('PointHoverBackgroundColor', RenderPointHoverBackgroundColor);
      FPointHoverBackgroundColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetPointHoverBorderColor(const Value: TAlphaColor);
begin
  if Value <> FPointHoverBorderColor then
    begin
      EnqueueChange('PointHoverBorderColor', RenderPointHoverBorderColor);
      FPointHoverBorderColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetPointHoverBorderWidth(const Value: Integer);
begin
  if Value <> FPointHoverBorderWidth then
    begin
      EnqueueChange('PointHoverBorderWidth', RenderPointHoverBorderWidth);
      FPointHoverBorderWidth := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetPointHoverRadius(const Value: Integer);
begin
  if Value <> FPointHoverRadius then
    begin
      EnqueueChange('PointHoverRadius', RenderPointHoverRadius);
      FPointHoverRadius := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetPointRadius(const Value: Integer);
begin
  if Value <> FPointRadius then
    begin
      EnqueueChange('PointRadius', RenderPointRadius);
      FPointRadius := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetPointStyle(const Value: TNvChartPointStyle);
begin
  if Value <> FPointStyle then
    begin
      EnqueueChange('PointStyle', RenderPointStyle);
      FPointStyle := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetShowLine(const Value: Boolean);
begin
  if Value <> FShowLine then
    begin
      EnqueueChange('ShowLine', RenderShowLine);
      FShowLine := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetSpanGaps(const Value: Boolean);
begin
  if Value <> FSpanGaps then
    begin
      EnqueueChange('SpanGaps', RenderSpanGaps);
      FSpanGaps := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieLine.SetSteppedLine(const Value: TNvChartSteppedLine);
begin
  if Value <> FSteppedLine then
    begin
      EnqueueChange('SteppedLine', RenderSteppedLine);
      FSteppedLine := Value;
      Invalidate;
    end;
end;

{ TDWGraphSerieBar }

constructor TNvChartSerieBar.Create(aMaster: INVRenderableComponent; aSerieItem: TNvChartSerieItem);
begin
  inherited;
  FHoverBorderWidth     := 1;
  FHoverBorderColor     := $25000000;
  FBorderSkipped        := dwgbsBottom;
  FHoverBackgroundColor  := $25000000;
end;

procedure TNvChartSerieBar.InternalRender(aJson: TJsonObject);
begin
  inherited;

  if FHoverBorderWidth <> 1 then
    RenderHoverBorderWidth(aJson);

  if FHoverBorderColor <> $25000000 then
    RenderHoverBorderColor(aJson);

  if FBorderSkipped <> dwgbsBottom then
    RenderBorderSkipped(aJson);

  if FHoverBackgroundColor <> $25000000 then
    RenderHoverBackgroundColor(aJson);

  if FHorizontal then
    aJson.S['type'] := 'horizontalBar'
  else
    aJson.S['type'] := 'bar';
end;

procedure TNvChartSerieBar.RenderBorderSkipped(aJson: TJsonObject);
begin
  aJson.S['borderSkipped'] := TNvChartBorderSkippedStr[FBorderSkipped];
end;

procedure TNvChartSerieBar.RenderHoverBackgroundColor(aJson: TJsonObject);
begin
  aJson.S['hoverBackgroundColor'] := AlphaColorToRGBA(FHoverbackgroundColor);
end;

procedure TNvChartSerieBar.RenderHoverBorderColor(aJson: TJsonObject);
begin
  aJson.S['hoverBorderColor'] := AlphaColorToRGBA(FHoverBorderColor);
end;

procedure TNvChartSerieBar.RenderHoverBorderWidth(aJson: TJsonObject);
begin
  aJson.I['hoverBorderWidth'] := FHoverBorderWidth;
end;

procedure TNvChartSerieBar.SetBorderSkipped(const Value: TNvChartBorderSkipped);
begin
  if Value <> FBorderSkipped then
    begin
      EnqueueChange('BorderSkipped', RenderBorderSkipped);
      FBorderSkipped := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieBar.SetHoverbackgroundColor(const Value: TAlphaColor);
begin
  if Value <> FHoverBackgroundColor then
    begin
      EnqueueChange('HoverBackgroundColor', RenderHoverBackgroundColor);
      FHoverBackgroundColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieBar.SetHoverBorderColor(const Value: TAlphaColor);
begin
  if Value <> FHoverBorderColor then
    begin
      EnqueueChange('HoverBorderColor', RenderHoverBorderColor);
      FHoverBorderColor := Value;
      Invalidate;
    end;
end;

procedure TNvChartSerieBar.SetHoverBorderWidth(const Value: Integer);
begin
  if Value <> FHoverBorderWidth then
    begin
      EnqueueChange('HoverBorderWidth', RenderHoverBorderWidth);
      FHoverBorderWidth := Value;
      Invalidate;
    end;
end;
//
// { TDWDBGraph }
//
// procedure TDWDBGraph.CheckData;
// var
// LField, AuxField: TField;
// I               : Integer;
// ISerieItem      : IDWDBGraphSerie;
// bmrk            : TBookmark;
// begin
// if DataSource <> nil then
// begin
// if DataSource.State in dsEditModes then
// Exit;
// if CheckDataSource(DataSource, DataLabels, LField) then
// begin
// { TODO 1 -oDELCIO -cIMPROVE : USE DATALINK NOTIFICATION TO AVOID ALWAYS UPDATE }
// // update fields for all series
// for I := 0 to FSeries.Count - 1 do
// begin
// if Supports(TDWDBGraphSerieItem(FSeries.Items[I]).FSerie, IDWDBGraphSerie, ISerieItem)
// then
// if Assigned(ISerieItem) then
// begin
// if CheckDataSource(DataSource, ISerieItem.DataField, AuxField) then
// ISerieItem.Field := AuxField;
// end;
// end;
// // generate new data
// DataSource.DataSet.DisableControls;
// bmrk := DataSource.DataSet.Bookmark;
// try
// // clear labels
// Labels.Clear;
// // clear all series data
// for I := 0 to FSeries.Count - 1 do
// TDWGraphSerieItem(FSeries.Items[I]).FSerie.Data.Clear;
// //
// DataSource.DataSet.First;
// while not DataSource.DataSet.Eof do
// begin
// Labels.Add(LField.DisplayText);
// for I := 0 to FSeries.Count - 1 do
// begin
// if Supports(TDWDBGraphSerieItem(FSeries.Items[I]).FSerie, IDWDBGraphSerie,
// ISerieItem) then
// if Assigned(ISerieItem) then
// begin
// AuxField := ISerieItem.Field;
//
// case AuxField.DataType of
// ftString, ftMemo, ftFmtMemo, ftWideString, ftFixedChar, ftWideMemo,
// ftFixedWideChar:
// TDWGraphSerieItem(FSeries.Items[I])
// .FSerie.Data.Add(AuxField.AsString);
//
// ftSmallint, ftInteger, ftWord, ftBoolean, ftAutoInc, ftShortint,
// ftLargeint, ftByte:
// TDWGraphSerieItem(FSeries.Items[I])
// .FSerie.Data.Add(AuxField.AsInteger);
// ftFloat, ftBCD, ftExtended, ftCurrency:
// TDWGraphSerieItem(FSeries.Items[I])
// .FSerie.Data.Add(AuxField.AsCurrency);
// ftDate: TDWGraphSerieItem(FSeries.Items[I])
// .FSerie.Data.Add(AuxField.AsDateTime);
// ftTime: TDWGraphSerieItem(FSeries.Items[I])
// .FSerie.Data.Add(AuxField.AsDateTime);
// ftDateTime:
// TDWGraphSerieItem(FSeries.Items[I])
// .FSerie.Data.Add(AuxField.AsDateTime);
// end;
// end;
// end;
// DataSource.DataSet.Next;
// end;
// finally
// DataSource.DataSet.GotoBookmark(bmrk);
// DataSource.DataSet.EnableControls;
// end;
// end;
// end;
// end;
//
// constructor TDWDBGraph.Create(AOwner: TComponent);
// begin
// inherited;
// FDataLabels := '';
// FDataLink   := nil;
// end;
//
// procedure TDWDBGraph.CreateSeriesCollection;
// begin
// FSeries := TOwnedCollection.Create(Self, TDWDBGraphSerieItem);
// end;
//
// destructor TDWDBGraph.Destroy;
// begin
// FreeAndNil(FDataLink);
// inherited;
// end;
//
// procedure TDWDBGraph.Notification(AComponent: TComponent; AOperation: TOperation);
// begin
// inherited Notification(AComponent, AOperation);
// if AOperation = opRemove then
// if FDataSource = AComponent then
// SetDataSource(nil);
// end;
//
// procedure TDWDBGraph.RenderAsync;
// begin
// CheckData;
// inherited;
// end;
//
// procedure TDWDBGraph.SetDataLabels(const Value: string);
// begin
// if not SameText(Value, FDataLabels) then
// begin
// FDataLabels := Value;
// Invalidate;
// end;
// end;
//
// procedure TDWDBGraph.SetDataSource(const Value: TDataSource);
// begin
// if Value <> FDataSource then
// begin
// FDataSource := Value;
// if Value = nil then
// begin
// FDataLabels := '';
// FreeAndNil(FDataLink);
// end
// else
// begin
// if FDataLink = nil then
// FDataLink          := TDWDatalink.Create(Self);
// FDataLink.DataSource := FDataSource;
// end;
// Invalidate;
// end;
// end;

{ TDWDBGraphSerieItem }

(* procedure TDWDBGraphSerieItem.CheckSerie(aSerieType: TDWGraphSerieTypes);
  var
  LSerieClass: TNvChartSerieClass;
  LOldSerie: TDWGraphSerieBase;
  begin
  LOldSerie   := nil;;
  LSerieClass := SerieTypeToSerieClass(aSerieType);
  if (not Assigned(FSerie)) or (FSerie.ClassType <> LSerieClass) then
  begin
  // save old serie
  if Assigned(FSerie) then
  LOldSerie := FSerie;
  // create new serie
  FSerie := LSerieClass.Create(TDWCustomGraph(Self.Collection.Owner));
  // copy old propertyes
  if Assigned(LOldSerie) then
  begin
  FSerie.FbackGroundColor := LOldSerie.BackGroundColor;
  FSerie.Title            := LOldSerie.Title;
  LOldSerie.Free;
  end;
  end;
  end; *)

function TNvDbChartSerieItem.SerieTypeToSerieClass(aSerieType: TNvChartSerieTypes)
  : TNvChartSerieClass;
begin
  case aSerieType of
    dwstLine: Result     := TNvDbChartSerieLine;
    dwstArea: Result     := TNvDbChartSerieArea;
    dwstBars: Result     := TNvDbChartSerieBar;
    dwstHorizBar: Result := TNvDbChartSerieBar;
  end;
end;

{ TDWDBGraphSerieLine }

function TNvDbChartSerieLine.GetDataField: string;
begin
  Result := FDataField;
end;

function TNvDbChartSerieLine.GetField: TField;
begin
  Result := FField;
end;

procedure TNvDbChartSerieLine.SetDataField(Value: string);
begin
  if not SameText(Value, FDataField) then
    begin
      FDataField := Value;
      Invalidate;
    end;
end;

procedure TNvDbChartSerieLine.SetField(Value: TField);
begin
  FField := Value;
end;

{ TDWDBGraphSerieBar }

function TNvDbChartSerieBar.GetDataField: string;
begin
  Result := FDataField;
end;

function TNvDbChartSerieBar.GetField: TField;
begin
  Result := FField;
end;

procedure TNvDbChartSerieBar.SetDataField(Value: string);
begin
  if not SameText(Value, FDataField) then
    begin
      FDataField := Value;
      Invalidate;
    end;
end;

procedure TNvDbChartSerieBar.SetField(Value: TField);
begin
  FField := Value;
end;

{ TDWDBGraphSerieArea }

function TNvDbChartSerieArea.GetDataField: string;
begin
  Result := FDataField;
end;

function TNvDbChartSerieArea.GetField: TField;
begin
  Result := FField;
end;

procedure TNvDbChartSerieArea.SetDataField(Value: string);
begin
  if not SameText(Value, FDataField) then
    begin
      FDataField := Value;
      Invalidate;
    end;
end;

procedure TNvDbChartSerieArea.SetField(Value: TField);
begin
  FField := Value;
end;

{ TDWGraphSerieDoughNut }

procedure TNvChartSerieDoughNut.InternalRender(aJson: TJsonObject);
begin
  inherited;
  aJson.S['type'] := 'doughnut';
end;

procedure TNvChartTitleFontOptions.SetDefaultFontStyle;
begin
  FDefaultFontStyle := [fsBold]
end;

procedure TNvChartTtpTltFontOptions.SetDefaultFontColor;
begin
  FDefaultFontColor := $FFFFFF;
end;

procedure TNvChartTtpTltFontOptions.SetDefaultFontStyle;
begin
  FDefaultFontStyle := [fsBold];
end;

procedure TNvChartTtpTltFontOptions.SetPropNames;
begin
  FFontStyleProp  := 'titleFontStyle';
  FFontSizeProp   := 'titleFontSize';
  FFontFamilyProp := 'titleFontFamily';
  FFontColorProp  := 'titleFontColor';
end;

procedure TNvChartTtpBodyFontOptions.SetDefaultFontColor;
begin
  FDefaultFontColor := $FFFFFF;
end;

procedure TNvChartTtpBodyFontOptions.SetPropNames;
begin
  FFontStyleProp  := 'bodyFontStyle';
  FFontSizeProp   := 'bodyFontSize';
  FFontFamilyProp := 'bodyFontFamily';
  FFontColorProp  := 'bodyFontColor';
end;

{ TNvChartTtpFtrFontOptions }

procedure TNvChartTtpFtrFontOptions.SetPropNames;
begin
  FFontStyleProp  := 'footerFontStyle';
  FFontSizeProp   := 'footerFontSize';
  FFontFamilyProp := 'footerFontFamily';
  FFontColorProp  := 'footerFontColor';
end;

{ TNvChartSeries }

function TNvChartSeries.ControlAjaxJson: TJsonObject;
begin
  Result := FChart.ControlAjaxJson.O['Data'];
end;

constructor TNvChartSeries.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  FChart := AOwner as TNvCustomChart;
end;

procedure TNvChartSeries.DequeueChange(const aName: string);
begin
  // Not Implemented yet
end;

procedure TNvChartSeries.EnqueueChange(const aName: string; const aProc: TPropChangeProc);
begin
  // Not Implemented yet
end;

function TNvChartSeries.GetComponent: TComponent;
begin
  Result := nil;
end;

function TNvChartSeries.GetID: string;
begin
  Result := '';
end;

procedure TNvChartSeries.Invalidate;
begin
  FChart.Invalidate;
end;

function TNvChartSeries.NeedSendChange: Boolean;
begin
  Result := FChart.NeedSendChange;
end;

function TNvChartSeries.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE
end;

procedure TNvChartSeries.Render;
var
  I     : Integer;
  _Serie: TNvChartSerieBase;
begin
  for I := 0 to Count - 1 do
    begin
      _Serie := (Items[I] as TNvChartSerieItem).Serie;
      if not _Serie.FReleased then
        _Serie.Render;
    end;
end;

function TNvChartSeries.Rendered: Boolean;
begin
  Result := FChart.Rendered;
end;

procedure TNvChartSeries.ReRender(Now: Boolean);
begin
  FChart.ReRender(Now);
end;

function TNvChartSeries._AddRef: Integer;
begin
  Result := -1;
  // -1 indicates no reference counting is taking place
end;

function TNvChartSeries._Release: Integer;
begin
  Result := -1;
  // -1 indicates no reference counting is taking place
end;

initialization

RegisterClass(TNvChartSerieItem);
RegisterClass(TNvChartSerieArea);
RegisterClass(TNvChartSerieBase);

finalization

UnRegisterClass(TNvChartSerieItem);
UnRegisterClass(TNvChartSerieArea);
UnRegisterClass(TNvChartSerieBase);

end.
