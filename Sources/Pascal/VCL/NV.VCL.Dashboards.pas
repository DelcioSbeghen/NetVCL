unit NV.VCL.Dashboards;

interface

uses
  Classes, NV.JSON, NV.Controls;

type

  TNvBreakPoint = class(TCollectionItem)
  private
    FMinWidth: Integer;
    FColumns : Integer;
  public
    property MinWidth: Integer read FMinWidth write FMinWidth;
    property Columns : Integer read FColumns write FColumns;
  end;

  TNVDashBreakPoints = class(TCollection)
  private
    function GetItem(Index: Integer): TNvBreakPoint;
    procedure SetItem(Index: Integer; const Value: TNvBreakPoint);
  public
    function Add: TNvBreakPoint;
    property Items[Index: Integer]: TNvBreakPoint read GetItem write SetItem; default;
  end;

  TNvDashboard = class(TNvWinControl)
  protected
    class function DefaultClassCss: string; override;
  private
    FAcceptWidgets: string;
    FAnimate      : Boolean;
    FCellHeight   : string;
    FDisableDrag  : Boolean;
    FDisableResize: Boolean;
    FMargin       : string;
    FBreakpoints  : TNVDashBreakPoints;
    FOnChange     : TNotifyEvent;
    FFloat        : Boolean;
    procedure SetAcceptWidgets(const Value: string);
    procedure SetAnimate(const Value: Boolean);
    procedure SetCellHeight(const Value: string);
    procedure SetDisableDrag(const Value: Boolean);
    procedure SetDisableResize(const Value: Boolean);
    procedure SetMargin(const Value: string);
    procedure SetBreakpoints(const Value: TNVDashBreakPoints);
    procedure SetFloat(const Value: Boolean);
  protected
    function NotDefaultAcceptWidgets: Boolean; inline;
    function NotDefaultCellHeight: Boolean; inline;
    function NotDefaultMargin: Boolean; inline;
    // Render props
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderEvents(aJson: TJsonObject); override;
    procedure RenderAcceptWidgets(JSON: TJsonObject);
    procedure RenderAnimate(JSON: TJsonObject);
    procedure RenderCellHeight(JSON: TJsonObject);
    procedure RenderDisableDrag(JSON: TJsonObject);
    procedure RenderDisableResize(JSON: TJsonObject);
    procedure RenderMargin(JSON: TJsonObject);
    procedure RenderBreakpoints(JSON: TJsonObject);
    procedure RenderFloat(aJson: TJsonObject);
    procedure AfterDesignDrop; override;
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
    procedure DoChange(aEvent: TJsonObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    property Floating;
  published
    // see https://github.com/gridstack/gridstack.js/tree/master/doc#grid-options
    property AcceptWidgets: string read FAcceptWidgets write SetAcceptWidgets
      stored NotDefaultAcceptWidgets;
    property Animate    : Boolean read FAnimate write SetAnimate default True;
    property Breakpoints: TNVDashBreakPoints read FBreakpoints write SetBreakpoints;
    property CellHeight : string read FCellHeight write SetCellHeight stored NotDefaultCellHeight;
    property ClassCss;
    property DisableDrag  : Boolean read FDisableDrag write SetDisableDrag default False;
    property DisableResize: Boolean read FDisableResize write SetDisableResize default False;
    property Float        : Boolean read FFloat write SetFloat default false;
    property Margin       : string read FMargin write SetMargin stored NotDefaultMargin;
    property OnChange     : TNotifyEvent read FOnChange write FOnChange;
  end;

  TNvDashbdItem = class(TNvWinControl)
  protected
    class function DefaultClassCss: string; override;
  private
    FAutoPosition : Boolean;
    FX            : Integer;
    FY            : Integer;
    FW            : Integer;
    FH            : Integer;
    FMaxW         : Integer;
    FMinW         : Integer;
    FMaxH         : Integer;
    FMinH         : Integer;
    FLocked       : Boolean;
    FNoResize     : Boolean;
    FNoMove       : Boolean;
    FSizeToContent: Boolean;
    FOnChange     : TNotifyEvent;
    procedure SetAutoPosition(const Value: Boolean);
    procedure SetH(const Value: Integer);
    procedure SetLocked(const Value: Boolean);
    procedure SetMaxH(const Value: Integer);
    procedure SetMaxW(const Value: Integer);
    procedure SetMinH(const Value: Integer);
    procedure SetMinW(const Value: Integer);
    procedure SetNoMove(const Value: Boolean);
    procedure SetNoResize(const Value: Boolean);
    procedure SetSizeToContent(const Value: Boolean);
    procedure SetW(const Value: Integer);
    procedure SetX(const Value: Integer);
    procedure SetY(const Value: Integer);
  protected
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderAutoPosition(aJson: TJsonObject);
    procedure RenderX(aJson: TJsonObject);
    procedure RenderY(aJson: TJsonObject);
    procedure RenderW(aJson: TJsonObject);
    procedure RenderH(aJson: TJsonObject);
    procedure RenderMaxW(aJson: TJsonObject);
    procedure RenderMinW(aJson: TJsonObject);
    procedure RenderMaxH(aJson: TJsonObject);
    procedure RenderMinH(aJson: TJsonObject);
    procedure RenderLocked(aJson: TJsonObject);
    procedure RenderNoResize(aJson: TJsonObject);
    procedure RenderNoMove(aJson: TJsonObject);
    procedure RenderSizeToContent(aJson: TJsonObject);
    procedure DoChange;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoPosition: Boolean read FAutoPosition write SetAutoPosition;
    property ClassCss;
    property X            : Integer read FX write SetX;
    property Y            : Integer read FY write SetY;
    property W            : Integer read FW write SetW;
    property H            : Integer read FH write SetH;
    property MaxW         : Integer read FMaxW write SetMaxW;
    property MinW         : Integer read FMinW write SetMinW;
    property MaxH         : Integer read FMaxH write SetMaxH;
    property MinH         : Integer read FMinH write SetMinH;
    property Locked       : Boolean read FLocked write SetLocked;
    property NoResize     : Boolean read FNoResize write SetNoResize;
    property NoMove       : Boolean read FNoMove write SetNoMove;
    property SizeToContent: Boolean read FSizeToContent write SetSizeToContent;
    property OnChange     : TNotifyEvent read FOnChange write FOnChange;
  end;

const
  DEFAULT_ACCEPT_WIDGETS = 'false';
  DEFAULT_CELL_HEIGHT    = 'auto';
  DEFAULT_MARGIN         = '10';

implementation

uses
  NV.VCL.Forms;

{ TNvDashboard }

procedure TNvDashboard.AfterDesignDrop;
begin
  inherited;
  // Default Breakpoints
  if FBreakpoints.Count = 0 then
    begin
      with FBreakpoints.Add do
        begin
          MinWidth := 768;
          Columns  := 1;
        end;
      with FBreakpoints.Add do
        begin
          MinWidth := 992;
          Columns  := 4;
        end;
      with FBreakpoints.Add do
        begin
          MinWidth := 1200;
          Columns  := 8;
        end;
    end;
end;

constructor TNvDashboard.Create(AOwner: TComponent);
begin
  inherited;
  FRenderPosition := False;
  FBreakpoints    := TNVDashBreakPoints.Create(TNvBreakPoint);
  FAcceptWidgets  := DEFAULT_ACCEPT_WIDGETS;
  FAnimate        := True;
  FCellHeight     := DEFAULT_CELL_HEIGHT;
end;

class function TNvDashboard.DefaultClassCss: string;
begin
  Result := 'grid-stack';
end;

destructor TNvDashboard.Destroy;
begin
  FBreakpoints.Free;
  inherited;
end;

procedure TNvDashboard.DoChange(aEvent: TJsonObject);
var
  JChanges: TJsonArray;
  JChange : TJsonObject;
  _Item   : TNvDashbdItem;
  C, I    : Integer;
begin
  JChanges := aEvent.A['changes'];

  for I := 0 to JChanges.Count - 1 do
    begin
      JChange := JChanges.O[I];
      for C   := 0 to ControlCount - 1 do

        if Controls[C] is TNvDashbdItem then
          begin
            _Item := TNvDashbdItem(Controls[C]);
            if _Item.ID = JChange.S['iId'] then
              begin
                // use fields
                _Item.FX := JChange.I['x'];
                _Item.FY := JChange.I['y'];
                _Item.FH := JChange.I['h'];
                _Item.FW := JChange.I['w'];
                _Item.DoChange;
              end;
          end;
    end;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNvDashboard.InternalRender(Json: TJsonObject);
begin
  inherited;
  if NotDefaultAcceptWidgets then
    RenderAcceptWidgets(Json);
  if not FAnimate then
    RenderAnimate(Json);
  if NotDefaultCellHeight then
    RenderCellHeight(Json);
  if FDisableDrag then
    RenderDisableDrag(Json);
  if FDisableResize then
    RenderDisableResize(Json);
  if NotDefaultMargin then
    RenderMargin(Json);
  if FFloat then
    RenderFloat(Json);
end;

function TNvDashboard.NotDefaultAcceptWidgets: Boolean;
begin
  Result := FAcceptWidgets <> DEFAULT_ACCEPT_WIDGETS;
end;

function TNvDashboard.NotDefaultCellHeight: Boolean;
begin
  Result := FCellHeight <> DEFAULT_CELL_HEIGHT;
end;

function TNvDashboard.NotDefaultMargin: Boolean;
begin
  Result := FMargin <> DEFAULT_MARGIN;
end;

function TNvDashboard.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  Result := inherited;
  if not Result and (AEventName = 'change') then
    begin
      DoChange(aEvent);
      Result := True;
    end;

end;

procedure TNvDashboard.RenderAcceptWidgets(JSON: TJsonObject);
begin
  JSON.S['AcceptWidgets'] := FAcceptWidgets;
end;

procedure TNvDashboard.RenderAnimate(JSON: TJsonObject);
begin
  JSON.B['Animate'] := FAnimate;
end;

procedure TNvDashboard.RenderBreakpoints(JSON: TJsonObject);
var
  i        : Integer;
  _JBpoints: TJsonArray;
begin
  _JBpoints := JSON.A['BreakPoints'];
  for i     := 0 to FBreakpoints.Count - 1 do
    with FBreakpoints.Items[i], _JBpoints.AddObject do
      begin
        I['w'] := MinWidth;
        I['C'] := Columns;
      end;
end;

procedure TNvDashboard.RenderCellHeight(JSON: TJsonObject);
begin
  JSON.S['CellHeight'] := FCellHeight;
end;

procedure TNvDashboard.RenderDisableDrag(JSON: TJsonObject);
begin
  JSON.B['DisableDrag'] := FAnimate;
end;

procedure TNvDashboard.RenderDisableResize(JSON: TJsonObject);
begin
  JSON.B['DisableResize'] := FAnimate;
end;

procedure TNvDashboard.RenderEvents(aJson: TJsonObject);
begin
  inherited;
  aJson.A['Events'].Add('change');
end;

procedure TNvDashboard.RenderFloat(aJson: TJsonObject);
begin
  aJson.B['Float'] := FFloat;
end;

procedure TNvDashboard.RenderMargin(JSON: TJsonObject);
begin
  JSON.S['Margin'] := FMargin;
end;

procedure TNvDashboard.Reset;
begin
    Screen.Ajax.AddCallFunction(ID, 'Reset', '');
end;

procedure TNvDashboard.SetAcceptWidgets(const Value: string);
begin
  if Value <> FAcceptWidgets then
    begin
      EnqueueChange('AcceptWidgets', RenderAcceptWidgets);
      FAcceptWidgets := Value;
      Invalidate;
    end;
end;

procedure TNvDashboard.SetAnimate(const Value: Boolean);
begin
  if Value <> FAnimate then
    begin
      EnqueueChange('Animate', RenderAnimate);
      FAnimate := Value;
      Invalidate;
    end;
end;

procedure TNvDashboard.SetBreakpoints(const Value: TNVDashBreakPoints);
begin
  if FBreakpoints <> Value then
    begin
      EnqueueChange('Breakpoints', RenderBreakpoints);
      FBreakpoints.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvDashboard.SetCellHeight(const Value: string);
begin
  if Value <> FCellHeight then
    begin
      EnqueueChange('CellHeight', RenderCellHeight);
      FCellHeight := Value;
      Invalidate;
    end;
end;

procedure TNvDashboard.SetDisableDrag(const Value: Boolean);
begin
  if Value <> FDisableDrag then
    begin
      EnqueueChange('DisableDrag', RenderDisableDrag);
      FDisableDrag := Value;
      Invalidate;
    end;
end;

procedure TNvDashboard.SetDisableResize(const Value: Boolean);
begin
  if Value <> FDisableResize then
    begin
      EnqueueChange('DisableResize', RenderDisableResize);
      FDisableResize := Value;
      Invalidate;
    end;

end;

procedure TNvDashboard.SetFloat(const Value: Boolean);
begin
  if Value <> FFloat then
    begin
      EnqueueChange('Float', RenderFloat);
      FFloat := Value;
      Invalidate;
    end;
end;

procedure TNvDashboard.SetMargin(const Value: string);
begin
  if Value <> FMargin then
    begin
      EnqueueChange('Margin', RenderMargin);
      FMargin := Value;
      Invalidate;
    end;

end;

{ TNVDashBreakPoints }

function TNVDashBreakPoints.Add: TNvBreakPoint;
begin
  Result := TNvBreakPoint(Inherited Add)
end;

function TNVDashBreakPoints.GetItem(Index: Integer): TNvBreakPoint;
begin
  Result := TNvBreakPoint(inherited Items[Index]);
end;

procedure TNVDashBreakPoints.SetItem(Index: Integer; const Value: TNvBreakPoint);
begin
  inherited Items[Index] := Value;
end;

{ TNvDashbdItem }

constructor TNvDashbdItem.Create(AOwner: TComponent);
begin
  inherited;
  FRenderPosition := False;
end;

class function TNvDashbdItem.DefaultClassCss: string;
begin
  Result := 'grid-stack-item';
end;

procedure TNvDashbdItem.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNvDashbdItem.InternalRender(Json: TJsonObject);
begin
  inherited;
  if FAutoPosition then
    RenderAutoPosition(Json);
  if FX > 0 then
    RenderX(Json);
  if FY > 0 then
    RenderY(Json);
  if FW <> 0 then
    RenderW(Json);
  if FH <> 0 then
    RenderH(Json);
  if FMaxW <> 0 then
    RenderMaxW(Json);
  if FMinW <> 0 then
    RenderMinW(Json);
  if FMaxH <> 0 then
    RenderMaxH(Json);
  if FMinH <> 0 then
    RenderMinH(Json);
  if FLocked then
    RenderLocked(Json);
  if FNoResize then
    RenderNoResize(Json);
  if FNoMove then
    RenderNoMove(Json);
  if FSizeToContent then
    RenderSizeToContent(Json);
end;

procedure TNvDashbdItem.RenderAutoPosition(aJson: TJsonObject);
begin
  aJson.B['AutoPosition'] := FAutoPosition;
end;

procedure TNvDashbdItem.RenderX(aJson: TJsonObject);
begin
  aJson.I['X'] := FX;
end;

procedure TNvDashbdItem.RenderY(aJson: TJsonObject);
begin
  aJson.I['Y'] := FY;
end;

procedure TNvDashbdItem.RenderW(aJson: TJsonObject);
begin
  aJson.I['W'] := FW;
end;

procedure TNvDashbdItem.RenderH(aJson: TJsonObject);
begin
  aJson.I['H'] := FH;
end;

procedure TNvDashbdItem.RenderMaxW(aJson: TJsonObject);
begin
  aJson.I['MaxW'] := FMaxW;
end;

procedure TNvDashbdItem.RenderMinW(aJson: TJsonObject);
begin
  aJson.I['MinW'] := FMinW;
end;

procedure TNvDashbdItem.RenderMaxH(aJson: TJsonObject);
begin
  aJson.I['MaxH'] := FMaxH;
end;

procedure TNvDashbdItem.RenderMinH(aJson: TJsonObject);
begin
  aJson.I['MinH'] := FMinH;
end;

procedure TNvDashbdItem.RenderLocked(aJson: TJsonObject);
begin
  aJson.B['Locked'] := FLocked;
end;

procedure TNvDashbdItem.RenderNoResize(aJson: TJsonObject);
begin
  aJson.B['NoResize'] := FNoResize;
end;

procedure TNvDashbdItem.RenderNoMove(aJson: TJsonObject);
begin
  aJson.B['NoMove'] := FNoMove;
end;

procedure TNvDashbdItem.RenderSizeToContent(aJson: TJsonObject);
begin
  aJson.B['SizeToContent'] := FSizeToContent;
end;

procedure TNvDashbdItem.SetAutoPosition(const Value: Boolean);
begin
  if Value <> FAutoPosition then
    begin
      EnqueueChange('AutoPosition', RenderAutoPosition);
      FAutoPosition := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetH(const Value: Integer);
begin
  if Value <> FH then
    begin
      EnqueueChange('H', RenderH);
      FH := Value;
      Invalidate;
    end;

end;

procedure TNvDashbdItem.SetLocked(const Value: Boolean);
begin
  if Value <> FLocked then
    begin
      EnqueueChange('Locked', RenderLocked);
      FLocked := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetMaxH(const Value: Integer);
begin
  if Value <> FMaxH then
    begin
      EnqueueChange('MaxH', RenderMaxH);
      FMaxH := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetMaxW(const Value: Integer);
begin
  if Value <> FMaxW then
    begin
      EnqueueChange('MaxW', RenderMaxW);
      FMaxW := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetMinH(const Value: Integer);
begin
  if Value <> FMinH then
    begin
      EnqueueChange('MinH', RenderMinH);
      FMinH := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetMinW(const Value: Integer);
begin
  if Value <> FMinW then
    begin
      EnqueueChange('MinW', RenderMinW);
      FMinW := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetNoMove(const Value: Boolean);
begin
  if Value <> FNoMove then
    begin
      EnqueueChange('NoMove', RenderNoMove);
      FNoMove := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetNoResize(const Value: Boolean);
begin
  if Value <> FNoResize then
    begin
      EnqueueChange('NoResize', RenderNoResize);
      FNoResize := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetSizeToContent(const Value: Boolean);
begin
  if Value <> FSizeToContent then
    begin
      EnqueueChange('SizeToContent', RenderSizeToContent);
      FSizeToContent := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetW(const Value: Integer);
begin
  if Value <> FW then
    begin
      EnqueueChange('W', RenderW);
      FW := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetX(const Value: Integer);
begin
  if Value <> FX then
    begin
      EnqueueChange('X', RenderX);
      FX := Value;
      Invalidate;
    end;
end;

procedure TNvDashbdItem.SetY(const Value: Integer);
begin
  if Value <> FY then
    begin
      EnqueueChange('Y', RenderY);
      FY := Value;
      Invalidate;
    end;
end;

end.
