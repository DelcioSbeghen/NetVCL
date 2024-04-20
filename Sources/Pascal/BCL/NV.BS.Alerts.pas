unit NV.BS.Alerts;

interface

uses
  Classes, NV.Controls, NV.BS.Controls, NV.BS.Containers, NV.Ajax, NV.Json, NV.BS.Types,
  NV.VCL.Images;

type

  TNvBsAlert = class(TNvBsGridContainer)
  protected
    class function BackgroundDefault: TBsBackground; override;
    class function FadeDefault: Boolean; override;
    class function DefaultClassCss: string; override;
    class function DefaultRenderText: Boolean; override;
  private
    FTimeOut       : Integer;
    FShowClose     : Boolean;
    FOnClose       : TNotifyEvent;
    FDestroyOnClose: Boolean;
    FFloatPos      : TNvFloatPos;
    procedure SetTimeOut(const Value: Integer);
    procedure SetShowClose(const Value: Boolean);
    procedure SetFloatPos(const Value: TNvFloatPos);
  protected
    procedure AddIncludes; override;
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderFloatPos(aJson: TJsonObject);
    procedure RenderTimeout(aJson: TJsonObject);
    procedure RenderShowClose(aJson: TJsonObject);
    // Events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
    procedure DoClose(aEvent: TJsonObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Close;
    procedure Show;
  published
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property DestroyOnClose: Boolean read FDestroyOnClose write FDestroyOnClose default False;
    property FloatPos      : TNvFloatPos read FFloatPos write SetFloatPos default fposNull;
    property Fade;
    property Grids;
    property Position;
    property Shadow default bssNone;
    property ShowClose: Boolean read FShowClose write SetShowClose default True;
    property Text;
    property TextProps;
    property TextVisible;
    property TimeOut: Integer read FTimeOut write SetTimeOut default 0;
    property Visible default False;
    property Width_;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  TNvBsToast = class(TNvBsAlert)
  protected
    class function SupportImage: Boolean; override; // Activate ImageListLink Support in class
    class function DefaultClassCss: string; override;
  private
    FTitle     : string;
    FTitleSmall: string;
    procedure SetTitle(const Value: string);
    // procedure SetImageIndex(const Value: Integer);
    // procedure SetImages(const Value: TNvCustomImageList);
    procedure SetTitleSmall(const Value: string);
    // function GetImages: TNvCustomImageList;
    // function GetImageIndex: Integer;
  protected
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderTitle(aJson: TJsonObject);
    procedure RenderTitleSmall(aJson: TJsonObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Hide;
  published
    // property Images    : TNvCustomImageList read GetImages write SetImages;
    // property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property ImageListLink;
    property TimeOut default 5000;
    property Title     : string read FTitle write SetTitle;
    property TitleSmall: string read FTitleSmall write SetTitleSmall;
  end;

  TBsSpint = (sptBorder, sptGrow);

  TNvBsSpinner = class(TNvBsGridControl)
  protected
    class function DefaultClassCss: string; override;
  private
    FSpinType: TBsSpint;
    FFloatPos: TNvFloatPos;
    procedure SetSpinType(const Value: TBsSpint);
    function GetColor: TBsBackground;
    procedure SetColor(const Value: TBsBackground);
    procedure SetFloatPos(const Value: TNvFloatPos);
  protected
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderFloatPos(aJson: TJsonObject);
  published
    property ClassCss;
    property Color   : TBsBackground read GetColor write SetColor default bsbgSecondary;
    property FloatPos: TNvFloatPos read FFloatPos write SetFloatPos default fposNull;
    property Grids;
    property Position;
    property SpinType: TBsSpint read FSpinType write SetSpinType default sptBorder;
    property Width_;
  end;

const
  TBsSpintStr: array [Low(TBsSpint) .. High(TBsSpint)] of string = //
    ('border', 'grow');

procedure ShowAlert(aText: string; aTimeout: Integer = 0; aParent: TNvWinControl = nil);

implementation

uses
  SysUtils, NV.VCL.Forms;

procedure ShowAlert(aText: string; aTimeout: Integer = 0; aParent: TNvWinControl = nil);
begin
  if aTimeout = 0 then
    aTimeout := 10000;
  with TNvBsAlert.Create(Application) do
    begin
      DestroyOnClose := True;
      Text           := aText;
      TimeOut        := aTimeout;
      if Parent = nil then
        begin
          FloatPos := fposTopRight;
          Parent   := Application.MainForm;
        end
      else
        Parent := aParent;
      Show;
    end;
end;

{ TNvBsAlert }

procedure TNvBsAlert.AddIncludes;
begin
  inherited;
  if Screen.Ajax <> nil then
    Screen.Ajax.AddInclude('nv.bs.alerts.js', reqModule, '/nv.bs.alerts.js');
end;

class function TNvBsAlert.BackgroundDefault: TBsBackground;
begin
  Result := bsbgInfo;
end;

procedure TNvBsAlert.Close;
begin
  Screen.Ajax.AddCallFunction(ID, 'Close', '');
end;

constructor TNvBsAlert.Create(AOwner: TComponent);
begin
  Visible := False;
  inherited;
  FShowClose := True;
end;

class function TNvBsAlert.DefaultClassCss: string;
begin
  result := 'alert';
end;

class function TNvBsAlert.DefaultRenderText: Boolean;
begin
  Result := True;
end;

procedure TNvBsAlert.DoClose(aEvent: TJsonObject);
begin
  Visible := False;
  if Assigned(FOnClose) then
    FOnClose(Self);
  if FDestroyOnClose then
    Free;
end;

class function TNvBsAlert.FadeDefault: Boolean;
begin
  Result := True;
end;

procedure TNvBsAlert.InternalRender(Json: TJsonObject);
begin
  inherited;
  if FFloatPos <> fposNull then
    RenderFloatPos(Json);
  if FTimeOut <> 0 then
    RenderTimeout(Json);
  if Not FShowClose then
    RenderShowClose(Json);
  Json.A['Events'].Add('close.bs.alert');
end;

function TNvBsAlert.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  if AEventName = 'close' then
    begin
      DoClose(aEvent);
      Result := True;
    end
  else
    Result := inherited;
end;

procedure TNvBsAlert.RenderFloatPos(aJson: TJsonObject);
begin
  aJson.S['FloatPos'] := TNvFloatPosStr[FFloatPos];
end;

procedure TNvBsAlert.RenderShowClose(aJson: TJsonObject);
begin
  aJson.B['ShowClose'] := FShowClose;
end;

procedure TNvBsAlert.RenderTimeout(aJson: TJsonObject);
begin
  aJson.I['Timeout'] := FTimeOut;
end;

procedure TNvBsAlert.SetFloatPos(const Value: TNvFloatPos);
begin
  if Value <> FFloatPos then
    begin
      EnqueueChange('FloatPos', RenderFloatPos);
      FFloatPos := Value;
      Invalidate;
    end;
end;

procedure TNvBsAlert.SetShowClose(const Value: Boolean);
begin
  if FShowClose <> Value then
    begin
      EnqueueChange('ShowClose', RenderShowClose);
      FShowClose := Value;
      Invalidate;
    end;
end;

procedure TNvBsAlert.SetTimeOut(const Value: Integer);
begin
  if FTimeOut <> Value then
    begin
      EnqueueChange('Timeout', RenderTimeOut);
      FTimeOut := Value;
    end;
end;

procedure TNvBsAlert.Show;
begin
  VIsible := True;
end;

{ TNvBsToast }

constructor TNvBsToast.Create(AOwner: TComponent);
begin
  inherited;
  FTimeOut := 5000;
end;

class function TNvBsToast.DefaultClassCss: string;
begin
  Result := 'toast';
end;

procedure TNvBsToast.Hide;
begin
  Visible := False;
end;

// function TNvBsToast.GetImageIndex: Integer;
// begin
// Result:= FImageChangeLink
// end;
//
// function TNvBsToast.GetImages: TNvCustomImageList;
// begin
// Result := FImageChangeLink.Images;
// end;

procedure TNvBsToast.InternalRender(Json: TJsonObject);
begin
  inherited;
  if FTitle <> '' then
    RenderTitle(Json);
  if FTitleSmall <> '' then
    RenderTitleSmall(Json);
end;

procedure TNvBsToast.RenderTitle(aJson: TJsonObject);
begin
  aJson.S['Title'] := FTitle;
end;

procedure TNvBsToast.RenderTitleSmall(aJson: TJsonObject);
begin
  aJson.S['TitleSmall'] := FTitleSmall;
end;

// procedure TNvBsToast.SetImageIndex(const Value: Integer);
// begin
// if Value <> FImageIndex then
// begin
// ImageListChange(nil);
// FImageIndex := Value;
// end;
// end;
//
// procedure TNvBsToast.SetImages(const Value: TNvCustomImageList);
// begin
// if Value <> Images then
// begin
// FImageChangeLink.Images := Value;
// ImageListChange(nil);
// end;
// end;

procedure TNvBsToast.SetTitle(const Value: string);
begin
  if Value <> FTitle then
    begin
      EnqueueChange('Title', RenderTitle);
      FTitle := Value;
      Invalidate;
    end;
end;

procedure TNvBsToast.SetTitleSmall(const Value: string);
begin
  if Value <> FTitleSmall then
    begin
      EnqueueChange('TitleSmall', RenderTitleSmall);
      FTitleSmall := Value;
      Invalidate;
    end;
end;

class function TNvBsToast.SupportImage: Boolean;
begin
  Result := True;
end;

{ TNvBsSpinner }

class function TNvBsSpinner.DefaultClassCss: string;
begin
  Result := 'spinner-border';
end;

function TNvBsSpinner.GetColor: TBsBackground;
begin
  Result := TextProps.Color;
end;

procedure TNvBsSpinner.InternalRender(Json: TJsonObject);
begin
  inherited;
  if FFloatPos <> fposNull then
    RenderFloatPos(Json);
end;

procedure TNvBsSpinner.RenderFloatPos(aJson: TJsonObject);
begin
  aJson.S['FloatPos'] := TNvFloatPosStr[FFloatPos];
end;

procedure TNvBsSpinner.SetColor(const Value: TBsBackground);
begin
  TextProps.Color := Value;
end;

procedure TNvBsSpinner.SetFloatPos(const Value: TNvFloatPos);
begin
  if Value <> FFloatPos then
    begin
      EnqueueChange('FloatPos', RenderFloatPos);
      FFloatPos := Value;
      Invalidate;
    end;
end;

procedure TNvBsSpinner.SetSpinType(const Value: TBsSpint);
begin
  if Value <> FSpinType then
    begin
      // change only css class
      ClassCss := ClassCss.Replace('spinner-' + TBsSpintStr[FSpinType],
        'spinner-' + TBsSpintStr[Value], []);
      FSpinType := Value;
      Invalidate;
    end;
end;

end.
