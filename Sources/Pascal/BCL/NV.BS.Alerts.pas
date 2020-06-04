unit NV.BS.Alerts;

interface

uses
  Classes, NV.BS.Containers, NV.Ajax, NV.Json, NV.BS.Types, NV.VCL.Images;

type

  TNvBsAlert = class(TNvBsGridContainer)
  protected
    class function BackgroundDefault: TBsBackground; override;
    class function FadeDefault: Boolean; override;
  private
    FTimeOut  : Integer;
    FShowClose: Boolean;
    FOnClose  : TNotifyEvent;
    procedure SetTimeOut(const Value: Integer);
    procedure SetShowClose(const Value: Boolean);
  protected
    procedure AddIncludes(Ajax: TNvAjax); override;
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); override;
    // Events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
    procedure DoClose(aEvent: TJsonObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Close;
  published
    property TimeOut  : Integer read FTimeOut write SetTimeOut default 0;
    property ShowClose: Boolean read FShowClose write SetShowClose default True;
    property OnClose  : TNotifyEvent read FOnClose write FOnClose;
    property Text;
    property Grids;
    property Background stored IsNotBgDefault;
    property Border;
    property Shadow default bssNone;
    property TextProps;
  end;

  TNvBsToast = class(TNvBsAlert)
  protected
    class function SupportImage: Boolean; override; // Activate ImageListLink Support in class
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
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
  published
    // property Images    : TNvCustomImageList read GetImages write SetImages;
    // property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property ImageListLink;
    property Title     : string read FTitle write SetTitle;
    property TitleSmall: string read FTitleSmall write SetTitleSmall;
    property Text;
    property Grids;
    property Background stored IsNotBgDefault;
    property Border;
    property Shadow default bssNone;
    property TextProps;
    property Visible default False;
  end;

implementation

{ TNvBsAlert }

procedure TNvBsAlert.AddIncludes(Ajax: TNvAjax);
begin
  inherited;
  if Ajax <> nil then
    Ajax.AddInclude('nv.bs.alerts.js', reqModule, '/nv.bs.alerts.js');
end;

class function TNvBsAlert.BackgroundDefault: TBsBackground;
begin
  Result := bsbgInfo;
end;

procedure TNvBsAlert.Close;
begin
  Ajax.AddCallFunction(ID, 'Close', '');
end;

constructor TNvBsAlert.Create(AOwner: TComponent);
begin
  inherited;
  FShowClose := True;
end;

procedure TNvBsAlert.DoClose(aEvent: TJsonObject);
begin
  Visible := False;
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

class function TNvBsAlert.FadeDefault: Boolean;
begin
  Result := True;
end;

procedure TNvBsAlert.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  inherited;
  if FTimeOut <> 0 then
    Json.I['Timeout'] := FTimeOut;
  if Not FShowClose then
    Json.B['ShowClose'] := FShowClose;
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

procedure TNvBsAlert.SetShowClose(const Value: Boolean);
begin
  if FShowClose <> Value then
    begin
      if NeedSendChange then
        ControlAjaxJson.B['ShowClose'] := Value;
      FShowClose                       := Value;
      Invalidate;
    end;
end;

procedure TNvBsAlert.SetTimeOut(const Value: Integer);
begin
  if FTimeOut <> Value then
    begin
      if NeedSendChange then
        ControlAjaxJson.I['Timeout'] := Value;
      FTimeOut                       := Value;
    end;
end;

{ TNvBsToast }

constructor TNvBsToast.Create(AOwner: TComponent);
begin
  inherited;
  Visible := False;
end;

destructor TNvBsToast.Destroy;
begin
  inherited;
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

procedure TNvBsToast.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  inherited;
  if FTitle <> '' then
    Json.S['Title'] := FTitle;
  if FTitleSmall <> '' then
    Json.S['TitleSmall'] := FTitleSmall;
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
      if Rendered then
        ControlAjaxJson.S['Title'] := Value;
      FTitle                       := Value;
      Invalidate;
    end;
end;

procedure TNvBsToast.SetTitleSmall(const Value: string);
begin
  if Value <> FTitleSmall then
    begin
      if Rendered then
        ControlAjaxJson.S['TitleSmall'] := Value;
      FTitleSmall                       := Value;
      Invalidate;
    end;
end;

procedure TNvBsToast.Show;
begin
  Visible := True;
end;

class function TNvBsToast.SupportImage: Boolean;
begin
  Result := True;
end;

end.
