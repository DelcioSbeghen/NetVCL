unit NV.VCL.Dialogs;

interface

uses
  Classes, UITypes, SysUtils, Consts, NV.Controls, NV.Ajax, NV.JSON, Generics.Collections;

const
  { Message dialog }
  mtWarning      = UITypes.TMsgDlgType.mtWarning;
  mtError        = UITypes.TMsgDlgType.mtError;
  mtInformation  = UITypes.TMsgDlgType.mtInformation;
  mtConfirmation = UITypes.TMsgDlgType.mtConfirmation;
  mtCustom       = UITypes.TMsgDlgType.mtCustom;

  mbYes      = UITypes.TMsgDlgBtn.mbYes;
  mbNo       = UITypes.TMsgDlgBtn.mbNo;
  mbOK       = UITypes.TMsgDlgBtn.mbOK;
  mbCancel   = UITypes.TMsgDlgBtn.mbCancel;
  mbAbort    = UITypes.TMsgDlgBtn.mbAbort;
  mbRetry    = UITypes.TMsgDlgBtn.mbRetry;
  mbIgnore   = UITypes.TMsgDlgBtn.mbIgnore;
  mbAll      = UITypes.TMsgDlgBtn.mbAll;
  mbNoToAll  = UITypes.TMsgDlgBtn.mbNoToAll;
  mbYesToAll = UITypes.TMsgDlgBtn.mbYesToAll;
  mbHelp     = UITypes.TMsgDlgBtn.mbHelp;
  mbClose    = UITypes.TMsgDlgBtn.mbClose;

type
  TMsgDlgType = UITypes.TMsgDlgType;
{$NODEFINE TMsgDlgType}
  TMsgDlgBtn = UITypes.TMsgDlgBtn;
{$NODEFINE TMsgDlgBtn}
  TMsgDlgButtons = UITypes.TMsgDlgButtons;
{$NODEFINE TMsgDlgButtons}
  //
{$HPPEMIT OPENNAMESPACE}
{$HPPEMIT 'using Uitypes::TMsgDlgBtn;'}
{$HPPEMIT 'using Uitypes::TMsgDlgButtons;'}
{$HPPEMIT 'using Uitypes::TMsgDlgType;'}
{$HPPEMIT CLOSENAMESPACE}
  //
  TNvMessageType = TMsgDlgType.mtWarning .. TMsgDlgType.mtInformation;

  TNvDialog = class(TNvWinControl)
  protected
    class function DefaultClassCss: string; override;
  private
    FDialogType: TMsgDlgType;
    FTitle     : string;
    procedure SetDialogType(const Value: TMsgDlgType);
    procedure SetTitle(const Value: string);
  protected
    procedure InternalRender(JSON: TJsonObject); override;
    procedure RenderTitle(JSON: TJsonObject);
    procedure RenderDialogType(JSON: TJsonObject);
    // Props
    property DialogType: TMsgDlgType read FDialogType write SetDialogType
      default TMsgDlgType.mtInformation;
    property Title: string read FTitle write SetTitle;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TNvDlgButtonItem = class
  private
    FCaption    : string;
    FModalResult: Integer;
    FDefault    : Boolean;
  public
    property Caption    : string read FCaption write FCaption;
    property ModalResult: Integer read FModalResult write FModalResult;
    property Default    : Boolean read FDefault write FDefault;
  end;

  TDlgBtnList = class(TList<TNvDlgButtonItem>);

  TNvButtonsDialog = class(TNvDialog)
  private
    class function DefaultRenderText: Boolean; override;
  private
    FButtons: TDlgBtnList;
    procedure UpdateButtons(JSON: TJsonObject);
    procedure DoButtonsUpdate(Sender: TObject; const Item: TNvDlgButtonItem;
      Action: TCollectionNotification);
  protected
    procedure InternalRender(JSON: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ShowModal: Integer; override;
    property Buttons: TDlgBtnList read FButtons;
    property DialogType;
    property Text;
    property Title;
  end;

  TNvMessageDialog = class(TNvDialog)
  private
    class function DefaultRenderText: Boolean; override;
  protected
  public
    procedure Show; reintroduce;
  published
    property DialogType;
    property Text;
    property Title;
  end;

const
  TNvDialogTypeStr: array [Low(TMsgDlgType) .. High(TMsgDlgType)] of string = //
    ('warning', 'error', 'info', 'confirm', 'custom');

procedure ShowMessage(Msg: string; Title: string; MsgType: TNvMessageType); overload;
procedure ShowMessage(Msg: string; Title: string); overload;
procedure ShowMessage(Msg: string); overload;

// TaskMessage Dialogs
function TaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint): Integer; overload;
function TaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
function TaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint; DefaultButton: TMsgDlgBtn; CustomButtonCaptions: array of string)
  : Integer; overload;

implementation

uses
  NV.VCL.Forms, NV.Languages, NV.Utils;

{$REGION 'ShowMessage'}

procedure ShowMessage(Msg: string; Title: string; MsgType: TNvMessageType);
var
  _Dlg: TNvMessageDialog;
begin
  if Application.MainForm.Designer <> nil then
    Exit;

  _Dlg := TNvMessageDialog.Create(nil);
  try
    _Dlg.Parent     := FindParentPage(Application.MainForm);
    _Dlg.Text       := Msg;
    _Dlg.Title      := Title;
    _Dlg.DialogType := MsgType;
    _Dlg.Show;
  finally
    _Dlg.Free;
  end;
end;

procedure ShowMessage(Msg: string; Title: string);
begin
  ShowMessage(Msg, Title, TMsgDlgType.mtInformation);
end;

procedure ShowMessage(Msg: string);
begin
  ShowMessage(Msg, LangRes.RsMessage + ':', TMsgDlgType.mtInformation);
end;

{$ENDREGION}
{$REGION 'TaskMessage Dialogs'}

function GetDefaultButton(const Buttons: TMsgDlgButtons): TMsgDlgBtn;
begin
  if mbOk in Buttons then
    Result := mbOk
  else if mbYes in Buttons then
    Result := mbYes
  else
    Result := mbRetry;
end;

function TaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint): Integer; overload;
begin
  Result := TaskMessageDlg(Title, Msg, DlgType, Buttons, HelpCtx, GetDefaultButton(Buttons));
end;

function TaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer; overload;
begin
  Result := TaskMessageDlg(Title, Msg, DlgType, Buttons, HelpCtx, DefaultButton, []);
end;

function Captions(AType: TMsgDlgType): Pointer;
begin
  Result := nil;

  case AType of
    TMsgDlgType.mtWarning: Result      := @SMsgDlgWarning;
    TMsgDlgType.mtError: Result        := @SMsgDlgError;
    TMsgDlgType.mtInformation: Result  := @SMsgDlgInformation;
    TMsgDlgType.mtConfirmation: Result := @SMsgDlgConfirm;
  end;
end;

function ButtonCaptions(AType: TMsgDlgBtn): Pointer;
begin
  Result := nil;
  case AType of
    TMsgDlgBtn.mbYes: Result      := @SMsgDlgYes;
    TMsgDlgBtn.mbNo: Result       := @SMsgDlgNo;
    TMsgDlgBtn.mbOK: Result       := @SMsgDlgOK;
    TMsgDlgBtn.mbCancel: Result   := @SMsgDlgCancel;
    TMsgDlgBtn.mbAbort: Result    := @SMsgDlgAbort;
    TMsgDlgBtn.mbRetry: Result    := @SMsgDlgRetry;
    TMsgDlgBtn.mbIgnore: Result   := @SMsgDlgIgnore;
    TMsgDlgBtn.mbAll: Result      := @SMsgDlgAll;
    TMsgDlgBtn.mbNoToAll: Result  := @SMsgDlgNoToAll;
    TMsgDlgBtn.mbYesToAll: Result := @SMsgDlgYesToAll;
    TMsgDlgBtn.mbHelp: Result     := @SMsgDlgHelp;
    TMsgDlgBtn.mbClose: Result    := @SMsgDlgClose;
  end;

end;

function TaskMessageDlg(const Title, Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint; DefaultButton: TMsgDlgBtn; CustomButtonCaptions: array of string)
  : Integer; overload;
const
  tdbHelp                                      = -1;
  LModalResults: array [TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry,
    mrIgnore, mrAll, mrNoToAll, mrYesToAll, tdbHelp, mrClose);
var
  _Dlg  : TNvButtonsDialog;
  DlgBtn: TMsgDlgBtn;
  LIndex: Integer;
  _Btn  : TNvDlgButtonItem;
begin
  if Application.MainForm.Designer <> nil then
    Exit;

  _Dlg := TNvButtonsDialog.Create(nil);
  try
    _Dlg.Parent := FindParentPage(Application.MainForm);

    LIndex     := -1;
    for DlgBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if DlgBtn in Buttons then
        begin
          Inc(LIndex);
          _Btn := TNvDlgButtonItem.Create;

          if LIndex <= High(CustomButtonCaptions) then
            _Btn.Caption := CustomButtonCaptions[LIndex]
          else
            _Btn.Caption := LoadResString(ButtonCaptions(DlgBtn));
          if DlgBtn = DefaultButton then
            _Btn.Default   := True;
          _Btn.ModalResult := LModalResults[DlgBtn];

          _Dlg.Buttons.Add(_Btn);
        end;

    // Set dialog properties

    if Title.IsEmpty then
      begin
        if DlgType <> mtCustom then
          _Dlg.Title := LoadResString(Captions(DlgType))
        else
          _Dlg.Title := Application.Title;
      end
    else
      _Dlg.Title := Title;

    _Dlg.Text := Msg;

    _Dlg.DialogType := DlgType;

    // Show dialog and return result
    Result := _Dlg.ShowModal;
  finally
    _Dlg.Free;
  end;
end;

{$ENDREGION}
{ TNvDialog }

constructor TNvDialog.Create(AOwner: TComponent);
begin
  inherited;
  Visible := False;
  // FRenderInvisible := False;
  FDialogType := TMsgDlgType.mtInformation;
end;

class function TNvDialog.DefaultClassCss: string;
begin
  Result := 'dialog'
end;

procedure TNvDialog.InternalRender(JSON: TJsonObject);
begin
  inherited;
  if FTitle <> '' then
    RenderTitle(JSON);
  if FDialogType <> TMsgDlgType.mtInformation then
    RenderDialogType(JSON);

  JSON.A['Events'].Add('close.nvjs');
end;

procedure TNvDialog.RenderDialogType(JSON: TJsonObject);
begin
  JSON.S['DialogType'] := TNvDialogTypeStr[FDialogType];
end;

procedure TNvDialog.RenderTitle(JSON: TJsonObject);
begin
  JSON.S['Title'] := FTitle;
end;

procedure TNvDialog.SetDialogType(const Value: TMsgDlgType);
begin
  if Value <> FDialogType then
    begin
      EnqueueChange('DialogType', RenderDialogType);
      FDialogType := Value;
      Invalidate;
    end;
end;

procedure TNvDialog.SetTitle(const Value: string);
begin
  if Value <> FTitle then
    begin
      EnqueueChange('Title', RenderTitle);
      FTitle := Value;
      Invalidate;
    end;
end;

{ TNvMessageDialog }

class function TNvMessageDialog.DefaultRenderText: Boolean;
begin
  Result := True;
end;


procedure TNvMessageDialog.Show;
begin
  ShowModal;
end;

{ TNvButtonsDialog }

constructor TNvButtonsDialog.Create(AOwner: TComponent);
begin
  inherited;
  FButtons          := TDlgBtnList.Create;
  FButtons.OnNotify := DoButtonsUpdate;
end;

class function TNvButtonsDialog.DefaultRenderText: Boolean;
begin
  Result := True;
end;

destructor TNvButtonsDialog.Destroy;
var
  _Btn: TObject;
begin
  for _Btn in FButtons do
    _Btn.Free;
  FButtons.Free;
  inherited;
end;

procedure TNvButtonsDialog.DoButtonsUpdate(Sender: TObject; const Item: TNvDlgButtonItem;
  Action: TCollectionNotification);
begin
  if (Action in [cnAdded, cnRemoved]) and FRendered then
    UpdateButtons(ControlAjaxJson);
end;

procedure TNvButtonsDialog.InternalRender(JSON: TJsonObject);
begin
  inherited;
  UpdateButtons(JSON);
end;

function TNvButtonsDialog.ShowModal: Integer;
begin
  Result := inherited;
end;

procedure TNvButtonsDialog.UpdateButtons(JSON: TJsonObject);
var
  _Button : TNvDlgButtonItem;
  _BtnJson: TJsonObject;
begin
  JSON.A['Buttons'].Clear;
  for _Button in FButtons do
    begin
      _BtnJson                  := JSON.A['Buttons'].AddObject;
      _BtnJson.S['Caption']     := _Button.Caption;
      _BtnJson.I['ModalResult'] := _Button.ModalResult;
    end;
end;

end.
