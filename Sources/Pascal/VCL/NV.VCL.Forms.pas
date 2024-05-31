unit NV.VCL.Forms;

interface

uses
  Classes, Windows, Messages, SysUtils, Controls, NV.VCL.Frame, Generics.Collections,
  NV.Ajax, NV.VCL.Page;

const
  WM_NV = WM_APP + 10000;

  WM_SCR_DATA = WM_NV + 1;

type
  TNVForm = class;

  TExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  TWMScreenData = record
    Msg: Cardinal;
    WParam: WParam;
    LParam: LParam;
    Result: LRESULT;
    Data: string;
  end;

  TNvScreen = class(TComponent)
  private
    FPage        : TNvPage;
    FDataId      : Integer;
    FDataReceived: TDictionary<Integer, String>;
    FActive      : Boolean;
    FForms       : TList;
    FDataModules : TList;
    FFrames      : Tlist;
  protected
    FUrlBase: string;
    // Browser requests
    procedure DataReceived(const Data:PChar);
    procedure MsgDataReceived(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddDataModule(DataModule: TDataModule);
    procedure AddForm(AForm: TNVForm);
    procedure AddFrame(AFrame: TNVBaseFrame);
    procedure RemoveDataModule(DataModule: TDataModule);
    procedure RemoveForm(AForm: TNVForm);
    procedure RemoveFrame(AFrame: TNVBaseFrame);
    function FormCount: Integer;
    function FrameCount: Integer;
    function DataModuleCount: Integer;
    function Forms(Index: Integer): TNVForm;
    function Frames(Index: Integer): TNVBaseFrame;
    function DataModules(Index: Integer): TDataModule;
    procedure Show; virtual; abstract;
    procedure Close; virtual;
    procedure UpdateScreen(Updates: string); virtual; abstract;
    function Ajax: TNvAjax; inline;
    function Active: Boolean; virtual;
    property Page: TNVPage read FPage;
  end;

  TNVForm = class(TNvFrame)
  private
    class function SupportImage: Boolean; override;
  private
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    function ShowModal: Integer; override;
    property InModal;
    property ModalResult;
  published
    property Caption;
    property ClassCss;
    property ClientWidth;
    property ClientHeight;
    property ImageListLink;
    property Visible default False;
  end;

  TNvApplication = class(TComponent)
  private
    FTerminate     : Boolean;
    FRootPath      : string;
   // FUrlBase       : string;
    FHandleCreated : Boolean;
    FObjectInstance: Pointer;
    FTitle         : string;
    FHandle        : HWnd;
    FCssFile       : string;
    FModalLevel    : Integer;
    FOnException   : TExceptionEvent;
    procedure WndProc(var Message: TMessage);
    function GetRootPath: string;
    procedure SetCssFile(const Value: string);
    function GetTitle: string;
  protected
    FMainForm      : TNVForm;
    FRunning       : Boolean;
    FDesignInstance: Boolean;
    function ProcessMessage(var Msg: TMsg): Boolean;
    procedure Idle(const Msg: TMsg);
    function IsPreProcessMessage(var Msg: TMsg): Boolean;
    procedure CreateHandle;
  public
    constructor Create(AOwner: TComponent); override;
    procedure HandleException(Sender: TObject);
    procedure HandleMessage;
    procedure ProcessMessages;
    procedure ModalStarted;
    procedure ModalFinished;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure ShowMessage(aMsg: string);
    procedure ShowException(E: Exception);
    procedure Initialize;
    // For design only ???
    function Handle: THandle;
    function ExeName: string;
    procedure Run; virtual;
    procedure Terminate;
    function UrlBase:string; inline;
    property CssFile: string read FCssFile write SetCssFile;
    property Terminated: Boolean read FTerminate;
   // property UrlBase: string read FUrlBase;
    property RootPath: string read GetRootPath write FRootPath;
    property Running: Boolean read FRunning;
    property MainForm: TNVForm read FMainForm;
    property Title: string read GetTitle write FTitle;
    property OnException: TExceptionEvent read FOnException write FOnException;
  end;

var
  Application   : TNvApplication = nil;
  Screen        : TNvScreen      = nil;
  InitScreenProc: procedure      = nil;

implementation

uses StrUtils, RTLConsts, {$IFNDEF FPC} Consts,{$ENDIF} NV.JSON, Rtti, NV.VCL.Dialogs,
  NV.Request.Exe;

var
  WindowClass: TWndClass = (style: 0; lpfnWndProc: @DefWindowProc; cbClsExtra: 0; cbWndExtra: 0;
    hInstance: 0; hIcon: 0; hCursor: 0; hbrBackground: 0; lpszMenuName: nil;
    lpszClassName: 'TApplication');

  { TNvApplicationBase }

constructor TNvApplication.Create(AOwner: TComponent);
var
  P         : PChar;
  ModuleName: array [0 .. 255] of Char;
begin
  inherited;
  GetModuleFileName(MainInstance, ModuleName, Length(ModuleName));
  P := AnsiStrRScan(ModuleName, '\');
  if P <> nil then
    StrCopy(ModuleName, P + 1);
  P := AnsiStrScan(ModuleName, '.');
  if P <> nil then
    P^ := #0;
  CharLower(CharNext(ModuleName));
  FTitle := ModuleName;
  if not IsLibrary then
    CreateHandle;
//  FUrlBase := Screen.FUrlBase;
end;

procedure TNvApplication.CreateForm(InstanceClass: TComponentClass;

  var Reference);
var
  Instance: TComponent;
begin
  Instance := nil;
  try
    Instance              := TComponent(InstanceClass.NewInstance);
    TComponent(Reference) := Instance;
    try
      Instance.Create(Self);
    except
      TComponent(Reference) := nil;
      Instance              := nil;
      raise;
    end;

    if (FMainForm = nil) and (Instance is TNVForm) then
      begin
        // TNVForm(Instance).HandleNeeded;
        FMainForm := TNVForm(Instance);

      end;
  finally
    // if (FMainForm = nil) and (Instance is TNVForm) then
    // TNVForm(Instance).FCreatingMainForm := False;
  end;

end;

procedure TNvApplication.CreateHandle;
var
  LHandle  : HWnd;
  SysMenu  : HMenu;
  TempClass: TWndClass;
begin

{$IFDEF MSWINDOWS}
  if not FHandleCreated and not IsConsole then
{$ENDIF}
{$IFDEF LINUX}
    if not FHandleCreated then
{$ENDIF}
      begin
        FObjectInstance         := MakeObjectInstance(WndProc);
        WindowClass.lpfnWndProc := @DefWindowProc;
        if not GetClassInfo(hInstance, WindowClass.lpszClassName, TempClass) then
          begin
            WindowClass.hInstance := hInstance;
            if Windows.RegisterClass(WindowClass) = 0 then
              raise EOutOfResources.Create(SWindowClass);
          end;
        LHandle := CreateWindowEx(WS_EX_TOOLWINDOW, WindowClass.lpszClassName, PChar(FTitle),
          WS_POPUP or WS_CAPTION or WS_CLIPSIBLINGS or WS_SYSMENU or WS_MINIMIZEBOX,
          GetSystemMetrics(SM_CXSCREEN) div 2, GetSystemMetrics(SM_CYSCREEN) div 2, 0, 0, 0, 0,
          hInstance, nil);

        FHandle        := LHandle;
        FHandleCreated := True;
        {$IFNDEF FPC}
        if TOSVersion.Check(5, 1) then
          WTSRegisterSessionNotification(LHandle, NOTIFY_FOR_THIS_SESSION);
        {$ENDIF}
        // if TOSVersion.Check(6, 0) then
        // BufferedPaintInit;

        SetWindowLong(FHandle, GWL_WNDPROC, LParam(FObjectInstance));

        if NewStyleControls then
          begin
            // SendMessage(Handle, WM_SETICON, ICON_BIG, LParam(GetIconHandle));
            // SetClassLong(Handle, GCL_HICON, LParam(GetIconHandle));
          end;
        SysMenu := GetSystemMenu(Handle, False);
        DeleteMenu(SysMenu, SC_MAXIMIZE, MF_BYCOMMAND);
        DeleteMenu(SysMenu, SC_SIZE, MF_BYCOMMAND);
        if NewStyleControls then
          DeleteMenu(SysMenu, SC_MOVE, MF_BYCOMMAND);
      end;
end;

function TNvApplication.ExeName: string;
begin
  Result := Paramstr(0);
end;

function TNvApplication.GetRootPath: string;
begin
  Result := FRootPath;
end;

function TNvApplication.GetTitle: string;
begin
  if FTitle.IsEmpty and Assigned(FMainForm) then
    Result := FMainForm.Caption
  else
    Result := FTitle;
end;

function TNvApplication.Handle: THandle;
begin
  Result := FHandle;
end;

procedure TNvApplication.HandleException(Sender: TObject);
var
  O: TObject;
begin
  if GetCapture <> 0 then
    SendMessage(GetCapture, WM_CANCELMODE, 0, 0);

  if FDesignInstance then
    Exit;

  O := ExceptObject;
  if O is Exception then
    begin
      if not(O is EAbort) then
        if Assigned(FOnException) then
          FOnException(Sender, Exception(O))
        else
          ShowException(Exception(O));
    end
  else
    SysUtils.ShowException(O, ExceptAddr);
end;

procedure TNvApplication.HandleMessage;
var
  Msg: TMsg;
begin
  if not ProcessMessage(Msg) then
    Idle(Msg);
end;

procedure TNvApplication.Idle(

  const Msg: TMsg);
var
  // Control: TControl;
  Done: Boolean;
begin
  // Control := DoMouseIdle;
  // if FShowHint and (FMouseControl = nil) then
  // CancelHint;
  // Application.Hint := GetLongHint(GetHint(Control));
  Done := True;
  try
    { TODO -Delcio -Application : Implement OnIdle Event }
    // if Assigned(FOnIdle) then FOnIdle(Self, Done);
    { TODO -oDelcio -cApplication : Implement TAction Idle Notification }
    // if Done then
    // if FActionUpdateDelay <= 0 then
    // DoActionIdle
    // else if IdleTimerHandle = 0 then
    // begin
    // // Constantly assigning to the IdleTimerDelegate causes a
    // // memory allocation, and alot of TFNTimerProc's appear in Gen0 because of this.
    // // Only assign the delgate once; that is all that is needed.
    // if not Assigned(IdleTimerDelegate) then
    // IdleTimerDelegate := @IdleTimerProc;
    // IdleTimerHandle     := SetTimer(0, 0, FActionUpdateDelay, IdleTimerDelegate);
    // if IdleTimerHandle = 0 then
    // DoActionIdle
    // end;
  except
    HandleException(Self);
  end;

{$IFDEF MSWINDOWS}
  if (GetCurrentThreadID = MainThreadID) and CheckSynchronize then
{$ENDIF}
{$IFDEF LINUX}
    if (Libc.GetCurrentThreadID = MainThreadID) and CheckSynchronize then
{$ENDIF}
      Done := False;

  if Done then
    WaitMessage;
end;

procedure TNvApplication.Initialize;
begin
  if FRootPath.IsEmpty then
    FRootPath := ExtractFilePath(ParamStr(0)) + 'www\';

  { This used to call InitProc, which was only used for COM and CORBA.
    Neither is used with the .NET version }

  if InitProc <> nil then
    TProcedure(InitProc);
end;

function TNvApplication.IsPreProcessMessage(var Msg: TMsg): Boolean;
var
  LMessage: TMessage;
begin
  // Update Local Controls with data received
  Result := (Msg.Message = WM_SCR_DATA) //
    and (Screen <> nil);
  if Result then
    begin
      LMessage.Msg    := Msg.Message;
      LMessage.WParam := Msg.WParam;
      LMessage.LParam := Msg.LParam;
      LMessage.Result := 0;
      Screen.MsgDataReceived(LMessage);
    end;
end;

procedure TNvApplication.ModalFinished;
begin
  dec(FModalLevel);
end;

procedure TNvApplication.ModalStarted;
var
  _CurrentLevel: Integer;
begin
  inc(FModalLevel);
  _CurrentLevel := FModalLevel;
  Screen.Ajax.EndUpdate;
  try
    Screen.Ajax.Invalidate;
    repeat
      try
        HandleMessage;
      except
        HandleException(Self);
      end;

      if Terminated then
        Abort;

    until (FModalLevel < _CurrentLevel);
  finally
    Screen.Ajax.BeginUpdate;
  end;
end;

function TNvApplication.ProcessMessage(
  var Msg: TMsg): Boolean;
var
  Handled  : Boolean;
  Unicode  : Boolean;
  MsgExists: Boolean;
begin
  Result := False;
  if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
    begin
      Unicode := (Msg.HWnd = 0) or IsWindowUnicode(Msg.HWnd);
      if Unicode then
        MsgExists := PeekMessageW(Msg, 0, 0, 0, PM_REMOVE)
      else
        MsgExists := PeekMessageA(Msg, 0, 0, 0, PM_REMOVE);

      if MsgExists then
        begin
          Result := True;
          if Msg.Message <> WM_QUIT then
            begin
              Handled := False;
              { TODO -oDelcio -cApplication : Implement OnMessage Event }
              // if Assigned(FOnMessage) then FOnMessage(Msg, Handled);
              if not IsPreProcessMessage(Msg) and
              { not IsHintMsg(Msg) and } not Handled { and
                not IsMDIMsg(Msg) and not IsKeyMsg(Msg) and not IsDlgMsg(Msg) }
              then
                begin
                  TranslateMessage(Msg);
                  if Unicode then
                    DispatchMessageW(Msg)
                  else
                    DispatchMessageA(Msg);
                end;
            end
          else
            FTerminate := True;
        end;
    end;
end;

procedure TNvApplication.ProcessMessages;
var
  Msg: TMsg;
begin
  while ProcessMessage(Msg) do { loop };
end;

procedure TNvApplication.Run;
begin
  FRunning := True;
  try
    if FMainForm <> nil then
      begin
        // Show "Screen" Window
        if Assigned(Screen) then
          Screen.Show;

        FMainForm.Show;
        repeat
          try
            HandleMessage;
          except
            HandleException(Self);
          end;
        until Terminated;
      end;
  finally
    FRunning := False;
  end;
end;

procedure TNvApplication.SetCssFile(const Value: string);
begin
  if Value <> FCssFile then
    begin
      // remove old
      if (Not FCssFile.IsEmpty) and (Screen <> nil) then
        Screen.FPage.RemoveCssFile(FCssFile);
      // add new
      if (Screen <> nil) then
        Screen.FPage.AddCssFile(Value);

      FCssFile := Value;
    end;
end;

procedure TNvApplication.ShowException(E: Exception);
var
  Msg : string;
  SubE: Exception;
begin
  Msg := E.Message;
  {$IFNDEF FPC}
  while True do
    begin
      SubE := E.GetBaseException;
      if SubE <> E then
        begin
          E := SubE;
          if E.Message <> '' then
            Msg := E.Message;
        end
      else
        Break;
    end;
  {$ENDIF}
  if (Msg <> '') and (Msg[Length(Msg)] > '.') then
    Msg := Msg + '.';
  { TODO -oDelcio -cDesign : Show Design exceptions }
  if not FDesignInstance then
    {$IFDEF FPC}
    begin
      OutputDebugString('!!!!!!NO FPC ISSO NÃO ESTÁ THREAD SAFE!!!!!!');
      NV.VCL.Dialogs.ShowMessage(Msg, FTitle, TMsgDlgType.mtError);
    end;
     {$ELSE}
     TThread.Synchronize(nil,
      procedure
      begin
        NV.VCL.Dialogs.ShowMessage(Msg, FTitle, TMsgDlgType.mtError);
      end);
     {$ENDIF}
end;

procedure TNvApplication.ShowMessage(aMsg: string);
begin
  NV.VCL.Dialogs.ShowMessage(aMsg);
end;

procedure TNvApplication.Terminate;
begin
  if CallTerminateProcs then
    PostQuitMessage(0);
end;

function TNvApplication.UrlBase: string;
begin
  Result:= Screen.FUrlBase;
end;

procedure TNvApplication.WndProc(var Message: TMessage);
var
  I                   : Integer;
  LMessage            : TMessage;
  SaveFocus, TopWindow: HWnd;
  ActivateMsg         : TWMActivateApp;
  EnableMsg           : TWMEnable;
  EndSessionMsg       : TWMEndSession;
{$IF DEFINED(CLR)}
  KeyMsg    : TWMKey;
  SettingMsg: TWMSettingChange;
{$ENDIF}
  procedure Default;
  begin
    if not(csDestroying in ComponentState) and (Handle > 0) then
      with Message do
        Result := DefWindowProc(Handle, Msg, WParam, LParam);
  end;

  procedure DrawAppIcon;
  var
    DC: HDC;
    PS: TPaintStruct;
  begin
    with Message do
      begin
        DC := BeginPaint(Handle, PS);
        // DrawIcon(DC, 0, 0, GetIconHandle);
        EndPaint(Handle, PS);
      end;
  end;

begin
  try
    Message.Result := 0;
    // for I := 0 to FWindowHooks.Count - 1 do
    // if TWindowHook(FWindowHooks[I]{$IFNDEF CLR}^{$ENDIF})(Message) then Exit;
    // CheckIniChange(Message);
    with Message do
      case Msg of
        WM_SCR_DATA:
          begin
            if FDesignInstance and (Screen <> nil) then
              Screen.MsgDataReceived(Message);
          end;
        WM_SYSCOMMAND:
          // case WParam and $FFF0 of
          // SC_MINIMIZE: Minimize;
          // SC_RESTORE: Restore;
          // else
            Default;
        // end;
        // WM_SIZE:
        // if WParam = SIZE_MINIMIZED then
        // FAppIconic := True;
        WM_CLOSE: FTerminate := True;
        WM_PAINT:
          if IsIconic(Handle) then
            DrawAppIcon
          else
            Default;
        WM_ERASEBKGND:
          begin
            Message.Msg := WM_ICONERASEBKGND;
            Default;
          end;
        // WM_QUERYDRAGICON: Result := GetIconHandle;
        WM_SETFOCUS:
          begin
            PostMessage(Handle, CM_ENTER, 0, 0);
            Default;
          end;
        WM_ACTIVATEAPP:
          begin
            Default;
            // ActivateMsg := TWMActivateApp(Message);
            // FActive := ActivateMsg.Active;
            // if FActive then
            // begin
            // if FMainFormOnTaskBar and FAppIconic then
            // FAppIconic := False;
            // RestoreTopMosts;
            // PostMessage(Handle, CM_ACTIVATE, 0, 0);
            // end
            // else
            // begin
            // NormalizeTopMosts;
            // PostMessage(Handle, CM_DEACTIVATE, 0, 0);
            // end;
          end;
        WM_ENABLE:
          begin
            // EnableMsg := TWMEnable(Message);
            // if EnableMsg.Enabled then
            // begin
            // if not DisablingWindows then
            // begin
            // RestoreTopMosts;
            // if FWindowList <> nil then
            // begin
            // EnableTaskWindows(FWindowList);
            // FWindowList := nil;
            // end;
            // end;
            // Default;
            // end else
            // begin
            // Default;
            // if (FWindowList = nil) and not DisablingWindows then
            // FWindowList := DisableTaskWindows(Handle);
            // NormalizeAllTopMosts;
            // end;
          end;
        WM_CTLCOLORMSGBOX .. WM_CTLCOLORSTATIC:
            Result := SendMessage(LParam, CN_BASE + Msg, WParam, LParam);
        WM_ENDSESSION:
          begin
            EndSessionMsg        := TWMEndSession(Message);
            EndSessionMsg.Result := 0;

            // // when the main form is minimized, the forms of the application
            // // do not receive this message because TApplication is the first
            // // to receive it and in turn it calls Halt()
            // if Assigned(MainForm) and (MainForm.WindowState = wsMinimized) then
            // NotifyForms(WM_ENDSESSION, WParam, LParam);

            if EndSessionMsg.EndSession then
              begin
                Application.Terminate;
                Halt;
              end;
          end;
        WM_QUERYENDSESSION: Message.Result := 1;
        // CM_ACTIONEXECUTE, CM_ACTIONUPDATE:
        // Message.Result := Ord(DispatchAction(Message.Msg, TBasicAction(Message.LParam)));
        CM_APPKEYDOWN:
          begin
            // if IsShortCut(TWMKey(Message)) then Result := 1;
          end;
        CM_APPSYSCOMMAND:
          // if MainForm <> nil then
          // with MainForm do
          // if (Handle <> 0) and IsWindowEnabled(Handle) and
          // IsWindowVisible(Handle) then
          // begin
          // FocusMessages := False;
          // SaveFocus := GetFocus;
          // Winapi.Windows.SetFocus(Handle);
          // Perform(WM_SYSCOMMAND, WParam, LParam);
          // Winapi.Windows.SetFocus(SaveFocus);
          // FocusMessages := True;
          // Result := 1;
          // end;
            ;
        CM_ACTIVATE:
          begin
            // if Assigned(FOnActivate) then
            // FOnActivate(Self);
            // if Assigned(Application.MainForm) and Application.MainFormOnTaskBar and
            // not IsWindowEnabled(Application.MainForm.Handle) and
            // (FLastActivePopup <> MainForm.Handle) then
            // Winapi.Windows.SetFocus(FLastActivePopup);
            // FLastActivePopup := 0;
          end;
        CM_DEACTIVATE:
          begin
            // FLastActivePopup := GetLastActivePopup(Handle);
            // if Assigned(FOnDeactivate) then FOnDeactivate(Self);
          end;
        CM_ENTER:
          if not IsIconic(Handle) and (GetFocus = Handle) then
            begin
              // TopWindow := FindTopMostWindow(0);
              // if TopWindow <> 0 then Winapi.Windows.SetFocus(TopWindow);
            end;
        WM_HELP,
        // CM_INVOKEHELP: InvokeHelp(WParam, LParam);
        // CM_WINDOWHOOK:
        // if wParam = 0 then
        // HookMainWindow(TWindowHook(Pointer(LParam)^)) else
        // UnhookMainWindow(TWindowHook(Pointer(LParam)^));
        // CM_DIALOGHANDLE:
        // if wParam = 1 then
        // Result := FDialogHandle
        // else
        // FDialogHandle := lParam;
        WM_SETTINGCHANGE:
          begin
            {$IFNDEF FPC}
            Mouse.SettingChanged(WParam);
            {$ENDIF}
            // SettingChange(TWMSettingChange(Message));

            Default;
          end;
        // WM_FONTCHANGE:
        // begin
        // Screen.ResetFonts;
        // Default;
        // end;
        // WM_THEMECHANGED:
        // begin
        // StyleServices.ApplyThemeChange;
        // LMessage.Msg := CM_STYLECHANGED;
        // LMessage.WParam := 0;
        // LMessage.LParam := 0;
        // LMessage.Result := 0;
        // for I := 0 to Screen.FormCount - 1 do
        // Screen.Forms[I].Broadcast(LMessage);
        // end;
        CM_INPUTLANGCHANGE:
          begin
            // NotifyForms(CM_INPUTLANGCHANGE, WParam, LParam);
          end;

        WM_NULL: CheckSynchronize;

        // WM_WTSSESSION_CHANGE:
        // Screen.GetMonitors;
      else Default;
      end;
  except
    HandleException(Self);
  end;
end;

{ TNvScreen }

function TNvScreen.Active: Boolean;
begin
  Result := FActive;
end;

procedure TNvScreen.AddDataModule(DataModule: TDataModule);
begin
  FDataModules.Add(DataModule);
end;

procedure TNvScreen.AddForm(AForm: TNVForm);
begin
  FForms.Add(AForm);
end;

procedure TNvScreen.AddFrame(AFrame: TNVBaseFrame);
begin
  FFrames.Add(AFrame);
end;

function TNvScreen.Ajax: TNvAjax;
begin
  Result := FPage.Ajax;
end;

procedure TNvScreen.Close;
begin
  FActive := False;
  Ajax.Json.Clear;
end;

constructor TNvScreen.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF FPC}System.{$ENDIF}Classes.AddDataModule    := AddDataModule;
  {$IFNDEF FPC}System.{$ENDIF}Classes.RemoveDataModule := RemoveDataModule;
  FForms                          := TList.Create;
  FDataModules                    := TList.Create;
  FFrames                         := TList.Create;
  FPage                           := TNvPage.Create(Self);
  FDataReceived                   := TDictionary<Integer, string>.Create;
end;

function TNvScreen.DataModuleCount: Integer;
begin
  Result := FDataModules.Count;
end;

function TNvScreen.DataModules(Index: Integer): TDataModule;
begin
  Result := TDataModule(FDataModules[Index]);
end;

procedure TNvScreen.DataReceived(const Data:PChar);
begin
  if Data = 'close' then
    PostMessage(Application.Handle, WM_CLOSE, 0, 0)
  else if Data = 'Active' then
    begin
      FActive := True;
      Ajax.Invalidate;
    end
  else // Save Data to read on message TNvScreen.MsgDataReceived
    begin
      inc(FDataId);
      FDataReceived.Add(FDataId, {$IFDEF FPC} PChar {$ELSE} PChar {$ENDIF}(Data));

      PostMessage(Application.Handle, WM_SCR_DATA, FDataId, 0);
    end;
end;

destructor TNvScreen.Destroy;
begin
  FDataReceived.Free;
  FForms.Free;
  FDataModules.Free;
  FFrames.Free;
  {$IFNDEF FPC}System.{$ENDIF}Classes.AddDataModule    := nil;
  {$IFNDEF FPC}System.{$ENDIF}Classes.RemoveDataModule := nil;
  if Screen = Self then
    Screen := nil;
  inherited;
end;

function TNvScreen.FormCount: Integer;
begin
  Result := FForms.Count;
end;

function TNvScreen.Forms(Index: Integer): TNVForm;
begin
  Result := TNVForm(FForms[Index]);
end;

function TNvScreen.FrameCount: Integer;
begin
  Result := FFrames.Count;
end;

function TNvScreen.Frames(Index: Integer): TNVBaseFrame;
begin
  Result := TNVBaseFrame(FFrames[Index]);
end;

procedure TNvScreen.MsgDataReceived(var Msg: TMessage);
var
  { TODO -oDelcio -cScreenMessage : Ver se é possivel remover este record e processar direto o TMessage }
  _Msg: TWMScreenData;
  J   : TJsonObject;
begin
  if FDataReceived.TryGetValue(Integer(Msg.WParam), _Msg.Data) then
    FDataReceived.Remove(Integer(Msg.WParam));

  _Msg.Msg := Msg.Msg;

  J := TJsonObject.Create;
  try
    if not _Msg.Data.IsEmpty then
      J.FromJSON(_Msg.Data);

    FPage.ProcessRequest(J);
  finally
    J.Free;
  end;

  Msg.Result := 1;
end;

procedure TNvScreen.RemoveDataModule(DataModule: TDataModule);
begin
  FDataModules.Remove(DataModule);
end;

procedure TNvScreen.RemoveForm(AForm: TNVForm);
begin
  FForms.Remove(AForm);
end;

procedure TNvScreen.RemoveFrame(AFrame: TNVBaseFrame);
begin
  FFrames.Remove(AFrame);
end;

{ TNVForm }

procedure TNVForm.CMVisibleChanged(var Message: TMessage);
begin
  if (Message.WParam = ord(True)) and (Parent = nil) and Assigned(Screen) and not InModal then
    Parent := Screen.FPage;
  inherited;

end;

constructor TNVForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  Visible := False;

  if (ClassType <> TNVForm) and not(csDesignInstance in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TNVForm) then
        raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
    end;
end;

function TNVForm.ShowModal: Integer;
begin
  Result := inherited;
end;

class function TNVForm.SupportImage: Boolean;
begin
  Result := True;
end;

// procedure TNVForm.VisibleChanging;
// begin
// inherited;
//
// end;

function FindGlobalComponent(const Name: string): TComponent;
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    begin
      Result := Screen.Forms(I);
      if not(csInline in Result.ComponentState) and (CompareText(Name, Result.Name) = 0) then
        Exit;
    end;
  for I := 0 to Screen.DataModuleCount - 1 do
    begin
      Result := Screen.DataModules(I);
      if CompareText(Name, Result.Name) = 0 then
        Exit;
    end;
  for I := 0 to Screen.FrameCount - 1 do
    begin
      Result := Screen.Frames(I);
      if CompareText(Name, Result.Name) = 0 then
        Exit;
    end;
  Result := nil;
end;

initialization

// InitProcs;
// RM_TaskBarCreated := RegisterWindowMessage('TaskbarCreated');
// RM_TaskBarButtonCreated := RegisterWindowMessage('TaskbarButtonCreated');
{$IFNDEF FPC}System.{$ENDIF}Classes.RegisterFindGlobalComponentProc(FindGlobalComponent);
// IdleTimerHandle := 0;

finalization

// if Application <> nil then
// DoneApplication;
// if HintDoneEvent <> 0 then
// CloseHandle(HintDoneEvent);
{$IFNDEF FPC}System.{$ENDIF}Classes.UnregisterFindGlobalComponentProc(FindGlobalComponent);

end.
