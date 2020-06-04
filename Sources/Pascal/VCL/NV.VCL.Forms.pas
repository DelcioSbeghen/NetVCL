unit NV.VCL.Forms;

interface

uses
  Classes, Windows, Messages, NV.VCL.Frame, NV.Browser, System.Generics.Collections;

const
  WM_NV = WM_APP + 10000;

  WM_SCR_DATA = WM_NV + 1;

type

  TWMScreenData = record
    Msg: Cardinal;
    WParam: WParam;
    LParam: LParam;
    Result: LRESULT;
    Data: string;
  end;

  TNvScreen = class(TNVBrowser)
  private
    FDataId      : Integer;
    FDataReceived: TDictionary<Integer, String>;
  protected
    procedure DataReceived(const Data: string);
    procedure MsgDataReceived(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show;
    procedure ShowDesign(aParent: TObject);
    procedure UpdateScreen(Updates: string);
  end;

  TNVForm = class(TNvFrame)
  public
    procedure Show; override;
  published
    property ClientWidth;
    property ClientHeight;
  end;

  TNvApplication = class(TComponent)
  private
    FTerminate     : Boolean;
    FRootPath      : string;
    FUrlBase       : string;
    FHandleCreated : Boolean;
    FObjectInstance: Pointer;
    FTitle         : string;
    FHandle        : HWnd;
    FCssFile       : string;
    procedure WndProc(var Message: TMessage);
    function GetRootPath: string;
    procedure SetCssFile(const Value: string);
  protected
    FMainForm      : TNVForm;
    FRunning       : Boolean;
    FDesignInstance: Boolean;
    procedure HandleMessage;
    procedure HandleException(Sender: TObject);
    function ProcessMessage(var Msg: TMsg): Boolean;
    procedure Idle(const Msg: TMsg);
    function IsPreProcessMessage(var Msg: TMsg): Boolean;
    procedure CreateHandle;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure ShowMessage(aMsg: string);
    procedure Initialize;
    // For design only ???
    function Handle: THandle;
    procedure Run; virtual;
    procedure Terminate;
    property CssFile: string read FCssFile write SetCssFile;
    property Terminated: Boolean read FTerminate;
    property UrlBase: string read FUrlBase;
    property RootPath: string read GetRootPath write FRootPath;
    property Running: Boolean read FRunning;
  end;

var
  Application: TNvApplication = nil;
  Screen     : TNvScreen      = nil;

implementation

uses SysUtils, RTLConsts, Consts, NV.JSON, VCL.Controls;

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
  FUrlBase := 'local://screen/';
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
        if TOSVersion.Check(5, 1) then
          WTSRegisterSessionNotification(LHandle, NOTIFY_FOR_THIS_SESSION);
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

function TNvApplication.GetRootPath: string;
begin
  { TODO -oDelcio -cApplication : GetRootPath in design and runtime mode }
  Result := 'D:\Delcio\Projetos\NetVCL\Demos\Dist\www\';
  // Result := FRootPath;
end;

function TNvApplication.Handle: THandle;
begin
  Result := FHandle;
end;

procedure TNvApplication.HandleException(Sender: TObject);
begin

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
              if not IsPreProcessMessage(Msg) and { not IsHintMsg(Msg) and } not Handled { and
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

procedure TNvApplication.Run;
begin
  if FRootPath.IsEmpty then
    FRootPath := ExtractFilePath(ParamStr(0)) + 'www/';

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

procedure TNvApplication.ShowMessage(aMsg: string);
begin
  //
end;

procedure TNvApplication.Terminate;
begin
  if CallTerminateProcs then
    PostQuitMessage(0);
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
            Mouse.SettingChanged(WParam);
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

constructor TNvScreen.Create(AOwner: TComponent);
begin
  inherited;
  FDataReceived := TDictionary<Integer, string>.Create;
end;

procedure TNvScreen.DataReceived(const Data: string);
begin
  // Save Data to read on message TNvScreen.MsgDataReceived
  inc(FDataId);
  FDataReceived.Add(FDataId, PChar(Data));

  PostMessage(Application.Handle, WM_SCR_DATA, FDataId, 0);
end;

destructor TNvScreen.Destroy;
begin
  if Screen = Self then
    Screen := nil;
  FDataReceived.Free;
  inherited;
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
    J.FromJSON(_Msg.Data);

    FPage.ProcessRequest(J);
  finally
    J.Free;
  end;

  Msg.Result := 1;
end;

procedure TNvScreen.Show;
begin
  CreateScreenBrowser(DataReceived);
  LoadUrl(Application.UrlBase);
end;

procedure TNvScreen.ShowDesign(aParent: TObject);
begin
  ShowDesignBrowser(aParent as TWinControl, DataReceived);
  LoadUrl(Application.UrlBase);
end;

procedure TNvScreen.UpdateScreen(Updates: string);
begin
  ExecuteJs(           //
    'App.ParseJson(' + //
    Updates            //
    + ')'              //
    );
end;

{ TNVForm }

procedure TNVForm.Show;
begin
  if Assigned(Screen) then
    Parent := Screen.FPage;
  inherited;

end;

end.
