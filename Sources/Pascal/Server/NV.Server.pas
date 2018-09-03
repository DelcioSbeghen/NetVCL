unit NV.Server;

interface

uses
  Classes, windows, Forms, SysUtils, StrUtils, NVJCLDebug, NV.Server.HttpServer,
  SyncObjs, StdCtrls, IdContext, IdCustomHTTPServer, IdThread,
  IdSchedulerOfThread, NV.Common.Classes;

type
  TDatamoduleClass = class of TDataModule;

  TNVServer = class(TComponent)
  private
    FHttpSrv: TNVHttpServer;
    {TODO -oDelcio -cImprove : Implement fast Threadsafe list with TMultipleReadExclusiveWriter and avoid Lock in subsequent calls }
    FHostedApps: TThreadStringList;
    FCriticalLog: TCriticalSection;
    FLogMemo: TMemo;
    FOnSessionAppTerminate: TNotifyEvent;
    FOnHostAppStop: TNotifyEvent;
    FOnHostAppStart: TNotifyEvent;
    FOnSessionAppStart: TNotifyEvent;
    procedure SetPort(const Value: string);
    function GetPort: string;
    procedure SetLogMemo(const Value: TMemo);
    procedure CreateLogfile;
    // Getting the current filename for the logfile
    function GetCurrentLogName: string;
    procedure WriteToLogFile(aLogMessage: string);
    function PrepareLogText(aMsg: string): string;
    procedure DoThreadSyncException(Thread: TJclDebugThread);
    procedure DoThreadRegistered(ThreadID: DWORD);
    procedure DoThreadUnregistered(ThreadID: DWORD);
    procedure HttpServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HttpServerCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    //procedure HttpServerSessionStart(Sender: TIdHTTPSession);
    procedure SetOnHostAppStart(const Value: TNotifyEvent);
    procedure SetOnHostAppStop(const Value: TNotifyEvent);
    procedure SetOnSessionAppStart(const Value: TNotifyEvent);
    procedure SetOnSessionAppTerminate(const Value: TNotifyEvent);
  protected
    procedure ProcessException(e: Exception);
    procedure DoNewSession(Sender: TObject);
    procedure DoSessionClose(Sender: TObject);
    procedure DoDWAppTerminate(Sender: TObject);
    //wrapper to INVAppThread.AddDatamodule
    procedure AddDataModule(DataModule: TDataModule);
    //wrapper to INVAppThread.RemoveDatamodule
    procedure RemoveDataModule(DataModule: TDataModule);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function AppPath: string;
    procedure Log(aMsg: string);
    class function GetDWServer: TNVServer;
    procedure AddApp(aApp: TComponent);
    procedure AddPackage(aPatch: string);
  published
    property Port: string read GetPort write SetPort;
    property HttpServer: TNVHttpServer read FHttpSrv;
    property LogMemo: TMemo read FLogMemo write SetLogMemo;
    property OnHostAppStart: TNotifyEvent read FOnHostAppStart write SetOnHostAppStart;
    property OnHostAppStop: TNotifyEvent read FOnHostAppStop write SetOnHostAppStop;
    property OnSessionAppTerminate: TNotifyEvent read FOnSessionAppTerminate write SetOnSessionAppTerminate;
    property OnSessionAppStart: TNotifyEvent read FOnSessionAppStart write SetOnSessionAppStart;
  end;

var
  gNVServer: TNVServer;

implementation

uses
  IOUtils, NV.HostApplication, NV.Session, NV.Request;

const
  BreakingLine = '//----------------------------------------------------------------------------//';

type
  THackIdThread = class(TIdThread);

  THackSessionTh = class(TNVSessionThread);

  THackHostApp = class(TNVHostApp);


  { TNVServer }

procedure TNVServer.AddApp(aApp: TComponent);
begin
  if not (aApp is TNVHostApp) then
    raise Exception.Create('Only add TNVHostApp');
  FHostedApps.AddObject(TNVHostApp(aApp).Domain, aApp);
end;

procedure TNVServer.AddDataModule(DataModule: TDataModule);
var
  _SessionTh: THackSessionTh;
begin
  if THackSessionTh.GetCurrent(_SessionTh) then
    _SessionTh.AddDataModule(DataModule);
end;

procedure TNVServer.AddPackage(aPatch: string);
var
  _Pkg: HMODULE;
  _HostAppClass: TPersistentClass;
  _HostApp: TNVHostApp;
  _ProcLoadApp: function(aServer: TNVServer): TNVHostApp;
begin
  _HostApp:= nil;

  _Pkg := LoadPackage(aPatch);
  if _Pkg = 0 then
    raise Exception.Create('Error on load package');

  @_ProcLoadApp := GetProcAddress(_Pkg, 'LoadApp');
  if @_ProcLoadApp = nil then
    raise Exception.Create('Cannot Locate LoadApp Function in Package.');

  _HostApp := _ProcLoadApp(Self);

  if _HostApp = nil then
    raise Exception.Create('Cannot Create Hosted App.');

  if not (_HostApp is TNVHostApp) then
    raise Exception.Create('Only add TNVHostApp');
  FHostedApps.AddObject(TNVHostApp(_HostApp).Domain, _HostApp);
end;

function TNVServer.AppPath: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

constructor TNVServer.Create(AOwner: TComponent);
begin
  inherited;
  gNVServer := Self;
  FHostedApps := TThreadStringList.Create;
  FCriticalLog := TCriticalSection.Create;
  with JclDebugThreadList do
  begin
    OnSyncException := DoThreadSyncException;
    OnThreadRegistered := DoThreadRegistered;
    OnThreadUnregistered := DoThreadUnregistered;
  end;
  FHttpSrv := TNVHttpServer.Create(Self);
  with FHttpSrv do
  begin
    AutoStartSession := False;
    SessionState := True;
    SessionTimeOut := 1200000;
    Port := '80';
    OnCommandGet := HttpServerCommandGet;
    OnCommandOther := HttpServerCommandOther;
   // OnSessionStart := HttpServerSessionStart;
  end;
  gNVServer := Self;
end;

destructor TNVServer.Destroy;
begin
  FHttpSrv.Active := False;
  FHttpSrv.Free;
  FCriticalLog.Free;
  FHostedApps.Free;
  inherited;
end;

class function TNVServer.GetDWServer: TNVServer;
begin
  Result := gNVServer;
end;

function TNVServer.GetCurrentLogName: string;
begin
  Result := AppPath + TPath.GetFileNameWithoutExtension(Application.ExeName) + FormatDateTime('_yyyy_mm_dd', now) + '.log';
end;

function TNVServer.GetPort: string;
begin
  Result := IntToStr(FHttpSrv.DefaultPort);
end;

procedure TNVServer.SetLogMemo(const Value: TMemo);
begin
  if FLogMemo <> Value then
    FLogMemo := Value;
end;

procedure TNVServer.SetOnHostAppStart(const Value: TNotifyEvent);
begin
  FOnHostAppStart := Value;
end;

procedure TNVServer.SetOnHostAppStop(const Value: TNotifyEvent);
begin
  FOnHostAppStop := Value;
end;

procedure TNVServer.SetOnSessionAppStart(const Value: TNotifyEvent);
begin
  FOnSessionAppStart := Value;
end;

procedure TNVServer.SetOnSessionAppTerminate(const Value: TNotifyEvent);
begin
  FOnSessionAppTerminate := Value;
end;

procedure TNVServer.SetPort(const Value: string);
begin
  if IntToStr(FHttpSrv.DefaultPort) <> Value then
    FHttpSrv.DefaultPort := StrToIntDef(Value, 8880);
end;

procedure TNVServer.Start;
begin
  Classes.AddDataModule := Self.AddDataModule;
  Classes.RemoveDataModule := Self.RemoveDataModule;
  FHttpSrv.Active := True;
end;

procedure TNVServer.Stop;
begin
  Classes.AddDataModule := nil;
  Classes.RemoveDataModule := nil;
  FHttpSrv.Active := False;
end;

function TNVServer.PrepareLogText(aMsg: string): string;
begin
  Result := DateTimeToStr(now) + ': ' + aMsg + #13#10 + BreakingLine;
end;

procedure TNVServer.ProcessException(e: Exception);
begin
  FCriticalLog.Acquire;
  try
    WriteToLogFile(PrepareLogText(e.Message));
  finally
    FCriticalLog.Release;
  end;
end;

procedure TNVServer.RemoveDataModule(DataModule: TDataModule);
var
  _SessionTh: THackSessionTh;
begin
  if THackSessionTh.GetCurrent(_SessionTh) then
    _SessionTh.RemoveDataModule(DataModule);
end;

// ** This procedure just creates a new Logfile an appends when it was created **
// credits: http://delphi.cjcsoft.net//viewthread.php?tid=47526
procedure TNVServer.CreateLogfile;
var
  F: TextFile;
  FN: string;
begin
  FN := GetCurrentLogName;
  // Assigns Filename to variable F
  AssignFile(F, FN);
  // Rewrites the file F
  Rewrite(F);
  // Open file for appending
  Append(F);
  // Write text to Textfile F
  WriteLn(F, BreakingLine);
  WriteLn(F, 'This Logfile was created on ' + FormatDateTime('yyyy/mm/dd', now));
  WriteLn(F, BreakingLine);
  WriteLn(F, '');
  // finally close the file
  CloseFile(F);
end;

// Procedure for appending a Message to an existing logfile with current Date and Time **
// credits http://delphi.cjcsoft.net//viewthread.php?tid=47526
procedure TNVServer.WriteToLogFile(aLogMessage: string);
var
  F: TextFile;
  FN: string;
begin
  if FLogMemo <> nil then
  begin
    TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
        FLogMemo.Lines.Add(aLogMessage);
      end);
  end;

  // Getting the current filename for the logfile
  FN := GetCurrentLogName;

  // Checking for file
  if (not FileExists(FN)) then
  begin
      // if file is not available then create a new file
    CreateLogfile;
  end;

  // Assigns Filename to variable F
  AssignFile(F, FN);
  // start appending text
  Append(F);
  // Write a new line with current date and message to the file
  WriteLn(F, aLogMessage);
  // Close file
  CloseFile(F)
end;

procedure TNVServer.DoDWAppTerminate(Sender: TObject);
begin
  if Assigned(FOnSessionAppTerminate) then
    FOnSessionAppTerminate(Sender);
end;

procedure TNVServer.DoNewSession(Sender: TObject);
begin

end;

procedure TNVServer.DoSessionClose(Sender: TObject);
begin

end;

procedure TNVServer.DoThreadRegistered(ThreadID: DWORD);
begin
  //
end;

procedure TNVServer.DoThreadSyncException(Thread: TJclDebugThread);
var
  LMsg: TStrings;
begin
  // MessageRichEdit.Lines.Add(Format('Exception in thread: %s', [Thread.ThreadInfo]));
  // Note: JclLastExceptStackList always returns list for *current* thread ID. To simplify getting the
  // stack of thread where an exception occured JclLastExceptStackList returns stack of the thread instead
  // of current thread when called *within* the JclDebugThreadList.OnSyncException handler. This is the
  // *only* exception to the behavior of JclLastExceptStackList described above.
  LMsg := TStringList.Create;
  try
    if Thread.SyncException <> nil then
      LMsg.Add(Exception(Thread.SyncException).Message);

    JclLastExceptStackList.AddToStrings(LMsg, False, True, True);

    if Assigned(FLogMemo) then
      FLogMemo.Lines.Add(PrepareLogText(LMsg.Text))
    else
      WriteToLogFile(PrepareLogText(LMsg.Text));
  finally
    LMsg.Free;
  end;
end;

procedure TNVServer.DoThreadUnregistered(ThreadID: DWORD);
begin
  //
end;

procedure TNVServer.HttpServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  _AppIndex: Integer;
  _HostedApp: THackHostApp;
  _NVRequest: TNVRequestTask;
begin
  try
    if FHostedApps.Find(ARequestInfo.Host, _AppIndex) then
    begin
      _HostedApp := THackHostApp(FHostedApps.Objects[_AppIndex]);
      if Assigned(_HostedApp) then
      begin
        _NVRequest := TNVRequestTask.Create(AContext, ARequestInfo, AResponseInfo);
        try
          _HostedApp.ProcessGet(_NVRequest);
        finally
          {Modal start request task is released in  TNVAppThread.SendRequestTask}
          if not (_NVRequest.ModalStatus in [rmsModalStart, rmsModalStartSended]) then
            _NVRequest.Free;
        end;
      end;
    end
    else
      AResponseInfo.ResponseNo := 404;
  except
    on e: Exception do
    begin
      AResponseInfo.ResponseNo := 500;
      AResponseInfo.ResponseText := AResponseInfo.ResponseText + ' Details: ' + e.Message;
      Log('ERROR ON PROCESS GET: ' + ARequestInfo.Document + '. Error: ' + e.Message);
    end;
  end;
end;

procedure TNVServer.HttpServerCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  TIdThread(TIdYarnOfThread(AContext.Yarn).Thread).Data := ARequestInfo.Session.Content.Objects[0];
  //to not Free DWApplication on Cleanup Thread
  Exclude(THackIdThread(TThread.Current).FOptions, itoDataOwner);

  AResponseInfo.ResponseNo := 501;
  AResponseInfo.ResponseText := AResponseInfo.ResponseText + #13#10 + 'HttpServerCommandOther not implemeted yet in DWServer';
end;

procedure TNVServer.Log(aMsg: string);
begin
  FCriticalLog.Acquire;
  try
    WriteToLogFile(PrepareLogText(aMsg));
  finally
    FCriticalLog.Release;
  end;
end;

initialization
  Include(JclStackTrackingOptions, stRawMode);
  JclStartExceptionTracking;

end.

