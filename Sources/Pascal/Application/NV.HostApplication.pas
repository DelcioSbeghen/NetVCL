unit NV.HostApplication;

interface

uses
  Classes, SysUtils, RTLConsts, NV.VCL.Page, NV.Interfaces, IdCustomHTTPServer,
  NV.Router, NV.Request, NV.Common.HostAppInterface;

type
  TGetDesignExename = function: string;

  TNVHostAppClass = class of TNVHostApp;

  TNVHostApp = class;

  TNVCreateSessionEvent = procedure(aApp: TNVHostApp; aRequest: TNVRequestTask; aSession: TComponent) of object;

  // list of NVSession actives
  TNVSessionList = class(TThreadList)
  public
    procedure RemoveAged;
    procedure TerminateAll;
    function Count: Integer;
  end;

  TNVHostApp = class(TDataModule, INVHostApp)
  private
    FRefreshCacheParam: string;
    FMainPage: TNVPageClass;
    FOnAppTerminate: TNotifyEvent;
    FAllowedPaths: TStrings;
    FCookieParam: string;
    FOnNewSession: TNVCreateSessionEvent;
    FLibDir: string;
    FDocDir:string;
    FTemplateCss: string;
    FOnSessionClose: TNotifyEvent;
    FComInitilization: Boolean;
    FRouter: TNVRouter;
    FDomain: string;
    FURLBase: string;
    FTerminatedUrl: string;
    function GetDocDir: string;
    function GetTemplateDir: string;
    function GetUrlBase: string;
    procedure SetAllowedPaths(const Value: TStrings);
    procedure SetComInitilization(const Value: Boolean);
    procedure SetCookieParam(const Value: string);
    procedure SetDocDir(const Value: string);
    procedure SetLibDir(const Value: string);
    procedure SetMainPage(const Value: TNVPageClass);
    procedure SetOnAppTerminate(const Value: TNotifyEvent);
    procedure SetOnNewSession(const Value: TNVCreateSessionEvent);
    procedure SetOnSessionClose(const Value: TNotifyEvent);
    procedure SetTemplateCss(const Value: string);
    procedure SetTemplateDir(const Value: string);
    procedure SetUrlBase(const Value: string);
    procedure CreateSession(aRequest: TNVRequestTask);
    procedure SetDomain(const Value: string);
    procedure SetTerminatedUrl(const Value: string);
    function GetDomain: string;
  protected
    FSessionList: TNVSessionList;
    FServer: INVServer;
    procedure ProcessGet(aRequest: PNVRequestTask);
    procedure DoNewSession(aRequest: TNVRequestTask; aSession: TComponent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function AppPath: string;
    function GetServer: INVServer;
    property Router: TNVRouter read FRouter;
  published
    property Domain: string read GetDomain write SetDomain;
    property DocDir: string read GetDocDir write SetDocDir;
    property TemplateDir: string read GetTemplateDir write SetTemplateDir;
    property LibDir: string read FLibDir write SetLibDir;
    property MainPage: TNVPageClass read FMainPage write SetMainPage;
    property UrlBase: string read GetUrlBase write SetUrlBase;
    property RefreshCacheParam: string read FRefreshCacheParam;
    property TemplateCss: string read FTemplateCss write SetTemplateCss;
    // class used to handle data of session need to be an TDWSession descendant
    //property HttpServer: TDWHttpServer read FHttpSrv; <-- AVOID EXPOSE THIS DIRECTLY
    property CookieParam: string read FCookieParam write SetCookieParam;
    property ComInitilization: Boolean read FComInitilization write SetComInitilization default False;
    //property LogMemo: TMemo read FLogMemo write SetLogMemo;
    property AllowedPaths: TStrings read FAllowedPaths write SetAllowedPaths;
    property TerminatedUrl: string read FTerminatedUrl write SetTerminatedUrl;
    property OnNewSession: TNVCreateSessionEvent read FOnNewSession write SetOnNewSession;
    property OnSessionClose: TNotifyEvent read FOnSessionClose write SetOnSessionClose;
    property OnAppTerminate: TNotifyEvent read FOnAppTerminate write SetOnAppTerminate;
  end;

var
  GetDesignExename: TGetDesignExename;

implementation

uses
  Forms, IdCookie, NV.Session, IdThread, IdSchedulerOfThread, NV.Dispatcher, NV.Utils;

type
  //hack to acess protected member
  IdReqHelper = class helper for TIdHTTPRequestInfo
    procedure SetSession(aValue: TIdHTTPSession);
  end;

  //hack to acess protected member
  IdRespHelper = class helper for TIdHTTPResponseInfo
    procedure SetSession(aValue: TIdHTTPSession);
  end;

  //hack to acess protected member
  THackIdThread = class(TIdThread);

  THackSessionTh = class(TNVSessionThread);

{ TNVHostApp }

function TNVHostApp.AppPath: string;
begin
  if csDesigning in componentState then
  begin
    if Assigned(GetDesignExename) then
      Result := extractFilePath(GetDesignExename) //extractFilePath(getActiveProject.ProjectOptions.TargetName)
    else
      Result := '';
  end
  else
    Result := extractFilePath(Application.exeName);
end;

constructor TNVHostApp.Create(AOwner: TComponent);
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
    FSessionList := TNVSessionList.Create;
    FLibDir := AppPath + 'www\netvcl\';
    FDocDir := AppPath + 'www\';
    FUrlBase := '';
    FComInitilization := False;
    FTerminatedUrl := '';
    FAllowedPaths := TStringList.Create;
    if (ClassType <> TNVHostApp) and not (csDesigning in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TNVHostApp) then
        raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
      if OldCreateOrder then
        DoCreate;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

procedure TNVHostApp.CreateSession(aRequest: TNVRequestTask);
var
  LCookie: TIdCookie;
  // under ARC, convert a weak reference to a strong reference before working with it
  LSessionList: TIdHTTPCustomSessionList;
  _Session: TIdHTTPSession;
  _SessionThread: TNVSessionThread;
  _SessionApp: TNVSessionApp;
  _Aux: string;
  _Dispatch: TDispatchDirFiles;
  I: Integer;
begin
  LSessionList := aRequest.Resp.HTTPServer.SessionList;
  if Assigned(LSessionList) then
  begin
     //create Indy Session
    _Session := LSessionList.CreateUniqueSession(aRequest.Req.RemoteIP);

    //Create Session Thread
    _SessionThread := TNVSessionThread.Create(_Session.SessionID, Self, FMainPage, aRequest);
    _SessionThread.Start;
    _SessionApp := _SessionThread.SessionApp; //dwapp

    //add lib patch dir to route
    _Aux := MakeValidFileUrl(UrlBase, LibDir, Self);
    _Dispatch := TDispatchDirFiles.Create;
    _Dispatch.AllowedFlag := afBeginBy;
    _SessionApp.Router.AddRoute(_Aux, _Dispatch);

    //add template Css to route
    if FTemplateCss <> '' then
    begin
      _Aux := MakeValidFileUrl(UrlBase, FTemplateCss, Self);
      _Dispatch := TDispatchDirFiles.Create;
      _Dispatch.AllowedFlag := afExactMatch;
      _SessionApp.Router.AddRoute(_Aux, _Dispatch);
      //LDWApplication.AddGetAlowedPath(Aux, afExactMatch);
    end;
  //add User defined Allowed paths to Route
  { TODO 1 -oDELCIO -cIMPLEMENT : !!!!!!!!!! Syncronize this or ERRROR }
    for I := 0 to FAllowedPaths.Count - 1 do
    begin
      _Aux := MakeValidFileUrl(UrlBase, FAllowedPaths[I], Self);
      _Dispatch := TDispatchDirFiles.Create;
      _Dispatch.AllowedFlag := afBeginBy;
      _SessionApp.Router.AddRoute(_Aux, _Dispatch);
      //LDWApplication.AddGetAlowedPath(DWServer.AllowedPaths[I], afBeginBy);
    end;

    _Session.Content.AddObject('NVSessionApp', _SessionApp){object[0]};
    _Session.Content.AddObject('NVSessionThread', _SessionThread){object[1]};

    LCookie := aRequest.Resp.Cookies.Add;
    LCookie.CookieName := aRequest.Resp.HTTPServer.SessionIDCookieName;
    LCookie.Value := _Session.SessionID;
    LCookie.Path := '/';    {Do not Localize}

      // By default the cookie will be valid until the user has closed his browser window.
      // MaxAge := SessionTimeOut div 1000;
    aRequest.Resp.SetSession(_Session);
    aRequest.Req.SetSession(_Session);
  end;
end;

destructor TNVHostApp.Destroy;
begin
  FSessionList.TerminateAll;
  FAllowedPaths.Free;
  FSessionList.Free;
  inherited;
end;

procedure TNVHostApp.DoNewSession(aRequest: TNVRequestTask; aSession: TComponent);
begin
  if Assigned(FOnNewSession) then
    FOnNewSession(Self, aRequest, aSession);
end;

function TNVHostApp.GetDocDir: string;
begin
  Result:= FDocDir;
end;

function TNVHostApp.GetDomain: string;
begin
  Result := FDomain;
end;

function TNVHostApp.GetServer: INVServer;
begin
  Result:= FServer;
end;

function TNVHostApp.GetTemplateDir: string;
begin

end;

function TNVHostApp.GetUrlBase: string;
begin

end;

procedure TNVHostApp.ProcessGet(aRequest: PNVRequestTask);
var
//  _SessionApp: TNVSessionApp;
  _SessionThread: TNVSessionThread;
  _IdThread: TIdThread;
begin
  // Create session if not already created
  if TNVRequestTask(aRequest).Req.Session = nil then
    CreateSession(TNVRequestTask(aRequest));

  //Get NVSession App and NVSession Thread from Indy Session
 // _SessionApp := TNVSessionApp(aRequest.Req.Session.Content.Objects[0]);
  _SessionThread := TNVSessionThread(TNVRequestTask(aRequest).Req.Session.Content.Objects[1]);

  //Attach NVSession Thread to Current Indy Thread for further use
  _IdThread := TIdThread(TIdYarnOfThread(TNVRequestTask(aRequest).Context.Yarn).Thread);
  _IdThread.Data := _SessionThread;

  //to not Free NVSession Thread on Cleanup Indy Thread
  Exclude(THackIdThread(_IdThread).FOptions, itoDataOwner);

  //Put Request in NVSession Thread queue
  THackSessionTh(_SessionThread).ProcessRequest(TNVRequestTask(aRequest));
end;

procedure TNVHostApp.SetAllowedPaths(const Value: TStrings);
begin
  FAllowedPaths := Value;
end;

procedure TNVHostApp.SetComInitilization(const Value: Boolean);
begin
  FComInitilization := Value;
end;

procedure TNVHostApp.SetCookieParam(const Value: string);
begin
  FCookieParam := Value;
end;

procedure TNVHostApp.SetDocDir(const Value: string);
begin
  FDocDir:= Value;
end;

procedure TNVHostApp.SetDomain(const Value: string);
begin
  FDomain := Value;
end;

procedure TNVHostApp.SetLibDir(const Value: string);
begin
  FLibDir := Value;
end;

procedure TNVHostApp.SetMainPage(const Value: TNVPageClass);
begin
  FMainPage := Value;
end;

procedure TNVHostApp.SetOnAppTerminate(const Value: TNotifyEvent);
begin
  FOnAppTerminate := Value;
end;

procedure TNVHostApp.SetOnNewSession(const Value: TNVCreateSessionEvent);
begin
  FOnNewSession := Value;
end;

procedure TNVHostApp.SetOnSessionClose(const Value: TNotifyEvent);
begin
  FOnSessionClose := Value;
end;

procedure TNVHostApp.SetTemplateCss(const Value: string);
begin
  FTemplateCss := Value;
end;

procedure TNVHostApp.SetTemplateDir(const Value: string);
begin

end;

procedure TNVHostApp.SetTerminatedUrl(const Value: string);
begin
  FTerminatedUrl := Value;
end;

procedure TNVHostApp.SetUrlBase(const Value: string);
begin

end;

procedure TNVHostApp.Start;
begin
  FRefreshCacheParam := FormatDateTime('yyyymmddhhnnsszzz', now);
end;

procedure TNVHostApp.Stop;
begin

end;

{ IdRespHelper }

procedure IdRespHelper.SetSession(aValue: TIdHTTPSession);
begin
  with Self do
    FSession := aValue;
end;

{ IdReqHelper }

procedure IdReqHelper.SetSession(aValue: TIdHTTPSession);
begin
  with Self do
    FSession := aValue;
end;

{ TNVSessionList }

function TNVSessionList.Count: Integer;
begin
  Result := LockList.Count;
  UnlockList;
end;

procedure TNVSessionList.RemoveAged;
var
  I: Integer;
  lList: TList;
begin
  lList := LockList;
  try
    for I := 0 to lList.Count - 1 do
    begin
      try
        if TNVSessionThread(lList.Items[I]).SessionApp.IsTimedOut then
          TNVSessionThread(lList.Items[I]).SessionApp.Terminate;
      except
          { TODO 1 -oDELCIO -cIMPLEMENT : log this !!!!!}
      end;
    end;
  finally
    UnlockList;
  end;
end;

procedure TNVSessionList.TerminateAll;
var
  I: Integer;
  lList: TList;
begin
  lList := LockList;
  try
    for I := 0 to lList.Count - 1 do
    begin
      try
        TNVSessionThread(lList.Items[I]).SessionApp.Terminate;
      except
          { TODO 1 -oDELCIO -cIMPLEMENT : log this !!!!!}
      end;
    end;
  finally
    UnlockList;
  end;

end;

end.

