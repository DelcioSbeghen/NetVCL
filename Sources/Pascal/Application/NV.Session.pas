unit NV.Session;

interface

uses
  Classes, SysUtils, NV.Classes, NV.Interfaces, NVJclDebug, NV.Router,
  NV.VCL.Page, NV.HostApplication, NV.Request, SyncObjs, NV.JSON, NV.WebSocket,
  NV.Cache, NV.Messages, NV.Languages, NV.Types;

type
  TNVLanguage = (nvlNone, nvlENUS, nvlPTBR);

  TNVSessionThread = class;

  TNVSessionApp = class(TComponent, INVSessionApp)
  private
    // For TimeOut Control
    FLastAccess: TDateTime;
    FRequestCount: Integer;
    FSessionTimeOut: Integer;
    // ID Of Application
    FAppID: string;
    // DWAppThread
    FAppTh: TNVSessionThread;
    // UserSession Data
    FUserSessionData: TObject;
    // response on callbacks
    FCallbackResp: TJsonObject;
    FMainpageClass: TNVPageClass;
    FMainPage: TNVBasePage;
    FPages: TList;
    // Internal redirect to an form, if is diff of nil,
    // the url is ignored and specified form is send
    FInternalRedirect: TNVBasePage;
    // the actual active page
    FActivePage: TNVBasePage;
    // Web Socket for Instant Updates
    FWebSocket: TNVWebSocket;
    FLanguage: TNVLanguage;
    FRouter: TNVRouter;
    procedure SetLanguage(const Value: TNVLanguage);
  protected
      // True if the current task is a callback
    FIsCallBack: Boolean;
    // List of files in the session cache
    FCacheList: TNVCacheList;
    // contais a list of Objects to be released(free on next thread loop)
    // this is needed for simulate the VCL Form.Release;
    FReleaseList: TList;
    function QueueMessage(aMsg: TNVMessage): Boolean;
    { Internal use, do not call directly.
     Adds a form to the list of instanced forms for this section. }
    procedure AddPage(aPage: TNVBasePage);
    { Internal use, do not call directly.
      Removes an form from the list of instanced forms for this section. }
    procedure RemovePage(aPage: TNVBasePage);
  public
  { Internal use, do not call directly. }
    constructor Create(aAppId: string; aMainPage: TNVPageClass; aAppTh: TNVSessionThread; aLanguage: TNVLanguage = nvlNone); overload;
    { @abstract(Internal use, do not call directly.) }
    destructor Destroy; override;
        // Show an Alert Message
    procedure ShowAlert(aMsg: string; aAlertStyle: TDWAlertStyle = bsasWarning; RawText: Boolean = False; aTimeOut: Integer = 0);
    // Return true if Session is Timed Out
    function IsTimedOut: Boolean;
    // Return True if is processing an Callback
    property IsCallback: Boolean read FIsCallBack;
    // Contains the Json Response for CallBacks
    function CallBackResp: TJsonObject;
    // terminate the DWpplication and session
    procedure Terminate;
    // Return if DwApplication is Terminating
    function Terminated: Boolean;
    { Returns the id of the current session. }
    function SessionID: string;
    property Router: TNVRouter read FRouter;
    property Language: TNVLanguage read FLanguage write SetLanguage;
  end;

  TNVSessionThread = class(TJclDebugThread{, INVSessionThread})
  strict private
    FWait: TEvent;
    // Queue of Messages to be processed
    FMsgQueue: TNVThreadList;
  private
    // Represents de Application Object for this thread
    FNVApp: TNVSessionApp;
    // To solve Stream Properties in  DWFindGlobalComponent
    FDatamodulesInstances: TList;
    // To solve Stream Properties in  DWFindGlobalComponent
    FPagesInstances: TList;
    // To solve Stream Properties in  DWFindGlobalComponent
    FFrameInstances: TList;
    FCurMSGIndex: Integer;
    // FRequestAfterModalEnd: Boolean;
    // FActiveTasks:TDWActiveTasks;
    FModalEndRequest: TNVRequestTask;
    FHostApp: TNVHostApp;
    procedure ProcessMessages(OnlyUpdates: Boolean = True; StartIndex: Integer = 0);
    procedure ExecuteRequestTask(aRequestTask: TNVRequestTask);
    procedure SendRequestTask(aRequestTask: TNVRequestTask);
    procedure ExecuteRequestMessage(LMsg: TNVMessage; var LRequest: TNVRequestTask);
    procedure ExecuteInvalidateMessage(LMsg: TNVMessage);
    procedure UpdateLang;
  protected
    procedure ProcessRequest(aRequest: TNVRequestTask);
    procedure AddDataModule(aDataModule: TDataModule);
    procedure RemoveDataModule(aDataModule: TDataModule);
    procedure Execute; override;
  public
    constructor Create(aAppId: string; aHostApp: TNVHostApp; aMainPage: TNVPageClass; aRequest: TNVRequestTask); reintroduce;
    destructor Destroy; override;
    class function GetCurrent(var ASessionTh): Boolean;
    function SessionApp: TNVSessionApp;
    function HostApp: TNVHostApp;
    procedure EnqueueMsg(aMsg: TNVMessage);
  end;

function NVPostMessage(aMsg: TNVMessage; aSession: TNVSessionApp): Boolean; overload;
{ Avoid using this because of overhead in the search for DWApplication,
  prefer first variation }

function NVPostMessage(aMsg: TNVMessage; aSessionID: string): Boolean; overload;

implementation

uses
  ActiveX, NV.Utils, Controls, NV.VCL.Controls, NV.VCL.Container,
  IdResourceStringsProtocols;

type
  THackHostApp = class(TNVHostApp);

  ThackControl = class(TNVControl);

  THackContainer = class(TNVContainer);

function NVPostMessage(aMsg: TNVMessage; aSession: TNVSessionApp): Boolean;
begin
  Result := False;
  if Assigned(aSession) then
    Result := aSession.QueueMessage(aMsg);
end;

function NVPostMessage(aMsg: TNVMessage; aSessionID: string): Boolean;
begin
  { TODO 1 -oDELCIO -cIMPLEMENT : DWPostMessage variation for String AppID }
  raise Exception.Create('NVPostMessage variation for string AppId not implemented yet!');
end;

{ TNVSessionThread }

procedure TNVSessionThread.AddDataModule(aDataModule: TDataModule);
begin

end;

constructor TNVSessionThread.Create(aAppId: string; aHostApp: TNVHostApp; aMainPage: TNVPageClass; aRequest: TNVRequestTask);
begin
  inherited Create(True, aAppId);
  FHostApp := aHostApp;
  FCurMSGIndex := 0;
  // FModalIndex              := 0;
  // FRequestAfterModalEnd := False;
  FreeOnTerminate := True;
  NameThreadForDebugging(aAppId, ThreadID);
  FWait := TEvent.Create(nil, True, False, '');
  FMsgQueue := TNVThreadList.Create;
  FNVApp := TNVSessionApp.Create(aAppId, aMainPage, Self);
  // FAppID          := aAppId;
  // FForms          := TList.Create;
  // FMainFormClass  := aMainForm;
  // FCallbacks      := TDWCallBacks.Create(Self);
  // FCallbackResp   := TDWXhrCallbackResp.Create(Self);
  // FGetHandler     := TDWHttpHandlerList.Create;
  // FGetAllowedPath := TDWHttpAllowedPath.Create;
  // FPostHandler    := TDWHttpHandlerList.Create;
  // FRequestCount   := 0;
  // FIsCallBack     := False;
  // Self.Start;
  FDatamodulesInstances := TList.Create;
  FPagesInstances := TList.Create;
  FFrameInstances := TList.Create;
  // FActiveTasks:= TDWActiveTasks.Create;
  THackHostApp(FHostApp).FSessionList.Add(Self);
  THackHostApp(FHostApp).DoNewSession(aRequest, FNVApp);
end;

destructor TNVSessionThread.Destroy;
begin

  inherited;
end;

procedure TNVSessionThread.EnqueueMsg(aMsg: TNVMessage);
begin
  FMsgQueue.Add(aMsg);
  FWait.SetEvent;
end;

procedure TNVSessionThread.Execute;
var
  Cominitiated: Boolean;
  R: Integer;
  ReleaseObj: TObject;
begin
  if FHostApp.ComInitilization then
  begin
    CoInitialize(nil);
    Cominitiated := True;
  end
  else
    Cominitiated := False;
  try
    repeat
      // process release list for free objects called Release
      for R := FNVApp.FReleaseList.Count - 1 downto 0 do
      begin
        try
          ReleaseObj := TObject(FNVApp.FReleaseList[R]);
          FNVApp.FReleaseList.Delete(R);

          if ReleaseObj.InheritsFrom(TNVBasePage) then
            FNVApp.RemovePage(TNVBasePage(ReleaseObj));

          ReleaseObj.Free;

        finally
          ReleaseObj := nil;
        end;
      end;
      ProcessMessages(False);
    until Terminated;
  finally
    if Terminated then
    begin
      if Assigned(FNVApp) then
      begin
        NVServer.HttpServer.EndSession(FNVApp.FAppID, '');
      end;
        // if Assigned(FDWApp) then
        // FDWApp.Free;
      if Cominitiated then
        CoUninitialize;
    end;
  end;
end;

procedure TNVSessionThread.ExecuteInvalidateMessage(LMsg: TNVMessage);
var
  LControl: TControl;
  LCmd: TJsonObject;
  LCmds: UTF8String;
  LResponse: TMemoryStream;
begin
  LControl := LMsg.Obj;
  if Assigned(LControl) then
  begin
    if (LControl is TNVControl) then
    begin
      LCmd := TJsonObject.Create;
      ThackControl(LControl).DrawWeb(LCmd);
      LCmds := LCmd.ToJson;
      LResponse := FNVApp.FWebSocket.BeginWrite;
      try
        LResponse.Write(LCmds[1], Length(LCmds));
      finally
        FNVApp.FWebSocket.EndWrite;
      end;
    end
    else if (LControl is TNVContainer) then
    begin
      LCmd := TJsonObject.Create;
      THackContainer(LControl).DrawWeb(LCmd);
      LCmds := LCmd.ToJson;
      LResponse := FNVApp.FWebSocket.BeginWrite;
      try
        LResponse.Write(LCmds[1], Length(LCmds));
      finally
        FNVApp.FWebSocket.EndWrite;
      end;
    end;
  end;
end;

procedure TNVSessionThread.ExecuteRequestMessage(LMsg: TNVMessage; var LRequest: TNVRequestTask);
begin
  LRequest := LMsg.Obj;
  if Assigned(LRequest) and (LRequest.Context <> nil) and (LRequest.Req <> nil) and (LRequest.Resp <> nil) then
  begin
    ExecuteRequestTask(LRequest);
      // nil request variable
    LRequest := nil;
  end
  else if Assigned(LRequest) then
    LRequest.Processed; // to avoid lock request
end;

procedure TNVSessionThread.ExecuteRequestTask(aRequestTask: TNVRequestTask);
var
  Found: Boolean;
  // LContext: TIdContext;
 // LRequest: TIdHTTPRequestInfo;
 // LResponse: TIdHTTPResponseInfo;
  RoutePath: TStringList;
begin
  try
    if FNVApp.FMainPage = nil then
      FNVApp.FMainPage := FNVApp.FMainPageClass.Create(FNVApp);


   // FActualRequest := aRequestTask;
    // if not FInModal then
    FNVApp.FIsCallBack := False;

    // LContext  := aRequestTask.Context;
   // LRequest  := aRequestTask.RequestInfo;
   // LResponse := aRequestTask.ResponseInfo;

    // Dispatch Request
    Found := False;
    RoutePath := TStringList.Create;
    RoutePath.Delimiter := '/';
    RoutePath.StrictDelimiter := True;
    RoutePath.Text := aRequestTask.Req.Document;

    Found:= FNVApp.Router.Route(RoutePath, aRequestTask);


    (*if TDWDispatch.DispatchCallBacks(aRequestTask, LRequest, LResponse) then
      Found := True
    else if TDWDispatch.DispatchForms(aRequestTask, LRequest, LResponse) then
      Found := True
    else if TDWDispatch.DispatchVirtualDocuments(aRequestTask, LRequest, LResponse) then
      Found := True
    else if TDWDispatch.DispatchNormalDocument(aRequestTask, LRequest, LResponse) then
      Found := True
    else if TDWDispatch.DispatchFormByName(aRequestTask, LRequest, LResponse) then
      Found := True; *)

    if not Found then
      aRequestTask.Resp.ResponseNo := 404;

    // end modal send
    { if (aRequestTask.FModalStatus = rmsModalStart) and (FActualRequest.FModalStatus = rmsModalEnd)
      then
      FActualRequest.ResponseInfo.ContentText := aRequestTask.ResponseInfo.ContentText; }

  finally
    if aRequestTask.ModalStatus = rmsNone then // normal request
      SendRequestTask(aRequestTask)
    else if aRequestTask.ModalStatus = rmsModalEnd then // modal end request
    begin
        // modal end request are not send now, is send  after exit StartModal loop in TDwAppThread.StartModal
      FModalEndRequest := aRequestTask;
    end
    else if aRequestTask.ModalStatus = rmsModalStartCosed then
    // Modal Start request after modal is closed
    begin
        // send changes over Modal End Request
      SendRequestTask(FModalEndRequest);
      FModalEndRequest := nil;
        // Modal Start request are not released after processed in TDWServer.HttpServerCommandGet
      aRequestTask.Free;
    end;
  end;
end;

class function TNVSessionThread.GetCurrent(var ASessionTh): Boolean;
var
  _TH:TThread;
begin
  _TH:= Self.Current;
  Result:= _TH.InheritsFrom(TNVSessionThread);
  if Result then
    TNVSessionThread(ASessionTh):= TNVSessionThread(_TH)
  else
    TNVSessionThread(ASessionTh) := nil;
end;

function TNVSessionThread.HostApp: TNVHostApp;
begin
  Result := FHostApp;
end;

procedure TNVSessionThread.ProcessMessages(OnlyUpdates: Boolean; StartIndex: Integer);
var
  // LQueue: TList;
  // QueueListLocked: Boolean;
  LMsg: TNVMessage;
  LRequest: TNVRequestTask;
  M: Integer;
begin
  try
    // procedd Queue of requests
    // LQueue          := FMsgQueue.LockList;
    // QueueListLocked := True;
    try
      if FMsgQueue.Count > 0 then
      begin
        M := StartIndex;
        while not (M > FMsgQueue.Count - 1) do
        begin
          LMsg := FMsgQueue.Items[M];
          FCurMSGIndex := M;
          if (LMsg <> nil) then
          begin
            if OnlyUpdates and (LMsg.Msg <> DM_INVALIDATE) then
            begin
              Inc(M);
              Continue;
            end;
            try
                    // remove work from queue
              FMsgQueue.Delete(M);
              case LMsg.Msg of
                DM_REQUEST:
                    ExecuteRequestMessage(LMsg, LRequest);
                DM_INVALIDATE:
                  ExecuteInvalidateMessage(LMsg);
                DM_UPDATE_LANG:
                  UpdateLang;
              end;
            finally
                    // Free MSG
              LMsg.Free;
            end;
          end
          else
          begin
                  // remove work from queue
            FMsgQueue.Delete(M);
          end;
        end;

          (*

            M := StartIndex;

            while Not(M > FMsgQueue.Count - 1) do
            begin
            LMsg         := FMsgQueue.Current^;
            FCurMSGIndex := M;
            if Assigned(LMsg.Obj) then
            begin
            if OnlyUpdates and (LMsg.Msg <> DM_INVALIDATE) then
            begin
            Inc(M);
            FMsgQueue.Next;
            Continue;
            end;
            try
            // remove MSG from queue
            FMsgQueue.Remove(@LMsg);
            //process MSG
            case LMsg.Msg of
            DM_REQUEST: ExecuteRequestMessage(LMsg, LRequest);
            DM_INVALIDATE: ExecuteInvalidateMessage(LMsg);
            end;
            finally
            //Free MSG
            //LMsg.Free;
            end;
            end
            else
            begin
            // remove work from queue
            FMsgQueue.Remove(@LMsg);
            end;
            end; *)
      end
      else
        // if no more work in queue
      begin
        FCurMSGIndex := 0;
          // FMsgQueue.UnlockList;
          // QueueListLocked := False;
          // wait for next queue work
        if Terminated then
          Exit;

        if not OnlyUpdates then
        begin
          FWait.WaitFor(INFINITE);
          FWait.ResetEvent;
        end;
      end;
    finally
      // if QueueListLocked then
      // FMsgQueue.UnlockList;
    end;
  except
    on e: Exception do
      // on error
    begin
        // try send 500 internal Error
      try
        if (LRequest <> nil) and (LRequest.Resp <> nil) and // ignore EAbort exceptions
          not (e is EAbort) then
        begin
          if FNVApp.FIsCallBack then
            FNVApp.ShowAlert(e.Message)
          else
            LRequest.Resp.ResponseText := RSHTTPInternalServerError + ''#13''#10'' + e.Message;
        end;
      except
      end;
        // handle exception an log stack trace
      HandleException(e);
    end;
  end;
  // ProcessException(e);
end;

procedure TNVSessionThread.ProcessRequest(aRequest: TNVRequestTask);
var
  LMsg: TNVMessage;
begin
  LMsg := TNVMessage.Create;
  LMsg.Msg := DM_REQUEST;
  LMsg.Obj := aRequest;
    //put message in thread queue
  if NVPostMessage(LMsg, FNVApp) then
    { Put IndyThread(current execution context) in wait status
      until DWAppThread process this RequestTask,
      IndyThread continue after TDWAppRequestTask.Processed
      in TNVAppThread.ExecuteRequestTask }
    aRequest.WaitForDwAppProcessIt;
end;

procedure TNVSessionThread.RemoveDataModule(aDataModule: TDataModule);
begin

end;

procedure TNVSessionThread.SendRequestTask(aRequestTask: TNVRequestTask);
begin
  // request of ModalEnd already sent on ModalStart
  if { FRequestAfterModalEnd or } (aRequestTask.IsProcessed) then
  begin
    raise Exception.Create(' TNVSessionThread.SendRequestTask: This request already sent.');

      // Modal Start request are not released after processed in TDWServer.HttpServerCommandGet
      // if (aRequestTask.FModalStatus = rmsModalStart) then
      // aRequestTask.Free;
      // Exit;
  end;

  try
    if FNVApp.IsCallback then
    begin
      aRequestTask.Resp.CacheControl := 'no-cache';
      aRequestTask.Resp.Pragma := 'no-cache';
      aRequestTask.Resp.ContentType := 'text/xml';
      aRequestTask.Resp.CharSet := 'UTF-8';
      aRequestTask.Resp.CustomHeaders.Values['Vary'] := 'Accept-Encoding';
        // if not FInModal then
      FNVApp.FIsCallBack := False;
    end;

    if Terminated then
    begin
      aRequestTask.Resp.Clear;
      aRequestTask.Resp.ContentStream.Free; // clear not clear ContentStream
      aRequestTask.Resp.ContentText := '';
      if Assigned(FNVApp) then
      begin
        FNVApp.CallBackResp.Clear;
        FNVApp.FCallbackResp.S['redirect'] := StringReplace(aRequestTask.Req.Referer, aRequestTask.Req.URI, '', [rfIgnoreCase]);

        aRequestTask.Resp.CacheControl := 'no-cache';
        aRequestTask.Resp.Pragma := 'no-cache';
        aRequestTask.Resp.ContentType := 'text/xml';
        aRequestTask.Resp.CharSet := 'UTF-8';
        aRequestTask.Resp.CustomHeaders.Values['Vary'] := 'Accept-Encoding';

        aRequestTask.Resp.ContentText := FNVApp.CallBackResp.ToJson;
      end;

        (* if DWServer.TerminatedUrl <> '' then
          LResponse.Redirect(DWServer.TerminatedUrl)
          else
          LResponse.Redirect(StringReplace(LRequest.Referer, LRequest.URI, '', [rfIgnoreCase]));
          LResponse.CloseConnection:=True; *)
    end;

    if aRequestTask.Resp.ContentType = '' then
    begin
      aRequestTask.Resp.ContentType := 'text/html; charset=UTF-8';
      aRequestTask.Resp.CharSet := 'UTF-8';
    end;

  finally
    aRequestTask.Processed;
    // FActualRequest := nil;
  end;
end;

function TNVSessionThread.SessionApp: TNVSessionApp;
begin
  Result := FNVApp;
end;

procedure TNVSessionThread.UpdateLang;
begin
  case FNVApp.Language of
    nvlENUS:
      SetLangEnUS;
    nvlPTBR:
      SetLangPtBr;
  else
    SetLangEnUS;
  end;
end;

{ TNVSessionApp }

procedure TNVSessionApp.AddPage(aPage: TNVBasePage);
//var
 // _JSON: TJsonObject;
begin
  if FPages.IndexOf(aPage) < 0 then
    FPages.Add(aPage);
 // _JSON := FCallbackResp.O[aPage.ID];
  Router.AddRoute(aPage.RouteName, aPage.Dispatcher);
end;

function TNVSessionApp.CallBackResp: TJsonObject;
begin
  Result := FCallbackResp;
end;

constructor TNVSessionApp.Create(aAppId: string; aMainPage: TNVPageClass; aAppTh: TNVSessionThread; aLanguage: TNVLanguage);
var
  LMsg: TNVMessage;
begin
  inherited Create(nil);
  FAppID := aAppId;
  FAppTh := aAppTh;
  FLanguage := aLanguage;
  FMainpageClass := aMainPage;
  FPages := TList.Create;
  FRouter := TNVRouter.Create;
  // FCallbacks              := TDWCallBacks.Create(Self);
  FCallbackResp := TJsonObject.Create;
  // FGetHandler             := TDWHttpHandlerList.Create;
  // FGetHandler.OwnsObjects := True;
  // FGetHandler.Duplicates  := dupIgnore;
  // FGetAllowedPath         := TDWHttpAllowedPath.Create;
  // FPostHandler    := TDWHttpHandlerList.Create;
  FRequestCount := 0;
  FIsCallBack := False;
  FCacheList := TNVCacheList.Create;
  FCacheList.OwnsObjects := True;
  FCacheList.Duplicates := dupIgnore;
  FReleaseList := TList.Create;
  FInternalRedirect := nil;
  LMsg := TNVMessage.Create;
  LMsg.Msg := DM_UPDATE_LANG;
  NVPostMessage(LMsg, Self);
end;

destructor TNVSessionApp.Destroy;
var
  I: Integer;
begin
  FRouter.Free;
  // FGetHandler.Free;
  // FGetAllowedPath.Free;
  // FPostHandler.Free;
  FCallbackResp.Free;
  // FCallbacks.Free;
  FPages.Free;
  FCacheList.Free;
  // process pending Objects to be released
  for I := 0 to FReleaseList.Count - 1 do
    TObject(FReleaseList[I]).Free;
  FReleaseList.Free;
  if Assigned(FWebSocket) then
  begin
    FWebSocket.Terminate;

    FWebSocket.Free;
  end;
  // release thread languare ressources
  FreeLang;
  inherited;
end;

function TNVSessionApp.IsTimedOut: Boolean;
begin

end;

function TNVSessionApp.QueueMessage(aMsg: TNVMessage): Boolean;
begin
  Result := False;
  if not FAppTh.Terminated then
  begin
    FAppTh.EnqueueMsg(aMsg);
    Result := True;
  end;
end;

procedure TNVSessionApp.RemovePage(aPage: TNVBasePage);
var
  Lindex: Integer;
begin
  Lindex := FPages.IndexOf(aPage);
  if Lindex > -1 then
    FPages.Delete(Lindex);
end;

function TNVSessionApp.SessionID: string;
begin
  Result := FAppID;
end;

procedure TNVSessionApp.SetLanguage(const Value: TNVLanguage);
begin
  FLanguage := Value;
end;

procedure TNVSessionApp.ShowAlert(aMsg: string; aAlertStyle: TDWAlertStyle; RawText: Boolean; aTimeOut: Integer);
begin
  raise Exception.Create('Falta Implementar ShowAlert');
  (* with TDWAlert.Create(aMsg, aAlertStyle, RawText) do
    begin
    Name    := DWGetUniqueComponentName(ActiveForm, 'DWShowAlert');
    Parent  := ActiveForm;
    TimeOut := aTimeOut;
    end; *)
end;

procedure TNVSessionApp.Terminate;
begin

end;

function TNVSessionApp.Terminated: Boolean;
begin

end;

end.

