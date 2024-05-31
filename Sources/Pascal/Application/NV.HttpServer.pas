unit NV.HttpServer;

interface

uses
  Classes, IdHTTPServer, IdContext, IdCustomHTTPServer, NV.HttpServerWebsockets,

  NV.VCL.Page,
  NV.VCL.Forms {keep NV.VCL.Forms last};

type

  TNVScreenServer = class(TNvScreen)
  private
  protected
    FBrowser  : TIdHTTPServer;
    FWebSochet: TNVWebSocket;
    // HttpServer
    procedure DoHttpCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure LoadInitialpage(AResponseInfo: TIdHTTPResponseInfo);
    procedure LoadRessource(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure LoadNetVclFiles(Url: string; Response: TIdHTTPResponseInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Active: Boolean; override;
    procedure Show; override;
    procedure Close; override;
    procedure UpdateScreen(Updates: string); override;
  end;

implementation

uses
  StrUtils, NV.Request.Exe, NV.Dispatcher;

{ TNVDesignBrowser }

function TNVScreenServer.Active: Boolean;
begin
  Result := FBrowser.Active { and inherited Active };
end;

procedure TNVScreenServer.Close;
begin
  inherited;
  FBrowser.Active := False;
end;

constructor TNVScreenServer.Create(AOwner: TComponent);
begin
  inherited;
  FUrlBase              := '/';
  FBrowser              := TIdHTTPServer.Create(Self);
  FBrowser.OnCommandGet := DoHttpCommandGet;
end;

destructor TNVScreenServer.Destroy;
begin
  if FBrowser.Active then
    FBrowser.Active := False;
  inherited;
end;

procedure TNVScreenServer.DoHttpCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  if TNVWebSocket.IsWebSocket(AContext.Connection, ARequestInfo, AResponseInfo) then
    Exit;

  if ARequestInfo.Document = Application.UrlBase then
    LoadInitialpage(AResponseInfo)
  else
    LoadRessource(AContext, ARequestInfo, AResponseInfo);
end;

procedure TNVScreenServer.LoadInitialpage(AResponseInfo: TIdHTTPResponseInfo);
var
  _Task: TNVExeRequestTask;
begin
  _Task := TNVExeRequestTask.Create;
  try
    Page.Dispatcher.Execute(_Task);

    AResponseInfo.ResponseNo  := 200;
    AResponseInfo.ContentText := _Task.Resp.Text;


    // Response.DataOut  := Integer(Pointer(_Task.Resp.Text));
    // Response.DataSize := Length(_Task.Resp.Text);
    // Response.MimeType := 'text/html';
    // Response.Status   := 200;

  finally
    _Task.Free;
  end;
end;

procedure TNVScreenServer.LoadNetVclFiles(Url: string; Response: TIdHTTPResponseInfo);
var
  _Task: TNVExeRequestTask;
  _Disp: TDispatchDirFiles;
begin
  _Task := TNVExeRequestTask.Create;
  _Disp := TDispatchDirFiles.Create;
  try

    (_Task.Req as TNvExeRequest).Initialize(Url);

    _Disp.AllowedFlag := afBeginBy;
    _Disp.Execute(_Task);

    Response.ContentText     := _Task.Resp.Text;
    Response.ContentType     := _Task.Resp.CustomHeaderValue['Content-Type'];
    Response.ContentEncoding := 'UTF-8';
    Response.CharSet         := 'UTF-8';

    // Response.DataOut  := Integer(Pointer(_Task.Resp.Text));
    // Response.DataSize := Length(_Task.Resp.Text);
    // Response.MimeType := PChar(_Task.Resp.CustomHeaderValue['Content-Type']);
    // Response.Status   := _Task.Resp.ResponseNo;

  finally
    _Task.Free;
    _Disp.Free;
  end;

end;

procedure TNVScreenServer.LoadRessource(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  if ARequestInfo.Document = Application.UrlBase then
    LoadInitialPage(AResponseInfo)
  else if StartsText(Application.UrlBase, ARequestInfo.Document) then
    LoadNetVclFiles(ARequestInfo.Document, AResponseInfo);
end;

procedure TNVScreenServer.Show;
begin
  FBrowser.Active := True;
  // LoadUrl(Application.UrlBase);
  inherited;
end;

procedure TNVScreenServer.UpdateScreen(Updates: string);
begin
  inherited;

  if Assigned(FWebSochet) then
    FWebSochet.SendMessage(

      // 'App.ParseJson(' + //
      Updates //
      // + ');'             //
      );

  // ExecuteJs(           //
  // 'App.ParseJson(' + //
  // Updates            //
  // + ');'             //
  // );
end;

initialization

if not Assigned(Screen) then
  Screen := TNVScreenServer.Create(nil);

end.
