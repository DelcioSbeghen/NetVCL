unit NV.Desktop;

interface

uses
  Classes, Windows, Messages, SysUtils, StrUtils, Forms {Keep forms before} , Controls, NV.VCL.Page,
  NV.VCL.Forms {keep NV.VCL.Forms last};

type

  TProcObjecCallback  = procedure of object;
  TNvSendDataCallback = procedure(const Value: PChar) of object;

  // Dont change this struct
  TNvResourceRequest = record
    Url: PChar;
    ResourceType: Word;
    PostData: PChar;
  end;

  // Dont change this struct
  TNvResourceResponse = record
    DataOut: Integer;
    DataSize: Integer;
    MimeType: string;
    Status: Integer;
  end;

  // Dont change this struct
  TNvResourceCallback = function(Request: TNvResourceRequest; var Response: TNvResourceResponse)
    : Boolean of object;

  TNVScreenBrowser = class(TNvScreen)
  private
    FParent       : TControl;
    FHookedWndProc: TWndMethod;
  protected
    FBrowser: Integer;
    procedure NvWndProc(var Message: TMessage);
    procedure UpdateParent;
    // moved from screen
    function DoResssourceRequest(Request: TNvResourceRequest; var Response: TNvResourceResponse)
      : Boolean; virtual;
    procedure LoadInitialPage(var Response: TNvResourceResponse);
    procedure LoadNetVclFiles(Url: string; var Response: TNvResourceResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowDesignBrowser(aParent: TControl; Callback: TNvSendDataCallback);
    procedure CreateNormalBrowser(Callback: TNvSendDataCallback);
    procedure CreateScreenBrowser(Callback: TNvSendDataCallback);
    procedure CopyScreen(DC: HDC);
    procedure SetParent(aParent: TWinControl);
    procedure ResizeBrowser(Height: Integer; Width: Integer);
    procedure ShowDevTools(MousePoint: TPoint);
    procedure CloseBrowser;
    procedure LoadUrl(Url: string);
    procedure LoadHtml(Html: string);
    procedure ExecuteJs(const Script: string);

    function Active: Boolean; override;
    procedure ShowDesign(aParent: TObject);
    procedure Show; override;
    procedure Close; override;
    procedure UpdateScreen(Updates: string); override;
  end;

procedure UnLoadDll;

implementation

uses
  VCL.ComCtrls, NV.Utils, NV.Request.Exe, NV.Dispatcher;

type

  THackCustomControl  = class(TCustomControl);
  THackGraphicControl = class(TGraphicControl);

  TCreate_Design_BrowserProc = function(InvalidateCallback: TProcObjecCallback;
    ResCallback: TNvResourceCallback): Integer; stdcall;
  TCreate_Normal_BrowserProc = function(SendDataCallback: TNvSendDataCallback;
    ResCallback: TNvResourceCallback)                   : Integer; stdcall;
  TCopy_ScreenProc       = function(Browser: Integer; DC: HDC): Boolean; stdcall;
  TSet_ParentProc        = procedure(Browser: Integer; ParentHandle: HWND); stdcall;
  TResize_BrowserProc    = procedure(Browser: Integer; Height: Integer; Width: Integer); stdcall;
  TLoad_UrlProc          = procedure(Browser: Integer; Url: PChar); stdcall;
  TSet_SendData_Callback = procedure(Browser: Integer;
    SendDataCallback: TNvSendDataCallback); stdcall;
  TExecute_Javascript        = procedure(Browser: Integer; const Script: PChar); stdcall;
  TLoad_Html                 = procedure(Browser: Integer; Html: PChar); stdcall;
  TClose_BrowserProc         = procedure(Browser: Integer); stdcall;
  TInitialize_CEF4DelphiProc = procedure(DisableSecurity: Boolean = False;
    RemoteDebuggerPort: Integer = 0); stdcall;
  TFinalize_CEF4Delphi = procedure; stdcall;
  TShow_DevTools       = procedure(Browser: Integer; MousePoint: TPoint); stdcall;

var
  hDll                 : THandle = 0;
  Create_Design_Browser: TCreate_Design_BrowserProc;
  Create_Normal_Browser: TCreate_Normal_BrowserProc;
  Create_Screen_Browser: TCreate_Normal_BrowserProc;
  Copy_Screen          : TCopy_ScreenProc;
  Set_Parent           : TSet_ParentProc;
  Resize_Browser       : TResize_BrowserProc;
  Load_Url             : TLoad_UrlProc;
  Set_SendData_Callback: TSet_SendData_Callback;
  Execute_Javascript   : TExecute_Javascript;
  Load_Html            : TLoad_Html;
  Show_DevTools        : TShow_DevTools;
  Close_Browser        : TClose_BrowserProc;
  Initialize_CEF4Delphi: TInitialize_CEF4DelphiProc;
  Finalize_CEF4Delphi  : TFinalize_CEF4Delphi;

procedure LoadDll;
var
  _CefPath, _RootPath: string;
begin
  _RootPath := GetNVSourcesPath('');
  _CefPath  := ExpandFileName(_RootPath + PathDelim + '..') + PathDelim + 'CEF' + PathDelim;

  hDll := LoadLibrary(PWideChar(_CefPath + 'NVBrowserDll.dll'));

  if hDll < 32 then
    raise Exception.Create('Error on Load NVBrowserDll.dll in path:' + _CefPath);

  Create_Design_Browser := GetProcAddress(hDll, PChar('Create_Design_Browser'));
  Create_Normal_Browser := GetProcAddress(hDll, PChar('Create_Normal_Browser'));
  Create_Screen_Browser := GetProcAddress(hDll, PChar('Create_Screen_Browser'));
  Copy_Screen           := GetProcAddress(hDll, PChar('Copy_Screen'));
  Set_Parent            := GetProcAddress(hDll, PChar('Set_Parent'));
  Resize_Browser        := GetProcAddress(hDll, PChar('Resize_Browser'));
  Load_Url              := GetProcAddress(hDll, PChar('Load_Url'));
  Set_SendData_Callback := GetProcAddress(hDll, PChar('Set_SendData_Callback'));
  Execute_Javascript    := GetProcAddress(hDll, PChar('Execute_Javascript'));
  Load_Html             := GetProcAddress(hDll, PChar('Load_Html'));
  Show_DevTools         := GetProcAddress(hDll, PChar('Show_DevTools'));
  Close_Browser         := GetProcAddress(hDll, PChar('Close_Browser'));
  Initialize_CEF4Delphi := GetProcAddress(hDll, PChar('Initialize_CEF4Delphi'));
  Finalize_CEF4Delphi   := GetProcAddress(hDll, PChar('Finalize_CEF4Delphi'));

end;

procedure UnLoadDll;
begin
  if hDll <> 0 then
    begin
      Finalize_CEF4Delphi;
      FreeLibrary(hDll);
      hDll := 0;
    end;
end;

{ TNVDesignBrowser }

function TNVScreenBrowser.Active: Boolean;
begin
  Result := (FBrowser <> 0) and inherited Active;
end;

procedure TNVScreenBrowser.Close;
begin
  inherited;
  CloseBrowser;
end;

procedure TNVScreenBrowser.CloseBrowser;
begin
  if Assigned(FParent) and Assigned(FHookedWndProc) then
    begin
      FParent.WindowProc := FHookedWndProc;
      FHookedWndProc     := nil;
      FParent            := nil;
      // {Browser}SetParent(nil);
    end;

  if (FBrowser > 0) and (hDll <> 0) then
    Close_Browser(FBrowser);
  FBrowser := 0;
end;

procedure TNVScreenBrowser.CopyScreen(DC: HDC);
begin
  Copy_Screen(FBrowser, DC);
end;

constructor TNVScreenBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FUrlBase := 'nvlocal://screen/';
end;

procedure TNVScreenBrowser.CreateNormalBrowser(Callback: TNvSendDataCallback);
var
  _DebugPort: Integer;
begin
  if DebugHook <> 0 then
    _DebugPort := 9555
  else
    _DebugPort := 0;

  if hDll < 32 then
    begin
      LoadDll;
      { TODO -oDelcio -cSECURITY : Enable Security in CEF(cors) }
      Initialize_CEF4Delphi(True, _DebugPort);
    end;

  FBrowser := Create_Normal_Browser(Callback, DoResssourceRequest);

  if FBrowser <> 0 then
    // begin
    // FHookedWndProc     := FParent.WindowProc;
    // FParent.WindowProc := NvWndProc;
    // end;

    // ResizeBrowser(aParent.Height, aParent.Width);
    // ShowDevTools(TPoint.Zero);

end;

procedure TNVScreenBrowser.CreateScreenBrowser(Callback: TNvSendDataCallback);
var
  _DebugPort: Integer;
begin
  if DebugHook <> 0 then
    _DebugPort := 9555
  else
    _DebugPort := 0;

  if hDll < 32 then
    begin
      LoadDll;
      { TODO -oDelcio -cSECURITY : Enable Security in CEF(cors) }
      Initialize_CEF4Delphi(True, _DebugPort);
    end;

  FBrowser := Create_Screen_Browser(Callback, DoResssourceRequest);

  if FBrowser <> 0 then
    // begin
    // FHookedWndProc     := FParent.WindowProc;
    // FParent.WindowProc := NvWndProc;
    // end;

    // ResizeBrowser(aParent.Height, aParent.Width);
end;

destructor TNVScreenBrowser.Destroy;
begin
  if FBrowser <> 0 then
    CloseBrowser;
  inherited;
end;

function TNVScreenBrowser.DoResssourceRequest(Request: TNvResourceRequest;
  var Response: TNvResourceResponse): Boolean;
var
  _Response: ^TNvResourceResponse;
begin
  Response.Status := 404;

  // To anonymous Method capture variable
  _Response := @Response;

  TThread.Synchronize(nil,
    procedure
    begin
      if Request.Url = Application.UrlBase then
        LoadInitialPage(_Response^)
      else if StartsText(Application.UrlBase, Request.Url) then
        LoadNetVclFiles(Request.Url, _Response^);
    end);

  Result := Response.Status <> 404;
end;

procedure TNVScreenBrowser.ExecuteJs(const Script: string);
var
  _Script: string;
begin
  if (DebugHook <> 0) and (Not Screen.Active) then
    asm int 3
    end;

  _Script := Copy(Script, 1, Script.Length);
  Execute_Javascript(FBrowser, PChar(_Script));
end;

procedure TNVScreenBrowser.LoadHtml(Html: string);
begin
  Load_Html(FBrowser, PChar(Html));
end;

procedure TNVScreenBrowser.LoadInitialPage(var Response: TNvResourceResponse);
var
  _Task: TNVExeRequestTask;
begin
  _Task := TNVExeRequestTask.Create;
  try
    Page.Dispatcher.Execute(_Task);

    Response.DataOut  := Integer(Pointer(_Task.Resp.Text));
    Response.DataSize := Length(_Task.Resp.Text);
    Response.MimeType := 'text/html';
    Response.Status   := 200;

  finally
    _Task.Free;
  end;
end;

procedure TNVScreenBrowser.LoadNetVclFiles(Url: string; var Response: TNvResourceResponse);
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

    Response.DataOut  := Integer(Pointer(_Task.Resp.Text));
    Response.DataSize := Length(_Task.Resp.Text);
    Response.MimeType := PChar(_Task.Resp.CustomHeaderValue['Content-Type']);
    Response.Status   := _Task.Resp.ResponseNo;

  finally
    _Task.Free;
    _Disp.Free;
  end;
end;

procedure TNVScreenBrowser.LoadUrl(Url: string);
begin
  Load_Url(FBrowser, PChar(Url));
end;

procedure TNVScreenBrowser.NvWndProc(var Message: TMessage);
var
  _ThisWndProc: TWndMethod;
begin
  if not(csDesigning in FParent.ComponentState) and (Message.Msg = WM_PAINT) then
    begin
      if FParent is TCustomControl then
        CopyScreen(THackCustomControl(FParent).Canvas.Handle)
      else if FParent is TGraphicControl then
        CopyScreen(THackGraphicControl(FParent).Canvas.Handle)
      else if FParent is TCustomListView then
        CopyScreen(TCustomListView(FParent).Canvas.Handle)
      else if FParent is TCustomForm then
        CopyScreen(TCustomForm(FParent).Canvas.Handle);

    end
  else if Message.Msg = WM_WINDOWPOSCHANGED then
    begin
      ResizeBrowser(FParent.Height, FParent.Width);
    end
  else if (Message.Msg = WM_ERASEBKGND) then
    begin
      if FParent is TCustomControl then
        CopyScreen(THackCustomControl(FParent).Canvas.Handle)
      else if FParent is TGraphicControl then
        CopyScreen(THackGraphicControl(FParent).Canvas.Handle)
      else if FParent is TCustomListView then
        CopyScreen(TCustomListView(FParent).Canvas.Handle)
      else if FParent is TCustomForm then
        CopyScreen(TCustomForm(FParent).Canvas.Handle);

      Exit;
    end;

  _ThisWndProc := Self.NvWndProc;

  if Assigned(FHookedWndProc) and (TMethod(_ThisWndProc).Code <> TMethod(FHookedWndProc).Code) then
    FHookedWndProc(Message);
end;

procedure TNVScreenBrowser.ResizeBrowser(Height, Width: Integer);
begin
  Resize_Browser(FBrowser, Height, Width);
end;

procedure TNVScreenBrowser.SetParent(aParent: TWinControl);
begin
  Set_Parent(FBrowser, aParent.Handle);
end;

procedure TNVScreenBrowser.Show;
begin
  CreateScreenBrowser(DataReceived);
  LoadUrl(Application.UrlBase);
  inherited;
end;

procedure TNVScreenBrowser.ShowDesign(aParent: TObject);
begin
  ShowDesignBrowser(aParent as TWinControl, DataReceived);
  LoadUrl(Application.UrlBase);
  // FActive := True;
end;

procedure TNVScreenBrowser.ShowDesignBrowser(aParent: TControl; Callback: TNvSendDataCallback);
begin

  if hDll < 32 then
    begin
      LoadDll;
      { TODO -oDelcio -cSECURITY : Enable Security in CEF(cors) }
      Initialize_CEF4Delphi(True);
    end;

  if FBrowser <> 0 then
    CloseBrowser;

  FBrowser := Create_Design_Browser(aParent.Invalidate, DoResssourceRequest);

  if FBrowser <> 0 then
    begin
      FParent            := aParent;
      FHookedWndProc     := FParent.WindowProc;
      FParent.WindowProc := NvWndProc;
    end;

  ResizeBrowser(aParent.Height, aParent.Width);

  Set_SendData_Callback(FBrowser, Callback);
end;

procedure TNVScreenBrowser.ShowDevTools(MousePoint: TPoint);
begin
  Show_DevTools(FBrowser, MousePoint);
end;

procedure TNVScreenBrowser.UpdateParent;
begin
  if FParent is TWinControl then
    FParent.Perform(WM_PAINT, 0, 0)
    // PostMessage(TWinControl(FParent).Handle, WM_PAINT, 0, 0)
  else
    FParent.Invalidate;
end;

procedure TNVScreenBrowser.UpdateScreen(Updates: string);
begin
  inherited;
  ExecuteJs(           //
    'App.ParseJson(' + //
    Updates            //
    + ');'             //
    );
end;

initialization

if not Assigned(Screen) then
  Screen := TNVScreenBrowser.Create(nil);

finalization

if (hDll > 32) and not(ExtractFilename(paramstr(0)).ToUpper = 'BDS.EXE') then
  UnLoadDll;

end.
