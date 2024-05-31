unit NV.Desktop;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, Windows, Messages, SysUtils, StrUtils, {$IFDEF FPC} LazUTF8, {$ENDIF} Forms {Keep forms before} , Controls, NV.VCL.Page,
  NV.VCL.Forms {keep NV.VCL.Forms last};

type
{$IFDEF FPC}
  ustring = UnicodeString;
  PuChar  = PUnicodeChar;
{$ELSE}
  ustring = string;
  PuChar  = PChar;
{$ENDIF}

  // Dont change this struct
  TNvResourceRequest = record
    Url: PuChar;
    ResourceType: Word;
    PostData: PuChar;
  end;

  // Dont change this struct
  TNvResourceResponse = record
    DataOut: Integer;
    DataSize: Integer;
    MimeType: ustring;
    Status: Integer;
  end;

  // Dont change this struct
  IScreenCallback = interface
    procedure DataReceivedUnicode(const uData: ustring);
    function DoResssourceRequest(Request: TNvResourceRequest;
      var Response: TNvResourceResponse): Boolean;
    procedure InvalidateScreen;
  end;

  { TNVScreenBrowser }

  TNVScreenBrowser = class(TNvScreen, IScreenCallback)
{$IFNDEF FPC}
  private
    FHookedWndProc: TWndMethod;
  protected
    procedure NvWndProc(var Message: TMessage);
{$ENDIF}
  private
    FParent: TControl;
  protected
    FBrowser: Integer;
    //procedure UpdateParent;
    procedure DataReceivedUnicode(const uData: ustring); inline;
    // moved from screen
    function DoResssourceRequest(Request: TNvResourceRequest; var Response: TNvResourceResponse)
      : Boolean; virtual;
    procedure LoadInitialPage(var Response: TNvResourceResponse);
    procedure LoadNetVclFiles(Url: string; var Response: TNvResourceResponse);
    procedure InvalidateScreen;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowDesignBrowser(aParent: TControl);
    procedure CreateNormalBrowser;
    procedure CreateScreenBrowser;
    procedure CopyScreen(DC: HDC);
    procedure SetParent(aParent: TWinControl);
    procedure ResizeBrowser(Height: Integer; Width: Integer);
    procedure ShowDevTools(MousePoint: TPoint);
    procedure CloseBrowser;
    procedure LoadUrl(Url: string);
    procedure LoadHtml(Html: string);
    procedure ExecuteJs(const Script: ustring);

    function Active: Boolean; override;
    procedure ShowDesign(aParent: TObject);
    procedure Show; override;
    procedure Close; override;
    procedure UpdateScreen(Updates: string); override;
  end;

procedure UnLoadDll;

implementation

uses
  Dialogs, ComCtrls, NV.Utils, NV.Request.Exe, NV.Dispatcher;

type

  THackCustomControl  = class(TCustomControl);
  THackGraphicControl = class(TGraphicControl);

  TCreate_Design_BrowserProc = function(aCallback: IScreenCallback): Integer; stdcall;
  TCreate_Normal_BrowserProc = function(aCallback: IScreenCallback): Integer; stdcall;
  TCopy_ScreenProc           = function(Browser: Integer; DC: HDC)           : Boolean; stdcall;
  TSet_ParentProc            = procedure(Browser: Integer; ParentHandle: HWND); stdcall;
  TResize_BrowserProc = procedure(Browser: Integer; Height: Integer; Width: Integer); stdcall;
  TLoad_UrlProc = procedure(Browser: Integer; Url: PuChar); stdcall;
  // TSet_SendData_Callback = procedure(Browser: Integer;
  // SendDataCallback: TNvSendDataCallback); stdcall;
  TExecute_Javascript        = procedure(Browser: Integer; const Script: PuChar); stdcall;
  TLoad_Html                 = procedure(Browser: Integer; Html: PuChar); stdcall;
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
  // Set_SendData_Callback: TSet_SendData_Callback;
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

  hDll := LoadLibrary({$IFDEF FPC} PChar {$ELSE} PWideChar {$ENDIF}(_CefPath + 'NVBrowserDll.dll'));

  if hDll < 32 then
    raise Exception.Create('Error on Load NVBrowserDll.dll in path:' + _CefPath);

  Create_Design_Browser := GetProcAddress(hDll, PChar('Create_Design_Browser'));
  Create_Normal_Browser := GetProcAddress(hDll, PChar('Create_Normal_Browser'));
  Create_Screen_Browser := GetProcAddress(hDll, PChar('Create_Screen_Browser'));
  Copy_Screen           := GetProcAddress(hDll, PChar('Copy_Screen'));
  Set_Parent            := GetProcAddress(hDll, PChar('Set_Parent'));
  Resize_Browser        := GetProcAddress(hDll, PChar('Resize_Browser'));
  Load_Url              := GetProcAddress(hDll, PChar('Load_Url'));
  // Set_SendData_Callback := GetProcAddress(hDll, PChar('Set_SendData_Callback'));
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
  {$IFNDEF FPC}
  if Assigned(FHookedWndProc) then
    begin
      FParent.WindowProc := FHookedWndProc;
      FHookedWndProc     := nil;
    end;
{$ENDIF}

  if Assigned(FParent) then
    FParent := nil;

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

procedure TNVScreenBrowser.CreateNormalBrowser;
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

  FBrowser := Create_Normal_Browser(Self);
end;

procedure TNVScreenBrowser.CreateScreenBrowser;
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

  FBrowser := Create_Screen_Browser(Self);
end;

destructor TNVScreenBrowser.Destroy;
begin
  FParent := nil;
  if FBrowser <> 0 then
    CloseBrowser;
  inherited;
end;

{$IFDEF FPC}

type
  TRessRequestSync = class
  public
    Request : TNvResourceRequest;
    Response: ^TNvResourceResponse;
    Screen  : TNVScreenBrowser;
    procedure LoadInitialpage;
    procedure LoadNetVclFiles;
  end;

procedure TRessRequestSync.LoadInitialpage;
begin
  Screen.LoadInitialPage(Response^);
end;

procedure TRessRequestSync.LoadNetVclFiles;
begin
  Screen.LoadNetVclFiles(Request.Url, Response^);
end;

function TNVScreenBrowser.DoResssourceRequest(Request: TNvResourceRequest;
  var Response: TNvResourceResponse): Boolean;
var
  _ReqSync: TRessRequestSync;
begin
  { TODO -oDELCIO : FPC - need to synchronize this, but looking when using TThread.Synchronize }
  if SameStr(Request.Url, Application.UrlBase) then
    LoadInitialPage(Response)
  else if StartsText(Application.UrlBase, Request.Url) then
    LoadNetVclFiles(Request.Url, Response);
  // _ReqSync := TRessRequestSync.Create;
  // try
  // _ReqSync.Request  := Request;
  // _ReqSync.Response := @Response;
  // _ReqSync.Screen   := Self;
  //
  // if SameStr(Request.Url, Application.UrlBase) then
  // TThread.Synchronize(nil, _ReqSync.LoadInitialPage)
  // else if StartsText(Application.UrlBase, Request.Url) then
  // TThread.Synchronize(nil, _ReqSync.LoadNetVclFiles);
  //
  // finally
  // _ReqSync.Free;
  // end;
{$ELSE}

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
{$ENDIF}
  Result := Response.Status <> 404;
end;

procedure TNVScreenBrowser.ExecuteJs(const Script: ustring);
var
  _Script: string;
begin
{$IFDEF CPUX86}
  if (DebugHook <> 0) and (Not Screen.Active) then
    asm int 3
    end;
{$ENDIF}
{$IFDEF FPC}
  Execute_Javascript(FBrowser, PuChar(Script))
{$ELSE}
  _Script := Copy(Script, 1, Script.Length);
  Execute_Javascript(FBrowser, PuChar(_Script))
{$ENDIF};
end;

procedure TNVScreenBrowser.LoadHtml(Html: string);
begin
  Load_Html(FBrowser, PuChar(Html));
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

procedure TNVScreenBrowser.InvalidateScreen;
begin
  if FParent <> nil then
    FParent.Invalidate;
end;

procedure TNVScreenBrowser.LoadUrl(Url: string);
{$IFDEF FPC}
var
  _Url: Ustring;
begin
  _Url := UTF8ToUTF16(Url);
  Load_Url(FBrowser, PuChar(_Url));
{$ELSE}
begin
  Load_Url(FBrowser, PChar(Url));
{$ENDIF}
end;

{$IFNDEF FPC}

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
    ResizeBrowser(FParent.Height, FParent.Width)
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
{$ENDIF}

procedure TNVScreenBrowser.ResizeBrowser(Height: Integer; Width: Integer);
begin
  Resize_Browser(FBrowser, Height, Width);
end;

procedure TNVScreenBrowser.SetParent(aParent: TWinControl);
begin
  Set_Parent(FBrowser, aParent.Handle);
end;

procedure TNVScreenBrowser.Show;
begin
  CreateScreenBrowser;
  LoadUrl(Application.UrlBase);
  inherited;
end;

procedure TNVScreenBrowser.ShowDesign(aParent: TObject);
begin
  ShowDesignBrowser(aParent as TWinControl);
  LoadUrl(Application.UrlBase);
end;

procedure TNVScreenBrowser.ShowDesignBrowser(aParent: TControl);
begin

  if hDll < 32 then
    begin
      LoadDll;
      { TODO -oDelcio -cSECURITY : Enable Security in CEF(cors) }
      Initialize_CEF4Delphi(True);
    end;

  if FBrowser <> 0 then
    CloseBrowser;

  FBrowser := Create_Design_Browser(Self);

  if FBrowser <> 0 then
    begin
      FParent := aParent;
{$IFNDEF FPC}
      FHookedWndProc     := FParent.WindowProc;
      FParent.WindowProc := NvWndProc;
{$ENDIF}
    end;

  ResizeBrowser(aParent.Height, aParent.Width);
end;

procedure TNVScreenBrowser.ShowDevTools(MousePoint: TPoint);
begin
  Show_DevTools(FBrowser, MousePoint);
end;

//procedure TNVScreenBrowser.UpdateParent;
//begin
//  if FParent is TWinControl then
//    FParent.Perform(WM_PAINT, 0, 0)
//    // PostMessage(TWinControl(FParent).Handle, WM_PAINT, 0, 0)
//  else
//    FParent.Invalidate;
//end;

procedure TNVScreenBrowser.DataReceivedUnicode(const uData: ustring);
{$IFDEF FPC}
var
  _Data: string;
begin
  _Data := UTF16ToUTF8(uData);
  DataReceived(PChar(_Data));
{$ELSE}
begin
  DataReceived(PChar(uData));
{$ENDIF}
end;

procedure TNVScreenBrowser.UpdateScreen(Updates: string);
begin
  inherited;
{$IFDEF FPC}
  ExecuteJs(UTF8ToUTF16( //
    'App.ParseJson(' +   //
    Updates              //
    + ');'               //
    ));
{$ELSE}
  ExecuteJs(           //
    'App.ParseJson(' + //
    Updates            //
    + ');'             //
    );
{$ENDIF}
end;

initialization

if not Assigned(Screen) then
  Screen := TNVScreenBrowser.Create(nil);

finalization

if (hDll > 32) {$IFNDEF FPC} and not(ExtractFilename(paramstr(0)).ToUpper = 'BDS.EXE') {$ENDIF} then
  UnLoadDll;

end.
