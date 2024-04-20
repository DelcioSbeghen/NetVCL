unit NV.Browser;

interface

uses
  Classes, Windows, Messages, Controls, NV.VCL.Page;

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

  TNVBrowser = class(TComponent)
  private
    FParent       : TControl;
    FHookedWndProc: TWndMethod;
  protected
    FBrowser: Integer;
    procedure NvWndProc(var Message: TMessage);
    procedure UpdateParent;
    function DoResssourceRequest(Request: TNvResourceRequest; var Response: TNvResourceResponse)
      : Boolean; virtual; abstract;
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
  end;

procedure UnLoadDll;

implementation

uses
  Forms, SysUtils, VCL.ComCtrls, NV.Utils, NV.VCL.Forms {keep NV.VCL.Forms last};

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

procedure TNVBrowser.CloseBrowser;
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

procedure TNVBrowser.CopyScreen(DC: HDC);
begin
  Copy_Screen(FBrowser, DC);
end;

constructor TNVBrowser.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TNVBrowser.CreateNormalBrowser(Callback: TNvSendDataCallback);
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

procedure TNVBrowser.CreateScreenBrowser(Callback: TNvSendDataCallback);
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

destructor TNVBrowser.Destroy;
begin
  if FBrowser <> 0 then
    CloseBrowser;
  inherited;
end;

procedure TNVBrowser.ExecuteJs(const Script: string);
var
  _Script: string;
begin
  if (DebugHook <> 0) and (Not Screen.Active) then
    asm int 3
    end;

  _Script := Copy(Script, 1, Script.Length);
  Execute_Javascript(FBrowser, PChar(_Script));
end;

procedure TNVBrowser.LoadHtml(Html: string);
begin
  Load_Html(FBrowser, PChar(Html));
end;

procedure TNVBrowser.LoadUrl(Url: string);
begin
  Load_Url(FBrowser, PChar(Url));
end;

procedure TNVBrowser.NvWndProc(var Message: TMessage);
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

procedure TNVBrowser.ResizeBrowser(Height, Width: Integer);
begin
  Resize_Browser(FBrowser, Height, Width);
end;

procedure TNVBrowser.SetParent(aParent: TWinControl);
begin
  Set_Parent(FBrowser, aParent.Handle);
end;

procedure TNVBrowser.ShowDesignBrowser(aParent: TControl; Callback: TNvSendDataCallback);
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

procedure TNVBrowser.ShowDevTools(MousePoint: TPoint);
begin
  Show_DevTools(FBrowser, MousePoint);
end;

procedure TNVBrowser.UpdateParent;
begin
  if FParent is TWinControl then
    FParent.Perform(WM_PAINT, 0, 0)
    // PostMessage(TWinControl(FParent).Handle, WM_PAINT, 0, 0)
  else
    FParent.Invalidate;
end;

initialization

finalization

if (hDll > 32) and not(ExtractFilename(paramstr(0)).ToUpper = 'BDS.EXE') then
  UnLoadDll;

end.
