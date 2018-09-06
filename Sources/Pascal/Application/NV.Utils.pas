unit NV.Utils;

interface

uses
  Classes, Controls, NV.VCL.Page, NV.Session, NV.Common.HostAppInterface;

type
  TJumpOfs = Integer;

  PXRedirCode = ^TXRedirCode;

  TXRedirCode = packed record
    Jump: Byte;
    Offset: TJumpOfs;
  end;

  // Find a parent Page of one NV Control
function FindParentPage(aControl: TControl): TNVBasePage;

function MakeValidFileUrl(const ARootUrl: string; const AFileUrl: string): string;

function MakeValidFileUrl2(const ARootUrl: string; const AFileUrl: string; ADisableCache: boolean = True): string;

function IIf(Expressao: Variant; ParteTRUE, ParteFALSE: Variant): Variant;

// get address of one procedure or methos
function GetProcAddr(Proc: Pointer): Pointer;
// hook procedure or method in one class to other procedure or method

procedure HookProc(Proc, Dest: Pointer; var BackupCode: TXRedirCode);
// unhook procedure or method with HookProc

procedure UnhookProc(Proc: Pointer; var BackupCode: TXRedirCode);
// replace an method in one class to other in the VMT class Table

procedure ReplaceVmtField(AClass: TClass; OldProc, NewProc: Pointer);

// return the NVSessionApp for this session
function NVSessionApp: TNVSessionApp;

// return the NVSessionThread for this session
function NVSessionThread: TNVSessionThread;

function NVServer: INVServer;

implementation

uses
  Windows, SysUtils, StrUtils, NV.VCL.Controls, NV.VCL.Container;

type
  PWin9xDebugThunk = ^TWin9xDebugThunk;

  TWin9xDebugThunk = packed record
    PUSH: Byte;
    Addr: Pointer;
    JMP: TXRedirCode;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;

  TAbsoluteIndirectJmp = packed record
    OpCode: Word; // $FF25(Jmp, FF /4)
    Addr: PPointer;
  end;

function FindParentPage(aControl: TControl): TNVBasePage;
var
  CompTest: TControl;
  // Check: IDWControl;
begin
  Result := nil;
  CompTest := aControl;
  while Assigned(CompTest) and (not (CompTest.InheritsFrom(TNVBasePage))) do
  begin
    if Assigned(CompTest.Parent) then
      CompTest := CompTest.Parent
    else
      CompTest := nil;
  end;
  if CompTest = nil then
  begin
    if TControl(aControl).Owner is TNVModuleContainer then
    begin
      CompTest := TControl(aControl.Owner.Owner);
      while (CompTest <> nil) and (not (CompTest.InheritsFrom(TNVBasePage))) do
        CompTest := TControl(CompTest.Owner);
    end;
  end;
  if CompTest <> nil then
    if CompTest.InheritsFrom(TNVBasePage) then
      Result := CompTest as TNVBasePage;
end;

function MakeValidFileUrl(const ARootUrl: string; const AFileUrl: string): string;
var
  _RootUrl: string;
  _FileUrl: string;
  _SessionTh: TNVSessionThread;
  _LibDir: string;
  _DocDir: string;
begin
  TNVSessionThread.GetCurrent(_SessionTh);
  //get root url
  if ARootUrl <> '' then   {TODO -oDelcio -cImprove : Remove _RootUrl Parameter if not Needed}
    _RootUrl := ARootUrl
  else if Assigned(_SessionTh) then
    _RootUrl := _SessionTh.HostApp.UrlBase;
  //get doc and lib dir
  if Assigned(_SessionTh) then
  begin
    _LibDir := _SessionTh.HostApp.LibDir;
    _DocDir := _SessionTh.HostApp.DocDir;
  end
  else
  begin
    _LibDir := '';
    _DocDir := '';
  end;

  _FileUrl := ReplaceStr(AFileUrl, '/<nvlibpath>/', _LibDir);
  _FileUrl := ExtractRelativePath(_DocDir + '\', _FileUrl);
  _FileUrl := StringReplace(_FileUrl, '\', '/', [rfReplaceAll]);
  Result := { _RootUrl + } '/' + _FileUrl;
end;

function MakeValidFileUrl2(const ARootUrl: string; const AFileUrl: string; ADisableCache: boolean = True): string;
var
  _DisableCache: boolean;
  _SessionTh: TNVSessionThread;
  _LibDir: string;
  //_DocDir: string;
  _RefreshCacheParam: string;
begin
  _DisableCache := ADisableCache;
  if TNVSessionThread.GetCurrent(_SessionTh) then
  begin
    _LibDir := _SessionTh.HostApp.LibDir;
   // _DocDir := _SessionTh.HostApp.DocDir;
    _RefreshCacheParam := _SessionTh.HostApp.RefreshCacheParam;
  end
  else
  begin
    _LibDir := '';
   // _DocDir := '';
    _RefreshCacheParam := '';
  end;

  if AnsiStartsStr('//', AFileUrl) or AnsiContainsStr(AFileUrl, '://') then
  begin
    _DisableCache := False;
    Result := ReplaceStr(AFileUrl, '/<dwlibpath>/', _LibDir);
  end
  else
    Result := MakeValidFileUrl(ARootUrl, AFileUrl);

  if _DisableCache then
    Result := Result + '?v=' + _RefreshCacheParam;
end;

function IIf(Expressao: Variant; ParteTRUE, ParteFALSE: Variant): Variant;
begin
  if Expressao then
    Result := ParteTRUE
  else
    Result := ParteFALSE;
end;

function GetProcAddr(Proc: Pointer): Pointer;

  function IsWin9xDebugThunk(AAddr: Pointer): boolean;
  begin
    Result := (AAddr <> nil) and (PWin9xDebugThunk(AAddr).PUSH = $68) and (PWin9xDebugThunk(AAddr).JMP.Jump = $E9);
  end;

begin
  if Proc <> nil then
  begin
    if (Win32Platform <> VER_PLATFORM_WIN32_NT) and IsWin9xDebugThunk(Proc) then
      Proc := PWin9xDebugThunk(Proc).Addr;
    if (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;

procedure HookProc(Proc, Dest: Pointer; var BackupCode: TXRedirCode);
var
  N: NativeUInt;
  Code: TXRedirCode;
begin
  Proc := GetProcAddr(Proc);
  Assert(Proc <> nil);
  if ReadProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), N) then
  begin
    Code.Jump := $E9;
    Code.Offset := PAnsiChar(Dest) - PAnsiChar(Proc) - SizeOf(Code);
    WriteProcessMemory(GetCurrentProcess, Proc, @Code, SizeOf(Code), N);
  end;
end;

procedure UnhookProc(Proc: Pointer; var BackupCode: TXRedirCode);
var
  N: NativeUInt;
begin
  if (BackupCode.Jump <> 0) and (Proc <> nil) then
  begin
    Proc := GetProcAddr(Proc);
    Assert(Proc <> nil);
    WriteProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), N);
    BackupCode.Jump := 0;
  end;
end;

procedure ReplaceVmtField(AClass: TClass; OldProc, NewProc: Pointer);
type
  PVmt = ^TVmt;

  TVmt = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
var
  I: Integer;
  Vmt: PVmt;
  N: NativeUInt;
  P: Pointer;
begin
  OldProc := GetProcAddr(OldProc);
  NewProc := GetProcAddr(NewProc);

  I := vmtSelfPtr div SizeOf(Pointer);
  Vmt := Pointer(AClass);
  while (I < 0) or (Vmt[I] <> nil) do
  begin
    P := Vmt[I];
    if (P <> OldProc) and (Integer(P) > $10000) and not IsBadReadPtr(P, 6) then
      P := GetProcAddr(P);
    if P = OldProc then
    begin
      WriteProcessMemory(GetCurrentProcess, @Vmt[I], @NewProc, SizeOf(NewProc), N);
      Exit;
    end;
    Inc(I);
  end;
end;

function NVSessionApp: TNVSessionApp;
var
  _SessionTh: TNVSessionThread;
begin
  if TNVSessionThread.GetCurrent(_SessionTh) then
    Result := _SessionTh.SessionApp
  else
    Result := nil;
  (*Result := nil;
  if (TThread.CurrentThread is TNVAppThread) then
    Result := TNVAppThread(TThread.Current).DWApp;
  // else
  // raise Exception.Create('Corrigir Isso'); *)
end;

function NVSessionThread: TNVSessionThread;
begin
  TNVSessionThread.GetCurrent(Result);
  (* Result := nil;
  if (TThread.CurrentThread is TNVSessionThread) then
    Result := TNVSessionThread(TThread.Current);
  // else
  // raise Exception.Create('Corrigir Isso');*)
end;

function NVServer: INVServer;
begin
  try
    Result := NVSessionThread.HostApp.GetServer;
  except
    Result := nil;
    if DebugHook <> 0 then
      raise;
  end;
end;

end.

