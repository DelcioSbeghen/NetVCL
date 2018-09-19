unit NV.Utils;

interface

uses
  Classes, Controls, NV.VCL.Page, NV.Session, NV.Common.HostAppInterface, NV.HostApplication;

type
  TJumpOfs = Integer;

  PXRedirCode = ^TXRedirCode;

  TXRedirCode = packed record
    Jump: Byte;
    Offset: TJumpOfs;
  end;
function TranslateChar(const Str: String; FromChar, ToChar: Char): String;
function htoin(Value: PWideChar; Len: Integer): Integer;
function UrlDecode(const Url: String; SrcCodePage: LongWord = CP_ACP;
  DetectUtf8: boolean = TRUE): String;
function AbsolutisePath(const Path: String): String;
function AdjustOSPathDelimiters(const Path: String): String;
function IsDirectory(const Path: String): boolean;
function RemoveDelimiters(const Path:string; Delimiter:Char = '/'):string;


  // Find a parent Page of one NV Control
function FindParentPage(aControl: TControl): TNVBasePage;

function MakeValidFileUrl(const ARootUrl: string; const AFileUrl: string; aHostApp:TNVHostApp = nil ): string;

function MakeValidFileUrl2(const ARootUrl: string; const AFileUrl: string; ADisableCache: boolean = True; aHostApp:TNVHostApp = nil ): string;

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

function TranslateChar(const Str: String; FromChar, ToChar: Char): String;
var
  i: Integer;
begin
  Result := Str;
  for i  := 1 to Length(Result) do
    if Result[i] = FromChar then
      Result[i] := ToChar;
end;

function htoin(Value: PWideChar; Len: Integer): Integer;
  function IsXDigit(Ch: WideChar): boolean;
  begin
    Result := ((Ch >= '0') and (Ch <= '9')) or ((Ch >= 'a') and (Ch <= 'f')) or
      ((Ch >= 'A') and (Ch <= 'F'));
  end;

  function XDigit(Ch: WideChar): Integer;
  begin
    case Ch of
      '0' .. '9': Result := Ord(Ch) - Ord('0');
    else Result          := (Ord(Ch) and 15) + 9;
    end;
  end;

var
  i: Integer;
begin
  Result := 0;
  i      := 0;
  while (i < Len) and (Value[i] = ' ') do
    i := i + 1;
  while (i < Len) and (IsXDigit(Value[i])) do
    begin
      Result := Result * 16 + XDigit(Value[i]);
      i      := i + 1;
    end;
end;

function UrlDecode(const Url: String; SrcCodePage: LongWord = CP_ACP;
  DetectUtf8: boolean = TRUE): String;
  var
  i, J, L: Integer;
  U8Str: AnsiString;
  Ch: AnsiChar;
begin
  L := Length(Url);
  SetLength(U8Str, L);
  i := 1;
  J := 0;
  while (i <= L) do
    begin
      Ch := AnsiChar(Url[i]);
      if Ch = '%' then
        begin
          Ch := AnsiChar(htoin(PChar(@Url[i + 1]), 2));
          inc(i, 2);
        end
      else if Ch = '+' then
        Ch := ' ';
      inc(J);
      U8Str[J] := Ch;
      inc(i);
    end;
  SetLength(U8Str, J);
  // if (SrcCodePage = CP_UTF8) or (DetectUtf8 and IsUtf8Valid(U8Str)) then { V7.24 }
  // {$IFDEF COMPILER12_UP}
  // Result := Utf8ToStringW(U8Str)
  // else
  // Result := AnsiToUnicode(U8Str, SrcCodePage);
  // {$ELSE}
  // Result := Utf8ToStringA(U8Str)
  // else
  Result := U8Str;
  // {$ENDIF}
end;

function AbsolutisePath(const Path: String): String;
var
  i, J, N: Integer;
begin
  if (Path = '') or (Path = '.') or (Path = '..') then
    begin
      Result := '';
      Exit;
    end;

  Result := Path;
  N      := 0;
  if (Length(Result) > 2) and (Copy(Result, Length(Result) - 1, 2) = {$IFDEF MSWINDOWS} '\.'
{$ELSE} '/.' {$ENDIF}) then
    Result := Copy(Result, 1, Length(Result) - 2);

  if Length(Result) > 1 then
    begin
      if (Result[1] = PathDelim) and (Result[2] = PathDelim) then
        begin
          N := 2;
          while (N < Length(Result)) and (Result[N + 1] <> PathDelim) do
            inc(N);
        end
      else if Result[2] = ':' then
        N := 2;
    end;

  if (Copy(Result, N + 1, 5) = PathDelim) or (Copy(Result, N + 1, 5) = {$IFDEF MSWINDOWS} '\.'
{$ELSE} '/.' {$ENDIF}) then
    begin
      Result := Copy(Result, 1, N + 1);
      Exit;
    end;

  while TRUE do
    begin
      i := Pos({$IFDEF MSWINDOWS} '\.\' {$ELSE} '/./' {$ENDIF}, Result);
      if i <= N then
        Break;
      Delete(Result, i, 2);
    end;
  while TRUE do
    begin
      i := Pos({$IFDEF MSWINDOWS} '\..' {$ELSE} '/..' {$ENDIF}, Result);
      if i <= N then
        Break;
      J := i - 1;
      while (J > N) and (Result[J] <> PathDelim) do
        Dec(J);
      if J <= N then
        Delete(Result, J + 2, i - J + 2)
      else
        Delete(Result, J, i - J + 3);
    end;
end;

function AdjustOSPathDelimiters(const Path: String): String;
begin
{$IFDEF MSWINDOWS}
  Result := TranslateChar(Path, '/', '\');
{$ELSE}
  Result := TranslateChar(Path, '\', '/');
{$ENDIF}
end;

function IsDirectory(const Path: String): boolean;
{$IFDEF MSWINDOWS}
var
  Attr: DWORD;
begin
  // Result:= Winapi.ShLwApi.PathIsDirectory(PChar(Path)); // Modified by Delcio 16/12/2016 00:22:20
  Attr   := GetFileAttributes(PChar(ExcludeTrailingPathdelimiter(Path)));
  Result := (Attr <> MaxDWord) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  Result := DirectoryExists(ExcludeTrailingPathdelimiter(Path));
end;
{$ENDIF}

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

function MakeValidFileUrl(const ARootUrl: string; const AFileUrl: string; aHostApp:TNVHostApp = nil ): string;
var
  _RootUrl: string;
  _FileUrl: string;
  _SessionTh: TNVSessionThread;
  _LibDir: string;
  _DocDir: string;
begin
   if aHostApp = nil then
    begin
      if TNVSessionThread.GetCurrent(_SessionTh) then
        aHostApp:= _SessionTh.HostApp;
    end;

  //get root url
  if ARootUrl <> '' then   {TODO -oDelcio -cImprove : Remove _RootUrl Parameter if not Needed}
    _RootUrl := ARootUrl
  else if aHostApp <> nil then
    _RootUrl := aHostApp.UrlBase;
  //get doc and lib dir
  if Assigned(aHostApp) then
  begin
    _LibDir := aHostApp.LibDir;
    _DocDir := aHostApp.DocDir;
  end
  else
  begin
    _LibDir := '';
    _DocDir := '';
  end;

  _FileUrl := ReplaceStr(AFileUrl, '/<nvlibpath>/', _LibDir); //replace components libdir tag to LibDir Path
  _FileUrl := ExtractRelativePath(_DocDir + '\', _FileUrl); //relative to DocDir Path
  _FileUrl := StringReplace(_FileUrl, '..\', '', [rfReplaceAll]); //if Doc Dir not subdir of Exe
  _FileUrl := StringReplace(_FileUrl, '\', '/', [rfReplaceAll]);  //change win '\' to web '/'
  Result := { _RootUrl + } '/' + _FileUrl;
end;

function MakeValidFileUrl2(const ARootUrl: string; const AFileUrl: string; ADisableCache: boolean = True; aHostApp:TNVHostApp = nil ): string;
var
  _DisableCache: boolean;
  _SessionTh: TNVSessionThread;
  _LibDir: string;
  //_DocDir: string;
  _RefreshCacheParam: string;
begin
  _DisableCache := ADisableCache;
  if aHostApp = nil then
    begin
      if TNVSessionThread.GetCurrent(_SessionTh) then
        aHostApp:= _SessionTh.HostApp;
    end;


  if aHostApp <> nil then
  begin
    _LibDir := aHostApp.LibDir;
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
    Result := ReplaceStr(AFileUrl, '/<nvlibpath>/', _LibDir);
  end
  else
    Result := MakeValidFileUrl(ARootUrl, AFileUrl, aHostApp);

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

function RemoveDelimiters(const Path:string; Delimiter:Char = '/'):string;
begin
  if Path[Low(Path)] = Delimiter then
    Result:= Copy(Path, 2, Path.Length -1)
  else
    Result:= Path;
  if Result[High(Result)] = Delimiter then
    SetLength(Result, Length(Result)-1);
end;

end.

