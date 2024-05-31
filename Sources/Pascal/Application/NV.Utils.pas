unit NV.Utils;

interface

uses
  Classes, Controls, DB, NV.VCL.Page, NV.Ajax, NV.Controls, Generics.Collections;

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
  DetectUtf8: boolean = True): String;
function AbsolutisePath(const Path: String): String;
function AdjustOSPathDelimiters(const Path: String): String;
function IsDirectory(const Path: String): boolean;
function RemoveDelimiters(const Path: string; Delimiter: Char = '/'): string;
function GetRandomString(NumChar: UInt32): string;
function ExtractBetweenTags(Const Line, TagI, TagF: string): string;
function TextToHTML(const AText: string; ReplaceEOLs: boolean = True;
  ReplaceSpaces: boolean = False): string;

function FindDesigner(Component: TComponent): INVDesignerHook; overload;
function FindDesigner(Component: TComponent; out aDesigner: INVDesignerHook): boolean; overload;

// Find a parent Page of one NV Control
function FindParentPage(aControl: TControl): TNVBasePage;
function FindAjax(aControl: TControl): TNvAjax;

function MakeValidFileUrl(const ARootUrl: string; const AFileUrl: string): string;

function MakeValidFileUrl2(const ARootUrl: string; const AFileUrl: string;
  ADisableCache: boolean = True): string;

function GetNVSourcesPath(ProjectPath: string): string;

function IIf(Expressao: Variant; ParteTRUE, ParteFALSE: Variant): Variant;

// get address of one procedure or methos
function GetProcAddr(Proc: Pointer): Pointer;
// hook procedure or method in one class to other procedure or method

procedure HookProc(Proc, Dest: Pointer; var BackupCode: TXRedirCode);
// unhook procedure or method with HookProc

procedure UnhookProc(Proc: Pointer; var BackupCode: TXRedirCode);
// replace an method in one class to other in the VMT class Table

procedure ReplaceVmtField(AClass: TClass; OldProc, NewProc: Pointer);

function GetDatasetIndex(Dataset: TDataset): TField;

/// / return the NVSessionApp for this session
// function NVSessionApp: TNVSessionApp;
//
/// / return the NVSessionThread for this session
// function NVSessionThread: TNVSessionThread;

// function NVServer: INVServer;

function RemoveWords(aOrig: string; aRemove: string; aSeparator: Char = ' ';
  aQuoteChar: Char = #0): string;
function AddWords(aOrig: string; aNews: string; aSeparator: char = ' ';
  aQuoteChar: Char = #0): string;

{$IFDEF FPC}
{$mode delphi}{$macro on}
const
  sArgumentOutOfRange_Index = 'Argument out of range index';
{ delphi compatibility aliases }
function DebugHook: integer stdcall; external 'kernel32.dll'  name 'IsDebuggerPresent';
//{$if fpc_fullversion < 30301}
function AtomicIncrement (var Target: integer): integer;overload; external name 'FPC_INTERLOCKEDINCREMENT';
function AtomicIncrement (var Target: int64): int64;overload; external name 'FPC_INTERLOCKEDINCREMENT';
function AtomicIncrement (var Target: QWord): QWord;overload; external name 'FPC_INTERLOCKEDINCREMENT';
//{$ifend}
{$ENDIF FPC}


implementation

uses
  Windows, SysUtils, StrUtils, Registry, Dialogs, NV.VCL.Forms;

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

  THackNVModuleContainer = class(TNVModuleContainer);

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
  DetectUtf8: boolean = True): String;
var
  i, J, L: Integer;
  U8Str  : AnsiString;
  Ch     : AnsiChar;
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
          Ch := AnsiChar(htoin({$IFDEF FPC}PWideChar{$ELSE}PChar{$ENDIF}(@Url[i + 1]), 2));
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

  while True do
    begin
      i := Pos({$IFDEF MSWINDOWS} '\.\' {$ELSE} '/./' {$ENDIF}, Result);
      if i <= N then
        Break;
      Delete(Result, i, 2);
    end;
  while True do
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
  CompTest     : TControl;
  ComponentTest: TComponent;
  // Check: IDWControl;
begin
  Result   := nil;
  CompTest := aControl;

  while Assigned(CompTest) and (not(CompTest.InheritsFrom(TNVBasePage))) do
    begin
      if (CompTest is TNVModuleContainer) //
        and (TNVModuleContainer(CompTest).Designer <> nil) then
        begin
          Result := TNVModuleContainer(CompTest).Designer.Page as TNVBasePage;
          Exit;
        end
      else if Assigned(CompTest.Parent) then
        CompTest := CompTest.Parent
      else
        CompTest := nil;
    end;

  if (CompTest <> nil) and (CompTest.InheritsFrom(TNVBasePage)) then
    Result := CompTest as TNVBasePage
  else
    begin
      ComponentTest := aControl;

      while Assigned(ComponentTest) and (not(ComponentTest.InheritsFrom(TNVBasePage))) do
        begin
          if ((ComponentTest is TNVModuleContainer) or
            (csSubComponent in ComponentTest.ComponentStyle)) //
            and                                               //
            (TControl(ComponentTest).Parent <> nil) then
            ComponentTest := TControl(ComponentTest).Parent
          else if Assigned(ComponentTest.Owner) then
            ComponentTest := ComponentTest.Owner
          else
            ComponentTest := nil;
        end;

      if (ComponentTest <> nil) and (ComponentTest.InheritsFrom(TNVBasePage)) then
        Result := ComponentTest as TNVBasePage;
    end;

end;

function FindAjax(aControl: TControl): TNvAjax;
var
  _Page: TNVBasePage;
begin
  Result := nil;
  _Page  := FindParentPage(aControl);
  if _Page <> nil then
    Result := _Page.Ajax;
  if Result = nil then
    Result := Screen.Ajax;
end;

function MakeValidFileUrl(const ARootUrl: string; const AFileUrl: string): string;
var
  _RootUrl: string;
  _FileUrl: string;
  _LibDir : string;
  _DocDir : string;
begin
  // get root url
  if ARootUrl <> '' then { TODO -oDelcio -cImprove : Remove _RootUrl Parameter if not Needed }
    _RootUrl := ARootUrl
  else
    _RootUrl := Application.UrlBase;

  // get doc and lib dir
  _LibDir := Application.RootPath + 'netvcl\';
  _DocDir := Application.RootPath;

  _FileUrl := ReplaceStr(AFileUrl, '/<nvlibpath>/', _LibDir);
  // replace components libdir tag to LibDir Path
  _FileUrl := ExtractRelativePath(_DocDir + '\', _FileUrl);       // relative to DocDir Path
  _FileUrl := StringReplace(_FileUrl, '..\', '', [rfReplaceAll]); // if Doc Dir not subdir of Exe
  _FileUrl := StringReplace(_FileUrl, '\', '/', [rfReplaceAll]);  // change win '\' to web '/'
  Result   := { _RootUrl + } './' + _FileUrl;
end;

function MakeValidFileUrl2(const ARootUrl: string; const AFileUrl: string;
  ADisableCache: boolean = True): string;
var
  _DisableCache: boolean;
  _LibDir      : string;
  // _DocDir: string;
  _RefreshCacheParam: string;
begin
  _DisableCache := ADisableCache;

  _LibDir := Application.RootPath + 'netvcl\';

  // _DocDir := _SessionTh.HostApp.DocDir;
  { TODO -oDelcio -cBrowserCache : MakeValidFileUrl2  RefreshCacheParam }
  // _RefreshCacheParam := _SessionTh.SessionApp.HostApp.RefreshCacheParam;
  _RefreshCacheParam := '';

  if AnsiStartsStr('//', AFileUrl) or AnsiContainsStr(AFileUrl, '://') then
    begin
      _DisableCache := False;
      Result        := ReplaceStr(AFileUrl, '/<nvlibpath>/', _LibDir);
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
    Result := (AAddr <> nil) and (PWin9xDebugThunk(AAddr).PUSH = $68) and
      (PWin9xDebugThunk(AAddr).JMP.Jump = $E9);
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
  N   : NativeUInt;
  Code: TXRedirCode;
begin
  Proc := GetProcAddr(Proc);
  Assert(Proc <> nil);
  if ReadProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), N) then
    begin
      Code.Jump   := $E9;
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

  TVmt = array [0 .. MaxInt div SizeOf(Pointer) - 1] of Pointer;
var
  i  : Integer;
  Vmt: PVmt;
  N  : NativeUInt;
  P  : Pointer;
begin
  OldProc := GetProcAddr(OldProc);
  NewProc := GetProcAddr(NewProc);

  i   := {$IFDEF FPC}-48{$ELSE}vmtSelfPtr{$ENDIF} div SizeOf(Pointer);
  Vmt := Pointer(AClass);
  while (i < 0) or (Vmt[i] <> nil) do
    begin
      P := Vmt[i];
      if (P <> OldProc) and (Integer(P) > $10000) and not IsBadReadPtr(P, 6) then
        P := GetProcAddr(P);
      if P = OldProc then
        begin
          WriteProcessMemory(GetCurrentProcess, @Vmt[i], @NewProc, SizeOf(NewProc), N);
          Exit;
        end;
      inc(i);
    end;
end;

// function NVSessionApp: TNVSessionApp;
// var
// _SessionTh: TNVSessionThread;
// begin
// if TNVSessionThread.GetCurrent(_SessionTh) then
// Result := _SessionTh.SessionApp
// else
// Result := nil;
// if (Result = nil) and Assigned(DesignSession) then
// Result := DesignSession;
//
// (* Result := nil;
// if (TThread.CurrentThread is TNVAppThread) then
// Result := TNVAppThread(TThread.Current).DWApp;
// // else
// // raise Exception.Create('Corrigir Isso'); *)
// end;
//
// function NVSessionThread: TNVSessionThread;
// begin
// TNVSessionThread.GetCurrent(Result);
//
// if (Result = nil) and Assigned(DesignSession) then
// Result := DesignSession.SessionThread;
//
// (* Result := nil;
// if (TThread.CurrentThread is TNVSessionThread) then
// Result := TNVSessionThread(TThread.Current);
// // else
// // raise Exception.Create('Corrigir Isso'); *)
// end;
//
// function NVServer: INVServer;
// begin
// try
// Result := NVSessionApp.HostApp.GetServer;
// except
// Result := nil;
// if DebugHook <> 0 then
// raise;
// end;
// end;

function RemoveDelimiters(const Path: string; Delimiter: Char = '/'): string;
begin
  if Path[Low(Path)] = Delimiter then
    Result := '.' + Path
  else
    Result := Path;

  if Not Result.IsEmpty and (Result[High(Result)] = Delimiter) then
    SetLength(Result, Length(Result) - 1);
end;

function GetRandomString(NumChar: UInt32): string;
const
  CharMap = 'qwertzuiopasdfghjklyxcvbnmQWERTZUIOPASDFGHJKLYXCVBNM1234567890'; { Do not Localize }
  MaxChar: UInt32 = Length(CharMap) - 1;
var
  i: Integer;
begin
  randomize;
  SetLength(Result, NumChar);
  for i := 1 to NumChar do
    begin
      // Add one because CharMap is 1-based
      Result[i] := CharMap[Random(MaxChar) + 1];
    end;
end;

function FindDesigner(Component: TComponent): INVDesignerHook;
var
  _Component: TComponent;
begin
  if (csDesigning in Component.ComponentState) then
    begin
      _Component := Component;
      while (_Component <> nil) do
        begin
          if (_Component is TNVModuleContainer) and
            (THackNVModuleContainer(_Component).FDesigner <> nil) then
            begin
              Result := THackNVModuleContainer(_Component).FDesigner;
              Exit;
            end
            // else if (_Component is TCustomForm) and (TCustomForm(_Component).Designer <> nil) then
            // begin
            // Result := TCustomForm(_Component).Designer;
            // Exit;
            // end;
            // else if csSubComponent in _Component.ComponentStyle then
            // _Component := TControl(_Component).Parent
          else
            _Component := _Component.Owner;
        end;
    end;

  Result := nil;
end;

function FindDesigner(Component: TComponent; out aDesigner: INVDesignerHook): boolean; overload;
begin
  aDesigner := FindDesigner(Component);
  Result    := aDesigner <> nil;
end;

function ExtractBetweenTags(Const Line, TagI, TagF: string): string;
var
  i, f: Integer;
begin
  i := Pos(TagI, Line);
  f := Pos(TagF, Copy(Line, i + Length(TagI), MaxInt));
  if (i > 0) and (f > 0) then
    Result := Copy(Line, i + Length(TagI), f - 1);
end;

function TextToHTML(const AText: string; ReplaceEOLs: boolean = True;
  ReplaceSpaces: boolean = False): string;
var
  POrig, PDest: PChar;
  // L_IsCallBack: Boolean;
begin
  // L_IsCallBack := DWApplication.IsCallback;
  SetLength(Result, Length(AText) * 10);
  POrig := PChar(AText);
  PDest := PChar(Result);
  while POrig^ <> #0 do
    begin
      case POrig^ of
        '&':
          begin
            FormatBuf(PDest^, 10 { 5*2 } , '&amp;', 10 { 5*2 } , []);
            inc(PDest, 4);
          end;
        '<', '>':
          begin
            if POrig^ = '<' then
              FormatBuf(PDest^, 8 { 4*2 } , '&lt;', 8 { 4*2 } , [])
            else
              FormatBuf(PDest^, 8 { 4*2 } , '&gt;', 8 { 4*2 } , []);
            inc(PDest, 3);
          end;
        '"':
          begin
            FormatBuf(PDest^, 12 { 6*2 } , '&quot;', 12 { 6*2 } , []);
            inc(PDest, 5);
          end;
        '''':
          begin
            FormatBuf(PDest^, 10 { 5*2 } , '&#39;', 10 { 5*2 } , []);
            inc(PDest, 4);
          end;
        '\':
          begin
            FormatBuf(PDest^, 10 { 5*2 } , '&#92;', 10 { 5*2 } , []);
            inc(PDest, 4);
          end;
        #10:
          if ReplaceEOLs then
            begin
              FormatBuf(PDest^, 8 { 4*2 } , '<br>', 8 { 4*2 } , []);
              inc(PDest, 3);
            end
          else
            PDest^ := POrig^;
        #13:
          if ReplaceEOLs then
            begin
              Dec(PDest);
            end
          else
            PDest^ := POrig^;
        #32:
          if ReplaceSpaces then
            begin
              // if L_IsCallBack then
              // begin
              // FormatBuf(PDest^, 20, '&amp;nbsp;', 20, []);
              // Inc(PDest, 9);
              // end
              // else
              // begin
              FormatBuf(PDest^, 12, '&nbsp;', 12, []);
              inc(PDest, 5);
              // end;
            end
          else
            PDest^ := POrig^;
      else PDest^  := POrig^
      end;
      inc(PDest);
      inc(POrig);
    end;
  SetLength(Result, PDest - PChar(Result));
end;

function GetNVSourcesPath(ProjectPath: string): string;
begin
  Result := '';
  with TRegistry.Create do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('Software\SRP Sistemas\NetVCL', True) then
        begin
          if ValueExists('SourcePath') then
            Result := ReadString('LibSourcePath')
          else
            WriteString('SourcePath', '');

          if (Result <> '') and not FileExists(Result + 'NetVCLSourcesRoot.txt') then
            Result := '';

          while Result = '' do
            begin
              ShowMessage('NetVCL source path specified is not valid.');
              if PromptForFileName(Result, 'NetVCLSourcesRoot.txt',
                'Select NetVcl Script Library Path') then
                begin
                  Result := ExtractFilePath(Result);

                  WriteString('LibSourcePath', Result);
                end;
            end;

        end;
    end;
end;

function GetDatasetIndex(Dataset: TDataset): TField;
var
  I    : Integer;
  Field: TField;
begin
  Result := nil;

  for I := 0 to DataSet.FieldCount - 1 do
    begin
      Field := DataSet.Fields[I];
      if (pfInKey in Field.ProviderFlags) then
        begin
          Result := Field;
          Exit;
        end;
    end;
end;

function RemoveWords(aOrig: string; aRemove: string; aSeparator: Char = ' ';
  aQuoteChar: Char = #0): string;
var
  _Olds, _Remove: TStrings;
  I             : Integer;
begin
  _Olds   := TStringList.Create;
  _Remove := TStringList.Create;
  try
    _Olds.Delimiter         := aSeparator;
    _Olds.StrictDelimiter   := True;
    _Olds.QuoteChar         := aQuoteChar;
    _Remove.Delimiter       := aSeparator;
    _Remove.StrictDelimiter := True;
    _Remove.QuoteChar       := aQuoteChar;

    _Olds.DelimitedText   := Trim(aOrig);
    _Remove.DelimitedText := Trim(aRemove);

    for I := _Remove.Count - 1 downto 0 do
      if _Olds.IndexOf(_Remove[I]) > -1 then
        _Olds.Delete(_Olds.IndexOf(_Remove[I]));

    Result := _Olds.DelimitedText;
  finally
    _Olds.Free;
    _Remove.Free;
  end;
end;

function AddWords(aOrig: string; aNews: string; aSeparator: char = ' ';
  aQuoteChar: Char = #0): string;
var
  _Olds, _News: TStrings;
  I           : Integer;
begin
  _Olds := TStringList.Create;
  _News := TStringList.Create;
  try
    _Olds.Delimiter       := aSeparator;
    _Olds.StrictDelimiter := True;
    _Olds.QuoteChar       := aQuoteChar;
    _News.Delimiter       := aSeparator;
    _News.StrictDelimiter := True;
    _News.QuoteChar       := aQuoteChar;

    _Olds.DelimitedText := Trim(aOrig);
    _News.DelimitedText := Trim(aNews);

    for I := 0 to _News.Count - 1 do
      if not _Olds.IndexOf(_News[I]) > -1 then
        _Olds.Add(_News[I]);

    Result := _Olds.DelimitedText;
  finally
    _Olds.Free;
    _News.Free;
  end;
end;

end.
