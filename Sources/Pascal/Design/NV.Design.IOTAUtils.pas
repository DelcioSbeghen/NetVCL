unit NV.Design.IOTAUtils;

{$IFDEF VER130}
{$DEFINE DELPHI_5_UP}
{$ENDIF}
{$IFDEF VER140}
{$DEFINE DELPHI_5_UP}
{$DEFINE DELPHI_6_UP}
{$ENDIF}
{$IFDEF VER150}
{$DEFINE DELPHI_5_UP}
{$DEFINE DELPHI_6_UP}
{$DEFINE DELPHI_7_UP}
{$ENDIF}

interface

uses
  ToolsAPI, NV.Controls;

function GetDelphiRootDirectory: string;
function GetProjectOutputDir(const Project: IOTAProject): string;
function GetProjectSourceEditor(const Project: IOTAProject): IOTASourceEditor;
function GetTargetExt(const Project: IOTAProject): string;
function GetTargetFileName(const Project: IOTAProject): string;
// other functions
function GetNVSourcesPath: string;
function GetActiveFormEditor: IOTAFormEditor;
function IsNvFormModule: Boolean;

implementation

uses
  Registry, Windows, Classes, SysUtils, IniFiles, Dialogs, NV.Design.Register;

{$IFNDEF DELPHI_6_UP}
function ExcludeTrailingPathDelimiter(const S: string): string; forward;
function IncludeTrailingPathDelimiter(const S: string): string; forward;
{$ENDIF}
function GetTargetExtOverride(const Project: IOTAProject): string; overload; forward;

// get Delphi root directory

function GetDelphiRootDirectory: string;
{$IFNDEF DELPHI_7_UP}
var
  Registry: TRegistry;
{$ENDIF}
begin
{$IFDEF DELPHI_7_UP}
  Result := (BorlandIDEServices as IOTAServices).GetRootDirectory;
{$ELSE}
  Registry := TRegistry.Create(KEY_READ);
  try
    if Registry.OpenKeyReadOnly((BorlandIDEServices as IOTAServices).GetBaseRegistryKey) then
      Result := Registry.ReadString('RootDir');
  finally
    Registry.Free;
  end;
{$ENDIF}
end;

// get Delphi environment variables (name-value pairs) from the registry

procedure GetEnvVars(Strings: TStrings);
var
  Registry: TRegistry;
  I       : Integer;
begin
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKeyReadOnly((BorlandIDEServices as IOTAServices).GetBaseRegistryKey +
      '\Environment Variables') then
      begin
        Registry.GetValueNames(Strings);
        for I        := 0 to Strings.Count - 1 do
          Strings[I] := Strings[I] + '=' + Registry.ReadString(Strings[I]);
      end;
  finally
    Registry.Free;
  end;
end;

// get output directory of a project

function GetProjectOutputDir(const Project: IOTAProject): string;
begin
  if Project.ProjectOptions.Values['GenPackage'] then // package project
    begin
      // use project options if specified
      Result := Project.ProjectOptions.Values['PkgDllDir'];
      // otherwise use environment options
      if Result = '' then
        Result := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions.Values
          ['PackageDPLOutput'];
    end
  else // non-package project, use project options
    Result := Project.ProjectOptions.Values['OutputDir'];

  // default is the project's path
  if Result = '' then
    Result := ExtractFilePath(Project.FileName);

  Result := IncludeTrailingPathDelimiter(Result);

  if ExtractFileDrive(Result) = '' then
    Result := ExtractFilePath(Project.FileName) + Result;
end;

// get project source editor

function GetProjectSourceEditor(const Project: IOTAProject): IOTASourceEditor;
var
  I: Integer;
begin
  Result := nil;
  for I  := 0 to Project.GetModuleFileCount - 1 do
    if Supports(Project.GetModuleFileEditor(I), IOTASourceEditor, Result) then
      Break;
end;

// get system environment variables

procedure GetSysVars(Strings: TStrings);
var
  P: PChar;
begin
  P := GetEnvironmentStrings;
  try
    repeat
      Strings.Add(P);
      P := StrEnd(P);
      Inc(P);
    until P^ = #0;
  finally
    FreeEnvironmentStrings(P);
  end;
end;



// get target extension

function GetTargetExt(const Project: IOTAProject): string;
begin
  // use {$E ...} override if specified
  Result := GetTargetExtOverride(Project);
  // otherwise use defaults
  if Result = '' then
    begin
      if Project.ProjectOptions.Values['GenPackage'] then // package
        Result := '.bpl'
      else if Project.ProjectOptions.Values['GenDll'] then // DLL
        Result := '.dll'
      else // application
        Result := '.exe';
    end;
end;

// read {$E ...} directive from project source

function GetTargetExtOverride(const ProjectSource: string): string; overload;
var
  P: PChar;
  procedure SkipComment(var P: PChar);
  begin
    case P^ of
      '{':
        begin
          while not(P^ in [#0, '}']) do
            Inc(P);
          if P^ = '}' then
            Inc(P);
        end;
      '/':
        if (P + 1)^ = '/' then
          begin
            while not(P^ in [#0, #10, #13]) do
              Inc(P);
            while (P^ in [#10, #13]) do
              Inc(P);
          end;
      '(':
        if (P + 1)^ = '*' then
          repeat
            Inc(P);
            case P^ of
              #0: Break;
              '*':
                if (P + 1)^ = ')' then
                  begin
                    Inc(P, 2);
                    Break;
                  end;
            end;
          until False;
      end;
    end;

    procedure SkipStringLiteral(var P: PChar);
    begin
      if P^ <> '''' then
        Exit;
      Inc(P);
      repeat
        case P^ of
          #0: Break;
          '''':
            begin
              Inc(P);
              if P^ = '''' then
                Inc(P)
              else
                Break;
            end;
        else Inc(P);
        end;
      until False;
    end;

    procedure SkipNonDirectives(var P: PChar);
    begin
      repeat
        case P^ of
          #0: Break;
          '''': SkipStringLiteral(P);
          '/':
            case (P + 1)^ of
              '/': SkipComment(P);
            else Inc(P);
            end;
          '(':
            case (P + 1)^ of
              '*': SkipComment(P);
            else Inc(P);
            end;
          '{':
            begin
              case (P + 1)^ of
                '$': Break;
              else SkipComment(P);
              end;
            end;
        else Inc(P);
        end;
      until False;
    end;
    begin
      P := PChar(ProjectSource);
      repeat
        SkipNonDirectives(P);
        case P^ of
          #0: Break;
          '{':
            if StrLIComp(P, '{$E ', 4) = 0 then
              begin
                Inc(P, 4);
                Result := '.';
                while P^ = ' ' do
                  Inc(P);
                while not(P^ in [#0, '}']) do
                  begin
                    if P^ <> ' ' then
                      Result := Result + P^;
                    Inc(P);
                  end;
                Break;
              end
            else
              SkipComment(P);
        end;
      until False;
    end;

    // read {$E ...} directive from project source module

    function GetTargetExtOverride(const Project: IOTAProject): string; overload;
    const
      BufferSize = 1024;
    var
      SourceEditor        : IOTASourceEditor;
      EditReader          : IOTAEditReader;
      Buffer              : array [0 .. BufferSize - 1] of AnsiChar;
      Stream              : TStringStream;
      ReaderPos, CharsRead: Integer;
    begin
      SourceEditor := GetProjectSourceEditor(Project);
      if Assigned(SourceEditor) then
        begin
          EditReader := SourceEditor.CreateReader;
          Stream     := TStringStream.Create('');
          try
            ReaderPos := 0;
            repeat
              CharsRead := EditReader.GetText(ReaderPos, Buffer, BufferSize - 1);
              Inc(ReaderPos, CharsRead);
              Buffer[CharsRead] := #0;
              Stream.WriteString(Buffer);
            until CharsRead < BufferSize - 1;
            Result := GetTargetExtOverride(Stream.DataString);
          finally
            Stream.Free;
          end;
        end;
    end;

    // get project target file name (with path), resolve $(...) macros if used

    function GetTargetFileName(const Project: IOTAProject): string;
    var
      PStart, PEnd                   : PChar;
      EnvVar, Value, FileName, Ext, S: string;
      EnvVars, SysVars               : TStringList;
      I                              : Integer;
    begin
      EnvVars := nil;
      SysVars := nil;
      try
        Result := GetProjectOutputDir(Project);
        PStart := StrPos(PChar(Result), '$(');
        while PStart <> nil do
          begin
            Value := '';

            PEnd := StrPos(PStart, ')');
            if PEnd = nil then
              Break;
            SetString(EnvVar, PStart + 2, PEnd - PStart - 2);
            if CompareText(EnvVar, 'DELPHI') = 0 then // $(DELPHI) macro is hardcoded
              Value := GetDelphiRootDirectory
            else
              begin
                // try Delphi environment variables from the registry
                if not Assigned(EnvVars) then
                  begin
                    EnvVars := TStringList.Create;
                    GetEnvVars(EnvVars);
                  end;

                for I := 0 to EnvVars.Count - 1 do
                  if CompareText(EnvVar, EnvVars.Names[I]) = 0 then
                    begin
{$IFDEF DELPHI_7_UP}
                      Value := ExcludeTrailingPathDelimiter(EnvVars.ValueFromIndex[I]);
{$ELSE}
                      Value := ExcludeTrailingPathDelimiter(EnvVars.Values[EnvVars.Names[I]]);
{$ENDIF}
                      Break;
                    end;
                if Value = '' then
                  begin
                    // try system environment variables
                    if not Assigned(SysVars) then
                      begin
                        SysVars := TStringList.Create;
                        GetSysVars(SysVars);
                      end;
                    for I := 0 to SysVars.Count - 1 do
                      if CompareText(EnvVar, SysVars.Names[I]) = 0 then
                        begin
{$IFDEF DELPHI_7_UP}
                          Value := ExcludeTrailingPathDelimiter(SysVars.ValueFromIndex[I]);
{$ELSE}
                          Value := ExcludeTrailingPathDelimiter(SysVars.Values[SysVars.Names[I]]);
{$ENDIF}
                          Break;
                        end;
                  end;
              end;

            I := PStart - PChar(Result) + 1;
            Delete(Result, I, Length(EnvVar) + 3);
            Insert(Value, Result, I);

            PStart := StrPos(PChar(Result), '$(');
          end;
        Ext      := GetTargetExt(Project);
        FileName := ChangeFileExt(ExtractFileName(Project.FileName), '');
        // include prefix/suffix/version for DLL and package projects
        if Project.ProjectOptions.Values['GenDll'] then
          begin
            S := Project.ProjectOptions.Values['SOPrefix'];
            if Project.ProjectOptions.Values['SOPrefixDefined'] then
              FileName := S + FileName;
            S          := Project.ProjectOptions.Values['SOSuffix'];
            if (S <> '') then
              FileName := FileName + S;
            FileName   := FileName + Ext;
            S          := Project.ProjectOptions.Values['SOVersion'];
            if S <> '' then
              FileName := FileName + '.' + S;
          end
        else
          FileName := FileName + Ext;
        Result     := Result + FileName;
      finally
        EnvVars.Free;
        SysVars.Free;
      end;
    end;

{$IFNDEF DELPHI_6_UP}

    function ExcludeTrailingPathDelimiter(const S: string): string;
    begin
      Result := ExcludeTrailingBackslash(S);
    end;

    function IncludeTrailingPathDelimiter(const S: string): string;
    begin
      Result := IncludeTrailingBackslash(S);
    end;
{$ENDIF}

    function GetNVSourcesPath: string;
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

function GetActiveFormEditor: IOTAFormEditor;
var
  Module: IOTAModule;
  Editor: IOTAEditor;
  i: Integer;
begin
Result := nil;
  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
if Module <> nil then
  begin
    for i := 0 to Module.GetModuleFileCount - 1 do
    begin
      Editor := Module.GetModuleFileEditor(i);
      if Supports(Editor, IOTAFormEditor, Result) then
        Break;
    end;
  end;
end;


    function IsNvFormModule: Boolean;
    var
      Editor: IOTAFormEditor;
    begin
      Result   := False;
      Editor := GetActiveFormEditor;

      if (Editor <> nil) and  ((Editor as INTAFormEditor).FormDesigner.Root is TNVWinControl) then
        begin
          Result := True;
        end;

    end;

end.
