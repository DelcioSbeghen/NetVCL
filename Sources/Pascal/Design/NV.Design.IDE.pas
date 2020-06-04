unit NV.Design.IDE;

interface

uses
  ToolsAPI;

type

  (* * A class to implement the IDE notifier interfaces * *)
  TITHelperIDENotifier = Class(TNotifierObject, IUnknown, IOTANotifier, IOTAIDENotifier,
    IOTAIDENotifier50, IOTAIDENotifier80)
  Strict Private
    { Private declarations }
{$IFDEF D2010} Strict {$ENDIF} Protected
    // IOTANotifier
    // IOTAIDENotifier
    Procedure FileNotification(NotifyCode: TOTAFileNotification; Const FileName: String;
      Var Cancel: Boolean);
    Procedure BeforeCompile(Const Project: IOTAProject; Var Cancel: Boolean); Overload;
    Procedure AfterCompile(Succeeded: Boolean); Overload;
    // IOTAIDENotifier50
    Procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); Overload;
    Procedure BeforeCompile(Const Project: IOTAProject; IsCodeInsight: Boolean;
      Var Cancel: Boolean); Overload;
    // IOTAIDENotifier80
    Procedure AfterCompile(Const Project: IOTAProject; Succeeded: Boolean;
      IsCodeInsight: Boolean); Overload;
    // General Methods
    Procedure ProcessAfterCompile(Const Project: IOTAProject; Succeeded, IsCodeInsight: Boolean);
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

implementation

uses
  NV.Design.IOTAUtils, SysUtils, NV.Design.CopyFiles, NV.VCL.Forms, NV.Browser, VCL.Dialogs;

var
  FIDENotifier: Integer;

Procedure TITHelperIDENotifier.AfterCompile(Succeeded: Boolean);
Begin
  // Null implementation
End;

Procedure TITHelperIDENotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);
var
  _ActiveProject: IOTAProject;
Begin
{$IFNDEF D2005} // For D7 and below
  _ActiveProject := GetActiveProject;

  ProcessAfterCompile(_ActiveProject, Succeeded, IsCodeInsight);
{$ENDIF}
End;

{$IFDEF D2005}

Procedure TITHelperIDENotifier.AfterCompile(Const Project: IOTAProject;
  Succeeded, IsCodeInsight: Boolean);
Begin // For D2005 and above
  ProcessAfterCompile(Project, Succeeded, IsCodeInsight);
End;
{$ENDIF}

procedure TITHelperIDENotifier.AfterCompile(const Project: IOTAProject;
  Succeeded, IsCodeInsight: Boolean);
begin

end;

Procedure TITHelperIDENotifier.BeforeCompile(Const Project: IOTAProject; IsCodeInsight: Boolean;
  Var Cancel: Boolean);

Begin
  if ExtractFileName(Project.ProjectOptions.TargetName) = 'NetVCLDesign.bpl' then
    begin
      ShowMessage(Project.ProjectOptions.TargetName);
      Screen.CloseBrowser;
      UnloadDll;
    end;
End;

Procedure TITHelperIDENotifier.BeforeCompile(Const Project: IOTAProject; Var Cancel: Boolean);
Begin
  // Null implementation
End;

Constructor TITHelperIDENotifier.Create;
Begin
  Inherited Create;
End;

Destructor TITHelperIDENotifier.Destroy;
Begin

  Inherited Destroy;
End;

Procedure TITHelperIDENotifier.FileNotification(NotifyCode: TOTAFileNotification;
  Const FileName: String; Var Cancel: Boolean);
Begin
  // Null implementation
End;

procedure TITHelperIDENotifier.ProcessAfterCompile(const Project: IOTAProject;
  Succeeded, IsCodeInsight: Boolean);
Var
  _Source, _Dest: string;
Begin
  If Assigned(Project) And Not IsCodeInsight And Succeeded Then
    Begin
      _Dest := GetProjectOutputDir(Project);
      if _Dest = '' then
        Exit;

      _Source := GetNVSourcesPath(_Dest);
      _Dest   := _Dest + 'www' + PathDelim + 'netvcl' + PathDelim;

      ForceDirectories(_Dest + 'js');
      ForceDirectories(_Dest + 'css');
      StartCopyFolderNews(_Source + PathDelim + 'Js' + PathDelim, _Dest + 'js', '*.js');
      StartCopyFolderNews(_Source + PathDelim + 'Css' + PathDelim, _Dest + 'css', '*.css');
    End;

  // if (Project.ProjectOptions.TargetName = 'NetVCLDesign.bpl') then
  // begin
  // Screen.CloseBrowser;
  // UnloadDll;
  // end;

End;

initialization

FIDENotifier := (BorlandIDEServices As IOTAServices).AddNotifier(TITHelperIDENotifier.Create);

finalization

If FIDENotifier > -1 Then
  (BorlandIDEServices As IOTAServices).RemoveNotifier(FIDENotifier);

End.
