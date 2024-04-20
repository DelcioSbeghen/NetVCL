unit NV.Design.AppWizard;

interface

uses
  Classes, System.SysUtils, Vcl.Dialogs, ToolsAPI, ExpertsModules;

type
  TNVAppWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier, IOTARepositoryWizard,
    IOTAProjectWizard)
  public
    // IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
  end;

  TNVAppCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator, IOTAProjectCreator50,
    IOTAProjectCreator80, IOTAProjectCreator160, IOTAProjectCreator190)
  private
    FOwner: IOTAProjectGroup;
  public
    constructor Create(const AOwner: IOTAProjectGroup);
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    // IOTAProjectCreator80
    function GetProjectPersonality: string;
    // IOTAProjectCreator160
    function GetFrameworkType: string;
    function GetPlatforms: TArray<string>;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);
    // IOTAProjectCreator190
    function GetSupportedPlatforms: TArray<string>;

  end;

  TNVAppSource = class(TInterfacedObject, IOTAFile)
  private
    FProjectName: string;
  public
    constructor Create(const ProjectName: string);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

implementation

uses
  NV.Design.AppModuleWizard, NV.Design.PageWizard, NV.Design.UserSessionWizard, PlatformAPI;

{ TNVAppWizard }

procedure TNVAppWizard.AfterSave;
begin

end;

procedure TNVAppWizard.BeforeSave;
begin

end;

procedure TNVAppWizard.Destroyed;
begin

end;

function GetActiveProjectGroup(const ModuleServices: IOTAModuleServices): IOTAProjectGroup;
var
  I: Integer;
begin
  Result := nil;
  for I  := 0 to ModuleServices.ModuleCount - 1 do
    if Supports(ModuleServices.Modules[I], IOTAProjectGroup, Result) then
      Break;
end;

procedure TNVAppWizard.Execute;
var
  // WizardForm: TzWizardForm;
  ModuleServices: IOTAModuleServices;
  ProjectGroup  : IOTAProjectGroup;
begin
  // ShowMessage('TNVAppWizard.Execute');
  // WizardForm := TzWizardForm.Create(Application);
  try
    // if WizardForm.ShowModal <> mrOk then
    // Exit;
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    ProjectGroup   := GetActiveProjectGroup(ModuleServices);
    // if ProjectGroup <> nil then
    // ShowMessage(ProjectGroup.FileName);
    ModuleServices.CreateModule(TNVAppCreator.Create(ProjectGroup));

  finally
    // WizardForm.Free;
  end;
end;

function TNVAppWizard.GetAuthor: string;
begin
  Result := 'Delcio Sbeghen SRP Sistemas';
end;

function TNVAppWizard.GetComment: string;
begin
  Result := 'Creates a new NetVCL Application';
end;

function TNVAppWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TNVAppWizard.GetIDString: string;
begin
  Result := 'NetVCL.NVApp';
end;

function TNVAppWizard.GetName: string;
begin
  Result := 'NetVCL Application';
end;

function TNVAppWizard.GetPage: string;
begin
  Result := 'NetVCL';
end;

function TNVAppWizard.GetState: TWizardState;
begin
  Result := [];
  // ShowMessage('TNVAppWizard.GetState');
end;

procedure TNVAppWizard.Modified;
begin
  // ShowMessage('TNVAppWizard.Modified');
end;

{ TNVAppCreator }

constructor TNVAppCreator.Create(const AOwner: IOTAProjectGroup);
begin
  inherited Create;
  FOwner := AOwner;
  // ShowMessage('TNVAppCreator.Create');
end;

function TNVAppCreator.GetCreatorType: string;
begin
  Result := sPackage;
  // ShowMessage('TNVAppCreator.GetCreatorType');
end;

function TNVAppCreator.GetExisting: Boolean;
begin
  Result := False;
  // ShowMessage('TNVAppCreator.GetExisting');
end;

function TNVAppCreator.GetFileName: string;
begin
  Result := GetCurrentDir + '\NetVCLApp.dpr';
  // ShowMessage('TNVAppCreator.GetFileName');
end;

function TNVAppCreator.GetFileSystem: string;
begin
  Result := '';
  // ShowMessage('TNVAppCreator.GetFileSystem');
end;

function TNVAppCreator.GetFrameworkType: string;
begin
  Result := sFrameworkTypeVCL;
end;

function TNVAppCreator.GetOptionFileName: string;
begin
  Result := '';
  // ShowMessage('TNVAppCreator.GetOptionFileName');
end;

function TNVAppCreator.GetOwner: IOTAModule;
begin
  Result := FOwner;
  // ShowMessage('TNVAppCreator.GetOwner');
end;

function TNVAppCreator.GetPlatforms: TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

function TNVAppCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

function TNVAppCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
  // ShowMessage('TNVAppCreator.GetProjectPersonality');
end;

function TNVAppCreator.GetShowSource: Boolean;
begin
  Result := False;
  // ShowMessage('TNVAppCreator.GetShowSource');
end;

function TNVAppCreator.GetSupportedPlatforms: TArray<string>;
begin
  Result := ['Win32', 'Win64'];
end;

function TNVAppCreator.GetUnnamed: Boolean;
begin
  Result := True;
  // ShowMessage('TNVAppCreator.GetUnnamed');
end;

procedure TNVAppCreator.NewDefaultModule;
begin
  // ShowMessage('TNVAppCreator.NewDefaultModule');
end;

procedure TNVAppCreator.NewDefaultProjectModule(const Project: IOTAProject);
var
  ModuleServices: IOTAModuleServices;
begin
  // ShowMessage('TNVAppCreator.NewDefaultProjectModule');
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  // Create the Server Main Form
  // ModuleServices.CreateModule(TNVAppModuleCreator.Create(Project));
  // Created the Web main Form
  ModuleServices.CreateModule(TNVPageModuleCreator.Create(Project, True));
  // Cteate the UserSession DataModule
  // ModuleServices.CreateModule(TNVUserSessionModuleCreator.Create(Project));
end;

function TNVAppCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
  // ShowMessage('TNVAppCreator.NewOptionSource');
end;

procedure TNVAppCreator.NewProjectResource(const Project: IOTAProject);
begin
  // ShowMessage('TNVAppCreator.NewProjectResource');
end;

function TNVAppCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  // ShowMessage('TNVAppCreator.NewProjectSource');
  Result := TNVAppSource.Create(ProjectName);
  // ShowMessage(Result.Source);
end;

procedure TNVAppCreator.SetInitialOptions(const NewProject: IOTAProject);
begin

end;

{ TNVAppSource }

constructor TNVAppSource.Create(const ProjectName: string);
begin
  inherited Create;
  FProjectName := ProjectName;
  // ShowMessage('TNVAppSource.Create');
end;

function TNVAppSource.GetAge: TDateTime;
begin
  // ShowMessage('TNVAppSource.GetAge');
  Result := -1;
end;

function TNVAppSource.GetSource: string;
begin
  Result := 'program DevTest;' + crlf2 +                          //
    '' + crlf +                                                   //
    'uses' + crlf +                                               //
    'NV.VCl.Forms,' + crlf +                                      //
    'NV.VCl.Charts,' + crlf +                                     //
    'uMainForm in ''uMainForm.pas'' {NVForm1: TNVForm};' + crlf + //
    '' + crlf +                                                   //
    '{$R *.res}' + crlf +                                         //
    '' + crlf +                                                   //
    '' + crlf +                                                   //
    'begin' + crlf +                                              //
    'Application.Initialize;' + crlf +                            //
    'Application.CreateForm(TMainForm, MainForm);' + crlf +         //
    ' Application.Run;' + crlf +                                  //
    '' + crlf +                                                   //
    '' + crlf +                                                   //
    'end.' + crlf                                                 //



  // ShowMessage('TNVAppSource.GetSource');
end;

end.
