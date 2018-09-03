unit NV.Design.AppWizard;

interface

uses
  Classes, System.SysUtils, Vcl.Dialogs, ToolsAPI, ExpertsModules;

type
  TNVAppWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier,
    IOTARepositoryWizard, IOTAProjectWizard)
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

  TNVAppCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator,
    IOTAProjectCreator50, IOTAProjectCreator80)
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
  NV.Design.AppModuleWizard, NV.Design.PageWizard, NV.Design.UserSessionWizard;

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

function GetActiveProjectGroup(const ModuleServices: IOTAModuleServices)
  : IOTAProjectGroup;
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
  ModuleServices.CreateModule(TNVAppModuleCreator.Create(Project));
  // Created the Web main Form
  ModuleServices.CreateModule(TNVPageModuleCreator.Create(Project, True));
  // Cteate the UserSession DataModule
  ModuleServices.CreateModule(TNVUserSessionModuleCreator.Create(Project));
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
  Result :=                                   //
    'package ' + FProjectName + ';' + CrLf2 + //
    '{$R *.res}' + CrLf +                     //
    '{$IFDEF IMPLICITBUILDING This IFDEF should not be used by users}' + CrLf +
  //
    '{$ALIGN 8}' + CrLf +           //
    '{$ASSERTIONS ON}' + CrLf +     //
    '{$BOOLEVAL OFF}' + CrLf +      //
    '{$DEBUGINFO OFF}' + CrLf +     //
    '{$EXTENDEDSYNTAX ON}' + CrLf + //
    '{$IMPORTEDDATA ON}' + CrLf +   //
    '{$IOCHECKS ON}' + CrLf +       //
    '{$LOCALSYMBOLS ON}' + CrLf +   //
    '{$LONGSTRINGS ON}' + CrLf +    //
    '{$OPENSTRINGS ON}' + CrLf +    //
    '{$OPTIMIZATION OFF}' + CrLf +  //
    '{$OVERFLOWCHECKS OFF}' + CrLf +
  ///
    '{$RANGECHECKS OFF}' + CrLf +        //
    '{$REFERENCEINFO ON}' + CrLf +       //
    '{$SAFEDIVIDE OFF}' + CrLf +         //
    '{$STACKFRAMES ON}' + CrLf +         //
    '{$TYPEDADDRESS OFF}' + CrLf +       //
    '{$VARSTRINGCHECKS ON}' + CrLf +     //
    '{$WRITEABLECONST OFF}' + CrLf +     //
    '{$MINENUMSIZE 1}' + CrLf +          //
    '{$IMAGEBASE $400000}' + CrLf +      //
    '{$DEFINE DEBUG}' + CrLf +           //
    '{$ENDIF IMPLICITBUILDING}' + CrLf + //
    '{$IMPLICITBUILD ON}' + CrLf2 +      //
  //
    'requires' + CrLf +         //
    '  rtl,' + CrLf +           //
    '  vcl,' + CrLf +           //
    '  dbrtl,' + CrLf +         //
    '  IndySystem,' + CrLf +    //
    '  IndyProtocols,' + CrLf + //
    '  IndyCore;' + CrLf2 +     //
  //
    'contains' + CrLf +                   //
    '  uApp in ''uApp.pas'',' + CrLf +           //
    '  uMainPage in ''uMainPage.pas'',' + CrLf +           //
    '  uNVUserSession in ''uNVUserSession.pas'';' + CrLf2 + //
  //
    'end.' + CrLf;
  // ShowMessage('TNVAppSource.GetSource');
end;

end.
