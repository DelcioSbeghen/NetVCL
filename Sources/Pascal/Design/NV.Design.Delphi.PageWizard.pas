unit NV.Design.Delphi.PageWizard;

interface

uses Classes, Winapi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, TypInfo;

type

  TNVPageWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier, IOTARepositoryWizard,
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

  TNVPageModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FOwner: IOTAModule;
    FHaveNames: Boolean;
    FImplFileName: string;
    FFormName: string;
    FIsMainForm: Boolean;
    procedure GetNewModuleAndClassName(out AFormName, AImplFileName: string);
    function GetModuleAndClassNamePrefix: String;
  public
    constructor Create(AOwner: IOTAModule; IsMainForm: Boolean); overload;
    constructor Create(AOwner: IOTAModule); overload;
    constructor Create; overload;
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TNVPageModuleSource = class(TInterfacedObject, IOTAFile)
  private
    FModuleIdent: string;
    FFormIdent: string;
    FAncestorIdent: string;
    FDefaultForm: Boolean;
  public
    constructor Create(const ModuleIdent, FormIdent, AncestorIdent: string; AsDefaultForm: Boolean);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

const
  CrLf2 = #13#10#13#10;
  CrLf  = #13#10;

implementation

uses
  SysUtils, System.Win.ComObj, NV.VCL.Forms{, NV.VCL.Labels};

procedure DebugMsg(const Msg: String);
begin
  // ShowMessage(Msg);
end;

procedure TNVPageWizard.AfterSave;
begin
  DebugMsg('AfterSave');
end;

procedure TNVPageWizard.BeforeSave;
begin
  DebugMsg('BeforeSave');
end;

procedure TNVPageWizard.Destroyed;
begin
  DebugMsg('TNVFormModuleCreator.Destroyed');
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

procedure TNVPageWizard.Execute;
var
  ModuleServices: IOTAModuleServices;
  ProjectGroup: IOTAProjectGroup;
begin

  DebugMsg('TNVFormWizard.Execute');

  ModuleServices := BorlandIDEServices as IOTAModuleServices;

  ModuleServices.CreateModule(TNVPageModuleCreator.Create);

end;

constructor TNVPageModuleCreator.Create(AOwner: IOTAModule);
begin
  Create(AOwner, False);
end;

constructor TNVPageModuleCreator.Create;
begin
  Create(nil, False);
end;

constructor TNVPageModuleCreator.Create(AOwner: IOTAModule; IsMainForm: Boolean);
begin
  inherited Create;
  FOwner      := AOwner;
  FIsMainForm := IsMainForm;
end;

procedure TNVPageModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  Form: IOTAComponent;
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  NativeFormEditor: INTAFormEditor;
 // aHelloWorld: TNVLabel;
begin
  DebugMsg('FormCreated');
  {if FIsMainForm then
    begin
      Form := FormEditor.GetRootComponent;
      // Add OnShow Event
      if Supports(FormEditor, INTAFormEditor, NativeFormEditor) then
        begin
          if NativeFormEditor.FormDesigner <> nil then
            begin
              aHelloWorld := TNVLabel(NativeFormEditor.FormDesigner.CreateComponent(TNVLabel,
                NativeFormEditor.FormDesigner.GetRoot, 0, 0, 0, 0));
              aHelloWorld.RawText := True;
              aHelloWorld.Caption := 'Hello World<br>DelphiWeb';
            end;
        end;
    end;  }
  (*
    var
    aHelloWorld   : IOTAComponent;
    begin
    DebugMsg('FormCreated');
    if FIsMainForm then
    begin
    with FormEditor.CreateComponent(FormEditor.GetRootComponent, 'TDWLabel', 0, 0, 200, 50) do
    begin
    SetPropByName('Caption', 'Hello World<br>DelphiWeb');
    end;
    end; *)
end;

function TNVPageModuleCreator.GetAncestorName: string;
begin
  Result := 'NVForm';
  DebugMsg('GetAncestorName: ' + Result);
end;

function TNVPageWizard.GetAuthor: string;
begin
  Result := 'Delcio Sbeghen SRP Sistemas';
  DebugMsg('GetAuthor: ' + Result);
end;

function TNVPageWizard.GetComment: string;
begin
  Result := 'Creates a new NVForm.';
  DebugMsg('GetComment: ' + Result);
end;

function TNVPageModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
  DebugMsg('GetCreatorType');
end;

function TNVPageModuleCreator.GetExisting: Boolean;
begin
  Result := False;
  DebugMsg('GetExisting');
end;

function TNVPageModuleCreator.GetFileSystem: string;
begin
  Result := '';
  DebugMsg('GetFileSystem: ' + Result);
end;

function TNVPageModuleCreator.GetFormName: string;
var
  LImplFileName: string;
begin
  if FIsMainForm then
    begin
      Result := 'MainForm';
    end
  else
    GetNewModuleAndClassName(Result, LImplFileName);
end;

procedure TNVPageModuleCreator.GetNewModuleAndClassName(out AFormName: string;
  out AImplFileName: string);
var
  LUnitIdent: string;
  LClassName: string;
  LFileName: string;
begin
  if not FHaveNames then
    begin
      FHaveNames := True;
      LClassName := GetModuleAndClassNamePrefix;
      if LClassName <> '' then
        begin
          (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName(LClassName + 'Unit',
            // Do not localize
            LUnitIdent, LClassName, LFileName);
          FImplFileName := LFileName;
          FFormName     := LClassName;
        end;
    end;
  AFormName     := FFormName;
  AImplFileName := FImplFileName;
end;

// define a form and unitname prefix
function TNVPageModuleCreator.GetModuleAndClassNamePrefix: String;
begin
  Result := '';
end;

function TNVPageWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TNVPageWizard.GetIDString: string;
begin
  Result := 'NetVCL.NVForm';
end;

function TNVPageModuleCreator.GetImplFileName: string;
begin
  if FIsMainForm then
    Result := GetCurrentDir + '\uMainForm.pas'
  else
    Result := '';
end;

function TNVPageModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TNVPageModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TNVPageWizard.GetName: string;
begin
  Result := 'NetVCL Form';
end;

function TNVPageModuleCreator.GetOwner: IOTAModule;
begin
  if FOwner <> nil then
    Result := FOwner
  else
    Result := GetActiveProject;
end;

function TNVPageWizard.GetPage: string;
begin
  Result := 'NetVCL';
  DebugMsg('GetPage: ' + Result);
end;

function TNVPageModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
  DebugMsg('GetShowForm');
end;

function TNVPageModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
  DebugMsg('GetShowSource');
end;

function TNVPageWizard.GetState: TWizardState;
begin
  Result := [];
  DebugMsg('GetState');
end;

function TNVPageModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
  DebugMsg('GetUnnamed');
end;

procedure TNVPageWizard.Modified;
begin
  DebugMsg('Modified');
end;

function TNVPageModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
  DebugMsg('NewFormFile');
end;

function TNVPageModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string)
  : IOTAFile;
begin
  Result := TNVPageModuleSource.Create(ModuleIdent, FormIdent, AncestorIdent, FIsMainForm);
  DebugMsg('NewImplSource');
end;

function TNVPageModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string)
  : IOTAFile;
begin
  Result := nil;
  DebugMsg('NewIntfSource');
end;

constructor TNVPageModuleSource.Create(Const ModuleIdent, FormIdent, AncestorIdent: string;
  AsDefaultForm: Boolean);
begin
  inherited Create;
  FModuleIdent   := ModuleIdent;
  FFormIdent     := FormIdent;     // Copy(FormIdent, 2, MaxInt);
  FAncestorIdent := AncestorIdent; // Copy(AncestorIdent, 2,MaxInt);
  FDefaultForm   := AsDefaultForm;
  DebugMsg('TNVFormModuleSource.Create: ' + FModuleIdent + ' ,' + FFormIdent + ' ,' +
    FAncestorIdent);
end;

function TNVPageModuleSource.GetAge: TDateTime;
begin
  Result := -1;
  DebugMsg('GetAge: ' + DateToStr(Result));
end;

function TNVPageModuleSource.GetSource: string;
const
  cHeader = '{ NetVCL is an VCL internet Framework for  Delphi' + CrLf +
    '| https://github.com/DelcioSbeghen/NetVCL' + CrLf + '| Developped by Delcio Sbeghen @SRP Sistemas'
    + CrLf + '| Under MIT Licence: https://opensource.org/licenses/MIT' + CrLf + '|' + CrLf +
    '| Credits:' + CrLf + '|   * The Server Core is based in Indy'
    + CrLf + '|   * The Json Library is based in JsonDataObjects:  https://github.com/ahausladen/JsonDataObjects'
    + CrLf + '|and changes by IWBootstrap Framework' + CrLf +
    '|   * The NetVCL is based on Bootstrap Framework: https://github.com/kattunga/IWBootstrapFramework'
    + CrLf + '}' + CrLf2;

const
  cSource = 'unit %0:s;' + CrLf2 +

    'interface' + CrLf2 +

    'uses' + CrLf + '  NV.VCL.Forms;' + CrLf +

    'type' + CrLf +

    '  T%1:s = class(T%2:s)' + CrLf + '  private' + CrLf + '    { private declarations }' + CrLf +
    '  public' + CrLf + '    { public declarations }' + CrLf + '  end;' + CrLf2 +

    'var' + CrLf +
    '%1:s:T%1:s;' + CrLf2 +

    'implementation' + CrLf2 +

    '{$R *.dfm}' + CrLf2 +

    'end.';
begin
  if FDefaultForm then
    Result := Format(cHeader + cSource, [FModuleIdent, FFormIdent, FAncestorIdent])
  else
    Result := Format(cSource, [FModuleIdent, FFormIdent, FAncestorIdent]);
end;

end.
