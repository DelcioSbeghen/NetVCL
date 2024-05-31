unit NV.Design.Delphi.FrameWizard;


interface

uses Classes, Winapi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, TypInfo;

type

  TNVFrameWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier, IOTARepositoryWizard,
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

  TNVFrameModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FOwner: IOTAModule;
    FHaveNames: Boolean;
    FImplFileName: string;
    FFormName: string;
    procedure GetNewModuleAndClassName(out AFormName, AImplFileName: string);
    function GetModuleAndClassNamePrefix: String;
  public
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

  TNVFrameModuleSource = class(TInterfacedObject, IOTAFile)
  private
    FModuleIdent: string;
    FFormIdent: string;
    FAncestorIdent: string;
  public
    constructor Create(const ModuleIdent, FormIdent, AncestorIdent: string);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

const
  CrLf2 = #13#10#13#10;
  CrLf  = #13#10;

implementation

uses
  SysUtils, System.Win.ComObj, NV.VCL.Frame{, NV.VCL.Labels};

procedure DebugMsg(const Msg: String);
begin
  // ShowMessage(Msg);
end;

procedure TNVFrameWizard.AfterSave;
begin
  DebugMsg('AfterSave');
end;

procedure TNVFrameWizard.BeforeSave;
begin
  DebugMsg('BeforeSave');
end;

procedure TNVFrameWizard.Destroyed;
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

procedure TNVFrameWizard.Execute;
var
  ModuleServices: IOTAModuleServices;
  ProjectGroup: IOTAProjectGroup;
begin

  DebugMsg('TNVFormWizard.Execute');

  ModuleServices := BorlandIDEServices as IOTAModuleServices;

  ModuleServices.CreateModule(TNVFrameModuleCreator.Create);

end;

constructor TNVFrameModuleCreator.Create;
begin
  Create(nil);
end;

constructor TNVFrameModuleCreator.Create(AOwner: IOTAModule);
begin
  inherited Create;
  FOwner      := AOwner;
end;

procedure TNVFrameModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
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

function TNVFrameModuleCreator.GetAncestorName: string;
begin
  Result := 'NVFrame';
  DebugMsg('GetAncestorName: ' + Result);
end;

function TNVFrameWizard.GetAuthor: string;
begin
  Result := 'Delcio Sbeghen SRP Sistemas';
  DebugMsg('GetAuthor: ' + Result);
end;

function TNVFrameWizard.GetComment: string;
begin
  Result := 'Creates a new NVFrame.';
  DebugMsg('GetComment: ' + Result);
end;

function TNVFrameModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
  DebugMsg('GetCreatorType');
end;

function TNVFrameModuleCreator.GetExisting: Boolean;
begin
  Result := False;
  DebugMsg('GetExisting');
end;

function TNVFrameModuleCreator.GetFileSystem: string;
begin
  Result := '';
  DebugMsg('GetFileSystem: ' + Result);
end;

function TNVFrameModuleCreator.GetFormName: string;
var
  LImplFileName: string;
begin
    GetNewModuleAndClassName(Result, LImplFileName);
end;

procedure TNVFrameModuleCreator.GetNewModuleAndClassName(out AFormName: string;
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
function TNVFrameModuleCreator.GetModuleAndClassNamePrefix: String;
begin
  Result := '';
end;

function TNVFrameWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TNVFrameWizard.GetIDString: string;
begin
  Result := 'NetVCL.NVFrame';
end;

function TNVFrameModuleCreator.GetImplFileName: string;
begin
    Result := '';
end;

function TNVFrameModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TNVFrameModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TNVFrameWizard.GetName: string;
begin
  Result := 'NetVCL Frame';
end;

function TNVFrameModuleCreator.GetOwner: IOTAModule;
begin
  if FOwner <> nil then
    Result := FOwner
  else
    Result := GetActiveProject;
end;

function TNVFrameWizard.GetPage: string;
begin
  Result := 'NetVCL';
  DebugMsg('GetPage: ' + Result);
end;

function TNVFrameModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
  DebugMsg('GetShowForm');
end;

function TNVFrameModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
  DebugMsg('GetShowSource');
end;

function TNVFrameWizard.GetState: TWizardState;
begin
  Result := [];
  DebugMsg('GetState');
end;

function TNVFrameModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
  DebugMsg('GetUnnamed');
end;

procedure TNVFrameWizard.Modified;
begin
  DebugMsg('Modified');
end;

function TNVFrameModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
  DebugMsg('NewFormFile');
end;

function TNVFrameModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string)
  : IOTAFile;
begin
  Result := TNVFrameModuleSource.Create(ModuleIdent, FormIdent, AncestorIdent);
  DebugMsg('NewImplSource');
end;

function TNVFrameModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string)
  : IOTAFile;
begin
  Result := nil;
  DebugMsg('NewIntfSource');
end;

constructor TNVFrameModuleSource.Create(Const ModuleIdent, FormIdent, AncestorIdent: string);
begin
  inherited Create;
  FModuleIdent   := ModuleIdent;
  FFormIdent     := FormIdent;     // Copy(FormIdent, 2, MaxInt);
  FAncestorIdent := AncestorIdent; // Copy(AncestorIdent, 2,MaxInt);
  DebugMsg('TNVFormModuleSource.Create: ' + FModuleIdent + ' ,' + FFormIdent + ' ,' +
    FAncestorIdent);
end;

function TNVFrameModuleSource.GetAge: TDateTime;
begin
  Result := -1;
  DebugMsg('GetAge: ' + DateToStr(Result));
end;

function TNVFrameModuleSource.GetSource: string;
const
  cSource = 'unit %0:s;' + CrLf2 +

    'interface' + CrLf2 +

    'uses' + CrLf + '  NV.VCL.Frame;' + CrLf +

    'type' + CrLf +

    '  T%1:s = class(T%2:s)' + CrLf + '  private' + CrLf + '    { private declarations }' + CrLf +
    '  public' + CrLf + '    { public declarations }' + CrLf + '  end;' + CrLf2 +

    '{ !!! Do Not Declare Global Variables !!! }' + CrLf2 +

    'implementation' + CrLf2 +

    '{$R *.dfm}' + CrLf2 +

    'end.';
begin
    Result := Format(cSource, [FModuleIdent, FFormIdent, FAncestorIdent]);
end;

end.
