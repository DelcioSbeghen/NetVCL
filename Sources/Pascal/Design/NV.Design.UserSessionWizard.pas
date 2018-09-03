unit NV.Design.UserSessionWizard;

interface

uses Classes, Winapi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, TypInfo;

type

  TNVUserSessionModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FOwner: IOTAModule;
    FImplFileName: string;
    FFormName: string;
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

  TNVUserSessionModuleSource = class(TInterfacedObject, IOTAFile)
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
  SysUtils, System.Win.ComObj, NV.UserSession;

procedure DebugMsg(const Msg: String);
begin
  // ShowMessage(Msg);
end;

constructor TNVUserSessionModuleCreator.Create;
begin
  Create(nil);
end;

constructor TNVUserSessionModuleCreator.Create(AOwner: IOTAModule);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TNVUserSessionModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TNVUserSessionModuleCreator.GetAncestorName: string;
begin
  Result := 'NVUserSession';
end;

function TNVUserSessionModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TNVUserSessionModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TNVUserSessionModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TNVUserSessionModuleCreator.GetFormName: string;
begin
  Result := 'NVSessionData';
end;

function TNVUserSessionModuleCreator.GetImplFileName: string;
begin
  Result := GetCurrentDir + '\uNVSessionData.pas';
end;

function TNVUserSessionModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TNVUserSessionModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TNVUserSessionModuleCreator.GetOwner: IOTAModule;
begin
  if FOwner <> nil then
    Result := FOwner
  else
    Result := GetActiveProject;
end;

function TNVUserSessionModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TNVUserSessionModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TNVUserSessionModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TNVUserSessionModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TNVUserSessionModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TNVUserSessionModuleSource.Create(ModuleIdent, FormIdent, AncestorIdent);
end;

function TNVUserSessionModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

constructor TNVUserSessionModuleSource.Create(Const ModuleIdent, FormIdent, AncestorIdent: string);
begin
  inherited Create;
  FModuleIdent   := ModuleIdent;
  FFormIdent     := FormIdent;     // Copy(FormIdent, 2, MaxInt);
  FAncestorIdent := AncestorIdent; // Copy(AncestorIdent, 2,MaxInt);
end;

function TNVUserSessionModuleSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TNVUserSessionModuleSource.GetSource: string;
const
  cSource = 'unit %0:s;' + CrLf2 +

    'interface' + CrLf2 +

    'uses' + CrLf + '  NV.UserSession;' + CrLf +

    'type' + CrLf +

    '  T%1:s = class(T%2:s)' + CrLf + '  private' + CrLf + '    { private declarations }' + CrLf +
    '  public' + CrLf + '    { public declarations }' + CrLf + '  end;' + CrLf2 +

    '{ !!! Do Not Declare Global Variables !!! }' + CrLf2 +

    'implementation' + CrLf2 +

  //  '  uses' + CrLf + '    NVGlobal;' + CrLf2 +
  // '{'#9'%CLASSGROUP ''Vcl.Controls.TControl''}' + CrLf2 +

    '{$R *.dfm}' + CrLf2 + CrLf2 +

    'initialization' + CrLf +
    //'  gUserSessionClass :=  T%1:s;' + CrLf2 +
    CrLf2 +
    'end.';
begin
  Result := Format(cSource, [FModuleIdent, FFormIdent, FAncestorIdent]);
end;

end.
