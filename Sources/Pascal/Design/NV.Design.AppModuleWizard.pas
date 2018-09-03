unit NV.Design.AppModuleWizard;

interface

uses Classes, Winapi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, TypInfo;

type

  TNVAppModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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

  TNVAppModuleSource = class(TInterfacedObject, IOTAFile)
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

constructor TNVAppModuleCreator.Create;
begin
  Create(nil);
end;

constructor TNVAppModuleCreator.Create(AOwner: IOTAModule);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TNVAppModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TNVAppModuleCreator.GetAncestorName: string;
begin
  Result := 'NVHostApp';
end;

function TNVAppModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TNVAppModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TNVAppModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TNVAppModuleCreator.GetFormName: string;
begin
  Result := 'NVApp';
end;

function TNVAppModuleCreator.GetImplFileName: string;
begin
  Result := GetCurrentDir + '\uApp.pas';
end;

function TNVAppModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TNVAppModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TNVAppModuleCreator.GetOwner: IOTAModule;
begin
  if FOwner <> nil then
    Result := FOwner
  else
    Result := GetActiveProject;
end;

function TNVAppModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TNVAppModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TNVAppModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TNVAppModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TNVAppModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TNVAppModuleSource.Create(ModuleIdent, FormIdent, AncestorIdent);
end;

function TNVAppModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

constructor TNVAppModuleSource.Create(Const ModuleIdent, FormIdent, AncestorIdent: string);
begin
  inherited Create;
  FModuleIdent   := ModuleIdent;
  FFormIdent     := FormIdent;     // Copy(FormIdent, 2, MaxInt);
  FAncestorIdent := AncestorIdent; // Copy(AncestorIdent, 2,MaxInt);
end;

function TNVAppModuleSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TNVAppModuleSource.GetSource: string;
const
  cSource = 'unit %0:s;' + CrLf2 +

    'interface' + CrLf2 +

    'uses' + CrLf + '  NV.HostApplication;' + CrLf +

    'type' + CrLf +

    '  T%1:s = class(T%2:s)' + CrLf + '  private' + CrLf + '    { private declarations }' + CrLf +
    '  public' + CrLf + '    { public declarations }' + CrLf + '  end;' + CrLf2 +

    '{ !!! Do Not Declare Global Variables !!! }' + CrLf2 +

    'function LoadApp(aServer: TNVServer):TNVHostApp;' + CrLf2 +

'exports' + CrLf +
'  LoadApp;'  + CrLf2 +

'implementation' + CrLf2 +

    '  uses' + CrLf + '    uMainPage;' + CrLf2 +
  // '{'#9'%CLASSGROUP ''Vcl.Controls.TControl''}' + CrLf2 +

    '{$R *.dfm}' + CrLf2 + CrLf2 +

'function LoadApp(aServer: TNVServer):TNVHostApp;' + CrLf +
'begin' + CrLf +
'  registerClass(T%1:s);' + CrLf +
'  Result:= T%1:s.Create(nil);' + CrLf +
'  Result.Domain:= ''localhost'';'+ CrLf +
'  Result.MainPage:= TMainPage;'+ CrLf +
'end;' + CrLf2 +



   // 'initialization' + CrLf +
    //'  gUserSessionClass :=  T%1:s;' + CrLf2 +
    CrLf2 +
    'end.';
begin
  Result := Format(cSource, [FModuleIdent, FFormIdent, FAncestorIdent]);
end;

end.

