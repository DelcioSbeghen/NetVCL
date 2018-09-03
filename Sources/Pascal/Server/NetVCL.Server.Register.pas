unit NetVCL.Server.Register;

interface

uses
  Classes, SysUtils, StrUtils, Dialogs, DesignIntf, DesignEditors, DMForm, Db;

type
  TNVFormModule = class(TCustomModule)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ValidateComponent(Component: TComponent); override;
    function ValidateComponentClass(ComponentClass: TComponentClass): Boolean; override;
  end;

  TNVDatamoduleModule = class(TDataModuleCustomModule) // TDataModuleDesignerCustomModule)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ValidateComponent(Component: TComponent); override;
  end;

procedure register;

implementation

uses
  NV.Server, NV.HostApplication, ToolsAPI, NV.Design.AppWizard, NV.Design.UserSessionWizard,
  NV.Design.PageWizard, NV.Design.AppModuleWizard, NV.VCL.Page, NV.UserSession;

procedure register;
begin
  RegisterComponents('NetVCL', [TNVServer]);
 // RegisterComponents('NetVCL', [TNVHostApp]);
   // DelphiWeb Wizards
  RegisterCustomModule(TNVPage, TNVFormModule);
 // RegisterCustomModule(TDWFrame, TNVFormModule);
 // RegisterCustomModule(TDWPopup, TNVFormModule);
  RegisterCustomModule(TNVUserSession, TDataModuleCustomModule);
  // UnlistPublishedProperty(TDWUserSession, 'OldCreateOrder');
  // UnlistPublishedProperty(TDWUserSession, 'OnCreate');
  // UnlistPublishedProperty(TDWUserSession, 'OnDestroy');
 // RegisterCustomModule(TDWDatamodule, TDataModuleCustomModule);
  RegisterPackageWizard(TNVAppWizard.Create);
  RegisterPackageWizard(TNVPageWizard.Create);
 // RegisterPackageWizard(TDWFrameWizard.Create);
 // RegisterPackageWizard(TDWPopupWizard.Create);
 // RegisterPackageWizard(TDWDataModuleWizard.Create);
end;


{ TNVFormModule }

procedure TNVFormModule.ExecuteVerb(Index: Integer);
var
  NewName: string;
begin
  if Index = 0 then
    begin
      NewName := Root.Name;
      if InputQuery('Panel Module Editor', 'New panel name:', NewName) then
        Root.Name := NewName;
    end;

end;

function TNVFormModule.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Rename...';
end;

function TNVFormModule.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TNVFormModule.ValidateComponent(Component: TComponent);
begin
  if not ValidateComponentClass(TComponentClass(Component.ClassType)) then
    raise Exception.Create('The NVForm or NVPage can host only NVControls');
end;

function TNVFormModule.ValidateComponentClass(ComponentClass: TComponentClass): Boolean;
begin
  if AnsiStartsText('NV.', ComponentClass.UnitName)
  or ComponentClass.InheritsFrom(TDataSet)
  or ComponentClass.ClassNameIs('TDataSource')
  or AnsiStartsText('Tfrx', ComponentClass.ClassName) then
    Result := True
  else
    Result := False;
end;

{ TDWDatamodule }

procedure TNVDatamoduleModule.ExecuteVerb(Index: Integer);
begin
  inherited;

end;

function TNVDatamoduleModule.GetVerb(Index: Integer): string;
begin
  Result := inherited;
end;

function TNVDatamoduleModule.GetVerbCount: Integer;
begin
  Result := inherited;
end;

procedure TNVDatamoduleModule.ValidateComponent(Component: TComponent);
begin
  inherited;
end;

end.

