unit NV.Design.Lazarus.Register;


interface

uses
  Windows, Classes, SysUtils,  ComponentEditors, PropEdits, StrUtils, Dialogs, ActnList, ProjectIntf,
  Db, Controls, Forms, NV.VCL.Page, NV.Controls, NV.Interfaces,
  NV.VCL.Forms;

type

{ TFileDescPascalNvForm }

TFileDescPascalNvForm = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

// Hook Desgigner(Lazarus) to INVDesignerHook(NetVCL)

  //{ TNVDesignerHook }
  //
  //TNVDesignerHook = class(TInterfacedObject, INVDesignerHook)
  //private
  //  FDesigner: TComponentEditorDesigner;
  //  FPage    : INVPage;
  //public
  //  constructor Create(aDesigner: TComponentEditorDesigner); virtual;
  //  destructor Destroy; override;
  //  function GetRoot: TComponent;
  //  procedure SelectComponent(Instance: TPersistent); overload;
  //  function GetComponent(const Name: string): TComponent;
  //  function CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
  //    Left, Top, Width, Height: Integer): TComponent;
  //  procedure Edit(const Component: TComponent);
  //  procedure Modified;
  //  procedure ResetDesignPage;
  //  function UniqueName(const BaseName: string): string;
  //  function Page: INVPage;
  //  property Root: TComponent read GetRoot;
  //end;


procedure Register;

implementation

uses
  FormEditingIntf, NV.Design.Lazarus.Designer,  NV.UserSession,  NV.VCL.Frame, NV.VCL.Images, NV.VCL.Dashboards,
  ExtCtrls, NV.Design.ImagelistEditor,
  NV.VCL.Charts, NV.JSON, NV.Design.JsonArrayEditor,
  NV.Desktop,
  NV.Design.ImageIndexEditor, NV.VCL.ActnList, NV.Design.ActionEditor;

const
  PALLETE_PAGE = 'NetVCL';

//var
//  NVDesignNotifier: IDesignNotification = nil;

procedure Register;
begin
  // RegisterComponents('NetVCL', [TNVHostApp]);

  // DelphiWeb Wizards
 // RegisterCustomModule(TNVFrame, TNVFormModule);
 RegisterProjectFileDescriptor(TFileDescPascalNvForm.Create, FileDescGroupName);
  FormEditingHook.RegisterDesignerMediator(TNvMediator);
 // RegisterCustomModule(TNVUserSession, TDataModuleCustomModule);

 // RegisterPackageWizard(TNVAppWizard.Create);
//  RegisterPackageWizard(TNVPageWizard.Create);
 // RegisterPackageWizard(TNVFrameWizard.Create);


  // Components
  RegisterComponents(PALLETE_PAGE, [TNvSvgImageList, TNvChart, TNvActionList, TNvDashboard,
    TNvDashbdItem]);
  RegisterClasses([TNvAction, TNVDashBreakPoints, TNvBreakPoint]);

  RegisterComponentEditor(TNvActionList, TNvActionListEditor);
  RegisterComponentEditor(TNvSvgImageList, TNvSvgImageListEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TNVImageListLink, 'ImageIndex', TNvImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(TJsonArray), nil, '', TNvJsonArrayEditor);
  RegisterPropertyEditor(TypeInfo(TBasicAction), TNvControl, 'Action', TNvActionProperty);
  RegisterPropertyEditor(TypeInfo(TBasicAction), TNvWinControl, 'Action', TNvActionProperty);
  RegisterActions('',
    [TNvAction { , FMX.Controls.TControlAction, FMX.StdActns.TValueRangeAction } ], nil);
end;

type

  // THackScreen      = class(TNvScreen);
  THackApplication = class(TNvApplication);

  THackPage      = class(TNVPage);
  THackComponent = class(TComponent);

{ TFileDescPascalNvForm }

constructor TFileDescPascalNvForm.Create;
begin
  inherited Create;
  Name:='NVForm'; // do not translate this
  ResourceClass:=TNVForm;
  UseCreateFormStatements:=true;
end;

function TFileDescPascalNvForm.GetInterfaceUsesSection: string;
begin
  Result:='Classes, SysUtils, NV.VCL.Forms';
end;

function TFileDescPascalNvForm.GetLocalizedName: string;
begin
  Result:='NVForm';
end;

function TFileDescPascalNvForm.GetLocalizedDescription: string;
begin
  Result:='Create a new NetVCL Form';
end;

//{ TNVDesignerHook }
//
//constructor TNVDesignerHook.Create(aDesigner: TComponentEditorDesigner);
//begin
//  Inherited Create;
//  FDesigner                                     := aDesigner;
//  THackApplication(Application).FDesignInstance := True;
//  THackApplication(Application).FRunning        := True;
//  FPage                                         := Screen.Page;
//end;
//
//destructor TNVDesignerHook.Destroy;
//begin
//  FDesigner := nil;
//  FPage     := nil;
//  inherited Destroy;
//end;
//
//function TNVDesignerHook.GetRoot: TComponent;
//begin
//   Result := FDesigner.LookupRoot;
//end;
//
//procedure TNVDesignerHook.SelectComponent(Instance: TPersistent);
//begin
//  FDesigner.SelectOnlyThisComponent(Instance as TComponent);
//end;
//
//function TNVDesignerHook.GetComponent(const Name: string): TComponent;
//begin
//  Result:= nil
//
//
//  Result := FDesigner.GetComponent(Name);
//end;
//
//function TNVDesignerHook.CreateComponent(ComponentClass: TComponentClass;
//  Parent: TComponent; Left, Top, Width, Height: Integer): TComponent;
//begin
//
//end;
//
//procedure TNVDesignerHook.Edit(const Component: TComponent);
//begin
//
//end;
//
//procedure TNVDesignerHook.Modified;
//begin
//
//end;
//
//procedure TNVDesignerHook.ResetDesignPage;
//begin
//
//end;
//
//function TNVDesignerHook.UniqueName(const BaseName: string): string;
//begin
//
//end;
//
//function TNVDesignerHook.Page: INVPage;
//begin
//
//end;

//  { TNVFormModule }
//
//procedure TNVFormModule.CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
//  out DesignForm: TCustomForm; out ComponentContainer: TWinControl);
//begin
//  raise Exception.Create('Need to implement Old Delphi Designer');
//
//  // DesignForm := TNvModuleDesigner.CreateEx(nil, Designer, ComponentContainer);
//  // (Root as TNVModuleContainer).Designer := TNVDesignerHook.Create(Designer);
//  // (ComponentContainer as TNvDesignPanel).Page.Parent := ComponentContainer;
//  // // (Root as TNVModuleContainer).Parent := ComponentContainer;
//  // FComponentContainer := ComponentContainer;
//end;
//
//constructor TNVFormModule.Create(ARoot: TComponent; const aDesigner: IDesigner);
//begin
//  inherited;
//
//end;
//
//procedure TNVFormModule.CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
//  out DesignForm: IHostForm; out ComponentContainer: TWinControl);
//begin
//  if Assigned(CreateDesignerFormProc) then
//    begin
//      CreateDesignerFormProc(Self, Designer, Root, DesignForm, ComponentContainer);
//
//      FComponentContainer := ComponentContainer;
//
//      FModule := Root as TNVModuleContainer;
//
//      Application.RootPath := GetProjectOutputDir(GetActiveProject) + '\www\';
//
//      if Application.RootPath.IsEmpty or
//        Not FileExists(Application.RootPath + 'netvcl\js\nv.classes.js') then
//        Application.RootPath := GetNVSourcesPath + '..\Demos\Dist\www\';
//    end
//  else
//    begin
//      DesignForm         := nil;
//      ComponentContainer := nil;
//    end;
//end;
//
//destructor TNVFormModule.Destroy;
//begin
//
//  inherited;
//end;
//
//procedure TNVFormModule.DoResize(Sender: TObject);
//begin
//  // ResizeBrowser(FComponentContainer.Height, FComponentContainer.Width);
//end;
//
//procedure TNVFormModule.ExecuteVerb(Index: Integer);
//begin
//  if Index = 0 then
//    (Screen as TNvScreenBrowser).ShowDevTools(Mouse.CursorPos);
//end;
//
//function TNVFormModule.GetVerb(Index: Integer): string;
//begin
//  if Index = 0 then
//    Result := 'Show Dev Tools';
//end;
//
//function TNVFormModule.GetVerbCount: Integer;
//begin
//  Result := 1;
//end;
//
//procedure TNVFormModule.ValidateComponent(Component: TComponent);
//begin
//  if not ValidateComponentClass(TComponentClass(Component.ClassType)) then
//    raise Exception.Create('The NVContainers can host only NVControls');
//end;
//
//function TNVFormModule.ValidateComponentClass(ComponentClass: TComponentClass): Boolean;
//begin
//  if AnsiStartsText('NV.', ComponentClass.UnitName) //
//    or ComponentClass.InheritsFrom(TNvControl)      //
//    or ComponentClass.InheritsFrom(TNvWinControl)   //
//    or ComponentClass.InheritsFrom(TDataSet)        //
//    or ComponentClass.InheritsFrom(TDataSource)     //
//    or ComponentClass.InheritsFrom(TDataModule)     //
//  then
//    Result := True
//  else
//    Result := False;
//end;
//
//{ TDWDatamodule }
//
//procedure TNVDatamoduleModule.ExecuteVerb(Index: Integer);
//begin
//  inherited;
//
//end;
//
//function TNVDatamoduleModule.GetVerb(Index: Integer): string;
//begin
//  Result := inherited;
//end;
//
//function TNVDatamoduleModule.GetVerbCount: Integer;
//begin
//  Result := inherited;
//end;
//
//procedure TNVDatamoduleModule.ValidateComponent(Component: TComponent);
//begin
//  inherited;
//end;

{ TNVDesignerHook }

//constructor TNVDesignerHook.Create(aDesigner: TObject);
//begin
//  inherited Create;
//  FDesigner                                     := aDesigner;
//  THackApplication(Application).FDesignInstance := True;
//  THackApplication(Application).FRunning        := True;
//  FPage                                         := Screen.Page;
//end;
//
//function TNVDesignerHook.CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
//  Left, Top, Width, Height: Integer): TComponent;
//begin
//  Result := FDesigner.CreateComponent(ComponentClass, Parent, Left, Top, Width, Height);
//end;
//
//destructor TNVDesignerHook.Destroy;
//begin
//  FDesigner := nil;
//  FPage     := nil;
//  inherited;
//end;
//
//procedure TNVDesignerHook.Edit(const Component: TComponent);
//begin
//  FDesigner.Edit(Component);
//end;
//
//function TNVDesignerHook.GetComponent(const Name: string): TComponent;
//begin
//  Result := FDesigner.GetComponent(Name);
//end;
//
//function TNVDesignerHook.GetRoot: TComponent;
//begin
//  Result := FDesigner.Root;
//end;
//
//procedure TNVDesignerHook.Modified;
//begin
//  FDesigner.Modified;
//end;
//
//function TNVDesignerHook.Page: INVPage;
//begin
//  Result := FPage;
//end;
//
//procedure TNVDesignerHook.ResetDesignPage;
//begin
//  THackPage(FPage as TNvPage).FControlsOrdered.Clear;
//  THackPage(FPage as TNvPage).FControlsOrdered.Add(FDesigner.Root);
//end;
//
//procedure TNVDesignerHook.SelectComponent(Instance: TPersistent);
//begin
//  FDesigner.SelectComponent(Instance);
//end;
//
//function TNVDesignerHook.UniqueName(const BaseName: string): string;
//begin
//  Result := FDesigner.UniqueName(BaseName);
//end;
//
//{ TNvDesignNotifier }
//
//procedure TNvDesignNotifier.DesignerClosed(const aDesigner: IDesigner; AGoingDormant: Boolean);
//begin
//  if Not(aDesigner.Root is TNVBaseFrame) then
//    Exit;
//
//  if not AGoingDormant then
//    begin
//      Screen.Close;
//      // Free DesignerHook by nil interface reference
//      TNVBaseFrame(aDesigner.Root).Designer := nil;
//    end;
//end;
//
//procedure TNvDesignNotifier.DesignerOpened(const aDesigner: IDesigner; AResurrecting: Boolean);
//var
//  _DesignerHook: TNVDesignerHook;
//
//begin
//  if Not(aDesigner.Root is TNVBaseFrame) then
//    Exit;
//
//  // If AResurrecting is True, then this designer
//  // has previously gone dormant and its design root is now being recreated.
//  if not AResurrecting //
//  // package reload
//    or (TNVBaseFrame(aDesigner.Root).Designer = nil) then
//    begin
//      _DesignerHook                         := TNVDesignerHook.Create(aDesigner);
//      TNVBaseFrame(aDesigner.Root).Designer := _DesignerHook;
//      (Screen as TNvScreenBrowser).ShowDesign(TNVBaseFrame(aDesigner.Root).Parent);
//    end
//  else
//    begin
//      _DesignerHook := TNVBaseFrame(aDesigner.Root).Designer as TNVDesignerHook;
//    end;
//
//  _DesignerHook.ResetDesignPage;
//
//  TNVBaseFrame(aDesigner.Root).Show;
//
//end;
//
//procedure TNvDesignNotifier.ItemDeleted(const aDesigner: IDesigner; AItem: TPersistent);
//begin
//
//end;
//
//procedure TNvDesignNotifier.ItemInserted(const aDesigner: IDesigner; AItem: TPersistent);
//begin
//
//end;
//
//procedure TNvDesignNotifier.ItemsModified(const aDesigner: IDesigner);
//begin
//
//end;
//
//procedure TNvDesignNotifier.SelectionChanged(const aDesigner: IDesigner;
//  const ASelection: IDesignerSelections);
//begin
//  { TODO -oDelcio -cDesigner : Change this behavior to IOTAEditorNotifier.ViewActivated }
//  // https://www.gexperts.org/open-tools-api-faq/
//  // http://www.davidghoyle.co.uk/WordPress/?p=1272
//  // https://github.com/Embarcadero/OTAPI-Docs/blob/main/wiki/IOTAEditorNotifier.md
//  if (aDesigner <> nil) and (aDesigner.Root is TNVModuleContainer) and (aDesigner.Root <> FlastRoot)
//  then
//    begin
//      FlastRoot := TNVModuleContainer(aDesigner.Root);
//      DesignerClosed(aDesigner, False);
//      DesignerOpened(aDesigner, False);
//    end;
//end;

//initialization
//
//NVDesignNotifier := TNVDesignNotifier.Create;
//RegisterDesignNotification(NVDesignNotifier);
//
//finalization
//
//if NVDesignNotifier <> nil then
//  UnRegisterDesignNotification(NVDesignNotifier);
//NVDesignNotifier := nil;

end.

