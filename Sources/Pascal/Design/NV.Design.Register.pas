unit NV.Design.Register;

interface

uses
  Windows, Classes, SysUtils, StrUtils, Dialogs, DesignIntf, DesignEditors, DMForm, Db, VCLEditors,
  Controls, Forms, WCtlForm, NV.VCL.Page, NV.Controls, NV.Interfaces, NV.VCL.Forms,

  NV.Design.IDE;

type

  // Hook IDesgigner(in DesignTime Package) to INVDesignerHook(in Runtime package)
  TNVDesignerHook = class(TInterfacedObject, INVDesignerHook)
  private
    FDesigner: IDesigner;
    FPage    : INVPage;
  public
    constructor Create(aDesigner: IDesigner; aPage: TNVBasepage); virtual;
    function GetRoot: TComponent;
    procedure SelectComponent(Instance: TPersistent); overload;
    function GetComponent(const Name: string): TComponent;
    function CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
      Left, Top, Width, Height: Integer): TComponent;
    procedure Edit(const Component: TComponent);
    procedure Modified;
    function UniqueName(const BaseName: string): string;
    property Root: TComponent read GetRoot;
    function Page: INVPage;
  end;

  TNVFormModule = class(TCustomModule { TWinControlCustomModule } , ICustomDesignForm,
    ICustomDesignForm80)
  private
    FComponentContainer: TWinControl;
    FModule            : TNVModuleContainer; // Module in desing
    FPage              : TNVBasepage;        // page is diff from module if module is Frame
  protected
    // procedure HandleNvApplicationMessages(Sender: TObject; var Done: Boolean);

  public
    destructor Destroy; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ValidateComponent(Component: TComponent); override;
    function ValidateComponentClass(ComponentClass: TComponentClass): Boolean; override;
    // ICustomDesignForm
    procedure CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
      out DesignForm: TCustomForm; out ComponentContainer: TWinControl); overload;
    /// /ICustomDesignForm80
    procedure CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
      out DesignForm: IHostForm; out ComponentContainer: TWinControl); overload;

    procedure DoResize(Sender: TObject);
  end;

  // TNVFrameModule = class(TNVFormModule, ICustomDesignForm, ICustomDesignForm80)
  // private
  // FComponentContainer: TWinControl;
  // FPageDesign:TNVpage;
  // public
  /// /    procedure ExecuteVerb(Index: Integer); override;
  /// /    function GetVerb(Index: Integer): string; override;
  /// /    function GetVerbCount: Integer; override;
  // //procedure ValidateComponent(Component: TComponent); override;
  // //function ValidateComponentClass(ComponentClass: TComponentClass): Boolean; override;
  // // ICustomDesignForm
  // procedure CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
  // out DesignForm: TCustomForm; out ComponentContainer: TWinControl); overload;
  // /// /ICustomDesignForm80
  // procedure CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
  // out DesignForm: IHostForm; out ComponentContainer: TWinControl); overload;
  // end;

  TNVDatamoduleModule = class(TDataModuleCustomModule) // TDataModuleDesignerCustomModule)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ValidateComponent(Component: TComponent); override;
  end;

var
  RootPath: string = '';

procedure Register;

implementation

uses
  ToolsAPI, NV.Design.AppWizard, NV.Design.UserSessionWizard,
  NV.Design.PageWizard, NV.Design.AppModuleWizard, NV.UserSession,
  NV.Design.ModuleDesigner,
  NV.Design.FrameWizard, NV.VCL.Frame, NV.Design.FrameDesigner, NV.VCL.Images,
  ExtCtrls, System.Threading, NV.Design.ImagelistEditor,
  NV.Design.IOTAUtils, NV.VCL.Charts, NV.JSON, NV.Design.JsonArrayEditor, NV.Browser,
  NV.Design.ImageIndexEditor;

const
  PALLETE_PAGE = 'NetVCL';

procedure Register;
begin
  // RegisterComponents('NetVCL', [TNVHostApp]);

  // DelphiWeb Wizards
  RegisterCustomModule(TNVPage, TNVFormModule);
  RegisterCustomModule(TNVFrame, TNVFormModule);
  RegisterCustomModule(TNVForm, TNVFormModule);
  // RegisterCustomModule(TNVFrame, TNVFormModule);
  // RegisterCustomModule(TNVHostApp, TDataModuleCustomModule);
  // RegisterCustomModule(TDWFrame, TNVFormModule);
  // RegisterCustomModule(TDWPopup, TNVFormModule);
  RegisterCustomModule(TNVUserSession, TDataModuleCustomModule);
  // UnlistPublishedProperty(TDWUserSession, 'OldCreateOrder');
  // UnlistPublishedProperty(TDWUserSession, 'OnCreate');
  // UnlistPublishedProperty(TDWUserSession, 'OnDestroy');
  // RegisterCustomModule(TDWDatamodule, TDataModuleCustomModule);
  RegisterPackageWizard(TNVAppWizard.Create);
  RegisterPackageWizard(TNVPageWizard.Create);
  RegisterPackageWizard(TNVFrameWizard.Create);
  // RegisterPackageWizard(TNVSeWizard.Create);
  // RegisterPackageWizard(TDWFrameWizard.Create);
  // RegisterPackageWizard(TDWPopupWizard.Create);
  // RegisterPackageWizard(TDWDataModuleWizard.Create);

  // Components
  RegisterComponents(PALLETE_PAGE, [TNvSvgImageList, TNvChart]);

  RegisterComponentEditor(TNvSvgImageList, TNvSvgImageListEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TNVImageListLink, 'ImageIndex', TNvImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(TJsonArray), nil, '', TNvJsonArrayEditor);
end;

type
  // THackComponent = class(TComponent)
  // end;

  THackScreen      = class(TNvScreen);
  THackApplication = class(TNvApplication);

  THackPage      = class(TNVPage);
  THackComponent = class(TComponent);

  { TNVFormModule }

procedure TNVFormModule.CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
  out DesignForm: TCustomForm; out ComponentContainer: TWinControl);
begin
  raise Exception.Create('Need to implement Old Delphi Designer');

  // DesignForm := TNvModuleDesigner.CreateEx(nil, Designer, ComponentContainer);
  // (Root as TNVModuleContainer).Designer := TNVDesignerHook.Create(Designer);
  // (ComponentContainer as TNvDesignPanel).Page.Parent := ComponentContainer;
  // // (Root as TNVModuleContainer).Parent := ComponentContainer;
  // FComponentContainer := ComponentContainer;
end;

procedure TNVFormModule.CreateDesignerForm(const Designer: IDesigner; Root: TComponent;
  out DesignForm: IHostForm; out ComponentContainer: TWinControl);
// var
// _ComponentContainer: TPanel;
begin
  if Assigned(CreateDesignerFormProc) then
    begin
      CreateDesignerFormProc(Self, Designer, Root, DesignForm, ComponentContainer);

      // _ComponentContainer := TPanel.Create(nil);
      // with _ComponentContainer do
      // begin
      // Align       := alClient;
      // ShowCaption := False;
      // Parent      := ComponentContainer.Parent;
      // SetDesignVisible(True);
      // end;
      // ComponentContainer := _ComponentContainer;

      // THackComponent(ComponentContainer).SetDesigning(True);
      // ComponentContainer.InsertControl(Root as TControl);

      FModule  := Root as TNVModuleContainer;
      RootPath := GetNVSourcesPath(GetProjectOutputDir(GetActiveProject));

      if Root is TNVBasepage then
        FPage := Root as TNVBasepage
      else if Root is TNVBaseFrame then
        begin
          // FPage := TNVPage.Create(nil);
          FPage := THackScreen(Screen).FPage;
          // FPage.Align  := alClient;
          // FPage.Parent := ComponentContainer.Parent;
          THackPage(FPage).SetDesigning(True);

          // add Module to FOrderredControls list (same as set parent)
          THackPage(FPage).FControlsOrdered.Add(FModule);
          // FPage.InsertControl(FModule);
          // FPage.InsertControl(FModule);
          // FModule.Parent:=  FPage;
        end;

      // ComponentContainer := FPage;
      // FModule.Parent     := FPage;

      FModule.Designer := TNVDesignerHook.Create(Designer, FPage);



      // FDesignServer := TNVDesignServer.Create(FPage);

      // FDesignBrowser.ShowDesignBrowser(ComponentContainer);
      // FDesignBrowser.LoadUrl('http://127.0.0.1:' + FDesignServer.Port);

      // Forms.Application.OnIdle:= HandleNvApplicationMessages;

      THackApplication(Application).FRunning        := True;
      THackApplication(Application).FDesignInstance := True;
      Screen.ShowDesign(ComponentContainer.Parent);
      // Screen.SetParent(ComponentContainer);
      FModule.Show;

      // To remove module from FPage.FControlsOrdered
      FModule.FreeNotification(FPage);





      // DesignForm := TNvModuleDesigner.CreateEx(nil, Designer, ComponentContainer);
      // (Root as TNVModuleContainer).Designer := TNVDesignerHook.Create(Designer);
      // (ComponentContainer as TNvDesignPanel).Page.Parent := ComponentContainer;
      // // (Root as TNVModuleContainer).Parent := ComponentContainer;
      // FComponentContainer := ComponentContainer;

      {






        //
        //      with TNVDesignBrowser.Create(DesignForm.GetForm) do
        //        begin
        //          ShowDesignBrowser(ComponentContainer);
        //          LoadUrl('www.google.com.br');
        //        end;
        //
        //        THackComponent(ComponentContainer).SetDesigning(True);


        //
        _ComponentContainer := TNvDesignPanel.Create(ComponentContainer.Owner, Root as TNVModuleContainer);

        _ComponentContainer.InitDesignBrowser(ComponentContainer.Parent);
        // _ComponentContainer:= ComponentContainer;

        // THackComponent(_ComponentContainer).SetDesigning(True);

        // _ComponentContainer.Show;







        ComponentContainer := _ComponentContainer;
        (Root as TNVModuleContainer).Parent := ComponentContainer;

        THackComponent(ComponentContainer).SetDesigning(True);
        THackComponent(ComponentContainer).SetDesignInstance(True);

        // _ComponentContainer.BringToFront;
        //
        // _ComponentContainer.OnResize:= DoResize;

        // THackComponent(ComponentContainer).SetDesigning(True);
        // ComponentContainer.Show;
        //
        // if (ComponentContainer as TNvDesignPanel).Module is TNVBaseFrame then
        // begin
        // (ComponentContainer as TNvDesignPanel).Module.Parent :=
        // (ComponentContainer as TNvDesignPanel).Page;
        // ComponentContainer.InsertControl((ComponentContainer as TNvDesignPanel).Module);
        // end
        // else
        // (ComponentContainer as TNvDesignPanel).Page.Parent := ComponentContainer;
        // (Root as TNVModuleContainer).Parent := ComponentContainer;
        // (Root as TNVModuleContainer).Designer := TCustomForm(Root.Owner).Designer;
        FComponentContainer := _ComponentContainer; }

    end
  else

    // DesignForm := TNvModuleDesigner.CreateEx(nil, Designer, ComponentContainer);

    begin
      DesignForm         := nil;
      ComponentContainer := nil;
    end;
end;

destructor TNVFormModule.Destroy;
begin

  inherited;
end;

procedure TNVFormModule.DoResize(Sender: TObject);
begin
  // ResizeBrowser(FComponentContainer.Height, FComponentContainer.Width);
end;

procedure TNVFormModule.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    NV.VCL.Forms.Screen.ShowDevTools(Mouse.CursorPos);
end;

function TNVFormModule.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Show Dev Tools';
end;

function TNVFormModule.GetVerbCount: Integer;
begin
  Result := 1;
end;

// procedure TNVFormModule.HandleNvApplicationMessages(Sender: TObject;
// var Done: Boolean);
// begin
// THackApplication(Application).HandleMessage;
// end;

procedure TNVFormModule.ValidateComponent(Component: TComponent);
begin
  if not ValidateComponentClass(TComponentClass(Component.ClassType)) then
    raise Exception.Create('The NVContainers can host only NVControls');
end;

function TNVFormModule.ValidateComponentClass(ComponentClass: TComponentClass): Boolean;
begin
  if AnsiStartsText('NV.', ComponentClass.UnitName) //
    or ComponentClass.InheritsFrom(TNvControl)      //
    or ComponentClass.InheritsFrom(TNvWinControl)   //
    or ComponentClass.InheritsFrom(TDataSet)        //
    or ComponentClass.InheritsFrom(TDataSource)     //
    or ComponentClass.InheritsFrom(TDataModule)     //
  then
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

{ TNVFrameModule }
//
// procedure TNVFrameModule.CreateDesignerForm(const Designer: IDesigner;
// Root: TComponent; out DesignForm: IHostForm;
// out ComponentContainer: TWinControl);
// begin
// if Assigned(CreateDesignerFormProc) then
// begin
// CreateDesignerFormProc(Self, Designer, Root, DesignForm, ComponentContainer);
// ComponentContainer := TNvDesignPanel.Create(nil, (Root as TNVBaseFrame).   as TNVBasepage);
// with ComponentContainer as TNvDesignPanel do
// begin
// Left        := 0;
// Top         := 0;
// Width       := 605;
// Height      := 418;
// Align       := alClient;
// BevelInner  := bvLowered;
// BevelOuter  := bvNone;
// BorderStyle := bsNone;
// TabOrder    := 0;
// Parent      := DesignForm.GetForm as TWinControl;
// end;
// THackComponent(ComponentContainer).SetDesigning(True);
// ComponentContainer.Show;
// (Root as TNVModuleContainer).Parent := ComponentContainer;
// //  (Root as TNVModuleContainer).Designer := TCustomForm(Root.Owner).Designer;
// FComponentContainer                 := ComponentContainer;
//
// end
// else
//
// // DesignForm := TNvModuleDesigner.CreateEx(nil, Designer, ComponentContainer);
//
// begin
// DesignForm         := nil;
// ComponentContainer := nil;
// end;
// end;
//
// procedure TNVFrameModule.CreateDesignerForm(const Designer: IDesigner;
// Root: TComponent; out DesignForm: TCustomForm;
// out ComponentContainer: TWinControl);
// begin
// DesignForm := TNvFrameDesigner.CreateEx(nil, Designer, ComponentContainer);
// (Root as TNVBaseFrame).Parent := ComponentContainer;
// FComponentContainer := ComponentContainer;
// end;

// procedure TNVFrameModule.ExecuteVerb(Index: Integer);
// begin
// if Index = 0 then
// (FComponentContainer as TNvDesignPanel).ShowDevTools;
// end;

// function TNVFrameModule.GetVerb(Index: Integer): string;
// begin
// if Index = 0 then
// Result := 'Show Dev Tools';
// end;

// function TNVFrameModule.GetVerbCount: Integer;
// begin
//
// end;

// procedure TNVFrameModule.ValidateComponent(Component: TComponent);
// begin
// inherited;
//
// end;
//
// function TNVFrameModule.ValidateComponentClass(
// ComponentClass: TComponentClass): Boolean;
// begin
//
// end;

{ TNVDesignerHook }

constructor TNVDesignerHook.Create(aDesigner: IDesigner; aPage: TNVBasepage);
begin
  inherited Create;
  FDesigner := aDesigner;
  FPage     := aPage;
end;

function TNVDesignerHook.CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
  Left, Top, Width, Height: Integer): TComponent;
begin
  Result := FDesigner.CreateComponent(ComponentClass, Parent, Left, Top, Width, Height);
end;

procedure TNVDesignerHook.Edit(const Component: TComponent);
begin
  FDesigner.Edit(Component);
end;

function TNVDesignerHook.GetComponent(const Name: string): TComponent;
begin
  Result := FDesigner.GetComponent(Name);
end;

function TNVDesignerHook.GetRoot: TComponent;
begin
  Result := FDesigner.Root;
end;

procedure TNVDesignerHook.Modified;
begin
  FDesigner.Modified;
end;

function TNVDesignerHook.Page: INVPage;
begin
  Result := FPage;
end;

procedure TNVDesignerHook.SelectComponent(Instance: TPersistent);
begin
  FDesigner.SelectComponent(Instance);
end;

function TNVDesignerHook.UniqueName(const BaseName: string): string;
begin
  Result := FDesigner.UniqueName(BaseName);
end;

end.
