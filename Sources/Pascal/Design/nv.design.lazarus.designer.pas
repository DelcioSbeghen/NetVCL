unit NV.Design.Lazarus.Designer;


{$mode delphi}{$H+}

interface

uses
  LCLProc, LCLType, Classes, SysUtils, FormEditingIntf, LCLIntf, Graphics,
  Dialogs, Forms{keep before NV.VCL.Forms}, ProjectIntf, Controls, NV.Controls,
  NV.Interfaces, NV.VCL.Forms, NV.VCL.Page;

type

  { TNvMediator }

  TNvMediator = class(TDesignerMediator,INVDesignerHook)
  private
    FNvForm: TNVForm;
    FPage:TNvPage;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetLCLForm(const AValue: TForm); override;
  public
    // needed by the Lazarus form editor
    class function CreateMediator(TheOwner, aForm: TComponent): TDesignerMediator;
      override;
    class function FormClass: TComponentClass; override;
    procedure GetBounds(AComponent: TComponent; out CurBounds: TRect); override;
    procedure SetBounds(AComponent: TComponent; NewBounds: TRect); override;
    procedure GetClientArea(AComponent: TComponent; out CurClientArea: TRect;
      out ScrollOffset: TPoint); override;
    procedure Paint; override;
    function ComponentIsIcon(AComponent: TComponent): boolean; override;
    function ParentAcceptsChild(Parent: TComponent;
      Child: TComponentClass): boolean; override;
  public
    // needed by TMyWidget
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
    property NvForm: TNvForm read FNvForm;
    //INVDesignerHook
    function GetRoot: TComponent;
    procedure SelectComponent(Instance: TPersistent); overload;
    function GetComponent(const aName: string): TComponent;
    function CreateComponent(ComponentClass: TComponentClass; Parent: TComponent;
      Left, Top, Width, Height: Integer): TComponent;
    procedure Edit(const Component: TComponent);
    procedure Modified;
    procedure ResetDesignPage;
    function UniqueName(const BaseName: string): string;
    function Page: INVPage;
    property Root: TComponent read GetRoot;
  public
    procedure GetObjInspNodeImageIndex(APersistent: TPersistent;
      var AIndex: integer); override;
  end;


implementation

uses
  MacroIntf, Registry, NV.VCL.Frame, NV.Desktop;

type
  THackApplication = class(TNvApplication);
  THackPage        = class(TNVPage);



function GetProjectOutputDir: string;
begin
  Result:='$(TargetFile)';
  if IDEMacros.SubstituteMacros(Result) then
    Result:=ExtractFileDir(Result)
  else
    Result:= '';
end;

function GetNVSourcesPath: string;
begin
  Result := '';
  with TRegistry.Create do
    begin
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('Software\SRP Sistemas\NetVCL', True) then
        begin
          if ValueExists('SourcePath') then
            Result := ReadString('LibSourcePath')
          else
            WriteString('SourcePath', '');

          if (Result <> '') and not FileExists(Result + 'NetVCLSourcesRoot.txt') then
            Result := '';

          while Result = '' do
            begin
              ShowMessage('NetVCL source path specified is not valid.');
              if PromptForFileName(Result, 'NetVCLSourcesRoot.txt',
                'Select NetVcl Script Library Path') then
                begin
                  Result := ExtractFilePath(Result);

                  WriteString('LibSourcePath', Result);
                end;
            end;

        end;
    end;
end;


{ TNvMediator }

constructor TNvMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Application.RootPath := GetProjectOutputDir + '\www\';

  if Application.RootPath.IsEmpty or
  Not FileExists(Application.RootPath + 'netvcl\js\nv.classes.js') then
    Application.RootPath := GetNVSourcesPath + '..\Demos\Dist\www\';
end;

destructor TNvMediator.Destroy;
begin
  if FNvForm <> nil then FNvForm.Designer := nil;
  FNvForm := nil;
  inherited Destroy;
end;

procedure TNvMediator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if FNvForm = AComponent then
    begin
      FNvForm.Designer := nil;
      FNvForm := nil;
    end;
  end;
end;

procedure TNvMediator.SetLCLForm(const AValue: TForm);
var
  _Old:TForm;
begin
  _Old:= LCLForm;

  inherited SetLCLForm(AValue);

  if (LCLForm <> _Old) and (LCLForm <> nil) //
  and (FNvForm <> nil) and (FNvForm is TNVBaseFrame) then
    begin
      THackApplication(Application).FDesignInstance := True;
      THackApplication(Application).FRunning        := True;
      FPage                                         := Screen.Page;
      TNVBaseFrame(FNvForm).Designer := Self;

      (Screen as TNvScreenBrowser).ShowDesign(LCLForm);

      ResetDesignPage;

      TNVBaseFrame(FNvForm).Show;
      (Screen as TNvScreenBrowser).ShowDevTools(Mouse.CursorPos);
    end;
end;

class function TNvMediator.CreateMediator(TheOwner, aForm: TComponent):
TDesignerMediator;
var
  Mediator: TNvMediator;
begin
  Result := inherited CreateMediator(TheOwner, aForm);
  Mediator := TNvMediator(Result);
  Mediator.FNvForm := aForm as TNvForm;
  Mediator.FNvForm.FreeNotification(Mediator);
  Mediator.FNvForm.Designer := Mediator;
end;

class function TNvMediator.FormClass: TComponentClass;
begin
  Result := TNVForm;
end;

procedure TNvMediator.GetBounds(AComponent: TComponent; out CurBounds: TRect);
var
  w: TControl;
begin
  if AComponent is TControl then
  begin
    w := TControl(AComponent);
    CurBounds := Bounds(w.Left, w.Top, w.Width, w.Height);
  end
  else
    inherited GetBounds(AComponent, CurBounds);
end;

procedure TNvMediator.InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
begin
  if (LCLForm = nil) or (not LCLForm.HandleAllocated) then exit;
  LCLIntf.InvalidateRect(LCLForm.Handle, @ARect, Erase);
end;

function TNvMediator.GetRoot: TComponent;
begin
  Result:= FNvForm;
end;

procedure TNvMediator.SelectComponent(Instance: TPersistent);
begin
  Designer.SelectOnlyThisComponent(Instance as TComponent);
end;

function TNvMediator.GetComponent(const aName: string): TComponent;
var
  i:integer;
begin
  Result:= FormEditingHook.FindComponentByName(aName);
  if Result = nil then
    for i:=0 to FNvForm.ComponentCount -1 do
      begin
        if FNvForm.Components[i].Name = aName then
          begin
            Result:= FNvForm.Components[i];
            Exit;
          end;
      end;
end;

function TNvMediator.CreateComponent(ComponentClass: TComponentClass;
  Parent: TComponent; Left, Top, Width, Height: Integer): TComponent;
var
  DisableAutoSize:boolean;
begin
  DisableAutoSize:=true;
  Result:=FormEditingHook.CreateComponent(Parent, ComponentClass, '', Left, Top, Width, Height,
    DisableAutoSize);
  if Result <> nil then
    begin
      if DisableAutoSize and (Result is TControl) then
          TControl(Result).EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TNvMediator.CreateComponent'){$ENDIF};
      Designer.PropertyEditorHook.PersistentAdded(Result,true);
    end;
end;

procedure TNvMediator.Edit(const Component: TComponent);
begin
  Designer.InvokeComponentEditor(Component);
end;

procedure TNvMediator.Modified;
begin
  Designer.Modified;
end;

procedure TNvMediator.ResetDesignPage;
begin
  THackPage(FPage as TNvPage).FControlsOrdered.Clear;
  THackPage(FPage as TNvPage).FControlsOrdered.Add(FNvForm);
end;

function TNvMediator.UniqueName(const BaseName: string): string;
begin
  Result:= Designer.CreateUniqueComponentName(BaseName);
end;

function TNvMediator.Page: INVPage;
begin
  Result:= FPage;
end;

procedure TNvMediator.GetObjInspNodeImageIndex(APersistent: TPersistent;
  var AIndex: integer);
begin
  if Assigned(APersistent) then
  begin
    if (APersistent is TNvWinControl)
    { and (TNvWinControl(APersistent).AcceptChildrenAtDesignTime)} then
      AIndex := FormEditingHook.GetCurrentObjectInspector.ComponentTree.ImgIndexBox
    else
    if (APersistent is TNvControl) then
      AIndex := FormEditingHook.GetCurrentObjectInspector.ComponentTree.ImgIndexControl
    else
      inherited;
  end;
end;

procedure TNvMediator.SetBounds(AComponent: TComponent; NewBounds: TRect);
begin
  if AComponent is TControl then
  begin
    TControl(AComponent).SetBounds(NewBounds.Left, NewBounds.Top,
      NewBounds.Right - NewBounds.Left, NewBounds.Bottom - NewBounds.Top);
  end
  else
    inherited SetBounds(AComponent, NewBounds);

  if AComponent = FNvForm then
     (Screen as TNvScreenBrowser).ResizeBrowser(FNvForm.ClientHeight, FNvForm.ClientWidth);
end;

procedure TNvMediator.GetClientArea(AComponent: TComponent;
  out CurClientArea: TRect; out ScrollOffset: TPoint);
var
  Widget: TControl;
begin
  if AComponent is TControl then
  begin
    Widget := TControl(AComponent);

    CurClientArea :=  Widget.ClientRect;
    // CurClientArea := Rect(Widget.BorderLeft, Widget.BorderTop,
     // Widget.Width - Widget.BorderRight,
     // Widget.Height - Widget.BorderBottom);
    ScrollOffset := Point(0, 0);
  end
  else
    inherited GetClientArea(AComponent, CurClientArea, ScrollOffset);
end;

procedure TNvMediator.Paint;

begin
  (Screen as TNvScreenBrowser).CopyScreen(LCLForm.Canvas.Handle);
  inherited Paint;
end;

function TNvMediator.ComponentIsIcon(AComponent: TComponent): boolean;
begin
  Result := not (AComponent is TNvControl) and not (AComponent is TNvWinControl);
end;

function TNvMediator.ParentAcceptsChild(Parent: TComponent;
  Child: TComponentClass): boolean;
begin
  Result := (Parent is TNvWinControl) and (Child.InheritsFrom(TNvControl) or Child.InheritsFrom(TNvWinControl));
end;


end.
