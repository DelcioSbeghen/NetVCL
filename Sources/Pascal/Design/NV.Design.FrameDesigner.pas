unit NV.Design.FrameDesigner;

interface

uses Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, DesignIntf, DesignEditors,
  DesignWindows, Menus, ComCtrls, ToolWin, Buttons, ActnList, NV.Controls,
  DesignerTypes, NV.VCL.Page, NV.Design.ModuleDesigner, NV.VCL.Frame;

type
  TNvFrameDesigner = class(TNvModuleDesigner, IHostForm)
  private
//    FDesigner      : IDesigner;
//    FLayoutComplete: Boolean;
    FPage:TNVPage;
    function GetModule: TNVBaseFrame;
//    procedure ReadLayout;
//    procedure SaveLayout;
    //procedure SetDesigner(const Value: IDesigner); overload;
    { Private declarations }
  protected
  procedure DropDesigningInComponentState; override;
    //procedure Activated; override;
   // procedure CreateParams(var Params: TCreateParams); override;
   // procedure KeyPress(var Key: Char); override;
    //procedure Resize; override;
   // procedure WMMove(var Message: TWMMove); message WM_MOVE;
  public
   // ComponentContainer: TNvDesignPanel;
    constructor CreateEx(AOwner: TComponent; const ADesigner: IDesigner;
      out AComponentContainer: TWinControl);
    destructor Destroy; override;
   // procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); override;
   // procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); override;
   // procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean); override;
   // procedure ItemsModified(const ADesigner: IDesigner); override;
    //function EditAction(Action: TEditAction): Boolean; override;
    //function GetEditState: TEditState; override;
    { IcxWebModuleDesignerNotify }
   // procedure SelectionChanged(const ADesigner: IDesigner;
    //  const ASelection: IDesignerSelections); override;
    //property Designer: IDesigner read FDesigner write SetDesigner;
    property Module: TNVBaseFrame read GetModule;
   //IHostForm
       procedure BringToFront;
    procedure CheckPosChanged;
    procedure DeinitializeDesigner(ARoot: TComponent);
    function GetCanPrint: Boolean;
    function GetCaption: string;
    function GetDesignerState: TDesignerState;
    function GetFont: TPersistent;
    function GetForm: TComponent;
    function GetFormImage: TObject;
    function GetScrollPos(Horiz: Boolean): Integer;
    function GetVisible: Boolean;
    function GetWindowState: TShowState;
    procedure HideWindow;
    function IsMenuKey(var Message: TWMKey): Boolean;
    procedure SetCaption(const ACaption: string);
    procedure SetDesigner(const ADesigner: IInterface); overload;
    procedure SetDesigning(DesignMode: Boolean);
    procedure Show;
    procedure ShowWindow(AShowState: TShowState);
    procedure SetFormDefaults(ARoot: TComponent; const ARootName: string; X, Y: Integer; Scale: Boolean);
    procedure Unmodify;

    property Caption: string read GetCaption write SetCaption;
    property Visible: Boolean read GetVisible;
  end;

   // var
  // goObjectTreeEditorForm: TNvFrameDesigner;

implementation

uses Dialogs, TypInfo;

{$R *.dfm}


type

 THackComponent = class(TComponent)
  end;

{ TcxWebModuleDesignWindow }

procedure TNvFrameDesigner.BringToFront;
begin

end;

procedure TNvFrameDesigner.CheckPosChanged;
begin

end;

constructor TNvFrameDesigner.CreateEx(AOwner: TComponent; const ADesigner: IDesigner;
  out AComponentContainer: TWinControl);
begin
  FPage:= TNVPage.Create(nil);
  InsertComponent(FPage);
  Designer := ADesigner;
  Module.Align:= alClient;
  FPage.InsertComponent(Module);
  Create(AOwner);
  DragKind:= dkDock;
  ComponentContainer :=  AComponentContainer as TNVModuleContainer;  //TNvDesignPanel.Create(Self, FPage{Module});
  with ComponentContainer do
    begin
      Left   := 0;
      Top    := 0;
      Width  := 605;
      Height := 418;
      Align := alClient;
      BevelInner  := bvLowered;
      BevelOuter  := bvNone;
      BorderStyle := bsNone;
      TabOrder    := 0;
      Parent:= Self;
    end;
  SetDesigning(True);
  KeyPreview := True;
  THackComponent(ComponentContainer).SetDesigning(True);
  AComponentContainer := ComponentContainer;
  ComponentContainer.Show;
end;

destructor TNvFrameDesigner.Destroy;
begin
  FPage.Free;
  inherited;
end;

function TNvFrameDesigner.IsMenuKey(var Message: TWMKey): Boolean;
begin

end;

//procedure TNvFrameDesigner.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
//begin
//end;

//procedure TNvFrameDesigner.Activated;
//begin
//  // Designer.Activate;
//  // Windows.SetFocus(DesignerPanel.Handle);
//  // TODO -> set Current OT component to ModuleManager
//end;

//procedure TNvFrameDesigner.CreateParams(var Params: TCreateParams);
//begin
//  inherited;
//  // commenting this line makes things even worse
//  Params.WndParent := Application.MainForm.Handle;
//end;

procedure TNvFrameDesigner.DeinitializeDesigner(ARoot: TComponent);
begin

end;

//procedure TNvFrameDesigner.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
//begin
//  if ADesigner = Designer then
//    begin
//      Destroying;
//      Release;
//    end;
//  // Close;
//end;

{ IDesignNotification }

//procedure TNvFrameDesigner.DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
//begin
//  if ADesigner = Designer then
//    begin
//      TDesignWindow(Self).Visible := True;
//      if Module <> nil then
//        begin
//          Caption := Module.Name;
//          ReadLayout;
//          DropDesigningInComponentState;
//          // CurrentComponent:=RC;
//        end;
//    end;
//end;

procedure TNvFrameDesigner.DropDesigningInComponentState;
var
  I        : Integer;
  Component: TComponent;
begin
  for I := 0 to ComponentCount - 1 do
    begin
      Component := Components[I];
      if (Component <> Designer.GetRoot) and (Component <> ComponentContainer)
      and not (Component is TNVBasepage) then
        THackComponent(Component).SetDesigning(False);
    end;
end;

//procedure TNvFrameDesigner.ItemsModified(const ADesigner: IDesigner);
//begin
//end;

//function TNvFrameDesigner.EditAction(Action: TEditAction): Boolean;
//begin
//  Result := False;
//end;

function TNvFrameDesigner.GetCanPrint: Boolean;
begin

end;

function TNvFrameDesigner.GetCaption: string;
begin
  Result:= inherited Caption;
end;

function TNvFrameDesigner.GetDesignerState: TDesignerState;
begin
  Result := [dsVisible];
end;

//function TNvFrameDesigner.GetEditState: TEditState;
//begin
//  Result := [esCanUndo, esCanRedo, esCanCut, esCanCopy, esCanPaste,
//    esCanDelete, esCanEditOle, esCanPrint, esCanSelectAll, esCanCreateTemplate,
//    esCanElide];
//end;

function TNvFrameDesigner.GetFont: TPersistent;
begin
  Result:= inherited Font;
end;

function TNvFrameDesigner.GetForm: TComponent;
begin
  Result:= Self;
end;

function TNvFrameDesigner.GetFormImage: TObject;
begin
  Result:= nil;
end;

function TNvFrameDesigner.GetModule: TNVBaseFrame;
begin
  if Designer = nil then
    Result := nil
  else if Designer.Root is TNVBaseFrame then
    Result := TNVBaseFrame(Designer.Root)
  else
    Result := nil;
end;

function TNvFrameDesigner.GetScrollPos(Horiz: Boolean): Integer;
begin
  Result:= 0;
end;

function TNvFrameDesigner.GetVisible: Boolean;
begin
  Result:= inherited Visible;
end;

function TNvFrameDesigner.GetWindowState: TShowState;
begin
  Result:= ssNormal;
end;

procedure TNvFrameDesigner.HideWindow;
begin
  inherited Hide;
end;

//procedure TNvFrameDesigner.KeyPress(var Key: Char);
//begin
//  inherited KeyPress(Key);
//end;

//procedure TNvFrameDesigner.ReadLayout;
//const
//  VisibleModuleSize = 10;
//var
//  DesignOffset, DesignSize, SplitSize: TPoint;
//begin
//  if Module <> nil then
//    begin
//      { DesignOffset:=Module.DesignOffset;
//        DesignSize:=Module.DesignSize;
//        SplitSize:=Module.SplitSize;
//        if (DesignOffset.X + DesignSize.X < VisibleModuleSize) then
//        DesignOffset.X := VisibleModuleSize - DesignSize.X;
//        if (DesignOffset.Y + DesignSize.Y < VisibleModuleSize) then
//        DesignOffset.Y := VisibleModuleSize - DesignSize.Y;
//        if (Screen.Width - DesignOffset.X < VisibleModuleSize) then
//        DesignOffset.X := Screen.Width - VisibleModuleSize;
//        if (Screen.Height - DesignOffset.Y < VisibleModuleSize) then
//        DesignOffset.Y := Screen.Height - VisibleModuleSize;
//        SetBounds(DesignOffset.X,DesignOffset.Y,DesignSize.X,DesignSize.Y); }
//      with Module do
//        Self.SetBounds(Left, Top, Width, Height);
//      Application.ProcessMessages;
//      FLayoutComplete := True;
//    end;
//end;

//procedure TNvFrameDesigner.Resize;
//begin
//  inherited;
//  SaveLayout;
//end;

//procedure TNvFrameDesigner.SaveLayout;
//begin
//  if FLayoutComplete and (Module <> nil) then
//    { with Module do begin
//      DesignOffset:=Point(Left, Top);
//      DesignSize:=Point(Width,Height);
//      end; }
//    begin
//      Module.Left   := Self.Left;
//      Module.Top    := Self.Top;
//      Module.Width  := Self.Width;
//      Module.Height := Self.Height;
//    end;
//end;
//
//procedure TNvFrameDesigner.SelectionChanged(const ADesigner: IDesigner;
//  const ASelection: IDesignerSelections);
//begin
//end;

procedure TNvFrameDesigner.SetCaption(const ACaption: string);
begin

end;

//procedure TNvFrameDesigner.SetDesigner(const Value: IDesigner);
//begin
//  if FDesigner <> Value then
//    begin
//      FDesigner := Value;
//      // if (FDesigner <> nil) and (FComponentDesigner = nil) then
//      // begin
//      // FComponentDesigner := Designers.DesignerFromExtension(Designer.DesignerExtention);
//      // SetBounds(200, ComponentDesigner. Environment.GetMainWindowSize.Bottom + 2, Width, Height);
//      // end;
//    end;
//end;

procedure TNvFrameDesigner.SetDesigner(const ADesigner: IInterface);
begin
if FDesigner <> ADesigner then
    begin
      FDesigner := ADesigner as IDesigner ;
    end;
end;

procedure TNvFrameDesigner.SetDesigning(DesignMode: Boolean);
begin
  inherited SetDesigning(DesignMode);
end;

procedure TNvFrameDesigner.SetFormDefaults(ARoot: TComponent;
  const ARootName: string; X, Y: Integer; Scale: Boolean);
begin

end;

procedure TNvFrameDesigner.Show;
begin
  inherited Show;
end;

procedure TNvFrameDesigner.ShowWindow(AShowState: TShowState);
begin
  //inherited ShowWindow(AShowState);
end;

procedure TNvFrameDesigner.Unmodify;
begin

end;

//procedure TNvFrameDesigner.WMMove(var Message: TWMMove);
//begin
//  inherited;
//  SaveLayout;
//end;

function GetControlByHandle(AHandle: THandle): TWinControl;
begin
  Result := Pointer(GetProp(AHandle, PChar(Format('Delphi%8.8x', [GetCurrentProcessID]))));
end;

end.
