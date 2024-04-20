unit NV.VCL.Page;

interface

uses
  Classes, RTLConsts, Controls, Generics.Collections, NV.Controls, NV.Dispatcher, NV.Ajax,
  NV.Router, NV.Interfaces,
  NV.JSON;

type
  TNVPageClass = class of TNVBasepage;

  // base for all pages
  TNVBasepage = class(TNVModuleContainer, INvPage)
  private
    FAjax      : TNvAjax;
    FRouter    : TNVRouter;
    FRouteName : string;
    FDispatcher: TDispatch;
    FModalList : TList<TNvWinControl>;
    procedure SetRouteName(const Value: string);
    procedure SetDispatcher(const Value: TDispatch);
    function GetDispatcher: TDispatch;
  protected
    FCssFiles: TStringList;
    procedure CreateRouter; virtual;

    // to remove module from FControlsList in design
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure AddCssFile(aFile: string);
    procedure RemoveCssFile(aFile: string);
    procedure AddModal(aControl: TNvWinControl);
    procedure RemoveModal(aControl: TNvWinControl);
    procedure ProcessRequest(J: TJsonObject); override;
    procedure Render; override;
    procedure ReRender(Now: Boolean = True); override;
    function Ajax: TNvAjax;
    property CssFiles: TStringList read FCssFiles;
    property Router: TNVRouter read FRouter;
    property Dispatcher: TDispatch read GetDispatcher write SetDispatcher;
    property RouteName: string read FRouteName write SetRouteName;
  end;

  TNVPage = class(TNVBasepage)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Dispatcher;
    property RouteName;
    property Top;
    property Left;
    property Width;
    property Height;
  end;

implementation

uses
  SysUtils, NV.DispatcherPage, NV.VCL.Frame;

{ TNVBasePage }

procedure TNVBasepage.AddCssFile(aFile: string);
var
  _Index: Integer;
begin
  _Index := FCssFiles.IndexOf(aFile);
  if _Index = -1 then
    begin
      FCssFiles.Add(aFile);
      ReRender(Rendered);
    end;
end;

procedure TNVBasepage.AddModal(aControl: TNvWinControl);
begin
  FModalList.Add(aControl);
end;

procedure TNVBasepage.AfterConstruction;
begin
  inherited;
end;

function TNVBasepage.Ajax: TNvAjax;
begin
  Result := FAjax;
end;

constructor TNVBasepage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderPosition:= False;
  FModalList := TList<TNvWinControl>.Create;
  FAjax      := TNvAjax.Create(Self);
  FCssFiles  := TStringList.Create;
  CreateRouter;
  // FlowDesign:=True;
  // FRouteName:= Name;
  FRouteName := '/index.html';

  ControlStyle := ControlStyle + [csAcceptsControls];
  if ((ClassType <> TNVBasepage) and (ClassType <> TNVPage) //
    and not(csDesignInstance in ComponentState)) then
    begin
      if not InitInheritedComponent(Self, TNVBasepage) then
        raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
    end
  else
    begin
      Width  := 320;
      Height := 240;
    end;

  FDispatcher := TDispatchPage.Create(Self);

  Router.AddDefault(Dispatcher);
  Router.AddRoute(RouteName, Dispatcher);

  // FHTMLPage := TDWHTMLPage.Create(Self);
  // to avoid store FHTMLPage to dfm
  // if csDesigning in ComponentState then
  // begin
  // RemoveComponent(FHTMLPage);
  // end;
  // FHTMLPage.Name := 'HTMLPage';
end;

procedure TNVBasepage.CreateRouter;
begin
  FRouter := TNVPageRouter.Create(Self);
end;

destructor TNVBasepage.Destroy;
begin
  FRouter.Free;
  FCssFiles.Free;
  inherited;
  FreeAndNil(FAjax);
  FModalList.Free;
end;

function TNVBasepage.GetDispatcher: TDispatch;
begin
  Result := FDispatcher;
end;

procedure TNVBasepage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Remove only design instances
  if (csDesignInstance in AComponent.ComponentState) //
    and (Operation = opRemove) then
    FControlsOrdered.Remove(AComponent);
  inherited;
end;

procedure TNVBasepage.ProcessRequest(J: TJsonObject);
begin
  Ajax.BeginUpdate;
  try
    inherited;
  finally
    Ajax.EndUpdate;
  end;
end;

procedure TNVBasepage.RemoveCssFile(aFile: string);
var
  _Index: Integer;
begin
  _Index := FCssFiles.IndexOf(aFile);
  if _Index > -1 then
    begin
      FCssFiles.Add(aFile);
      ReRender(Rendered);
    end;
end;

procedure TNVBasepage.RemoveModal(aControl: TNvWinControl);
begin
  FModalList.Remove(aControl);
end;

procedure TNVBasepage.Render;
var
  I: Integer;
begin
  inherited Render;

  for I := 0 to FModalList.Count - 1 do
    FModalList[I].Render;
end;

procedure TNVBasepage.ReRender(Now: Boolean);
var
  I: Integer;
begin
  for I := 0 to FModalList.Count - 1 do
    FModalList[I].ReRender(Now);
  inherited;
end;

procedure TNVBasepage.SetDispatcher(const Value: TDispatch);
begin
  FDispatcher := Value;
end;

procedure TNVBasepage.SetRouteName(const Value: string);
begin
  FRouteName := Value;
end;

{ TNVPage }

constructor TNVPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

end.
