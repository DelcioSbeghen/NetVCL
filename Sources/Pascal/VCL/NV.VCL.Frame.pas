unit NV.VCL.Frame;

interface

uses
  Messages, Controls, NV.Controls, Classes, NV.JSON, NV.Types;

type
  TNVCloseEvent = NV.Types.TNVCloseEvent;
  TNvFrameClass = class of TnvFrame;

  // base for all Frames
  TNVBaseFrame = class(TNVModuleContainer)
  private
    FOnActivate: TNotifyEvent;
    FOnClose   : TNVCloseEvent;
    FOnShow    : TNotifyEvent;
    FOnCreate  : TNotifyEvent;
    FOnDestroy : TNotifyEvent;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    function WebClassType: string; override;
    procedure InternalRender(JSON: TJsonObject); override;
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function HandleCreateException: Boolean; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); virtual;
    destructor Destroy; override;
    // procedure AfterConstruction; override;
    property Caption;
    property ImageListLink;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnClose: TNVCloseEvent read FOnClose write FOnClose;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TNVFrame = class(TNVBaseFrame)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ClassCss;
    property Top;
    property Left;
    property Width;
    property Height;
    property OnActivate;
    property OnCreate;
    property OnDestroy;
    property OnShow;
    property OnClose;
  end;

implementation

uses
  NV.Router, RTLConsts, NV.VCL.Forms;

{ TNVBaseFrame }

// procedure TNVBaseFrame.AfterConstruction;
// begin
// inherited;
//
// end;

procedure TNVBaseFrame.AfterConstruction;
begin
  inherited;
  DoCreate;
end;

procedure TNVBaseFrame.BeforeDestruction;
begin
  inherited;
  DoDestroy;
end;

procedure TNVBaseFrame.CMVisibleChanged(var Message: TMessage);
begin
  inherited;

  if (Message.WParam = ord(True)) then
    try
      if Assigned(FOnShow) then
        FOnShow(Self);
    except
      Application.HandleException(Self);
    end;
end;

constructor TNVBaseFrame.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);

  if ((ClassType <> TNVBaseFrame) and (ClassType <> TnvFrame)) and
    not(csDesignInstance in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TNVBaseFrame) then
        raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
    end;
end;

constructor TNVBaseFrame.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Width        := 320;
  Height       := 240;
  if Self is TNVForm then
    Screen.AddForm(TNVForm(Self))
  else
    Screen.AddFrame(Self);
end;

destructor TNVBaseFrame.Destroy;
begin
  if Screen <> nil then
    begin
      if Self is TNVForm then
        Screen.RemoveForm(TNVForm(Self))
      else
        Screen.RemoveFrame(Self);
    end;

  inherited;
end;

procedure TNVBaseFrame.DoCreate;
begin
  if Assigned(FOnCreate) then
    try
      FOnCreate(Self);
    except
      if not HandleCreateException then
        raise;
    end;
end;

procedure TNVBaseFrame.DoDestroy;
begin
  if Assigned(FOnDestroy) then
    try
      FOnDestroy(Self);
    except
      Application.HandleException(Self);
    end;
end;

function TNVBaseFrame.HandleCreateException: Boolean;
begin
  Application.HandleException(Self);
  Result := True;
end;

procedure TNVBaseFrame.InternalRender(JSON: TJsonObject);
begin
  inherited;
  // Attach panel to designer page in design mode
  if (csDesigning in ComponentState) and (Designer <> nil) and (Designer.Root = Self) then
    JSON.S['Parent'] := (Screen.Ajax.Page as TNvWinControl).ID;
end;


function TNVBaseFrame.WebClassType: string;
begin
  Result := 'TNVFrame';
end;

{ TNVFrame }

constructor TNVFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

end.
