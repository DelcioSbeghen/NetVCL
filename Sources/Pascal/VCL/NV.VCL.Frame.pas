unit NV.VCL.Frame;

interface

uses
  NV.Controls, System.Classes, NV.Ajax, NV.JSON, NV.Types;

type

  TNvFrameClass = class of TnvFrame;

  // base for all Frames
  TNVBaseFrame = class(TNVModuleContainer)
  private
    FOnActivate: TNotifyEvent;
    FOnClose   : TNVCloseEvent;
    FOnShow    : TNotifyEvent;
  protected
    function WebClassType: string; override;
    procedure InternalRender(Ajax: TNvAjax; JSON: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show; virtual;
    // procedure AfterConstruction; override;
    procedure Render(Ajax: TNvAjax); override;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClose: TNVCloseEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property Caption;
  end;

  TnvFrame = class(TNVBaseFrame)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Top;
    property Left;
    property Width;
    property Height;
    property OnActivate;
    property OnShow;
    property OnClose;
  end;

implementation

uses
  NV.Router, VCL.Controls, System.RTLConsts;

{ TNVBaseFrame }

// procedure TNVBaseFrame.AfterConstruction;
// begin
// inherited;
//
// end;

constructor TNVBaseFrame.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csAcceptsControls];
  if ((ClassType <> TNVBaseFrame) and (ClassType <> TnvFrame)) and
    not(csDesignInstance in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TNVBaseFrame) then
        raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
    end
  else
    begin
      Width  := 320;
      Height := 240;
    end;
end;

destructor TNVBaseFrame.Destroy;
begin

  inherited;
end;

procedure TNVBaseFrame.InternalRender(Ajax: TNvAjax; JSON: TJsonObject);
begin
  inherited;
  // Attach panel to designer page in design mode
  if (csDesigning in ComponentState) and (Parent <> nil) and not(Parent is TNvWinControl) then
    JSON.S['Parent'] := 'NVRoot';
end;

procedure TNVBaseFrame.Render(Ajax: TNvAjax);
begin
  inherited;

end;

procedure TNVBaseFrame.Show;
begin
  inherited Show;
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

function TNVBaseFrame.WebClassType: string;
begin
  Result := 'TNVFrame';
end;

{ TNVFrame }

constructor TnvFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

end.
