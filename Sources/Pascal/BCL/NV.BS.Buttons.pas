unit NV.BS.Buttons;

interface

uses
  Classes, Controls, NV.Interfaces, NV.BS.Containers, NV.Ajax, NV.Json, NV.BS.Types, NV.VCL.Images,
  NV.BS.HtmlControls;

type
  TBsButtonVariant = (bsbInfo, bsbPrimary, bsbSecondary, bsbSuccess, bsbDanger, bsbWarning, bsbDark,
    bsbLight, bsbLink);
  TBsButtonSize = (bsbsDefault, bsbsSm, bsbsLg);

  TNvBsButton = class(TNvBsGridContainer)
  protected
    class function DefaultHtmlTag: string; override;
    class function SupportImage: Boolean; override;
    class function DefaultDataToggle: TNvBsDataToggle; virtual;
    class function DefaultClassCss: string; override;
    class function DefaultRenderText: Boolean; override;
  private
    FVariant    : TBsButtonVariant;
    FOutline    : Boolean;
    FTransparent: Boolean;
    FSize       : TBsButtonSize;
    FDataToggle : TNvBsDataToggle;
    FDataTarget : INvControl;
    procedure SetVariant(const Value: TBsButtonVariant);
    procedure SetSize(const Value: TBsButtonSize);
    procedure SetDataToggle(const Value: TNvBsDataToggle);
    procedure SetDataTarget(const Value: INvControl);
    procedure SetOutline(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
  protected
    // render
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderVariant(aJson: TJsonObject); dynamic;
    procedure RenderOutline(aJson: TJsonObject); dynamic;
    procedure RenderTransparent(aJson: TJsonObject); dynamic;
    procedure RenderSize(aJson: TJsonObject); dynamic;
    procedure RenderDataToggle(aJson: TJsonObject); dynamic;
    procedure RenderDataTarget(aJson: TJsonObject); dynamic;
    //
    function IsNotDefaultDataToggle: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  published
    property Border;
    property DataToggle: TNvBsDataToggle read FDataToggle write SetDataToggle
      stored IsNotDefaultDataToggle;
    property DataTarget: INvControl read FDataTarget write SetDataTarget;
    property ImageListLink;
    property Shadow default bssNone;
    property Size: TBsButtonSize read FSize write SetSize default bsbsDefault;
    property Action;
    property Caption;
    property CaptionVisible;
    property ClassCss;
    property Grids;
    property Outline: Boolean read FOutline write SetOutline default False;
    property Position;
    property TextProps;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Variant    : TBsButtonVariant read FVariant write SetVariant default bsbPrimary;
    property Visible;
    property Width_;
    property OnClick;
  end;

  TNvBsButtonGroup = class(TNvBsGridContainer)
  protected
    class function DefaultClassCss: string; override;
  private
    FVertical: Boolean;
    FSize    : TBsButtonSize;
    procedure SetSize(const Value: TBsButtonSize);
    procedure SetVertical(const Value: Boolean);
  protected
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderSize(aJson: TJsonObject);
    procedure RenderVertical(aJson: TJsonObject);
  published
    property Size    : TBsButtonSize read FSize write SetSize default bsbsDefault;
    property Vertical: Boolean read FVertical write SetVertical;
    property Grids;
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width_;
  end;

  TNvBsButtonToolbar = class(TNvBsGridContainer)
  protected
    class function DefaultClassCss: string; override;
  private
    FSize: TBsButtonSize;
    procedure SetSize(const Value: TBsButtonSize);
    procedure RenderSize(aJson: TJsonObject);
  protected
    procedure InternalRender(Json: TJsonObject); override;
  published
    property Size: TBsButtonSize read FSize write SetSize default bsbsDefault;
    property ClassCss;
    property Grids;
    property Background stored IsNotBgDefault;
    property Border;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width_;
  end;

  TNvBsButtonDropdown = class(TNvBsButton)
  protected
    class function DefaultClassCss: string; override;
    class function DefaultDataToggle: TNvBsDataToggle; override;
  end;

  TNvBsDropdownContainer = class(TNvBsCustomDropdownContainer)
  published
    property AlignStart;
    property AlignEnd;
    property ClassCss;
    property Position;
    property Width_;
  end;

  TNvBsDropDown = class(TNvBsGridContainer)
  private
    FButton: TNvBsButtonDropdown;
    FMenu  : TNvBsDropdownContainer;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    function GetOnShow: TNotifyEvent;
    procedure SetOnShow(const Value: TNotifyEvent);
    function GetOnHide: TNotifyEvent;
    procedure SetOnHide(const Value: TNotifyEvent);
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Button: TNvBsButtonDropdown read FButton;
    property ClassCss;
    property Grids;
    property Menu: TNvBsDropdownContainer read FMenu;
    property Position;
    property Width_;
    property OnShow: TNotifyEvent read GetOnShow write SetOnShow;
    property OnHide: TNotifyEvent read GetOnHide write SetOnHide;
  end;

const
  TBsButtonVariantStr: array [Low(TBsButtonVariant) .. High(TBsButtonVariant)] of string = //
    ('info', 'primary', 'secondary', 'success', 'danger', 'warning', 'dark', 'light', 'link');

  TBsButtonSizeStr: array [Low(TBsButtonSize) .. High(TBsButtonSize)] of string = //
    ('', 'sm', 'lg');

implementation

uses
  Windows, NV.VCL.Forms;

{ TNvBsAlert }

class function TNvBsButton.DefaultClassCss: string;
begin
  Result := 'btn btn-primary';
end;

class function TNvBsButton.DefaultDataToggle: TNvBsDataToggle;
begin
  Result := bsdtNull;
end;

class function TNvBsButton.DefaultHtmlTag: string;
begin
  Result := 'button';
end;

class function TNvBsButton.DefaultRenderText: Boolean;
begin
  Result := True;
end;

procedure TNvBsButton.Click;
begin
  inherited;
end;

constructor TNvBsButton.Create(AOwner: TComponent);
begin
  inherited;
  FVariant    := bsbPrimary;
  FDataToggle := DefaultDataToggle;
end;

procedure TNvBsButton.InternalRender(Json: TJsonObject);
begin
  inherited;
  if FOutline then
    RenderOutline(Json);
  if FVariant <> bsbInfo then
    RenderVariant(Json);
  if FTransparent then
    RenderTransparent(Json);
  if FSize <> bsbsDefault then
    RenderSize(Json);
  if IsNotDefaultDataToggle then
    RenderDataToggle(JSon);
  if FDataTarget <> nil then
    RenderDataTarget(Json);
end;

function TNvBsButton.IsNotDefaultDataToggle: Boolean;
begin
  Result := FDataToggle <> DefaultDataToggle;
end;

procedure TNvBsButton.RenderDataTarget(aJson: TJsonObject);
begin
  if FDataTarget <> nil then
    aJson.S['DataTarget'] := '#' + FDataTarget.ID
  else
    aJson.S['DataTarget'] := '';
end;

procedure TNvBsButton.RenderDataToggle(aJson: TJsonObject);
begin
  aJson.S['DataToggle'] := TBsButtonDataToggleStr[FDataToggle];
end;

procedure TNvBsButton.RenderOutline(aJson: TJsonObject);
begin
  aJson.B['Outline'] := FOutline;
end;

procedure TNvBsButton.RenderSize(aJson: TJsonObject);
begin
  aJson.S['Size'] := TBsButtonSizeStr[FSize];
end;

procedure TNvBsButton.RenderTransparent(aJson: TJsonObject);
begin
  aJson.B['Transparent'] := FTransparent;
end;

procedure TNvBsButton.RenderVariant(aJson: TJsonObject);
begin
  aJson.S['Variant'] := TBsButtonVariantStr[FVariant];
end;

procedure TNvBsButton.SetDataTarget(const Value: INvControl);
begin
  if FDataTarget <> Value then
    begin
      EnqueueChange('DataTarget', RenderDataTarget);
      FDataTarget := Value;
      Invalidate;
    end;
end;

procedure TNvBsButton.SetDataToggle(const Value: TNvBsDataToggle);
begin
  if Value <> FDataToggle then
    begin
      EnqueueChange('DataToggle', RenderDataToggle);
      FDataToggle := Value;
      Invalidate;
    end;
end;

procedure TNvBsButton.SetOutline(const Value: Boolean);
begin
  if Value <> FOutline then
    begin
      EnqueueChange('Outline', RenderOutline);
      FOutline := Value;
      Invalidate;
    end;
end;

procedure TNvBsButton.SetSize(const Value: TBsButtonSize);
begin
  if Value <> FSize then
    begin
      EnqueueChange('Size', RenderSize);
      FSize := Value;
      Invalidate;
    end;
end;

procedure TNvBsButton.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
    begin
      EnqueueChange('Transparent', RenderTransparent);
      FTransparent := Value;
      Invalidate;
    end;
end;

procedure TNvBsButton.SetVariant(const Value: TBsButtonVariant);
begin
  if Value <> FVariant then
    begin
      EnqueueChange('Variant', RenderVariant);
      FVariant := Value;
      Invalidate;
    end;
end;

class function TNvBsButton.SupportImage: Boolean;
begin
  Result := True;
end;

{ TNvBsButtonGroup }

class function TNvBsButtonGroup.DefaultClassCss: string;
begin
  Result := 'btn-group';
end;

procedure TNvBsButtonGroup.InternalRender(Json: TJsonObject);
begin
  inherited;
  if FSize <> bsbsDefault then
    RenderSize(Json);
  if FVertical then
    RenderVertical(Json);
end;

procedure TNvBsButtonGroup.RenderSize(aJson: TJsonObject);
begin
  aJson.S['Size'] := TBsButtonSizeStr[FSize]
end;

procedure TNvBsButtonGroup.RenderVertical(aJson: TJsonObject);
begin
  aJson.B['Vertical'] := FVertical
end;

procedure TNvBsButtonGroup.SetSize(const Value: TBsButtonSize);
begin
  if Value <> FSize then
    begin
      EnqueueChange('Size', RenderSize);
      FSize := Value;
      Invalidate;
    end;
end;

procedure TNvBsButtonGroup.SetVertical(const Value: Boolean);
begin
  if Value <> FVertical then
    begin
      EnqueueChange('Vertical', RenderVertical);
      FVertical := Value;
      Invalidate;
    end;
end;

{ TNvBsButtonToolbar }

class function TNvBsButtonToolbar.DefaultClassCss: string;
begin
  Result := 'btn-toolbar';
end;

procedure TNvBsButtonToolbar.InternalRender(Json: TJsonObject);
begin
  inherited;
  if FSize <> bsbsDefault then
    RenderSize(Json);
end;

procedure TNvBsButtonToolbar.RenderSize(aJson: TJsonObject);
begin
  aJson.S['Size'] := TBsButtonSizeStr[FSize]
end;

procedure TNvBsButtonToolbar.SetSize(const Value: TBsButtonSize);
begin
  if Value <> FSize then
    begin
      EnqueueChange('Size', RenderSize);
      FSize := Value;
      Invalidate;
    end;
end;

{ TNvBsDropDown }

procedure TNvBsDropDown.Click;
begin
  inherited;
  if (csDesigning in ComponentState) then
    begin
      if NeedSendChange then
        Screen.Ajax.AddCallFunction(FButton.ID, 'Toggle', '');
      Invalidate;
    end;
end;

procedure TNvBsDropDown.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  if (csDesigning in ComponentState) then
    Message.Result := HTCLIENT;
end;

constructor TNvBsDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FButton := TNvBsButtonDropdown.Create(Self);
  FButton.SetSubComponent(True, 'Button');
  FMenu := TNvBsDropdownContainer.Create(Self);
  FMenu.SetSubComponent(True, 'Menu');
end;

function TNvBsDropDown.GetOnHide: TNotifyEvent;
begin
  Result := FMenu.OnHide;
end;

function TNvBsDropDown.GetOnShow: TNotifyEvent;
begin
  Result := FMenu.OnShow;
end;

procedure TNvBsDropDown.SetOnHide(const Value: TNotifyEvent);
begin
  FMenu.OnHide := Value;
end;

procedure TNvBsDropDown.SetOnShow(const Value: TNotifyEvent);
begin
  FMenu.OnShow := Value;
end;

{ TButtonDropdown }

class function TNvBsButtonDropdown.DefaultClassCss: string;
begin
  Result := 'btn dropdown-toggle';
end;

class function TNvBsButtonDropdown.DefaultDataToggle: TNvBsDataToggle;
begin
  Result := bsdtdropdown;
end;

end.
