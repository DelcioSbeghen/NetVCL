unit NV.BS.Buttons;

interface

uses
  Classes, NV.BS.Containers, NV.Ajax, NV.Json, NV.BS.Types, NV.VCL.Images;

type
  TBsButtonVariant = (bsbInfo, bsbPrimary, bsbSecondary, bsbSuccess, bsbDanger, bsbWarning, bsbDark,
    bsbLight, bsbLink);
  TBsButtonSize = (bsbsDefault, bsbsSm, bsbsLg);

  TNvBsButton = class(TNvBsGridContainer)
  protected
    class function DefaultHtmlTag: string; override;
    class function SupportImage: Boolean; override;
  private
    FVariant: TBsButtonVariant;
    FSize   : TBsButtonSize;
    FBlock  : Boolean;
    procedure SetVariant(const Value: TBsButtonVariant);
    procedure SetSize(const Value: TBsButtonSize);
    procedure SetBlock(const Value: Boolean);
  protected
    procedure AddIncludes(Ajax: TNvAjax); override;
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Block  : Boolean read FBlock write SetBlock;
    property Size   : TBsButtonSize read FSize write SetSize default bsbsDefault;
    property Variant: TBsButtonVariant read FVariant write SetVariant default bsbPrimary;
    property Caption;
    property Grids;
    // property Background stored IsNotBgDefault; conflict with variant
    property Border;
    property ImageListLink;
    property Shadow default bssNone;
    property TextProps;
    property OnClick;
  end;

  TNvBsButtonGroup = class(TNvBsGridContainer)
  private
    FVertical: Boolean;
    FSize    : TBsButtonSize;
    procedure SetSize(const Value: TBsButtonSize);
    procedure SetVertical(const Value: Boolean);
  protected
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); override;
  published
    property Size    : TBsButtonSize read FSize write SetSize default bsbsDefault;
    property Vertical: Boolean read FVertical write SetVertical;
    property Grids;
    property Background stored IsNotBgDefault;
    property Border;
    property Shadow default bssNone;
    property TextProps;
  end;

  TNvBsButtonToolbar = class(TNvBsGridContainer)
  private
    FSize: TBsButtonSize;
    procedure SetSize(const Value: TBsButtonSize);
  protected
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); override;
  published
    property Size: TBsButtonSize read FSize write SetSize default bsbsDefault;
    property Grids;
    property Background stored IsNotBgDefault;
    property Border;
    property Shadow default bssNone;
    property TextProps;
  end;

const
  TBsButtonVariantStr: array [Low(TBsButtonVariant) .. High(TBsButtonVariant)] of string = //
    ('info', 'primary', 'secondary', 'success', 'danger', 'warning', 'dark', 'light', 'link');

  TBsButtonSizeStr: array [Low(TBsButtonSize) .. High(TBsButtonSize)] of string = //
    ('', 'sm', 'lg');

implementation

{ TNvBsAlert }

procedure TNvBsButton.AddIncludes(Ajax: TNvAjax);
begin
  inherited;
  if Ajax <> nil then
    Ajax.AddInclude('nv.bs.buttons.js', reqModule, '/nv.bs.buttons.js');
end;

class function TNvBsButton.DefaultHtmlTag: string;
begin
  Result := 'button';
end;

constructor TNvBsButton.Create(AOwner: TComponent);
begin
  inherited;
  FVariant := bsbPrimary;
end;

procedure TNvBsButton.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  inherited;
  if FVariant <> bsbInfo then
    Json.S['Variant'] := TBsButtonVariantStr[FVariant];
  if FSize <> bsbsDefault then
    Json.S['Size'] := TBsButtonSizeStr[FSize];
  if FBlock then
    Json.B['Block'] := FBlock;
end;

procedure TNvBsButton.SetBlock(const Value: Boolean);
begin
  if Value <> FBlock then
    begin
      if NeedSendChange then
        ControlAjaxJson.B['Block'] := Value;
      FBlock                       := Value;
      Invalidate;
    end;
end;

procedure TNvBsButton.SetSize(const Value: TBsButtonSize);
begin
  if Value <> FSize then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Size'] := TBsButtonSizeStr[Value];
      FSize                       := Value;
      Invalidate;
    end;
end;

procedure TNvBsButton.SetVariant(const Value: TBsButtonVariant);
begin
  if Value <> FVariant then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Variant'] := TBsButtonVariantStr[Value];
      FVariant                       := Value;
      Invalidate;
    end;
end;

class function TNvBsButton.SupportImage: Boolean;
begin
  Result := True;
end;

{ TNvBsButtonGroup }

procedure TNvBsButtonGroup.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  inherited;
  if FSize <> bsbsDefault then
    Json.S['Size'] := TBsButtonSizeStr[FSize];
  if FVertical then
    Json.B['Vertical'] := FVertical;
end;

procedure TNvBsButtonGroup.SetSize(const Value: TBsButtonSize);
begin
  if Value <> FSize then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Size'] := TBsButtonSizeStr[Value];
      FSize                       := Value;
      Invalidate;
    end;
end;

procedure TNvBsButtonGroup.SetVertical(const Value: Boolean);
begin
  if Value <> FVertical then
    begin
      if NeedSendChange then
        ControlAjaxJson.B['Vertical'] := Value;
      FVertical                       := Value;
      Invalidate;
    end;
end;

{ TNvBsButtonToolbar }

procedure TNvBsButtonToolbar.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  inherited;
  if FSize <> bsbsDefault then
    Json.S['Size'] := TBsButtonSizeStr[FSize];
end;

procedure TNvBsButtonToolbar.SetSize(const Value: TBsButtonSize);
begin
  if Value <> FSize then
    begin
      if NeedSendChange then
        ControlAjaxJson.S['Size'] := TBsButtonSizeStr[Value];
      FSize                       := Value;
      Invalidate;
    end;
end;

end.
