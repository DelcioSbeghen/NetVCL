unit NV.BS.Inputs;

interface

uses
  Classes, SysUtils, Controls, Messages, Nv.Types, NV.Controls, NV.BS.Buttons, NV.BS.Controls,
  NV.BS.Containers,
  NV.Ajax, NV.Json, DB, Vcl.DBCtrls,
  Generics.Collections, NV.VCL.Images, NV.Interfaces, NV.VCL.ActnList;

type
  TNvRenderOrder    = NV.Types.TNvRenderOrder;
  TBsButtonVariant  = NV.BS.Buttons.TBsButtonVariant;
  TBsButtonSize     = NV.BS.Buttons.TBsButtonSize;
  TNvBsCustomLookup = class;
  TNvBsCustomImput  = class;
  TNvBsInputActions = class;

  TNvListSourceLink = class(TDataLink)
  private
    FDBLookupControl: TNvBsCustomLookup;
  protected
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetChanged; override;
  public
    constructor Create;
  end;

  // Action in dropdown inputs(Selects, Lists, Search)
  TNvBsInputAction = class(TCollectionItem)
  private
    FAction     : TNvCustomAction;
    FVariant    : TBsButtonVariant;
    FShowCaption: Boolean;
    FShowIcon   : Boolean;
    procedure SetAction(const Value: TNvCustomAction);
    procedure SetVariant(const Value: TBsButtonVariant);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetShowIcon(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    // procedure ProcessRequest(J: TJsonObject);
  public
    constructor Create(Collection: TCollection); override;
    // function GetData: string;
    function Collection: TNvBsInputActions; reintroduce;
  published
    property Action     : TNvCustomAction read FAction write SetAction;
    property Enabled    : Boolean read GetEnabled write SetEnabled;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property ShowIcon   : Boolean read FShowIcon write SetShowIcon default True;
    property Variant    : TBsButtonVariant read FVariant write SetVariant default bsbPrimary;
  end;

  // Actions in dropdown inputs(Selects, Lists, Search)
  TNvBsInputActions = class(TOwnedCollection)
  private
    function GetInputAction(Index: Integer): TNvBsInputAction;
    procedure SetInputAction(Index: Integer; const Value: TNvBsInputAction);
  protected
    procedure Update(Item: TCollectionItem); override;
    function Input: TNvBsCustomImput;
    property InputAction[Index: Integer]: TNvBsInputAction read GetInputAction
      write SetInputAction; default;
  public
    // function GetData: string;
  end;

  TNvBsCustomImput = class(TNvBsGridControl)
  protected
    class function DefaultClassCss: string; override;
    class function DefaultRenderText: Boolean; override;
  private
    FValue        : string;
    FDatalink     : TFieldDataLink;
    FMaxLength    : Integer;
    FReadOnly     : Boolean;
    FOnChange     : TNotifyEvent;
    FDelayedChange: Integer;
    procedure SetValue(const Value: string);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetMaxLength(const Value: Integer);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetDelayedChange(const Value: Integer);
  protected
    procedure ActionsUpdated; virtual;
    // RenderProps
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderValue(aJson: TJsonObject); dynamic;
    procedure RenderMaxLength(aJson: TJsonObject); dynamic;
    procedure RenderReadOnly(aJson: TJsonObject); dynamic;
    procedure RenderEvents(aJson: TJsonObject); override;
    procedure RenderDelayedChange(aJson: TJsonObject); dynamic;
    // Events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
    procedure DoChange(aEvent: TJsonObject);
    // DB only
    procedure ActiveChange(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    procedure InternalDataChange; virtual;
    procedure EditingChange(Sender: TObject); virtual;
    procedure UpdateData(Sender: TObject); virtual;
    procedure ResetMaxLength; virtual;
    procedure SetReadOnly(const Value: Boolean);
    function DataLinkIsValid: Boolean;
    procedure InternalSetValue(const Value: string); virtual;
    // properties
    property Value: string read FValue write SetValue;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property DelayedChange: Integer read FDelayedChange write SetDelayedChange default -1;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ValidateValue; virtual;
    function DataLink: TFieldDataLink;
  published
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TNvBsCustomLookup = class(TNvBsCustomImput)
  private
    FListFields      : TList<TField>;
    FListLink        : TNvListSourceLink;
    FKeyValue        : Variant;
    FKeyFieldName    : string;
    FListActive      : Boolean;
    FKeyField        : TField;
    FListField       : TField;
    FListFieldName   : string;
    FListFieldIndex  : Integer;
    FListDataChanging: Integer;
    FListInvalid     : Boolean;
    FActions         : TNvBsInputActions;
    function GetListSource: TDataSource;
    procedure SetKeyFieldName(const Value: string);
    procedure SetKeyValue(const Value: Variant);
    procedure SetListSource(const Value: TDataSource);
    procedure CheckNotCircular;
    procedure SetListFieldName(const Value: string);
    procedure SetActions(const Value: TNvBsInputActions);
  protected
    procedure UpdateData(Sender: TObject); override;
    procedure UpdateListFields; virtual;
    procedure KeyValueChanged; virtual;
    function LocateKey: Boolean; virtual;
    function LocateValue(Value: string): Boolean; virtual;
    procedure InternalSetValue(const Value: string); override;
    procedure InternalDataChange; override;
    procedure InvalidateList;
    // Render Props
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderItems(Json: TJsonObject); virtual;
    // Events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
    procedure DoExecuteAction(aEvent: TJsonObject);
    //
    property Actions: TNvBsInputActions read FActions write SetActions;
    property KeyField: string read FKeyFieldName write SetKeyFieldName;
    property KeyValue: Variant read FKeyValue write SetKeyValue;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex default 0;
    property ListFields: TList<TField> read FListFields;
    property ListLink: TNvListSourceLink read FListLink;
    property ListField: string read FListFieldName write SetListFieldName;
    property ListSource: TDataSource read GetListSource write SetListSource;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TNvBsCustomSelectInput = class(TNvBsCustomImput)
  private
    FItems  : TStrings;
    FValues : TStrings;
    FActions: TNvBsInputActions;
    procedure SetItems(const Value: TStrings);
    procedure SetValues(const Value: TStrings);
    procedure DoItemsChange(Sender: TObject);
    procedure SetActions(const Value: TNvBsInputActions);
  protected
    // Render Props
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderValue(aJson: TJsonObject); override;
    procedure RenderItems(Json: TJsonObject);
    // Events
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
    procedure DoExecuteAction(aEvent: TJsonObject);
    //
    function HasValues: Boolean; inline;
    procedure InternalSetValue(const Value: string); override;
    // props
    property Actions: TNvBsInputActions read FActions write SetActions;
    property Items: TStrings read FItems write SetItems;
    property Values: TStrings read FValues write SetValues;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Addpair(aItem, aValue: string);
    procedure ClearItems;
  end;

  TNvBsInput = class(TNvBsCustomImput)
  private
  protected
  public
  published
    property Background;
    property Border;
    property Caption;
    property CaptionVisible;
    property ClassCss;
    property DataField;
    property DataSource;
    property DelayedChange;
    property Enabled;
    property MaxLength;
    property Position;
    property ReadOnly;
    property Value;
    property Grids;
    property Shadow;
    property TextProps;
    property Width_;
  end;

  TCustomDate = class;

  TGetDateEvent = procedure(Sender: TCustomDate; var aDate: TDateTime) of object;

  TCustomDate = class(TCollectionItem)
  private
    FEndDate       : TDateTime;
    FCaption       : string;
    FStartDate     : TDateTime;
    FOnGetStartDate: TGetDateEvent;
    FOnGetEndDate  : TGetDateEvent;
    procedure SetCaption(const Value: string);
    procedure SetEndDate(const Value: TDateTime);
    procedure SetStartDate(const Value: TDateTime);
    function GetEndDate: TDateTime;
    function GetStartDate: TDateTime;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function Component: TComponent;
  published
    property Caption       : string read FCaption write SetCaption;
    property StartDate     : TDateTime read GetStartDate write SetStartDate;
    property EndDate       : TDateTime read GetEndDate write SetEndDate;
    property OnGetStartDate: TGetDateEvent read FOnGetStartDate write FOnGetStartDate;
    property OnGetEndDate  : TGetDateEvent read FOnGetEndDate write FOnGetEndDate;
  end;

  TCustomDates = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TCustomDate;
    procedure SetItem(Index: Integer; const Value: TCustomDate);
  public
    function Add: TCustomDate; reintroduce;
    property Items[Index: Integer]: TCustomDate read GetItem write SetItem; default;
  end;

  TNvBsInputDate = class(TNvBsInput)
  private
    FFormat     : string;
    FCustomDates: TCustomDates;
    procedure SetCustomDates(const Value: TCustomDates);
  protected
    FDate: TDateTime;
    procedure SetFormat(const Value: string);
    procedure SetDate(const Value: TDateTime); virtual;
    procedure InternalSetValue(const Value: string); override;
    // Render Props
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderFormat(aJson: TJsonObject); dynamic;
    procedure RenderCustomDates(aJson: TJsonObject); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CustomDates: TCustomDates read FCustomDates write SetCustomDates;
    property Date       : TDateTime read FDate write SetDate;
    property Format     : string read FFormat write SetFormat;
  end;

  TNvBsInputDateTime = class(TNvBsInput)
  end;

  TNvBsDateRange = class(TNvBsInputDate)
  private
    FEndDate: TDateTime;
    procedure SetEndDate(const Value: TDateTime);
  protected
    procedure SetDate(const Value: TDateTime); override;
    procedure InternalSetValue(const Value: string); override;
    // Render Props
    procedure InternalRender(Json: TJsonObject); override;
    // procedure RenderEndDate(aJson: TJsonObject); dynamic;
  published
    property StartDate: TDateTime read FDate write SetDate;
    property EndDate  : TDateTime read FEndDate write SetEndDate;
  end;

  TNvBsSelect = class(TNvBsCustomSelectInput)
  protected
  public
  published
    property Actions;
    property Background;
    property Border;
    property Caption;
    property CaptionVisible;
    property ClassCss;
    property DataField;
    property DataSource;
    property Enabled;
    property Items;
    property MaxLength;
    property Position;
    property ReadOnly;
    property Value;
    property Grids;
    property Shadow;
    property TextProps;
    property Values;
    property Width_;
  end;

  TNvBsMemo = class(TNvBsCustomImput)
  private
  protected
    procedure InternalDataChange; override;
  public
  published
    property Background;
    property Border;
    property Caption;
    property CaptionVisible;
    property ClassCss;
    property DataField;
    property DataSource;
    property DelayedChange;
    property MaxLength;
    property Position;
    property ReadOnly;
    property Value;
    property Grids;
    property Shadow;
    property TextProps;
    property Width_;
  end;

  TNvBsRange = class(TNvBsCustomImput)
  private
  protected
  public
  published
    property Background;
    property Border;
    property Caption;
    property CaptionVisible;
    property ClassCss;
    property DataField;
    property DataSource;
    property MaxLength;
    property Position;
    property ReadOnly;
    property Grids;
    property Shadow;
    property TextProps;
    property Width_;
  end;

  TNvBsCheckBoxState = (bscbUnchecked, bscbChecked, bscbGrayed);

  TNvBsCheckbox = class(TNvBsCustomImput)
  protected
    class function DefaultClassCss: string; override;
  private
    FValueCheck  : string;
    FValueUncheck: string;
    // FState       : TNvBsCheckBoxState;
    procedure SetChecked(const Value: Boolean);
    function IsNotDefaultValueUnChecked: Boolean;
    function IsNotDefaultValueChecked: Boolean;
    procedure SetValueCheck(const Value: string);
    procedure SetValueUncheck(const Value: string);
    function GetChecked: Boolean; inline;
    function IsStateStored: Boolean;
    procedure SetState(const Value: TNvBsCheckBoxState);
    function GetState: TNvBsCheckBoxState;
  protected
    procedure ActiveChange(Sender: TObject); override;
    procedure UpdateData(Sender: TObject); override;
    // RenderProps
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderValueChecked(aJson: TJsonObject); dynamic;
    procedure RenderValueUnChecked(aJson: TJsonObject); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    property Checked: Boolean read GetChecked write SetChecked;
  published
    property Background;
    property Border;
    property Caption;
    property CaptionVisible;
    property ClassCss;
    property DataField;
    property DataSource;
    property MaxLength;
    property Position;
    property ReadOnly;
    property Grids;
    property Shadow;
    property ValueChecked: string read FValueCheck write SetValueCheck
      stored IsNotDefaultValueChecked nodefault;
    property ValueUnchecked: string read FValueUncheck write SetValueUncheck
      stored IsNotDefaultValueUnChecked nodefault;
    // Deixar por ultimo
    property State: TNvBsCheckBoxState read GetState write SetState stored IsStateStored;
    property Value stored false;
    property Width_;
  end;

  TNvBsSwitch = class(TNvBsCheckbox)
  protected
    class function DefaultClassCss: string; override;
  end;

  TNvBsLookupSelect = class(TNvBsCustomLookup)
  public
    property ListFields;
    property KeyValue;
  published
    property Background;
    property Border;
    property Caption;
    property CaptionVisible;
    property ClassCss;
    property DataField;
    property DataSource;
    property Enabled;
    property KeyField;
    property ListField;
    property ListFieldIndex;
    property ListSource;
    property MaxLength;
    property Position;
    property ReadOnly;
    property Value;
    property Grids;
    property Shadow;
    property TextProps;
    property Width_;
  end;

  TNvBSInputAddon = class(TNvBsCustomControl)
  private
    FPosition: TNvRenderOrder;
    procedure SetPosition(const Value: TNvRenderOrder);
  protected
    // Render props
    procedure InternalRender(Json: TJsonObject); override;
    procedure RenderPosition(aJson: TJsonObject);
  published
    property Position: TNvRenderOrder read FPosition write SetPosition;
  end;

  TNvBsInputAddonIcon = class(TNvBSInputAddon)
  protected
    class function DefaultClassCss: string; override;
    class function SupportImage: Boolean; override;
  published
    property ImageListLink;
  end;

  TNvBsInputAddonText = class(TNvBSInputAddon)
  protected
    class function DefaultClassCss: string; override;
    class function DefaultRenderText: Boolean; override;
  published
    property Text;
  end;

  // action addon
  TNvBsInputAddonAction = class(TNvBSInputAddon)
  protected
    class function DefaultClassCss: string; override;
    class function SupportImage: Boolean; override;
  private
    FVariant: TBsButtonVariant;
    // FSize       : TBsButtonSize;
    FShowIcon: Boolean;
    // procedure SetSize(const Value: TBsButtonSize);
    procedure SetVariant(const Value: TBsButtonVariant);
    procedure SetShowIcon(const Value: Boolean);
  protected
    // Render props
    procedure InternalRender(Json: TJsonObject); override;
    // procedure RenderAction(aJson: TJsonObject);
    // procedure RenderCaption(aJson: TJsonObject);
    // procedure RenderImage(aJson: TJsonObject);
    procedure RenderVariant(aJson: TJsonObject);
    // procedure RenderEnabled(aJson: TJsonObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property CaptionVisible;
    property Enabled;
    // property Size       : TBsButtonSize read FSize write SetSize default bsbsDefault;
    property ShowIcon: Boolean read FShowIcon write SetShowIcon default True;
    property Variant : TBsButtonVariant read FVariant write SetVariant default bsbLight;
  end;

  // Container for input and addons https://getbootstrap.com/docs/5.3/forms/input-group/
  TNvBsInputGroup = class(TNvBsGridContainer)
  protected
    // class function DefaultClassCss: string; override;
  private
    procedure CMControlListChange(var Message: TMessage); message CM_CONTROLLISTCHANGE;
  published
    property Background;
    property Border;
    property ClassCss;
    property Grids;
    property Position;
    property Shadow;
    property TextProps;
    property Width_;
  end;

implementation

uses
  Variants, DBConsts, NV.VCL.Forms;

const
  SPECIFY_ADDON_TYPE_ERR = 'Need to use Addons.Addxxx to add correct addon type';

  { TNvBsInput }

  { TNvBsCustomImput }

procedure TNvBsCustomImput.ActionsUpdated;
begin
  // in descendants
end;

procedure TNvBsCustomImput.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

constructor TNvBsCustomImput.Create(AOwner: TComponent);
begin
  inherited;
  FDelayedChange := -1;
end;

procedure TNvBsCustomImput.DataChange(Sender: TObject);
var
  _OldUpdateData   : TNotifyEvent;
  _OldBlockReadSize: Integer;
begin
  if DataLinkIsValid and Assigned(DataLink.Field) then
    begin
      if not(csDesigning in ComponentState) then
        begin
          if (FDatalink.Field.DataType in [ftString, ftWideString]) and (MaxLength = 0) then
            MaxLength := FDatalink.Field.Size;
        end;

      // Temporary Disable Datalink UpdateData
      _OldUpdateData := FDatalink.OnUpdateData;
      try
        FDatalink.OnUpdateData := nil;

        InternalDataChange;
      finally
        FDatalink.OnUpdateData := _OldUpdateData;
      end;
    end;
end;

function TNvBsCustomImput.DataLink: TFieldDataLink;
begin
  if not Assigned(FDatalink) then
    begin
      FDatalink                 := TFieldDataLink.Create;
      FDatalink.Control         := Self;
      FDatalink.OnDataChange    := DataChange;
      FDatalink.OnEditingChange := EditingChange;
      FDatalink.OnUpdateData    := UpdateData;
      FDatalink.OnActiveChange  := ActiveChange;
    end;
  Result := FDatalink;
end;

procedure TNvBsCustomImput.InternalDataChange;
begin
  Value := FDatalink.Field.Text;
end;

class function TNvBsCustomImput.DefaultClassCss: string;
begin
  Result := 'form-group';
end;

class function TNvBsCustomImput.DefaultRenderText: Boolean;
begin
  Result := True;
end;

destructor TNvBsCustomImput.Destroy;
begin
  if Assigned(FDatalink) then
    FDatalink.Free;
  FDatalink := nil;
  inherited;
end;

procedure TNvBsCustomImput.DoChange(aEvent: TJsonObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNvBsCustomImput.EditingChange(Sender: TObject);
begin
  if Not(csDesigning in ComponentState) and DataLinkIsValid then
    ReadOnly := not FDatalink.Editing;
end;

function TNvBsCustomImput.GetDataField: string;
begin
  Result := DataLink.FieldName;
end;

function TNvBsCustomImput.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

procedure TNvBsCustomImput.InternalRender(Json: TJsonObject);
begin
  inherited;
  RenderValue(Json);
  if FMaxLength > 0 then
    RenderMaxLength(Json);
  if FReadOnly then
    RenderReadonly(JSon);
  if FDelayedChange > -1 then
    RenderDelayedChange(Json);
end;

procedure TNvBsCustomImput.InternalSetValue(const Value: string);
begin
  FValue := Value;
end;

function TNvBsCustomImput.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  if AEventName = 'change' then
    begin
      DoChange(aEvent);
      Result := True;
    end
  else
    Result := inherited;
end;

function TNvBsCustomImput.DataLinkIsValid: Boolean;
begin
  Result := (FDatalink <> nil) and (FDatalink.DataSource <> nil) and (FDatalink.FieldName <> '');
end;

procedure TNvBsCustomImput.RenderDelayedChange(aJson: TJsonObject);
begin
  aJson.I['DelayedChange'] := FDelayedChange;
end;

procedure TNvBsCustomImput.RenderEvents(aJson: TJsonObject);
begin
  inherited;
  if Assigned(FOnChange) then
    aJson.A['Events'].Add('change');
end;

procedure TNvBsCustomImput.RenderMaxLength(aJson: TJsonObject);
begin
  aJson.I['MaxLength'] := FMaxLength;
end;

procedure TNvBsCustomImput.RenderReadOnly(aJson: TJsonObject);
begin
  aJson.B['ReadOnly'] := FReadOnly
end;

procedure TNvBsCustomImput.RenderValue(aJson: TJsonObject);
begin
  aJson.S['Value'] := Value;
end;

procedure TNvBsCustomImput.ResetMaxLength;
var
  F: TField;
begin
  if (MaxLength > 0) and DataLinkIsValid then
    begin
      F := FDatalink.Field;
      if Assigned(F) and (F.DataType in [ftString, ftWideString]) and (F.Size = MaxLength) then
        MaxLength := 0;
    end;
end;

procedure TNvBsCustomImput.SetValue(const Value: string);
begin
  if Value <> FValue then
    begin
      InternalSetValue(Value);

      EnqueueChange('Value', RenderValue);

      if DataLinkIsValid then
        begin
          FDatalink.Modified;
          FDatalink.UpdateRecord;
        end;
      Invalidate;
    end;
end;

procedure TNvBsCustomImput.SetDataField(const Value: string);
begin
  if not(csDesigning in ComponentState) then
    ResetMaxLength;
  DataLink.FieldName := Value;
  EditingChange(nil);
end;

procedure TNvBsCustomImput.SetDataSource(const Value: TDataSource);
begin
  if not(DataLink.DataSourceFixed and (csLoading in ComponentState)) then
    DataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
  EditingChange(nil);
end;

procedure TNvBsCustomImput.SetDelayedChange(const Value: Integer);
begin
  if FMaxLength <> Value then
    begin
      EnqueueChange('DelayedChange', RenderDelayedChange);
      FDelayedChange := Value;
      Invalidate;
    end;
end;

procedure TNvBsCustomImput.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then
    begin
      EnqueueChange('MaxLength', RenderMaxLength);
      FMaxLength := Value;
      Invalidate;
    end;
end;

procedure TNvBsCustomImput.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  EnqueueChange('Events', RenderEvents);
end;

procedure TNvBsCustomImput.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
    begin
      EnqueueChange('ReadOnly', RenderReadOnly);
      FReadOnly := Value;
    end;
end;

procedure TNvBsCustomImput.UpdateData(Sender: TObject);
begin
  ValidateValue;
  FDatalink.Field.Text := Value;
end;

procedure TNvBsCustomImput.ValidateValue;
begin
  //
end;

{ TNvBsCustomSelectImput }

procedure TNvBsCustomSelectInput.Addpair(aItem, aValue: string);
begin
  FItems.Add(aItem);
  FValues.Add(aValue);
end;

procedure TNvBsCustomSelectInput.ClearItems;
begin
  FItems.Clear;
  FValues.Clear;
end;

constructor TNvBsCustomSelectInput.Create(AOwner: TComponent);
begin
  inherited;
  FItems                       := TStringList.Create;
  TStringList(FItems).OnChange := DoItemsChange;
  FValues                      := TStringList.Create;
  FActions                     := TNvBsInputActions.Create(Self, TNvBsInputAction);
end;

destructor TNvBsCustomSelectInput.Destroy;
begin
  FItems.Free;
  FValues.Free;
  FActions.Free;
  inherited;
end;

procedure TNvBsCustomSelectInput.DoExecuteAction(aEvent: TJsonObject);
var
  _I: Integer;
begin
  _I := aEvent.I['Action'];
  FActions[_I].FAction.Execute;
end;

procedure TNvBsCustomSelectInput.DoItemsChange(Sender: TObject);
begin
  EnqueueChange('Items', RenderItems);
  Invalidate;
end;

function TNvBsCustomSelectInput.HasValues: Boolean;
begin
  Result := (Values.Count > 0) and (Values.Count = Items.Count);
end;

procedure TNvBsCustomSelectInput.InternalRender(Json: TJsonObject);
begin
  inherited;
  RenderItems(Json);
end;

procedure TNvBsCustomSelectInput.InternalSetValue(const Value: string);
begin
  if FUpdatingFromRequest and HasValues and not Value.IsEmpty then
    FValue := Values[Items.IndexOf(Value)]
  else
    inherited;
end;

function TNvBsCustomSelectInput.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  if AEventName = 'exec-action' then
    begin
      DoExecuteAction(aEvent);
      Result := True;
    end
  else
    Result := inherited;
end;

procedure TNvBsCustomSelectInput.RenderItems(Json: TJsonObject);
var
  Arr        : TJsonArray;
  _I         : Integer;
  _ActionItem: TNvBsInputAction;
begin
  Arr := JSon.A['Items'];

  Arr.Clear;

  if Value.IsEmpty then
    with Arr.AddObject do
      begin
        s['Value']    := '';
        B['Selected'] := True;
        B['Disabled'] := True;
      end;

  for _I := 0 to FItems.Count - 1 do
    with Arr.AddObject do
      begin
        s['Value'] := FItems[_I];
        if Value = FItems[_I] then
          B['Selected'] := True;
      end;

  for _I := 0 to FActions.Count - 1 do
    with Arr.AddObject do
      begin
        _ActionItem := FActions[_I];
        if _ActionItem.Action <> nil then
          begin
            { TODO -oDelcio -Select Actions : Add Icon and other props of action }
            s['Value']  := _ActionItem.FAction.Caption;
            I['Action'] := _I;
          end;
      end;

end;

procedure TNvBsCustomSelectInput.RenderValue(aJson: TJsonObject);
var
  _Idx: Integer;
begin
  if HasValues and not FValue.IsEmpty then
    begin
      _Idx := FValues.IndexOf(FValue);
      if _Idx > -1 then
        aJson.S['Value'] := Items[_Idx]
      else
        aJson.S['Value'] := '';
    end
  else
    inherited;
end;

procedure TNvBsCustomSelectInput.SetActions(const Value: TNvBsInputActions);
begin
  if Value <> nil then
    FActions.Assign(Value);
end;

procedure TNvBsCustomSelectInput.SetItems(const Value: TStrings);
begin
  if FItems <> Value then
    begin
      EnqueueChange('Items', RenderItems);
      FItems.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvBsCustomSelectInput.SetValues(const Value: TStrings);
begin
  if FValues <> Value then
    begin
      FValues.Assign(Value);
    end;
end;

{ TNvBsSelect }

{ TNvBsMemo }

procedure TNvBsMemo.InternalDataChange;
begin
  if DataLinkIsValid and Assigned(DataLink.Field) and DataLink.Field.IsBlob then
    Value := DataLink.Field.AsString
  else
    inherited;
end;

{ TNvBsRange }

{ TNvBsCheckBox }

procedure TNvBsCheckbox.ActiveChange(Sender: TObject);
  procedure AdjustCheckUncheckedValues(const Checked, Unchecked: string);
  begin
    if not IsNotDefaultValueUnChecked then
      ValueChecked := Checked;
    if not IsNotDefaultValueUnChecked then
      ValueUnchecked := Unchecked;
  end;

begin
  inherited;
  if DataLinkIsValid and FDatalink.Active and (FDatalink.Field <> nil) then
    begin
      case FDatalink.Field.DataType of
        ftString, ftFixedChar, ftWideString, ftFixedWideChar: AdjustCheckUncheckedValues('V', 'F');
        ftSmallint, ftInteger, ftWord, ftLargeint, ftShortint, ftLongWord, ftByte:
            AdjustCheckUncheckedValues('1', '0');
        ftBoolean: AdjustCheckUncheckedValues(STextTrue, STextFalse);
      end;
    end;
end;

constructor TNvBsCheckbox.Create(AOwner: TComponent);
begin
  inherited;
  FValueCheck   := 'True';
  FValueUncheck := 'False';
end;

class function TNvBsCheckbox.DefaultClassCss: string;
begin
  Result := 'form-check';
end;

function TNvBsCheckbox.GetChecked: Boolean;
begin
  Result := Value = ValueChecked;
end;

function TNvBsCheckbox.GetState: TNvBsCheckBoxState;
begin
  if Value = ValueChecked then
    Result := bscbChecked
  else if Value = ValueUnChecked then
    Result := bscbUnChecked
  else
    Result := bscbGrayed;
end;

procedure TNvBsCheckbox.InternalRender(Json: TJsonObject);
begin
  // need to set before all
  if FValueCheck <> 'True' then
    RenderValueChecked(Json);
  if FValueUnCheck <> 'False' then
    RenderValueUnChecked(Json);
  inherited;
end;

function TNvBsCheckbox.IsStateStored: Boolean;
begin
  Result := not DataLinkIsValid;
end;

procedure TNvBsCheckbox.RenderValueChecked(aJson: TJsonObject);
begin
  aJson.S['ValueChecked'] := Value;
end;

procedure TNvBsCheckbox.RenderValueUnChecked(aJson: TJsonObject);
begin
  aJson.S['ValueUnChecked'] := Value;
end;

function TNvBsCheckbox.IsNotDefaultValueChecked: Boolean;
begin
  Result := not SameText(FValueCheck, 'True');
end;

function TNvBsCheckbox.IsNotDefaultValueUnChecked: Boolean;
begin
  Result := not SameText(FValueUncheck, 'False');
end;

procedure TNvBsCheckbox.SetChecked(const Value: Boolean);
begin
  if (Checked <> Value) or (State = bscbGrayed) then
    begin
      if Value then
        Self.Value := FValuecheck
      else
        Self.Value := FValueUncheck;
    end;
end;

procedure TNvBsCheckbox.SetState(const Value: TNvBsCheckBoxState);
begin
  case Value of
    bscbUnchecked: Self.Value := FValueUnCheck;
    bscbChecked: Self.Value   := FValueCheck;
    bscbGrayed: Self.Value    := '';
  end;
end;

procedure TNvBsCheckbox.SetValueCheck(const Value: string);
begin
  if Value <> FValueCheck then
    begin
      EnqueueChange('ValueChecked', RenderValueChecked);
      FValueCheck := Value;
    end;
end;

procedure TNvBsCheckbox.SetValueUncheck(const Value: string);
begin
  if Value <> FValueUncheck then
    begin
      EnqueueChange('ValueUnChecked', RenderValueUnChecked);
      FValueUncheck := Value;
    end;
end;

procedure TNvBsCheckbox.UpdateData(Sender: TObject);
begin
  if State = bscbGrayed then
    FDataLink.Field.Clear
  else if FDataLink.Field.DataType = ftBoolean then
    FDataLink.Field.AsBoolean := Checked
  else
    inherited;
end;

{ TNvBsCustomLookup }

procedure TNvBsCustomLookup.CheckNotCircular;
begin
  if FListLink.Active and FListLink.DataSet.IsLinkedTo(DataSource) then
    DatabaseError(SCircularDataLink);
end;

constructor TNvBsCustomLookup.Create(AOwner: TComponent);
begin
  inherited;
  FListLink                  := TNvListSourceLink.Create;
  FListLink.FDBLookupControl := Self;
  FListFields                := TList<TField>.Create;
  FKeyValue                  := Null;
  FActions                   := TNvBsInputActions.Create(Self, TNvBsInputAction);
end;

destructor TNvBsCustomLookup.Destroy;
begin
  FListFields.Free;
  FListLink.Free;
  FActions.Free;
  inherited;
end;

procedure TNvBsCustomLookup.DoExecuteAction(aEvent: TJsonObject);
var
  _I: Integer;
begin
  _I := aEvent.I['Action'];
  FActions[_I].FAction.Execute;
end;

function TNvBsCustomLookup.GetListSource: TDataSource;
begin
  Result := FListLink.DataSource;
end;

procedure TNvBsCustomLookup.InternalDataChange;
begin
  KeyValue := FDatalink.Field.Value;
end;

procedure TNvBsCustomLookup.InternalRender(Json: TJsonObject);
begin
  inherited;
  RenderItems(Json);
end;

procedure TNvBsCustomLookup.InternalSetValue(const Value: string);
begin
  if FListActive and LocateValue(Value) then
    KeyValue := FKeyField.Text
  else
    DatabaseError(SFieldOutOfRange);
end;

procedure TNvBsCustomLookup.InvalidateList;
begin
  if FListDataChanging > 0 then
    Exit;

  FListInvalid := True;

  if ListLink.Active and not ListLink.Editing then
    EnqueueChange('Items', RenderItems);

  KeyValueChanged;
end;

procedure TNvBsCustomLookup.KeyValueChanged;
begin
  if FListActive and LocateKey then
    FValue := FListField.DisplayText
  else
    FValue := '';

  EnqueueChange('Value', RenderValue);

  Invalidate;
end;

function TNvBsCustomLookup.LocateKey: Boolean;
var
  KeySave: Variant;
begin
  Result := False;
  try
    Inc(FListDataChanging);
    try
      KeySave := FKeyValue;
      if not VarIsNull(FKeyValue) and FListLink.DataSet.Active and
        FListLink.DataSet.Locate(FKeyFieldName, FKeyValue, []) then
        begin
          Result    := True;
          FKeyValue := KeySave;
        end;
    except
    end;
  finally
    Dec(FListDataChanging);
  end;
end;

function TNvBsCustomLookup.LocateValue(Value: string): Boolean;
begin
  Result := False;
  try
    Inc(FListDataChanging);
    try
      if FListLink.DataSet.Active and FListLink.DataSet.Locate(FListField.FieldName, Value, []) then
        Result := True;
    except
    end;
  finally
    Dec(FListDataChanging);
  end;
end;

function TNvBsCustomLookup.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  if AEventName = 'exec-action' then
    begin
      DoExecuteAction(aEvent);
      Result := True;
    end
  else
    Result := inherited;
end;

procedure TNvBsCustomLookup.RenderItems(Json: TJsonObject);
var
  Arr        : TJsonArray;
  I, J       : Integer;
  _Option    : string;
  _Dataset   : TDataSet;
  _LastActive: Integer;
begin
  Arr := JSon.A['Items'];

  Arr.Clear;

  if ListLink.Active and (FListFields.Count > 0) and Assigned(FKeyField) then
    begin
      _Dataset    := ListLink.Dataset;
      _LastActive := ListLink.ActiveRecord;
      for I       := 0 to _Dataset.RecordCount - 1 do
        begin
          ListLink.ActiveRecord := I;

          with Arr.AddObject do
            begin
              _Option := '';
              for J   := 0 to FListFields.Count - 1 do
                if _Option.IsEmpty then
                  _Option := FListFields[J].Text
                else
                  _Option := _Option + ' - ' + FListFields[J].DisplayText;

              s['Value'] := _Option;
              if FKeyValue = FKeyField.Text then
                B['Selected'] := True;
            end;
        end;
      ListLink.ActiveRecord := _LastActive;

      FListInvalid := False;
    end;
end;

procedure TNvBsCustomLookup.SetActions(const Value: TNvBsInputActions);
begin
  if Value <> nil then
    FActions.Assign(Value);
end;

procedure TNvBsCustomLookup.SetKeyFieldName(const Value: string);
begin
  if FKeyFieldName <> Value then
    begin
      FKeyFieldName := Value;
      UpdateListFields;
    end;
end;

function VarEquals(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;

procedure TNvBsCustomLookup.SetKeyValue(const Value: Variant);
begin
  if not VarEquals(FKeyValue, Value) then
    begin
      FKeyValue := Value;
      KeyValueChanged;
    end;
end;

procedure TNvBsCustomLookup.SetListFieldName(const Value: string);
begin
  if FListFieldName <> Value then
    begin
      FListFieldName := Value;
      UpdateListFields;
    end;
end;

procedure TNvBsCustomLookup.SetListSource(const Value: TDataSource);
begin
  FListLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TNvBsCustomLookup.UpdateData(Sender: TObject);
begin
  ValidateValue;
  FDatalink.Field.Value := KeyValue;
end;

procedure TNvBsCustomLookup.UpdateListFields;
var
  DataSet    : TDataSet;
  ResultField: TField;
begin
  FListActive := False;
  FKeyField   := nil;
  FListField  := nil;
  FListFields.Clear;

  if FListLink.Active and (FKeyFieldName <> '') then
    begin
      CheckNotCircular;
      DataSet   := FListLink.DataSet;
      FKeyField := GetFieldProperty(DataSet, Self, FKeyFieldName);
      try
        DataSet.GetFieldList(FListFields, FListFieldName);
      except
        DatabaseErrorFmt(SFieldNotFound, [FListFieldName], Self);
      end;

      if FListFields.Count = 0 then
        FListFields.Add(FKeyField);
      if (FListFieldIndex >= 0) and (FListFieldIndex < FListFields.Count) then
        FListField := FListFields[FListFieldIndex]
      else
        FListField := FListFields[0];

      FListActive := True;
    end;
end;

{ TNvListSourceLink }

constructor TNvListSourceLink.Create;
begin
  inherited Create;
  VisualControl := True;
end;

procedure TNvListSourceLink.DataSetChanged;
begin
  inherited;
  if FDBLookupControl <> nil then
    FDBLookupControl.InvalidateList;
  if DataSet <> nil then
    BufferCount := DataSet.RecordCount + 1;
end;

procedure TNvListSourceLink.ActiveChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.UpdateListFields;
  if DataSet <> nil then
    BufferCount := DataSet.RecordCount + 1;
end;

procedure TNvListSourceLink.LayoutChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.UpdateListFields;
end;

{ TNvBsInputDate }

constructor TNvBsInputDate.Create(AOwner: TComponent);
begin
  inherited;
  FCustomDates := TCustomDates.Create(Self, TCustomDate);

end;

procedure TNvBsInputDate.InternalRender(Json: TJsonObject);
begin
  inherited;
  if not FFormat.IsEmpty then
    RenderFormat(Json);

  if FCustomDates.Count > 0 then
    RenderCustomDates(Json);
end;

procedure TNvBsInputDate.InternalSetValue(const Value: string);
begin
  FDate  := StrToDate(Value);
  FValue := Value;
end;

procedure TNvBsInputDate.RenderCustomDates(aJson: TJsonObject);
var
  _JDates    : TJsonArray;
  _CustomDate: TCustomDate;
  I          : Integer;
begin
  _JDates := aJson.A['CustomDates'];
  for I   := 0 to FCustomDates.Count - 1 do
    begin
      _CustomDate := FCustomDates[I];
      with _JDates.AddObject do
        begin
          S['Name']      := _CustomDate.FCaption;
          S['StartDate'] := FormatDateTime('YYYY-MM-DD', _CustomDate.StartDate);
          S['EndDate']   := FormatDateTime('YYYY-MM-DD', _CustomDate.EndDate);
        end;
    end;
end;

procedure TNvBsInputDate.RenderFormat(aJson: TJsonObject);
begin
  aJson.O['Options'].S['format'] := FFormat
end;

procedure TNvBsInputDate.SetCustomDates(const Value: TCustomDates);
begin
  if FCustomDates <> Value then
    begin
      EnqueueChange('CustomDates', RenderCustomDates);
      FCustomDates.Assign(Value);
      Invalidate;
    end;
end;

procedure TNvBsInputDate.SetDate(const Value: TDateTime);
begin
  if Value <> FDate then
    Self.Value := DateToStr(Value);
end;

procedure TNvBsInputDate.SetFormat(const Value: string);
begin
  if Value <> FFormat then
    begin
      EnqueueChange('Format', RenderFormat);
      FFormat := Value;
      Invalidate;
    end;
end;

{ TNvBsDateRange }

procedure TNvBsDateRange.InternalRender(Json: TJsonObject);
begin
  inherited;
  // if FEndDate > 0 then
  // RenderEndDate(Json);
end;

procedure TNvBsDateRange.InternalSetValue(const Value: string);
var
  _Dates: TArray<string>;
begin
  _Dates   := Value.Split(['-']);
  FDate    := StrToDate(_Dates[0].Trim);
  FEndDate := StrToDate(_Dates[1].Trim);
  FValue   := Value;
end;

// procedure TNvBsDateRange.RenderEndDate(aJson: TJsonObject);
// begin
// aJson.D['EndDate'] := FEndDate;
// end;

procedure TNvBsDateRange.SetDate(const Value: TDateTime);
begin
  if FEndDate = 0 then
    inherited
  else
    Self.Value := DateToStr(Value) + ' - ' + DateToStr(FEndDate);
end;

procedure TNvBsDateRange.SetEndDate(const Value: TDateTime);
begin
  if Value <> FEndDate then
    begin
      // EnqueueChange('EndDate', RenderEndDate);

      { TODO -oDelcio -cTNvBsDateRange : Implementar EndDate Datalink }
      // if EndDataLinkIsValid then
      // begin
      // FEndDatalink.Modified;
      // FEndDatalink.UpdateRecord;
      // end;

      FEndDate   := Value;
      Self.Value := DateToStr(FDate) + ' - ' + DateToStr(Value);
      Invalidate;
    end;
end;

{ TCustomDates }

function TCustomDates.Add: TCustomDate;
begin
  Result := inherited Add as TCustomDate;
end;

function TCustomDates.GetItem(Index: Integer): TCustomDate;
begin
  Result := inherited Items[Index] as TCustomDate;
end;

procedure TCustomDates.SetItem(Index: Integer; const Value: TCustomDate);
begin
  inherited Items[Index] := Value;
end;

{ TCustomDateBase }

procedure TCustomDate.AssignTo(Dest: TPersistent);
begin
  with Dest as TCustomDate do
    begin
      FCaption   := Self.FCaption;
      FStartDate := Self.StartDate;
      FEndDate   := Self.FEndDate;
    end;

end;

function TCustomDate.Component: TComponent;
begin
  Result := Collection.Owner as TComponent;
end;

function TCustomDate.GetEndDate: TDateTime;
var
  _EndDate: TDateTime;
begin
  _EndDate := FEndDate;
  if not(csDesigning in Component.ComponentState) //
    and Assigned(FOnGetEndDate) then
    FOnGetEndDate(Self, _EndDate);

  Result := _EndDate;
end;

function TCustomDate.GetStartDate: TDateTime;
var
  _StartDate: TDateTime;
begin
  _StartDate := FStartDate;
  if not(csDesigning in Component.ComponentState) //
    and Assigned(FOnGetStartDate) then
    FOnGetStartDate(Self, _StartDate);

  Result := _StartDate;
end;

procedure TCustomDate.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TCustomDate.SetEndDate(const Value: TDateTime);
begin
  FEndDate := Value;
end;

procedure TCustomDate.SetStartDate(const Value: TDateTime);
begin
  FStartDate := Value;
end;

{ TNvBsInputIcon }

class function TNvBsInputAddonIcon.DefaultClassCss: string;
begin
  Result := 'input-group-text';
end;

class function TNvBsInputAddonIcon.SupportImage: Boolean;
begin
  Result := True;
end;

{ TNvBsInputText }

class function TNvBsInputAddonText.DefaultClassCss: string;
begin
  Result := 'input-group-text';
end;

class function TNvBsInputAddonText.DefaultRenderText: Boolean;
begin
  Result := True;
end;

{ TNvBSInputAddon }

procedure TNvBSInputAddon.InternalRender(Json: TJsonObject);
begin
  inherited;
  RenderPosition(Json);
end;

procedure TNvBSInputAddon.RenderPosition(aJson: TJsonObject);
begin
  aJson.S['Position'] := TNvRenderOrderStr[FPosition];
end;

procedure TNvBSInputAddon.SetPosition(const Value: TNvRenderOrder);
begin
  if FPosition <> Value then
    begin
      EnqueueChange('Position', RenderPosition);
      FPosition := Value;
      Invalidate;
    end;
end;

{ TNvBsInputAction }

constructor TNvBsInputAddonAction.Create(AOwner: TComponent);
begin
  inherited;
  FVariant  := bsbLight;
  FShowIcon := True;
end;

class function TNvBsInputAddonAction.DefaultClassCss: string;
begin
  Result := 'btn';
end;

procedure TNvBsInputAddonAction.InternalRender(Json: TJsonObject);
begin
  inherited;
  // if FAction <> nil then
  // RenderAction(Json);
  if FVariant <> bsbPrimary then
    RenderVariant(Json);
end;

// procedure TNvBsInputAddonAction.RenderAction(aJson: TJsonObject);
// begin
// RenderCaption(aJson);
// RenderEnabled(aJson);
// RenderImage(aJson);
// end;

// procedure TNvBsInputAddonAction.RenderCaption(aJson: TJsonObject);
// begin
// if FShowCaption and (FAction <> nil) then
// aJson.S['Caption'] := FAction.Caption
// else
// aJson.S['Caption'] := '';
// end;

// procedure TNvBsInputAddonAction.RenderEnabled(aJson: TJsonObject);
// begin
// aJson.B['Enabled'] := (FAction <> nil) and FAction.Enabled;
// end;

// procedure TNvBsInputAddonAction.RenderImage(aJson: TJsonObject);
// begin
// if FShowIcon and (FAction <> nil) then
// aJson.S['Image'] := FAction.ImageListLink.Render
// else
// aJson.S['Image'] := '';
// end;

procedure TNvBsInputAddonAction.RenderVariant(aJson: TJsonObject);
begin
  aJson.S['Variant'] := TBsButtonVariantStr[FVariant];
end;

procedure TNvBsInputAddonAction.SetShowIcon(const Value: Boolean);
begin
  if Value <> FShowIcon then
    begin
      EnqueueChange('Image', RenderImage);
      FShowIcon := Value;
      Invalidate;
    end;
end;

procedure TNvBsInputAddonAction.SetVariant(const Value: TBsButtonVariant);
begin
  if Value <> FVariant then
    begin
      EnqueueChange('Variant', RenderVariant);
      FVariant := Value;
      Invalidate;
    end;
end;

class function TNvBsInputAddonAction.SupportImage: Boolean;
begin
  Result := True;
end;

{ TNvBsInputAction }

procedure TNvBsInputAction.AssignTo(Dest: TPersistent);
var
  _Dest: TNvBsInputAction;
  // avoid to assign ancestor Action - bug in inherited forms
  function AssignAction: TNvCustomAction;
  var
    _DestRoot, _SourceRoot: TWinControl;
  begin
    if Not Assigned(Self.Action) then
      Exit(nil);
    // try
    _DestRoot   := _Dest.Collection.Input.Owner as TWinControl;
    _SourceRoot := Self.Collection.Input.Owner as TWinControl;
    if (_DestRoot <> nil) and (_SourceRoot <> nil) and _DestRoot.InheritsFrom(_SourceRoot.ClassType)
      and (Self.Action.Owner = _SourceRoot) then
      begin
        Result := _DestRoot.FindComponent(Self.Action.Name) as TNvCustomAction;
        if Result = nil then
          Result := Self.Action;
      end
    else
      Result := Self.Action;
    // except
    // Result:= Self.Action;
    // end;
  end;

begin
  if Dest is TNvBsInputAction then
    begin
      _Dest := TNvBsInputAction(Dest);
      with _Dest do
        begin
          FAction      := AssignAction;
          FVariant     := Self.Variant;
          FShowCaption := Self.ShowCaption;
          FShowIcon    := Self.ShowIcon;
        end
    end
  else
    inherited;
end;

function TNvBsInputAction.Collection: TNvBsInputActions;
begin
  Result := Inherited Collection as TNvBsInputActions;
end;

constructor TNvBsInputAction.Create(Collection: TCollection);
begin
  inherited;
  FVariant     := bsbPrimary;
  FShowCaption := True;
  FShowIcon    := True;
end;

function TNvBsInputAction.GetEnabled: Boolean;
begin
  Result := (FAction <> nil) and FAction.Enabled;
end;

procedure TNvBsInputAction.SetAction(const Value: TNvCustomAction);
begin
  FAction := Value;
end;

procedure TNvBsInputAction.SetEnabled(Value: Boolean);
begin
  if FAction <> nil then
    FAction.Enabled := Value;
end;

procedure TNvBsInputAction.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
end;

procedure TNvBsInputAction.SetShowIcon(const Value: Boolean);
begin
  FShowIcon := Value;
end;

procedure TNvBsInputAction.SetVariant(const Value: TBsButtonVariant);
begin
  FVariant := Value;
end;

{ TNvBsInputGroup }

procedure TNvBsInputGroup.CMControlListChange(var Message: TMessage);
var
  _Control: TControl;
begin
  if (Boolean(Message.LParam) = True) then
    begin
      _Control := TControl(Message.WParam);
      if not(_Control is TNvBsCustomImput) //
        and not(_Control is TNvBSInputAddon) then
        raise Exception.Create('TNvBsInputGroup only accept BsInputs and BsInputAddons');
    end;
  inherited;
end;

// class function TNvBsInputGroup.DefaultClassCss: string;
// begin
// Result := 'input-group';
// end;

{ TNvBsSwitch }

class function TNvBsSwitch.DefaultClassCss: string;
begin
  Result := 'form-check form-switch';
end;

{ TNvBsInputActions }

function TNvBsInputActions.GetInputAction(Index: Integer): TNvBsInputAction;
begin
  Result := Items[Index] as TNvBsInputAction;
end;

function TNvBsInputActions.Input: TNvBsCustomImput;
begin
  Result := Owner as TNvBsCustomImput;
end;

procedure TNvBsInputActions.SetInputAction(Index: Integer; const Value: TNvBsInputAction);
begin
  Items[Index] := Value;
end;

procedure TNvBsInputActions.Update(Item: TCollectionItem);
begin
  inherited;
  Input.ActionsUpdated;
end;

end.
