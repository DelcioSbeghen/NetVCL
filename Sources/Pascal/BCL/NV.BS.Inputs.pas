unit NV.BS.Inputs;

interface

uses
  Classes, NV.BS.Controls, NV.Ajax, NV.Json, DB, Vcl.DBCtrls;

type
  TNvBsCustomImput = class(TNvBsGridControl)
  private
    FValue    : string;
    FDatalink : TFieldDataLink;
    FMaxLength: Integer;
    FReadOnly : Boolean;
    procedure SetValue(const Value: string);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetMaxLength(const Value: Integer);
  protected
    procedure InternalRender(Ajax: TNvAjax; Json: TJsonObject); override;
    // DB only
    procedure ActiveChange(Sender: TObject); virtual;
    procedure DataChange(Sender: TObject); virtual;
    procedure EditingChange(Sender: TObject); virtual;
    procedure UpdateData(Sender: TObject); virtual;
    procedure ResetMaxLength; virtual;
    procedure SetReadOnly(const Value: Boolean);
    function DataLinkIsValid: Boolean;
    // properties
    property Value: string read FValue write SetValue;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  public
    destructor Destroy; override;
    procedure ValidateValue; virtual;
    function DataLink: TFieldDataLink;
  published
  end;

  TNvBsInput = class(TNvBsCustomImput)
  private
  protected
    procedure AddIncludes(Ajax: TNvAjax); override;
  public
  published
    property Background;
    property Border;
    property Caption;
    property DataField;
    property DataSource;
    property MaxLength;
    property ReadOnly;
    property Value;
    property Grids;
    property Shadow;
    property TextProps;
  end;

  TNvBsSelect = class(TNvBsCustomImput)
  private
  protected
    procedure AddIncludes(Ajax: TNvAjax); override;
  public
  published
    property Background;
    property Border;
    property Caption;
    property DataField;
    property DataSource;
    property MaxLength;
    property ReadOnly;
    property Value;
    property Grids;
    property Shadow;
    property TextProps;
  end;

  TNvBsMemo = class(TNvBsCustomImput)
  private
  protected
    procedure AddIncludes(Ajax: TNvAjax); override;
  public
  published
    property Background;
    property Border;
    property Caption;
    property DataField;
    property DataSource;
    property MaxLength;
    property ReadOnly;
    property Value;
    property Grids;
    property Shadow;
    property TextProps;
  end;

  TNvBsRange = class(TNvBsCustomImput)
  private
  protected
    procedure AddIncludes(Ajax: TNvAjax); override;
  public
  published
    property Background;
    property Border;
    property Caption;
    property DataField;
    property DataSource;
    property MaxLength;
    property ReadOnly;
    property Grids;
    property Shadow;
    property TextProps;
  end;

  TNvBsCheckbox = class(TNvBsCustomImput)
  private
  protected
    procedure AddIncludes(Ajax: TNvAjax); override;
  public
  published
    property Background;
    property Border;
    property Caption;
    property DataField;
    property DataSource;
    property MaxLength;
    property ReadOnly;
    property Grids;
    property Shadow;
  end;

implementation

{ TNvBsInput }

procedure TNvBsInput.AddIncludes(Ajax: TNvAjax);
begin
  inherited;
  if Ajax <> nil then
    Ajax.AddInclude('nv.bs.inputs.js', reqModule, '/nv.bs.inputs.js');
end;

{ TNvBsCustomImput }

procedure TNvBsCustomImput.ActiveChange(Sender: TObject);
begin
  ResetMaxLength;
end;

procedure TNvBsCustomImput.DataChange(Sender: TObject);
var
  _OldUpdateData: TNotifyEvent;
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
        Value                  := FDatalink.Field.Text;
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

destructor TNvBsCustomImput.Destroy;
begin
  if Assigned(FDatalink) then
    FDatalink.Free;
  FDatalink := nil;
  inherited;
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

procedure TNvBsCustomImput.InternalRender(Ajax: TNvAjax; Json: TJsonObject);
begin
  inherited;
  Json.S['Value'] := FValue;
  if FMaxLength > 0 then
    Json.I['MaxLength'] := FMaxLength;
  if FReadOnly then
    Json.B['ReadOnly'] := FReadOnly;
end;

function TNvBsCustomImput.DataLinkIsValid: Boolean;
begin
  Result := (FDatalink <> nil) and (FDatalink.DataSource <> nil) and (FDatalink.FieldName <> '');
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
      if NeedSendChange then
        ControlAjaxJson.S['Value'] := Value;
      FValue                       := Value;
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

procedure TNvBsCustomImput.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then
    begin
      if Rendered then
        ControlAjaxJson.I['MaxLength'] := Value;
      FMaxLength                       := Value;
    end;
end;

procedure TNvBsCustomImput.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
    begin
      if Rendered then
        ControlAjaxJson.B['ReadOnly'] := Value;
      FReadOnly                       := Value;
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

{ TNvBsSelect }

procedure TNvBsSelect.AddIncludes(Ajax: TNvAjax);
begin
  inherited;
  if Ajax <> nil then
    Ajax.AddInclude('nv.bs.inputs.js', reqModule, '/nv.bs.inputs.js');
end;

{ TNvBsMemo }

procedure TNvBsMemo.AddIncludes(Ajax: TNvAjax);
begin
  inherited;
  if Ajax <> nil then
    Ajax.AddInclude('nv.bs.inputs.js', reqModule, '/nv.bs.inputs.js');
end;

{ TNvBsRange }

procedure TNvBsRange.AddIncludes(Ajax: TNvAjax);
begin
  inherited;
  if Ajax <> nil then
    Ajax.AddInclude('nv.bs.inputs.js', reqModule, '/nv.bs.inputs.js');
end;

{ TNvBsCheckBox }

procedure TNvBsCheckbox.AddIncludes(Ajax: TNvAjax);
begin
  inherited;
  if Ajax <> nil then
    Ajax.AddInclude('nv.bs.inputs.js', reqModule, '/nv.bs.inputs.js');
end;

end.
