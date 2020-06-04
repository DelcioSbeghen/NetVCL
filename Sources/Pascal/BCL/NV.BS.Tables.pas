unit NV.BS.Tables;

interface

uses
  Classes, DB, NV.Interfaces, NV.BS.Controls, NV.VCL.DBCtrls, NV.JSON, NV.Ajax, NV.BS.Types;

type
  TpDBGridOption = (dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator, dgColumnResize,
    dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete,
    dgCancelOnExit, dgMultiSelect, dgTitleClick, dgTitleHotTrack);
  TpDBGridOptions = set of TpDBGridOption;

const
  DEFAULT_STYLES = [sttBordered, sttHover];

type
  TNvBsTableColumns = class;

  TNvBsTableColumn = class(TCollectionItem)
  private
    FFieldName: string;
    FTitle    : string;
    procedure SetFieldName(const Value: string);
    procedure SetTitle(const Value: string);
  protected
    procedure Render(aJson: TJsonObject);
  public
    function Columns: TNvBsTableColumns;
  published
    property FieldName: string read FFieldName write SetFieldName;
    property Title    : string read FTitle write SetTitle;
  end;

  TNvBsTableColumns = class(TOwnedCollection)
  private
    function GetColumn(Index: Integer): TNvBsTableColumn;
    procedure SetColumn(Index: Integer; const Value: TNvBsTableColumn);
  protected
    procedure Render;
    procedure Update(Item: TCollectionItem); override;
  public
    function Grid: INvGrid;
    property Column[Index: Integer]: TNvBsTableColumn read GetColumn write SetColumn; default;
  end;

  TNvBsTableBase = class(TNvBsControl, INvGrid)
  private
    FColums : TNvBsTableColumns;
    FOptions: TpDBGridOptions;
    // FRows         : TJsonArray;
    FRowsChanged  : Boolean;
    FLayoutChanged: Boolean;
    FStyles       : TNvBsTableStyles;
    FHeaderStyle  : TNvBsTHeaderStyle;
    procedure SetColumns(const Value: TNvBsTableColumns);
    procedure SetOptions(const Value: TpDBGridOptions);
    procedure SetStyles(const Value: TNvBsTableStyles);
    procedure SetHeaderStyle(const Value: TNvBsTHeaderStyle);
  protected
    procedure LayoutChanged;
    procedure RowsChanged;
    procedure UpdateLayout;
    procedure UpdateRows; virtual; abstract;
    //
    procedure InternalRender(Ajax: TNvAjax; JSON: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
  published
    property Columns    : TNvBsTableColumns read FColums write SetColumns;
    property HeaderStyle: TNvBsTHeaderStyle read FHeaderStyle write SetHeaderStyle;
    property Styles     : TNvBsTableStyles read FStyles write SetStyles default DEFAULT_STYLES;
  end;

  TNvBsTable = class(TNvBsTableBase)
  private
    FData: TJsonArray;
    procedure SetData(const Value: TJsonArray);
  protected
    procedure UpdateRows; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Data: TJsonArray read FData write SetData;
  end;

  TNvBsDbTable = class(TNvBsTableBase, INvDbGrid)
  private
    FDataLink     : TpGridDatalink;
    FDisableOnEdit: Boolean;
    FBookmarks    : TpBookmarkList;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetDisableOnEdit(const Value: Boolean);
  protected
    // IDBGrid
    procedure LinkActive(Value: Boolean);
    procedure DataChanged;
    procedure Scroll(Distance: Integer);
    procedure EditingChanged;
    procedure RecordChanged(aField: TField);
    procedure UpdateData; // save to dataset
    function SelectedField: TField;
    function Datalink: TDatalink;
    procedure InvalidateRow(ARow: Longint);
    function GetRow: Longint;
    //
    procedure GetDatasetRowData(ARow: TJsonObject);
    procedure UpdateActiveRowData;
    procedure UpdateRows; override;
    procedure UpdateActiveRow; // Active row and pagination
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource   : TDataSource read GetDataSource write SetDataSource;
    property DisableOnEdit: Boolean read FDisableOnEdit write SetDisableOnEdit;
  end;

function TableStylesToClasses(aStyles: TNvBsTableStyles): string;

implementation

uses
  Controls;

function TableStylesToClasses(aStyles: TNvBsTableStyles): string;
var
  _Style: TNvBsTableStyle;
begin
  Result := 'table';

  for _Style in aStyles do
    Result := Result + ' table-' + TNvBsTableStyleStr[_Style];
end;

{ TNvBsTableColumn }

function TNvBsTableColumn.Columns: TNvBsTableColumns;
begin
  Result := Collection as TNvBsTableColumns;
end;

procedure TNvBsTableColumn.Render(aJson: TJsonObject);
begin
  aJson.S['title'] := FTitle;
  aJson.S['field'] := FFieldName;
end;

procedure TNvBsTableColumn.SetFieldName(const Value: string);
begin
  if Value <> FFieldName then
    begin
      FFieldName := Value;
      Changed(False);
    end;
end;

procedure TNvBsTableColumn.SetTitle(const Value: string);
begin
  if Value <> FTitle then
    begin
      FTitle := Value;
      Changed(False);
    end;
end;

{ TNvBsTableColumns }

function TNvBsTableColumns.GetColumn(Index: Integer): TNvBsTableColumn;
begin
  Result := Items[Index] as TNvBsTableColumn;
end;

function TNvBsTableColumns.Grid: INvGrid;
begin
  Result := Owner as TNvBsTableBase;
end;

procedure TNvBsTableColumns.Render;
var
  _Json: TJsonArray;
  I    : Integer;
begin
  _Json := Grid.ControlAjaxJson.O['Options'].A['columns'];
  _Json.Clear;
  for I := 0 to Count - 1 do
    Column[I].Render(_Json.AddObject);
end;

procedure TNvBsTableColumns.SetColumn(Index: Integer; const Value: TNvBsTableColumn);
begin
  Column[Index].Assign(Value);
end;

procedure TNvBsTableColumns.Update(Item: TCollectionItem);
begin
  inherited;
  Grid.LayoutChanged;
  Grid.Invalidate;
end;

{ TNvBsTableBase }

constructor TNvBsTableBase.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  FColums      := TNvBsTableColumns.Create(Self, TNvBsTableColumn);
  FStyles      := DEFAULT_STYLES;
  FOptions     := [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgCancelOnExit,
    dgTitleClick, dgTitleHotTrack];
end;

destructor TNvBsTableBase.Destroy;
begin
  FColums.Free;
  inherited;
end;

procedure TNvBsTableBase.InternalRender(Ajax: TNvAjax; JSON: TJsonObject);
begin
  inherited;
  UpdateLayout;
  UpdateRows;
end;

procedure TNvBsTableBase.SetColumns(const Value: TNvBsTableColumns);
begin
  Columns.Assign(Value);
end;

procedure TNvBsTableBase.SetHeaderStyle(const Value: TNvBsTHeaderStyle);
begin
  if Value <> FHeaderStyle then
    begin
      LayoutChanged;

      FHeaderStyle := Value;
      Invalidate;
    end;
end;

procedure TNvBsTableBase.Invalidate;
begin
  if FLayoutChanged and NeedSendChange then
    UpdateLayout;

  if FRowsChanged and NeedSendChange then
    UpdateRows;

  inherited;
end;

procedure TNvBsTableBase.SetOptions(const Value: TpDBGridOptions);
begin
  FOptions := Value;
end;

procedure TNvBsTableBase.SetStyles(const Value: TNvBsTableStyles);
begin
  if Value <> FStyles then
    begin
      LayoutChanged;

      FStyles := Value;
      Invalidate;
    end;
end;

procedure TNvBsTableBase.RowsChanged;
begin
  FRowsChanged := True;
end;

procedure TNvBsTableBase.UpdateLayout;
var
  _Json: TJsonObject;
begin
  _Json := ControlAjaxJson.O['Options'];

  if NeedSendChange or (FStyles <> DEFAULT_STYLES) then
    _Json['classes'] := TableStylesToClasses(FStyles);

  if NeedSendChange or (FHeaderStyle <> sthUndefined) then
    _Json['theadClasses'] := TNvBsTHeaderStyleStr[FHeaderStyle];

  FColums.Render;
  FLayoutChanged := False;
end;

procedure TNvBsTableBase.LayoutChanged;
begin
  FLayoutChanged := True;
end;

{ TNvBsDbTable }

constructor TNvBsDbTable.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  // FRows        := TJsonArray.Create;
  // FColums      := TNvBsTableColumns.Create(Self, TNvBsTableColumn);
  FBookmarks := TpBookmarkList.Create(Self);
  FDataLink  := TpGridDatalink.Create(Self);
  // FOptions     := [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgCancelOnExit,
  // dgTitleClick, dgTitleHotTrack];
end;

procedure TNvBsDbTable.DataChanged;
begin
  // if not HandleAllocated then Exit;
  RowsChanged;
  UpdateActiveRow;
  // InvalidateEditor;
  // ValidateRect(Handle, nil);
  Invalidate;
end;

function TNvBsDbTable.Datalink: TDatalink;
begin
  Result := FDataLink;
end;

destructor TNvBsDbTable.Destroy;
begin
  FDataLink.Free;
  FBookmarks.Free;
  // FColums.Free;
  // FRows.Free;
  inherited;
end;

procedure TNvBsDbTable.EditingChanged;
begin
  if not(csDesigning in ComponentState) and DisableOnEdit then
    Enabled := not FDataLink.Editing;
end;

procedure TNvBsDbTable.UpdateRows;
var
  _ActiveBkp: Integer;
  I         : Integer;
  _DataRows : TJsonArray;
begin
  inherited;
  _DataRows := TJsonArray.Create;
  try

    _DataRows.Clear; // Save rows only on Browser ???????????????????

    if FDataLink.Active and not FDataLink.Editing then
      begin
        _ActiveBkp := FDataLink.ActiveRecord;
        FDataLink.DataSet.DisableControls;
        try
          // fast way fo update Rows, Check for pagination, only visible, etc
          for I := 0 to FDataLink.RecordCount - 1 do
            begin
              FDataLink.ActiveRecord := I;
              GetDatasetRowData(_DataRows.AddObject);
            end;
        finally
          FDataLink.ActiveRecord := _ActiveBkp;
          FDataLink.DataSet.EnableControls;
        end;
        FRowsChanged := False;
      end;

    ControlAjaxJson.A['Data'].Assign(_DataRows);
  finally
    _DataRows.Free;
  end;
end;

procedure TNvBsDbTable.GetDatasetRowData(ARow: TJsonObject);
begin

end;

function TNvBsDbTable.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TNvBsDbTable.GetRow: Longint;
begin
  Result := FDataLink.ActiveRecord;
end;

procedure TNvBsDbTable.InvalidateRow(ARow: Longint);
begin
  if ARow = GetRow then
    begin
      UpdateActiveRow;
      UpdateActiveRowData;
      Invalidate
    end
  else
    begin
      RowsChanged;
      Invalidate;
    end;
end;

procedure TNvBsDbTable.LinkActive(Value: Boolean);
begin
  FBookmarks.LinkActive(Value);
  try
    LayoutChanged;
    RowsChanged;
  finally
    UpdateActiveRow;
    Invalidate;
  end;
end;

procedure TNvBsDbTable.RecordChanged(aField: TField);
begin
  UpdateActiveRow;
  UpdateActiveRowData;
  Invalidate;
end;

procedure TNvBsDbTable.Scroll(Distance: Integer);
begin
  UpdateActiveRow;
  Invalidate;
end;

function TNvBsDbTable.SelectedField: TField;
begin
  // For editing only  ???
end;

procedure TNvBsDbTable.SetDataSource(const Value: TDataSource);
begin
  if Value = FDataLink.DataSource then
    Exit;
  FBookmarks.Clear;
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TNvBsDbTable.SetDisableOnEdit(const Value: Boolean);
begin
  FDisableOnEdit := Value;
end;

procedure TNvBsDbTable.UpdateData;
begin
  // Save edit data to Dataset Field;
end;

procedure TNvBsDbTable.UpdateActiveRow;
begin
  ControlAjaxJson.I['ActiveRow'] := FDataLink.ActiveRecord;
end;

procedure TNvBsDbTable.UpdateActiveRowData;
var
  _Row: TJsonObject;
begin
  _Row := TJsonObject.Create;
  try
    GetDatasetRowData(_Row);
    ControlAjaxJson.O['ActiveRowData'] := _Row;
  finally
    _Row.Free;
  end;
end;

{ TNvBsTable }

constructor TNvBsTable.Create(AOwner: TComponent);
begin
  inherited;
  FData := TJsonArray.Create;
end;

destructor TNvBsTable.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TNvBsTable.SetData(const Value: TJsonArray);
begin
  FData.Assign(Value);
  RowsChanged;
  Invalidate;
end;

procedure TNvBsTable.UpdateRows;
begin
  inherited;
  ControlAjaxJson.A['Data'].Assign(FData);
  FRowsChanged:= False;
end;

initialization

RegisterClasses([TNvBsTableColumns, TNvBsTableColumn]);

end.
