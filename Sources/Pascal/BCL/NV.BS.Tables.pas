unit NV.BS.Tables;

interface

uses
  Classes, SysUtils, DBConsts, RTLConsts, DB, NV.Interfaces, NV.BS.Controls, NV.VCL.DBCtrls,
  NV.JSON, NV.Ajax,
  NV.BS.Types, NV.VCL.ActnList, NV.BS.Buttons;

type
  TpDBGridOption = (dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator, dgColumnResize,
    dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete,
    dgCancelOnExit, dgMultiSelect, dgTitleClick, dgTitleHotTrack);
  TpDBGridOptions = set of TpDBGridOption;
  TMovedEvent     = procedure(Sender: TObject; FromIndex, ToIndex: Longint) of object;

const
  DEFAULT_STYLES = [sttBordered, sttHover];

type
  TNvBsTableColumns = class;
  TNvBsTableColumn  = class;
  TNvBsDbTable      = class;
  TNvBsTableBase    = class;

  EInvalidGridOperation = class(Exception);

  TpGridDatalink = class(TDataLink)
  private
    FModified    : Boolean;
    FInUpdateData: Boolean;
    FGrid        : TNvBsDbTable;
  protected
    // DataLink overrides
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AGrid: TNvBsDbTable);
    destructor Destroy; override;
    // function AddMapping(const FieldName: string): Boolean;
    // procedure ClearMapping;
    procedure Modified;
    procedure Reset;
    // property DefaultFields: Boolean read GetDefaultFields;
    // property FieldCount: Integer read FFieldCount;
    // property Fields[I: Integer]: TField read GetFields;
    // property SparseMap: Boolean read FSparseMap write FSparseMap;
    property Grid: TNvBsDbTable read FGrid;
  end;

  TpBookmarkList = class
  private
    FList      : array of TBookmark;
    FGrid      : TNvBsDbTable;
    FCache     : TBookmark;
    FCacheIndex: Integer;
    FCacheFind : Boolean;
    FLinkActive: Boolean;
    function GetCount: Integer;
    function GetCurrentRowSelected: Boolean;
    function GetItem(Index: Integer): TBookmark;
    procedure InsertItem(Index: Integer; Item: TBookmark);
    procedure DeleteItem(Index: Integer);
    procedure SetCurrentRowSelected(Value: Boolean);
    procedure DataChanged(Sender: TObject);
  protected
    function CurrentRow: TBookmark;
    function Compare(const Item1, Item2: TBookmark): Integer;
  public
    constructor Create(AGrid: TNvBsDbTable);
    destructor Destroy; override;
    procedure Clear;  // free all bookmarks
    procedure Delete; // delete all selected rows from dataset
    function Find(const Item: TBookmark; var Index: Integer): Boolean;
    function IndexOf(const Item: TBookmark): Integer;
    function Refresh: Boolean; // drop orphaned bookmarks; True = orphans found
    property Count: Integer read GetCount;
    procedure LinkActive(Value: Boolean);
    property CurrentRowSelected: Boolean read GetCurrentRowSelected write SetCurrentRowSelected;
    property Items[Index: Integer]: TBookmark read GetItem; default;
  end;

  TNvBsFormatterClass = class of TNvBsTblColFormatterBase;

  TNvBsTblColFormatterBase = class(TPersistent)
  private
    FColumn: TNvBsTableColumn;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(aColumn: TNvBsTableColumn); virtual; // (AOwner: TComponent); override;
    procedure Render(aJson: TJsonObject); virtual; abstract;
    function GetData: string; virtual; abstract;
  end;

  TNvBsFmtAction = class(TCollectionItem)
  private
    FAction     : TNvCustomAction;
    FVariant    : TBsButtonVariant;
    FSize       : TBsButtonSize;
    FShowCaption: Boolean;
    FShowIcon   : Boolean;
    procedure SetAction(const Value: TNvCustomAction);
    procedure SetSize(const Value: TBsButtonSize);
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
    function GetData: string;
  published
    property Action     : TNvCustomAction read FAction write SetAction;
    property Enabled    : Boolean read GetEnabled write SetEnabled;
    property Size       : TBsButtonSize read FSize write SetSize default bsbsDefault;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property ShowIcon   : Boolean read FShowIcon write SetShowIcon default True;
    property Variant    : TBsButtonVariant read FVariant write SetVariant default bsbPrimary;
  end;

  TNvBsFmtActions = class(TOwnedCollection)
  private
    function GetColAction(Index: Integer): TNvBsFmtAction;
    procedure SetColAction(Index: Integer; const Value: TNvBsFmtAction);
  protected
    procedure Update(Item: TCollectionItem); override;
    function Formatter: TNvBsTblColFormatterBase;
    property ColAction[Index: Integer]: TNvBsFmtAction read GetColAction
      write SetColAction; default;
  public
    function GetData: string;
  end;

  TNvBsTblColFormatterActions = class(TNvBsTblColFormatterBase)
  private
    FColActions: TNvBsFmtActions;
    procedure SetColActions(const Value: TNvBsFmtActions);
  protected
  public
    constructor Create(aColumn: TNvBsTableColumn); override; // (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Render(aJson: TJsonObject); override;
    function GetData: string; override;
  published
    property ColActions: TNvBsFmtActions read FColActions write SetColActions;
  end;

  TNvBsTableColumn = class(TCollectionItem)
  private
    FFieldName    : string;
    FTitle        : string;
    FField        : TField;
    FFormatter    : TNvBsTblColFormatterBase;
    FFormaterClass: TNvBsFormatterClass;
    FPersistent   : Boolean;
    procedure SetFieldName(const Value: string);
    procedure SetTitle(const Value: string);
    function GetField: TField;
    procedure SetField(Value: TField);
    procedure SetFormaterClass(const Value: TNvBsFormatterClass);
    procedure SetFormatter(const Value: TNvBsTblColFormatterBase);
    procedure SetFormaterClassName(const Value: string);
    function GetFormaterClassName: string;
  protected
    // procedure DefineProperties(Filer: TFiler); override;
    // procedure ReadFmtClass(Reader: TReader);
    // procedure WriteFmtClass(Writer: TWriter);
    procedure Render(aJson: TJsonObject);
    procedure GetData(ARow: TJsonObject);
    function GetDbTable: TNvBsDbTable;
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function Columns: TNvBsTableColumns;
    property Field: TField read GetField write SetField;
    property Persistent: Boolean read FPersistent write FPersistent;
  published
    property FieldName        : string read FFieldName write SetFieldName;
    property FormaterClassName: string read GetFormaterClassName write SetFormaterClassName;
    property FormaterClass    : TNvBsFormatterClass read FFormaterClass write SetFormaterClass
      stored False;
    property Formatter: TNvBsTblColFormatterBase read FFormatter write SetFormatter;
    property Title    : string read FTitle write SetTitle;
  end;

  TNvBsTableColumns = class(TOwnedCollection)
  private
    FCombined: Boolean;
    function GetColumn(Index: Integer): TNvBsTableColumn;
    procedure SetColumn(Index: Integer; const Value: TNvBsTableColumn);
  protected
    procedure Render;
    procedure Update(Item: TCollectionItem); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    function Grid: TNvBsTableBase; inline;
    procedure ClearNoPersistent;
    function PersistentCount: Integer;
    property Column[Index: Integer]: TNvBsTableColumn read GetColumn write SetColumn; default;
  published
    property Combined: Boolean read FCombined write FCombined;
  end;

  TNvBsTableBase = class(TNvBsControl)
  private
    FColumns: TNvBsTableColumns;
    FOptions: TpDBGridOptions;
    // FRows         : TJsonArray;
    FDataChanged  : Boolean;
    FUpdateLock   : Byte;
    FLayoutChanged: Boolean;
    // FLayoutLock          : Byte;
    FActiveRow           : LongInt;
    FActiveRowChanged    : Boolean;
    FActiveRowDataChanged: Boolean;
    FColumsChanged       : Boolean;
    FStyles              : TNvBsTableStyles;
    FHeaderStyle         : TNvBsTHeaderStyle;
    FBookmarks           : TpBookmarkList;
    FAllSelected         : Boolean;
    FOnSelectionChange   : TNotifyEvent;
    FOnColumnMoved       : TMovedEvent;
    FPageSize            : Integer;
    FDataIdField         : string;
    procedure SetColumns(const Value: TNvBsTableColumns);
    procedure SetOptions(const Value: TpDBGridOptions);
    procedure SetStyles(const Value: TNvBsTableStyles);
    procedure SetHeaderStyle(const Value: TNvBsTHeaderStyle);
    procedure SetOnSelectionChange(const Value: TNotifyEvent);
    procedure SetOnColumnMoved(const Value: TMovedEvent);
    procedure SetPageSize(const Value: Integer);
    procedure SetDataIdField(const Value: string); virtual;
    procedure ReadColumns(Reader: TReader);
    procedure WriteColumns(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoClickCell(aEvent: TJsonObject); virtual;
    procedure DoClickAction(aEvent: TJsonObject); virtual;
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
    // Render
    procedure InternalRender(JSON: TJsonObject); override;
    procedure RenderLayout(JSON: TJsonObject); virtual;
    procedure RenderData(JSON: TJsonObject); virtual; abstract;
    procedure RenderActiveRow(JSON: TJsonObject);
    procedure RenderActiveRowData(JSON: TJsonObject);
    // Table Methods
    procedure DoSelectionChange;
    function GetCurrRow: Longint; virtual;
    procedure GetRowData(ARow: TJsonObject); virtual; abstract;
    // Common Properties
    property SelectedRows: TpBookmarkList read FBookmarks;
    property PageSize: Integer read FPageSize write SetPageSize default 50;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write SetOnSelectionChange;
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write SetOnColumnMoved;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Avoid multiple updates
    // procedure BeginLayout;
    // procedure EndLayout;
    procedure BeginUpdate;
    procedure EndUpdate;
    //
    procedure Invalidate; override;
    procedure SelectAll;
    procedure UnSelectAll;
    function FindColumnByName(const aName: string): TNvBsTableColumn;
    property UpdateLock: Byte read FUpdateLock;
  published
    property Columns    : TNvBsTableColumns read FColumns write SetColumns;
    property DataIdField: string read FDataIdField write SetDataIdField;
    property HeaderStyle: TNvBsTHeaderStyle read FHeaderStyle write SetHeaderStyle;
    property Styles     : TNvBsTableStyles read FStyles write SetStyles default DEFAULT_STYLES;
    property ClassCss;
  end;

  TNvBsTable = class(TNvBsTableBase)
  private
    FData: TJsonArray;
    procedure SetData(const Value: TJsonArray);
  protected
    procedure RenderData(JSON: TJsonObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Data: TJsonArray read FData write SetData;
  end;

  TNvBsDbTable = class(TNvBsTableBase)
  private
    FDataLink     : TpGridDatalink;
    FDataIdDBField: TField;
    FDisableOnEdit: Boolean;
    // FBookmarks    : TpBookmarkList;
    FReadOnly: Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetDisableOnEdit(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetDataIdField(const Value: string); override;
    procedure GoToDatasetRecord(aRec: Int64);
  protected
    procedure DoClickCell(aEvent: TJsonObject); override;
    procedure DoClickAction(aEvent: TJsonObject); override;
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
    function GetCurrRow: Longint; override;
    procedure UpdateRowCount;
    //
    procedure RenderData(JSON: TJsonObject); override;
    procedure RenderLayout(JSON: TJsonObject); override;
    procedure GetRowData(ARow: TJsonObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckIDDbField;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedRows;
  published
    property DataSource   : TDataSource read GetDataSource write SetDataSource;
    property DisableOnEdit: Boolean read FDisableOnEdit write SetDisableOnEdit;
    property PageSize;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property OnColumnMoved;
  end;

function TableStylesToClasses(aStyles: TNvBsTableStyles): string;

procedure RegisterColFormatter(aClass: TNvBsFormatterClass);
function GetColFormatterList: TStringList;

implementation

uses
  StrUtils, Math, Controls, NV.Utils;

var
  FormaterClasses: TStringList;

type
  TWriterHelper = class Helper for TWriter
    procedure WriteValue(Value: TValueType);
  end;

procedure RegisterColFormatter(aClass: TNvBsFormatterClass);
begin
  RegisterClass(aClass);
  FormaterClasses.AddObject(aClass.ClassName, TObject(aClass));
end;

function GetColFormatterList: TStringList;
begin
  Result := FormaterClasses;
end;

procedure RaiseGridError(const S: string);
begin
  raise EInvalidGridOperation.Create(S);
end;

{ TpGridDatalink }

procedure TpGridDatalink.ActiveChanged;
begin
  FGrid.LinkActive(Active);
  FModified := False;
end;

constructor TpGridDatalink.Create(AGrid: TNvBsDbTable);
begin
  inherited Create;
  FGrid         := AGrid;
  VisualControl := True;
end;

procedure TpGridDatalink.DataSetChanged;
begin
  inherited;
  FGrid.DataChanged;
  FModified := False;
end;

procedure TpGridDatalink.DataSetScrolled(Distance: Integer);
begin
  FGrid.Scroll(Distance);
end;

destructor TpGridDatalink.Destroy;
begin

  inherited;
end;

procedure TpGridDatalink.EditingChanged;
begin
  FGrid.EditingChanged;
end;

procedure TpGridDatalink.LayoutChanged;
begin
  FGrid.FLayoutChanged := True;
  FGrid.UpdateRowCount;
  FGrid.Invalidate;
  inherited LayoutChanged;
end;

procedure TpGridDatalink.Modified;
begin
  FModified := True;
end;

procedure TpGridDatalink.RecordChanged(Field: TField);
begin
  // If we get called with a field that is not the selected field
  // and the selected field is modified we need force it to be updated first.
  if FModified and Assigned(Field) and (Field.FieldKind = fkData) and (FGrid.SelectedField <> Field)
    and not FInUpdateData then
    UpdateData;

  FGrid.RecordChanged(Field);
  FModified := False;
end;

procedure TpGridDatalink.Reset;
begin
  if FModified then
    RecordChanged(nil)
  else
    Dataset.Cancel;
end;

procedure TpGridDatalink.UpdateData;
begin
  FInUpdateData := True;
  try
    if FModified then
      FGrid.UpdateData;
    FModified := False;
  finally
    FInUpdateData := False;
  end;
end;

{ TpBookmarkList }

procedure TpBookmarkList.Clear;
begin
  if Length(FList) = 0 then
    Exit;
  SetLength(FList, 0);
  FGrid.Invalidate;
end;

function TpBookmarkList.Compare(const Item1, Item2: TBookmark): Integer;
begin
  with FGrid.Datalink.Datasource.Dataset do
    Result := CompareBookmarks(TBookmark(Item1), TBookmark(Item2));
end;

constructor TpBookmarkList.Create(AGrid: TNvBsDbTable);
begin
  inherited Create;
  SetLength(FList, 0);
  FGrid := AGrid;
end;

function TpBookmarkList.CurrentRow: TBookmark;
begin
  if not FLinkActive then
    RaiseGridError(sDataSetClosed);
  Result := FGrid.Datalink.Datasource.Dataset.Bookmark;
end;

procedure TpBookmarkList.DataChanged(Sender: TObject);
begin
  FCache      := nil;
  FCacheIndex := -1;
end;

procedure TpBookmarkList.Delete;
var
  I: Integer;
begin
  with FGrid.Datalink.Datasource.Dataset do
    begin
      DisableControls;
      try
        for I := Length(FList) - 1 downto 0 do
          begin
            Bookmark := FList[I];
            Delete;
            DeleteItem(I);
          end;
      finally
        EnableControls;
      end;
    end;
end;

procedure TpBookmarkList.DeleteItem(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= Count) then
    raise EListError.Create(SListIndexError);
  Temp := FList[Index];
  // The Move below will overwrite this slot, so we need to finalize it first
  FList[Index] := nil;
  if Index < Count - 1 then
    begin
      System.Move(FList[Index + 1], FList[Index], (Count - Index - 1) * SizeOf(Pointer));
      // Make sure we don't finalize the item that was in the last position.
      PPointer(@FList[Count - 1])^ := nil;
    end;
  SetLength(FList, Count - 1);
  DataChanged(Temp);

end;

destructor TpBookmarkList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TpBookmarkList.Find(const Item: TBookmark; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  if (Item = FCache) and (FCacheIndex >= 0) then
    begin
      Index  := FCacheIndex;
      Result := FCacheFind;
      Exit;
    end;
  Result := False;
  L      := 0;
  H      := Length(FList) - 1;
  while L <= H do
    begin
      I := (L + H) shr 1;
      C := Compare(FList[I], Item);
      if C < 0 then
        L := I + 1
      else
        begin
          H := I - 1;
          if C = 0 then
            begin
              Result := True;
              L      := I;
            end;
        end;
    end;
  Index       := L;
  FCache      := Item;
  FCacheIndex := Index;
  FCacheFind  := Result;
end;

function TpBookmarkList.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TpBookmarkList.GetCurrentRowSelected: Boolean;
var
  Index: Integer;
begin
  Result := Find(CurrentRow, Index);
end;

function TpBookmarkList.GetItem(Index: Integer): TBookmark;
begin
  Result := FList[Index];
end;

function TpBookmarkList.IndexOf(const Item: TBookmark): Integer;
begin
  if not Find(Item, Result) then
    Result := -1;
end;

procedure TpBookmarkList.InsertItem(Index: Integer; Item: TBookmark);
begin
  if (Index < 0) or (Index > Count) then
    raise EListError.Create(SListIndexError);
  SetLength(FList, Count + 1);
  if Index < Count - 1 then
    begin
      Move(FList[Index], FList[Index + 1], (Count - Index - 1) * SizeOf(Pointer));
      // The slot we opened up with the Move above has a dangling pointer we don't want finalized
      PPointer(@FList[Index])^ := nil;
    end;
  FList[Index] := Item;
  DataChanged(TObject(Item));
end;

procedure TpBookmarkList.LinkActive(Value: Boolean);
begin
  Clear;
  FLinkActive := Value;
end;

function TpBookmarkList.Refresh: Boolean;
var
  I: Integer;
begin
  Result := False;
  with FGrid.DataLink.Datasource.Dataset do
    try
      CheckBrowseMode;
      for I := Length(FList) - 1 downto 0 do
        if not BookmarkValid(TBookmark(FList[I])) then
          begin
            Result := True;
            DeleteItem(I);
          end;
    finally
      UpdateCursorPos;
      if Result then
        FGrid.Invalidate;
    end;
end;

procedure TpBookmarkList.SetCurrentRowSelected(Value: Boolean);
var
  Index  : Integer;
  Current: TBookmark;
begin
  Current := CurrentRow;
  if (Length(Current) = 0) or (Find(Current, Index) = Value) then
    Exit;
  if Value then
    InsertItem(Index, Current)
  else
    DeleteItem(Index);
  FGrid.InvalidateRow(FGrid.GetCurrRow);

end;

function TableStylesToClasses(aStyles: TNvBsTableStyles): string;
var
  _Style: TNvBsTableStyle;
begin
  Result := 'table';

  for _Style in aStyles do
    Result := Result + ' table-' + TNvBsTableStyleStr[_Style];
end;

{ TNvBsTableColumn }

procedure TNvBsTableColumn.AssignTo(Dest: TPersistent);
var
  _Dest: TNvBsTableColumn;
begin
  if Dest is TNvBsTableColumn then
    begin
      _Dest := TNvBsTableColumn(Dest);
      with _Dest do
        begin
          FFieldName := Self.FFieldName;
          FTitle     := Self.FTitle;
          // Self.FField
          FormaterClass := Self.FFormaterClass;
          FFormatter.Assign(Self.FFormatter);
          FPersistent := Self.FPersistent;
        end
    end
  else
    inherited;
end;

function TNvBsTableColumn.Columns: TNvBsTableColumns;
begin
  Result := Collection as TNvBsTableColumns;
end;

// procedure TNvBsTableColumn.DefineProperties(Filer: TFiler);
// begin
// inherited;
// Filer.DefineProperty('FormaterClass', ReadFmtClass, WriteFmtClass, FormaterClass <> nil);
// end;

constructor TNvBsTableColumn.Create(Collection: TCollection);
begin
  inherited;
  FPersistent := True;
end;

destructor TNvBsTableColumn.Destroy;
begin
  if FFormatter <> nil then
    FreeAndNil(FFormatter);
  inherited;
end;

procedure TNvBsTableColumn.GetData(ARow: TJsonObject);
begin
  if FFormatter <> nil then
    ARow.r[FieldName] := FFormatter.GetData
  else if Field <> nil then
    ARow.s[FieldName] := Field.DisplayText;
end;

function TNvBsTableColumn.GetDbTable: TNvBsDbTable;
begin
  if Assigned(Collection) and (Columns.Owner is TNvBsDbTable) then
    Result := (Columns.Owner as TNvBsDbTable)
  else
    Result := nil;
end;

function TNvBsTableColumn.GetDisplayName: string;
begin
  if FFieldName <> '' then
    Result := FFieldName
  else
    Result := inherited;
end;

function TNvBsTableColumn.GetField: TField;
var
  Table: TNvBsDbTable;
begin
  if (FField = nil) and (Length(FFieldName) > 0) then
    begin
      Table := GetDbTable;
      if (Table <> nil) and Assigned(Table.DataLink.DataSet) then
        with Table.Datalink.Dataset do
          if Active or (lcPersistent in Fields.LifeCycles) then
            SetField(FindField(FieldName));
    end;
  Result := FField;
end;

function TNvBsTableColumn.GetFormaterClassName: string;
begin
  if FFormaterClass <> nil then
    Result := FFormaterClass.ClassName
  else
    Result := '';
end;

procedure TNvBsTableColumn.Render(aJson: TJsonObject);
begin
  aJson.S['title'] := FTitle;
  aJson.S['field'] := FFieldName;
  if Formatter <> nil then
    Formatter.Render(aJson);
end;

procedure TNvBsTableColumn.SetField(Value: TField);
var
  Table: TNvBsDbTable;
begin
  if FField = Value then
    Exit;

  Table := GetDbTable;
  if Assigned(FField) and (Table <> nil) then
    FField.RemoveFreeNotification(Table);
  if Assigned(Value) and (csDestroying in Value.ComponentState) then
    Value := nil; // don't acquire references to fields being destroyed
  if FField = Value then
    Exit;
  FField := Value;
  if Assigned(Value) then
    begin
      if Table <> nil then
        FField.FreeNotification(Table);
      FFieldName := Value.FullName;
    end;
  if not FPersistent and not(csDesigning in Table.ComponentState) { IsStored } then
    begin
      if Value = nil then
        FFieldName := '';
      // Columns.Delete(Self);
      // Free;
      // RestoreDefaults;
    end;
  // Changed(False);
end;

procedure TNvBsTableColumn.SetFieldName(const Value: string);
begin
  if Value <> FFieldName then
    begin
      FFieldName := Value;
      Changed(False);
    end;
end;

procedure TNvBsTableColumn.SetFormaterClass(const Value: TNvBsFormatterClass);
begin
  if FFormaterClass <> Value then
    begin
      FFormaterClass := Value;
      // FRuleClass := GetRuleClass;
      if FFormatter = nil then
        FFormatter := FFormaterClass.Create(Self)
      else if FFormatter.ClassType <> FFormaterClass then
        begin
          FreeAndNil(FFormatter);
          FFormatter := FFormaterClass.Create(Self);
          try
            if (csDesigning in TControl(Collection.Owner).ComponentState) and
              (not(csLoading in TControl(Collection.Owner).ComponentState)) then
              begin
                FindRootDesigner(Self).Notification(Self, opRemove);
                FindRootDesigner(Self).Notification(Self, opInsert);
              end;
            // FindRootDesigner(Self).Modified;
          except
            // To avoid DesignTime Errors
          end;
        end;
      Changed(False);
    end;
end;

procedure TNvBsTableColumn.SetFormaterClassName(const Value: string);
var
  I: Integer;
begin
  if Value <> FormaterClassName then
    begin
      I := FormaterClasses.IndexOf(Value);
      if I > -1 then
        FormaterClass := TNvBsFormatterClass(FormaterClasses.Objects[I])
      else
        FormaterClass := nil;
    end;
end;

procedure TNvBsTableColumn.SetFormatter(const Value: TNvBsTblColFormatterBase);
begin
  if FFormatter <> Value then
    FFormatter.Assign(Value);
end;

procedure TNvBsTableColumn.SetTitle(const Value: string);
begin
  if Value <> FTitle then
    begin
      FTitle := Value;
      Changed(False);
    end;
end;


// procedure TNvBsTableColumn.WriteFmtClass(Writer: TWriter);
// var
// Formatters: TStrings;
// I         : Integer;
// begin
// Formatters := GetColFormatterList;
// I          := Formatters.IndexOfObject(TObject(FormaterClass));
// if I > -1 then
// Writer.WriteString(Formatters[I]);
// end;

{ TNvBsTableColumns }

procedure TNvBsTableColumns.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TNvBsTableColumns then
    TNvBsTableColumns(Dest).FCombined := FCombined;
end;

procedure TNvBsTableColumns.ClearNoPersistent;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if not Column[I].Persistent then
      Column[I].Free;
end;

function TNvBsTableColumns.GetColumn(Index: Integer): TNvBsTableColumn;
begin
  Result := Items[Index] as TNvBsTableColumn;
end;

function TNvBsTableColumns.Grid: TNvBsTableBase;
begin
  Result := Owner as TNvBsTableBase;
end;

function TNvBsTableColumns.PersistentCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I  := 0 to Count - 1 do
    if Self[I].Persistent then
      Inc(Result);
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
  Grid.FLayoutChanged := True;
  Grid.Invalidate;
end;

{ TNvBsTableBase }

// procedure TNvBsTableBase.BeginLayout;
// begin
// BeginUpdate;
/// /  Inc(FLayoutLock);
// end;

procedure TNvBsTableBase.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

constructor TNvBsTableBase.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  FColumns     := TNvBsTableColumns.Create(Self, TNvBsTableColumn);
  FStyles      := DEFAULT_STYLES;
  FOptions     := [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgCancelOnExit,
    dgTitleClick, dgTitleHotTrack];
  FPageSize := 50;
end;

procedure TNvBsTableBase.DefineProperties(Filer: TFiler);
// var
// StoreColums: Boolean;
// I          : Integer;
begin
  inherited;
  // StoreColums := False;
  // for I       := 0 to FColumns.Count - 1 do
  // if Columns[I].Persistent then
  // begin
  // StoreColums := True;
  // Break;
  // end;

  // Filer.DefineProperty('Columns', ReadColumns, WriteColumns, True);
end;

destructor TNvBsTableBase.Destroy;
begin
  FColumns.Free;
  inherited;
end;

procedure TNvBsTableBase.DoClickAction(aEvent: TJsonObject);
var
  i, a   : Integer;
  _Col   : TNvBsTableColumn;
  _Action: TNvCustomAction;
begin
  if aEvent.s['action'].IsEmpty then
    raise Exception.Create('TNvBsTableBase.DoClickAction: Action Name is Empty');

  for i := 0 to Columns.Count - 1 do
    begin
      _Col := Columns[I];
      if _Col.Formatter is TNvBsTblColFormatterActions then
        with _col.Formatter as TNvBsTblColFormatterActions do
          for a := 0 to ColActions.Count - 1 do
            begin
              _Action := ColActions[a].Action;
              if (_Action <> nil) and (_Action.Name = aEvent.s['action']) then
                begin
                  _Action.Execute;
                  Exit;
                end;
            end;
    end;
end;

procedure TNvBsTableBase.DoClickCell(aEvent: TJsonObject);
begin

end;

procedure TNvBsTableBase.DoSelectionChange;
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

// procedure TNvBsTableBase.EndLayout;
// begin
// // if FLayoutLock > 0 then
// //  Dec(FLayoutLock);
//
// EndUpdate;
// end;

procedure TNvBsTableBase.EndUpdate;
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
end;

function TNvBsTableBase.FindColumnByName(const aName: string): TNvBsTableColumn;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
    begin
      if Columns[I].FieldName = aName then
        begin
          Result := Columns[I];
          Exit;
        end;
    end;

  Result := nil;
end;

function TNvBsTableBase.GetCurrRow: Longint;
begin
  Result := FActiveRow;
end;

procedure TNvBsTableBase.InternalRender(JSON: TJsonObject);
begin
  inherited;
  // JSON.A['Events'].Add('click-cell.bs.table');
  RenderLayout(JSON);
  RenderData(JSON);
  RenderActiveRow(JSON);
end;

procedure TNvBsTableBase.SelectAll;
var
  ABookmark: TBookmark;
begin
  { TODO -TDelcio -cWeb : TNvBsTableBase.SelectAll }
  // if (dgMultiSelect in Options) and Datalink.Active then
  // begin
  // with Datalink.Dataset do
  // begin
  // if (BOF and EOF) then
  // Exit;
  //
  // DisableControls;
  // try
  // ABookmark := GetBookmark;
  // try
  // // SMSelectionChanging;
  // First;
  // while not EOF do
  // begin
  // SelectedRows.CurrentRowSelected := True;
  // Next;
  // end;
  // finally
  // try
  // if BookmarkValid(ABookmark) then
  // GotoBookmark(ABookmark);
  // except
  // end;
  // FreeBookmark(ABookmark);
  // end;
  // FAllSelected := True;
  // finally
  // // SMSelectionChanged;
  // EnableControls;
  // end;
  // DoSelectionChange;
  // end;
  // end;
end;

procedure TNvBsTableBase.SetColumns(const Value: TNvBsTableColumns);
begin
  if FColumns <> Value then
    Columns.Assign(Value);
end;

procedure TNvBsTableBase.SetDataIdField(const Value: string);
begin
  if Value <> FDataIdField then
    begin
      FDataIdField := Value;

      FLayoutChanged := True;
      Invalidate;
    end;
end;

procedure TNvBsTableBase.SetHeaderStyle(const Value: TNvBsTHeaderStyle);
begin
  if Value <> FHeaderStyle then
    begin
      FLayoutChanged := True;

      FHeaderStyle := Value;
      Invalidate;
    end;
end;

procedure TNvBsTableBase.Invalidate;
begin
  if FUpdateLock > 0 then
    Exit;

  if FLayoutChanged then
    EnqueueChange('Layout', RenderLayout);

  if FDataChanged then
    EnqueueChange('Data', RenderData);

  if FActiveRowChanged then
    EnqueueChange('ActiveRow', RenderActiveRow);

  if FActiveRowDataChanged then
    EnqueueChange('ActiveRowData', RenderActiveRowData);

  inherited;
end;

function TNvBsTableBase.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  case IndexStr(AEventName, ['click-cell', 'click-action']) of
    0:
      begin
        DoClickCell(aEvent);
        Result := True;
      end;
    1:
      begin
        DoClickAction(aEvent);
        Result := True;
      end;
  else Result := inherited;
  end;
end;

procedure TNvBsTableBase.SetOnColumnMoved(const Value: TMovedEvent);
begin
  FOnColumnMoved := Value;
end;

procedure TNvBsTableBase.SetOnSelectionChange(const Value: TNotifyEvent);
begin
  FOnSelectionChange := Value;
end;

procedure TNvBsTableBase.SetOptions(const Value: TpDBGridOptions);
begin
  FOptions := Value;
end;

procedure TNvBsTableBase.SetPageSize(const Value: Integer);
begin
  if Value <> FPageSize then
    begin
      FLayoutChanged := True;

      FPageSize := Value;
      Invalidate;
    end;
end;

procedure TNvBsTableBase.SetStyles(const Value: TNvBsTableStyles);
begin
  if Value <> FStyles then
    begin
      FStyles := Value;

      FLayoutChanged := True;
      Invalidate;
    end;
end;

procedure TNvBsTableBase.UnSelectAll;
begin
  SelectedRows.Clear;
  FAllSelected := False;
  DoSelectionChange;
end;

procedure TNvBsTableBase.ReadColumns(Reader: TReader);
begin
  Columns.Clear;
  // Reader.ReadValue;
  Reader.ReadCollection(Columns);
end;

procedure TNvBsTableBase.WriteColumns(Writer: TWriter);
var
  I          : Integer;
  OldAncestor: TPersistent;
begin
  OldAncestor     := Writer.Ancestor;
  Writer.Ancestor := nil;
  try
    Writer.WriteValue(vaCollection);

    if FColumns <> nil then
      for I := 0 to FColumns.Count - 1 do
        if FColumns[I].Persistent then
          begin
            Writer.WriteListBegin;
            Writer.WriteProperties(FColumns.Items[I]);
            Writer.WriteListEnd;
          end;
    Writer.WriteListEnd;
  finally
    Writer.Ancestor := OldAncestor;
  end;
end;

procedure TNvBsTableBase.RenderActiveRow(JSON: TJsonObject);
begin
  JSON.I['ActiveRow'] := GetCurrRow;
  FActiveRowChanged   := False;
end;

procedure TNvBsTableBase.RenderActiveRowData(JSON: TJsonObject);
begin
  GetRowData(JSON.O['ActiveRowData']);
  FActiveRowDataChanged := False;
end;

procedure TNvBsTableBase.RenderLayout(JSON: TJsonObject);
begin
  if NeedSendChange or (FStyles <> DEFAULT_STYLES) then
    JSON['classes'] := TableStylesToClasses(FStyles);

  if NeedSendChange or (FHeaderStyle <> sthUndefined) then
    JSON['theadClasses'] := TNvBsTHeaderStyleStr[FHeaderStyle];

  JSON['idField'] := FDataIdField;

  JSON['virtualScroll']   := True;
  JSON['loadingTemplate'] :=
    '''<div class="ph-item" style="width: 100%; height: 100%; padding: 0;margin: 0;"><div class="ph-picture" style="width: 100%; height: 100%; padding: 0;margin: 0;"></div></div>''';

  FColumns.Render;
  FLayoutChanged := False;
end;

{ TNvBsDbTable }

procedure TNvBsDbTable.CheckIDDbField;
begin
  if (FDataIdDBField = nil) and (FDataLink.Active) then
    begin

      if not FDataIdField.IsEmpty then
        FDataIdDBField := FDataLink.DataSet.FindField(FDataIdField);

      if FDataIdDBField = nil then
        begin
          FDataIdDBField := GetDatasetIndex(FDataLink.DataSet);
          if FDataIdDBField <> nil then
            DataIdField := FDataIdDBField.FieldName;
        end;

    end;

  if (FDataIdDBField <> nil) and (FDataIdField <> FDataIdDBField.FieldName) then
    DataIdField := FDataIdDBField.FieldName;

  if FDataIdField.IsEmpty then
    FDataIdField := 'RowNo';
end;

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
  FDataChanged      := True;
  FActiveRowChanged := True;
  // InvalidateEditor;
  // ValidateRect(Handle, nil);
  UpdateRowCount;
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

procedure TNvBsDbTable.GoToDatasetRecord(aRec: Int64);
begin
  if FDataLink.Active then
    FDataLink.DataSet.RecNo := aRec + 1
  else
    raise Exception.Create('DataLink is not active');
end;

procedure TNvBsDbTable.DoClickAction(aEvent: TJsonObject);
begin
  GoToDatasetRecord(aEvent.L['index']);

  inherited;

end;

procedure TNvBsDbTable.DoClickCell(aEvent: TJsonObject);
begin
  GoToDatasetRecord(aEvent.L['index']);

  inherited;
end;

procedure TNvBsDbTable.EditingChanged;
begin
  if not(csDesigning in ComponentState) and DisableOnEdit then
    Enabled := not FDataLink.Editing;
end;

procedure TNvBsDbTable.RenderData(JSON: TJsonObject);
var
  _ActiveBkp: Integer;
  I         : Integer;
  _DataRows : TJsonArray;
begin
  inherited;

  _DataRows := JSON.A['Data'];

  _DataRows.Clear; // Save rows only on Browser ???????????????????

  if FDataLink.Active and not FDataLink.Editing then
    begin
      _ActiveBkp := FDataLink.ActiveRecord;
      // FDataLink.DataSet.RecNo := FDataLink.Dataset.RecordCount;

      // PageSize;
      // FDataLink.DataSet.DisableControls;
      try

        // fast way fo update Rows, Check for pagination, only visible, etc
        for I := 0 to FDataLink.RecordCount - 1 do
          begin
            FDataLink.ActiveRecord := I;
            GetRowData(_DataRows.AddObject);
          end;

        FDataChanged := False;
      finally
        FDataLink.ActiveRecord := _ActiveBkp;
        // FDataLink.DataSet.EnableControls;
      end;
    end;
end;

procedure TNvBsDbTable.GetRowData(ARow: TJsonObject);
var
  I: Integer;
begin
  if (FDataLink.ActiveRecord >= FDataLink.RecordCount) or (FDataLink.ActiveRecord < 0) then
    Exit;

  if FDataIdDBField <> nil then
    CheckIDDbField;

  if FDataIdDBField <> nil then
    ARow.s[FDataIdField] := FDataIdDBField.AsString
  else
    ARow.s[FDataIdField] := FDataLink.ActiveRecord.ToString;

  for I := 0 to Columns.Count - 1 do
    Columns[I].GetData(ARow);
  // if Columns[I].Field <> nil then
  // ARow.s[Columns[I].FieldName] := Columns[I].Field.DisplayText;
end;

function TNvBsDbTable.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TNvBsDbTable.GetCurrRow: Longint;
begin
  if FDataLink.Active then
    Result := FDataLink.ActiveRecord
  else
    Result := 0;
end;

procedure TNvBsDbTable.InvalidateRow(ARow: Longint);
begin
  if ARow = GetCurrRow then
    begin
      { Se já está no registro certo, acho que esse não precisa??? }
      FActiveRowChanged     := True;
      FActiveRowDataChanged := True;
      Invalidate
    end
  else
    begin
      FDataChanged := True;
      Invalidate;
    end;
end;

procedure TNvBsDbTable.LinkActive(Value: Boolean);
begin
  FBookmarks.LinkActive(Value);
  FDataChanged      := True;
  FActiveRowChanged := True;
  FLayoutChanged    := True;
  FColumsChanged    := True;
  UpdateRowCount;
  Invalidate;
end;

procedure TNvBsDbTable.Notification(AComponent: TComponent; Operation: TOperation);
var
  I         : Integer;
  NeedLayout: Boolean;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    begin
      if (FDataLink <> nil) then
        if (AComponent = DataSource) then
          DataSource := nil
        else if (AComponent is TField) then
          begin
            NeedLayout := False;
            BeginUpdate;
            try
              for I := 0 to Columns.Count - 1 do
                with Columns[I] do
                  if Field = AComponent then
                    begin
                      Field      := nil;
                      NeedLayout := True;
                    end;

              if FDataIdDBField = AComponent then
                begin
                  FDataIdDBField := nil;
                  NeedLayout     := True;
                end;

            finally
              if NeedLayout and Assigned(FDatalink.Dataset) and
                not(csDestroying in FDatalink.DataSet.ComponentState) and not FDatalink.Dataset.ControlsDisabled
              then
                begin
                  EndUpdate;
                  Invalidate;
                end
              else
                EndUpdate;
            end;
          end;
    end;
end;

procedure TNvBsDbTable.RecordChanged(aField: TField);
begin
  FActiveRowChanged     := True;
  FActiveRowDataChanged := True;
  Invalidate;
end;

procedure TNvBsDbTable.Scroll(Distance: Integer);
begin
  FActiveRowChanged := True;
  Invalidate;
end;

function TNvBsDbTable.SelectedField: TField;
begin
  // For editing only  ???
end;

procedure TNvBsDbTable.SetDataIdField(const Value: string);
var
  _Changed: Boolean;
begin
  _Changed := Value <> FDataIdField;
  inherited;
  if _Changed and FDataLink.Active then
    begin
      if FDataIdDBField <> nil then
        FDataIdDBField.RemoveFreeNotification(Self);

      FDataIdDBField := FDataLink.DataSet.FindField(FDataIdField);
      FDataIdDBField.FreeNotification(Self);
    end;
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

procedure TNvBsDbTable.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TNvBsDbTable.UpdateData;
begin
  // Save edit data to Dataset Field;
end;

procedure TNvBsDbTable.UpdateRowCount;
begin
  FDataLink.BufferCount := FDataLink.DataSet.RecordCount + 1;
end;

procedure TNvBsDbTable.RenderLayout(JSON: TJsonObject);
var
  I     : Integer;
  _Col  : TNvBsTableColumn;
  _Field: TField;
begin
  if FDataIdDBField = nil then
    CheckIDDbField;

  if FDataLink.Active and ((FColumns.PersistentCount = 0) or (FColumns.Combined and FColumsChanged))
  then
    begin
      BeginUpdate;
      try
        Columns.ClearNoPersistent;
        with Datalink.Dataset do
          for I := 0 to FieldList.Count - 1 do
            with FieldList[I] do
              if Visible then
                begin
                  _Col            := Columns.Add as TNvBsTableColumn;
                  _Col.Persistent := False;
                  _Col.FieldName  := FieldName;
                  _Col.Title      := DisplayLabel;
                end;
        FColumsChanged := False;
      finally
        EndUpdate;
      end;
    end;

  inherited;
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
  FDataChanged := True;
  Invalidate;
end;

procedure TNvBsTable.RenderData(JSON: TJsonObject);
begin
  inherited;
  JSON.A['Data'].Assign(FData);
  FDataChanged := False;
end;

{ TNvBsTblColFormatterActions }

procedure TNvBsTblColFormatterActions.Assign(Source: TPersistent);
begin
  if Source is TNvBsTblColFormatterActions then
    with TNvBsTblColFormatterActions(Source) do
      begin
        Self.ColActions := ColActions;
      end
  else
    inherited;
end;

constructor TNvBsTblColFormatterActions.Create(aColumn: TNvBsTableColumn);
begin
  inherited;
  FColActions := TNvBsFmtActions.Create(Self, TNvBsFmtAction);
end;

destructor TNvBsTblColFormatterActions.Destroy;
begin
  FColActions.Free;
  inherited;
end;

function TNvBsTblColFormatterActions.GetData: string;
begin
  Result := ColActions.GetData;
end;

procedure TNvBsTblColFormatterActions.Render(aJson: TJsonObject);
begin
  if ColActions.Count > 0 then
    begin
      aJson.s['formatter'] := 'actionFormatter';
      aJson.s['class']     := 'actions';

      // aJson.r['formatter'] := 'function(value, row, index){return ''<div class="btn-group">' +
      // ColActions.Render + '</div>''}';

      // // 'function actionFormatter(value, row, index){ return ''<div class="btn-group"><button class="btn btn-warning">Act1</button><button class="btn btn-danger">Act2</button></div>''}';
      aJson.S['events'] := 'actionEvents';
    end;
end;

procedure TNvBsTblColFormatterActions.SetColActions(const Value: TNvBsFmtActions);
begin
  if FColActions <> value then
    FColActions.Assign(Value);
end;

{ TNvBsFmtActions }

function TNvBsFmtActions.Formatter: TNvBsTblColFormatterBase;
begin
  Result := Owner as TNvBsTblColFormatterBase;
end;

function TNvBsFmtActions.GetColAction(Index: Integer): TNvBsFmtAction;
begin
  Result := Items[Index] as TNvBsFmtAction;
end;

function TNvBsFmtActions.GetData: string;
var
  I         : Integer;
  _ColAction: TNvBsFmtAction;
begin
  Result := '[';
  for I  := 0 to Count - 1 do
    begin
      Result := Result + ColAction[I].GetData + ',';
    end;
  if Count > 0 then
    Result := Result.Remove(Result.Length - 1);
  Result   := Result + ']';
end;

procedure TNvBsFmtActions.SetColAction(Index: Integer; const Value: TNvBsFmtAction);
begin
  Items[Index].Assign(Value);
end;

procedure TNvBsFmtActions.Update(Item: TCollectionItem);
begin
  inherited;

end;

{ TNvBsFmtAction }

procedure TNvBsFmtAction.AssignTo(Dest: TPersistent);
var
  _Dest: TNvBsFmtAction;
  // avoid to assign ancestor Action - bug in inherited forms
  function AssignAction: TNvCustomAction;
  var
    _DestRoot, _SourceRoot: TWinControl;
  begin
    if Not Assigned(Self.Action) then
      Exit(nil);
    // try
    _DestRoot   := (_Dest.Collection.Owner as TNvBsTblColFormatterActions).GetOwner as TWinControl;
    _SourceRoot := (Self.Collection.Owner as TNvBsTblColFormatterActions).GetOwner as TWinControl;
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
  if Dest is TNvBsFmtAction then
    begin
      _Dest := TNvBsFmtAction(Dest);
      with _Dest do
        begin
          FAction      := AssignAction;
          FVariant     := Self.Variant;
          FSize        := Self.Size;
          FShowCaption := Self.ShowCaption;
          FShowIcon    := Self.ShowIcon;
        end
    end
  else
    inherited;
end;

constructor TNvBsFmtAction.Create(Collection: TCollection);
begin
  inherited;
  FVariant     := bsbPrimary;
  FShowCaption := True;
  FShowIcon    := True;
end;

function TNvBsFmtAction.GetData: string;
begin
  if FAction <> nil then
    with TJsonObject.Create do
      try
        s['n'] := FAction.Name;
        if FVariant <> bsbPrimary then
          s['v'] := TBsButtonVariantStr[FVariant];
        if FSize <> bsbsDefault then
          s['s'] := TBsButtonSizeStr[FSize];
        if FShowCaption then
          s['c'] := FAction.Caption;
        if FShowIcon and FAction.ImageListLink.IsVisible then
          s['i'] := FAction.ImageListLink.Render;

        Result := ToJSON;
      finally
        Free;
      end;
end;

function TNvBsFmtAction.GetEnabled: Boolean;
begin
  Result := (FAction <> nil) and FAction.Enabled;
end;

procedure TNvBsFmtAction.SetAction(const Value: TNvCustomAction);
begin
  FAction := Value;
end;

procedure TNvBsFmtAction.SetEnabled(Value: Boolean);
begin
  if FAction <> nil then
    FAction.Enabled := Value;
end;

procedure TNvBsFmtAction.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
end;

procedure TNvBsFmtAction.SetShowIcon(const Value: Boolean);
begin
  FShowIcon := Value;
end;

procedure TNvBsFmtAction.SetSize(const Value: TBsButtonSize);
begin
  FSize := Value;
end;

procedure TNvBsFmtAction.SetVariant(const Value: TBsButtonVariant);
begin
  FVariant := Value;
end;

{ TNvBsTblColFormatterBase }

constructor TNvBsTblColFormatterBase.Create(aColumn: TNvBsTableColumn); // (AOwner: TComponent);
begin
  inherited Create;
  FColumn := aColumn;
  // Don't remove, because class type instantiation occurs
end;

function TNvBsTblColFormatterBase.GetOwner: TPersistent;
begin
  Result := FColumn.GetDbTable.Owner;
end;

{ TWriterHelper }

procedure TWriterHelper.WriteValue(Value: TValueType);
{$IF    SizeOf(TValueType) = 1}
var
  Buf: UInt8;
begin
  Buf := UInt8(Value);
  Write(Buf, 1);
end;
{$ELSE  SizeOf(TValueType) = 1}

var
  Buf: UInt32;
begin
  Buf := UInt32(Value);
  Write(Buf, SizeOf(Value));
end;
{$ENDIF SizeOf(TValueType) = 1}

initialization

FormaterClasses        := TStringList.Create;
FormaterClasses.Sorted := True;
RegisterColFormatter(TNvBsTblColFormatterActions);

finalization

FormaterClasses.Free;

end.
