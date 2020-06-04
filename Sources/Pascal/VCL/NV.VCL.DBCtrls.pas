unit NV.VCL.DBCtrls;

interface

uses
   NV.Interfaces, DB, SysUtils;

type

    EInvalidGridOperation = class(Exception);

  TpGridDatalink = class(TDataLink)
  private
    FModified    : Boolean;
    FInUpdateData: Boolean;
    FGrid        : INvDbGrid;
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
    constructor Create(AGrid: INvDbGrid);
    destructor Destroy; override;
    // function AddMapping(const FieldName: string): Boolean;
    // procedure ClearMapping;
    procedure Modified;
    procedure Reset;
    // property DefaultFields: Boolean read GetDefaultFields;
    // property FieldCount: Integer read FFieldCount;
    // property Fields[I: Integer]: TField read GetFields;
    // property SparseMap: Boolean read FSparseMap write FSparseMap;
    property Grid: INvDbGrid read FGrid;
  end;

  TpBookmarkList = class
  private
    FList      : array of TBookmark;
    FGrid      : INvDbGrid;
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
    constructor Create(AGrid: INvDbGrid);
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

procedure RaiseGridError(const S: string);

implementation

uses
  DBConsts, RTLConsts;

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

constructor TpGridDatalink.Create(AGrid: INvDbGrid);
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
  FGrid.LayoutChanged;
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
    if Length(FList) = 0 then Exit;
  SetLength(FList, 0);
  FGrid.Invalidate;
end;

function TpBookmarkList.Compare(const Item1, Item2: TBookmark): Integer;
begin
  with FGrid.Datalink.Datasource.Dataset do
    Result := CompareBookmarks(TBookmark(Item1), TBookmark(Item2));
end;

constructor TpBookmarkList.Create(AGrid: INvDbGrid);
begin
  inherited Create;
  SetLength(FList, 0);
  FGrid := AGrid;
end;

function TpBookmarkList.CurrentRow: TBookmark;
begin
    if not FLinkActive then RaiseGridError(sDataSetClosed);
  Result := FGrid.Datalink.Datasource.Dataset.Bookmark;
end;

procedure TpBookmarkList.DataChanged(Sender: TObject);
begin
    FCache := nil;
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
      for I := Length(FList)-1 downto 0 do
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
  if Index < Count-1 then
  begin
    System.Move(FList[Index + 1], FList[Index],
      (Count - Index - 1) * SizeOf(Pointer));
    // Make sure we don't finalize the item that was in the last position.
    PPointer(@FList[Count-1])^ := nil;
  end;
  SetLength(FList, Count-1);
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
    Index := FCacheIndex;
    Result := FCacheFind;
    Exit;
  end;
  Result := False;
  L := 0;
  H := Length(FList) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(FList[I], Item);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
  FCache := Item;
  FCacheIndex := Index;
  FCacheFind := Result;
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
    Move(FList[Index], FList[Index + 1],
      (Count - Index - 1) * SizeOf(Pointer));
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
    if Result then FGrid.Invalidate;
  end;
end;

procedure TpBookmarkList.SetCurrentRowSelected(Value: Boolean);
var
  Index: Integer;
  Current: TBookmark;
begin
  Current := CurrentRow;
  if (Length(Current) = 0) or (Find(Current, Index) = Value) then Exit;
  if Value then
    InsertItem(Index, Current)
  else
    DeleteItem(Index);
  FGrid.InvalidateRow(FGrid.GetRow);

end;

end.
