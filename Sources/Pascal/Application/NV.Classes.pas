unit NV.Classes;

interface

uses
  Classes, Windows, RTLConsts, SysUtils, SyncObjs;

type

{-------------------------------------------------------------
  TThreadStringList
  Author: Tilo Eckert
  Date: 19.06.2004

  TThreadStringList is a simple wrapper for TStringList.
  This makes it possible to access a StringList from
  different threads without any conflicts.
  Most functions and properties are included.
 -------------------------------------------------------------
  TThreadStringList ist ein einfacher Wrapper für TStringList,
  der es ermöglicht von verschiedenen Threads auf eine
  Stringliste zuzugreifen ohne das Konflikte entstehen.
  Die wichtigsten Funktionen und Eigenschaften sind enthalten.
 -------------------------------------------------------------}
  TThreadStringList = class
  private
    FStringList: TStringList;
    FLock: TRTLCriticalSection;
    function GetDuplicates: TDuplicates;
    procedure SetDuplicates(dup: TDuplicates);
    function GetCapacity: Integer;
    procedure SetCapacity(capa: Integer);
    function GetCommaText: string;
    procedure SetCommaText(const S: string);
    function GetCount: Integer;
    function GetDelimiter: Char;
    procedure SetDelimiter(delim: Char);
    function GetDelimitedText: string;
    procedure SetDelimitedText(const S: string);
    function GetNames(Index: Integer): string;
    function GetValues(const Name: string): string;
    procedure SetValues(const Name: string; S: string);
    function GetStrings(Index: Integer): string;
    procedure SetStrings(Index: Integer; S: string);
    function GetAsText: string;
    procedure SetAsText(S: string);
    procedure AssignError(Source: TPersistent);
    function GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; const Value: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function LockList: TStringList;
    procedure UnlockList;
    function Add(const S: string): Integer;
    procedure AddStrings(Strings: TStrings);
    procedure Delete(Index: Integer);
    procedure Clear;
    procedure Exchange(Index1, Index2: Integer);
    function Find(const S: string; var Index: Integer): Boolean;
    procedure Insert(Index: Integer; const S: string);
    function IndexOf(const S: string): Integer;
    function IndexOfName(const Name: string): Integer;
    procedure Sort;
    function GetText: {$IFDEF FPC} PChar {$ELSE} PWideChar {$ENDIF};
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure Assign(Source: TPersistent);
    //object need to be Thread Safe
    function AddObject(const S: string; AObject: TObject): Integer;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Names[Index: Integer]: string read GetNames;
    property Values[const Name: string]: string read GetValues write SetValues;
    property Strings[Index: Integer]: string read GetStrings write SetStrings; default;
    property Text: string read GetAsText write SetAsText;
    //objects need to be Thread Safe
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
  end;

  TNVThreadList = class
  private
    FLock: TCriticalSection;
    FList: TList;
    FDuplicates: TDuplicates;
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; const Value: Pointer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function LockList: TList;
    procedure Remove(Item: Pointer); inline;
    {$IFNDEF FPC}
    procedure RemoveItem(Item: Pointer; Direction: TList.TDirection);
    {$ENDIF}
    procedure UnlockList; inline;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    procedure Delete(aIndex: Integer);
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
    property Count: Integer read GetCount;
  end;



implementation

{ TThreadStringList }

constructor TThreadStringList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FStringList := TStringList.Create;
  FStringList.Duplicates := dupIgnore;
end;

destructor TThreadStringList.Destroy;
begin
  LockList;
  try
    FStringList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;
end;

function TThreadStringList.LockList: TStringList;
begin
  EnterCriticalSection(FLock);
  Result := FStringList;
end;

procedure TThreadStringList.PutObject(Index: Integer; const Value: TObject);
begin
  LockList;
  try
    FStringList.Objects[Index] := Value;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

function TThreadStringList.Add(const S: string): Integer;
begin
  Result := -1;
  LockList;
  try
    Result := FStringList.Add(S);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := -1;
  LockList;
  try
    Result := FStringList.AddObject(S, AObject);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.AddStrings(Strings: TStrings);
begin
  LockList;
  try
    FStringList.AddStrings(Strings);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Assign(Source: TPersistent);
begin
  LockList;
  try
    if Source.ClassType = TThreadStringList then
      FStringList.Assign(TThreadStringList(Source).FStringList)
    else if Source is TStringList then
      FStringList.Assign(Source)
    else
      AssignError(Source);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.AssignError(Source: TPersistent);
var
  SourceName: string;
begin
  if Source <> nil then
    SourceName := Source.ClassName
  else
    SourceName := 'nil';
  raise EConvertError.CreateResFmt(@SAssignError, [SourceName, ClassName]);
end;

procedure TThreadStringList.Delete(Index: Integer);
begin
  LockList;
  try
    FStringList.Delete(Index);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Clear;
begin
  LockList;
  try
    FStringList.Clear;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Exchange(Index1, Index2: Integer);
begin
  LockList;
  try
    FStringList.Exchange(Index1, Index2);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.Find(const S: string; var Index: Integer): Boolean;
begin
  LockList;
  try
    Result := FStringList.Find(S, Index);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Insert(Index: Integer; const S: string);
begin
  LockList;
  try
    FStringList.Insert(Index, S);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.IndexOf(const S: string): Integer;
begin
  Result := -1;
  LockList;
  try
    Result := FStringList.IndexOf(S);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.IndexOfName(const Name: string): Integer;
begin
  Result := -1;
  LockList;
  try
    Result := FStringList.IndexOfName(Name);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Sort;
begin
  LockList;
  try
    FStringList.Sort;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetText: {$IFDEF FPC} PChar {$ELSE} PWideChar {$ENDIF};
begin
  LockList;
  try
    Result := FStringList.GetText;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.LoadFromFile(const FileName: string);
begin
  LockList;
  try
    FStringList.LoadFromFile(FileName);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.LoadFromStream(Stream: TStream);
begin
  LockList;
  try
    FStringList.LoadFromStream(Stream);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SaveToFile(const FileName: string);
begin
  LockList;
  try
    FStringList.SaveToFile(FileName);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SaveToStream(Stream: TStream);
begin
  LockList;
  try
    FStringList.SaveToStream(Stream);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetDuplicates: TDuplicates;
begin
  LockList;
  try
    Result := FStringList.Duplicates;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetDuplicates(dup: TDuplicates);
begin
  LockList;
  try
    FStringList.Duplicates := dup;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetCapacity: Integer;
begin
  LockList;
  try
    Result := FStringList.Capacity;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetCapacity(capa: Integer);
begin
  LockList;
  try
    FStringList.Capacity := capa;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetCommaText: string;
begin
  LockList;
  try
    Result := FStringList.CommaText;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetCommaText(const S: string);
begin
  LockList;
  try
    FStringList.CommaText := S;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetCount: Integer;
begin
  LockList;
  try
    Result := FStringList.Count;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetDelimiter: Char;
begin
  LockList;
  try
    Result := FStringList.Delimiter;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetDelimiter(delim: Char);
begin
  LockList;
  try
    FStringList.Delimiter := delim;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetDelimitedText: string;
begin
  LockList;
  try
    Result := FStringList.DelimitedText;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetDelimitedText(const S: string);
begin
  LockList;
  try
    FStringList.DelimitedText := S;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetNames(Index: Integer): string;
begin
  LockList;
  try
    Result := FStringList.Names[Index];
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetObject(Index: Integer): TObject;
begin
  Result := nil;
  LockList;
  try
    Result := FStringList.Objects[Index];
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetValues(const Name: string): string;
begin
  LockList;
  try
    Result := FStringList.Values[Name];
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetValues(const Name: string; S: string);
begin
  LockList;
  try
    FStringList.Values[Name] := S;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetStrings(Index: Integer): string;
begin
  LockList;
  try
    Result := FStringList.Strings[Index];
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetStrings(Index: Integer; S: string);
begin
  LockList;
  try
    FStringList.Strings[Index] := S;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetAsText: string;
begin
  LockList;
  try
    Result := FStringList.Text;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.SetAsText(S: string);
begin
  LockList;
  try
    FStringList.Text := S;
  finally
    UnlockList;
  end;
end;

{ TNVThreadList }
{ TDWThreadList }

destructor TNVThreadList.Destroy;
begin
  LockList; // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
end;

function TNVThreadList.GetCount: Integer;
begin
  try
    Result := LockList.Count;
  finally
    UnlockList;
  end;
end;

function TNVThreadList.GetItem(Index: Integer): Pointer;
begin
  try
    Result := LockList.Items[Index];
  finally
    UnlockList;
  end;
end;

procedure TNVThreadList.Add(Item: Pointer);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
      FList.Error({$IFNDEF FPC} @ {$ENDIF}SDuplicateItem, IntPtr(Item));
  finally
    UnlockList;
  end;
end;

procedure TNVThreadList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

constructor TNVThreadList.Create;
begin
  inherited Create;
  FLock       := TCriticalSection.Create;
  FList       := TList.Create;
  FDuplicates := dupIgnore;
end;

function TNVThreadList.LockList: TList;
begin
  FLock.Acquire;
  Result := FList;
end;

procedure TNVThreadList.Remove(Item: Pointer);
begin
  {$IFDEF FPC}
   LockList;
  try
    FList{$IFDEF FPC} .Remove {$ELSE} .RemoveItem {$ENDIF}(Item);
  finally
    UnlockList;
  end;
  {$ELSE}
  RemoveItem(Item, TList.TDirection.FromBeginning);
  {$ENDIF}
end;

{$IFNDEF FPC}
procedure TNVThreadList.RemoveItem(Item: Pointer; Direction: TList.TDirection);
begin
  LockList;
  try
    FList.RemoveItem(Item, Direction);
  finally
    UnlockList;
  end;
end;
{$ENDIF}

procedure TNVThreadList.Delete(aIndex: Integer);
begin
  try
    LockList.Delete(aIndex);
  finally
    UnlockList;
  end;
end;

procedure TNVThreadList.SetItem(Index: Integer; const Value: Pointer);
begin
  try
    LockList.Items[Index] := Value;
  finally
    UnlockList;
  end;
end;

procedure TNVThreadList.UnlockList;
begin
  FLock.Release;
end;
end.

