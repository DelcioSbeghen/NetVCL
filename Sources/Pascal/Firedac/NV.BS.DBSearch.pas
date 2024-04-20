unit NV.BS.DBSearch;

interface

uses
  Classes, NV.BS.Inputs, firedac.Comp.Client, DB, EAVQuery, NV.JSON, NV.BS.Tables;

type
  TBeforeSearchEvent = procedure(var aSelectClause: string; var aFromClause: string;
    var aWhereClause: string; var aOrderClause: string) of Object;

  TColunaItem = class(TCollectionItem)
  private
    FLargura    : Integer;
    FCampo      : string;
    FMoeda      : Boolean;
    FLarguraAuto: Boolean;
    procedure SetCampo(const Value: string);
    procedure SetLargura(const Value: Integer);
    procedure SetMoeda(const Value: Boolean);
    procedure SetLarguraAuto(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property LarguraAuto: Boolean read FLarguraAuto write SetLarguraAuto default True;
    property Campo      : string read FCampo write SetCampo;
    property Largura    : Integer read FLargura write SetLargura;
    property Moeda      : Boolean read FMoeda write SetMoeda;
  end;

  TColunas = class(TCollection)
  private
    FOwner: TComponent;
    function GetColumnItem(Index: Integer): TColunaItem;
    procedure SetColumnItem(Index: Integer; const Value: TColunaItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TColunaItem;
    function Insert(Index: Integer): TColunaItem;
    property Items[Index: Integer]: TColunaItem read GetColumnItem write SetColumnItem;
  end;

  TCamposBuscaItem = class(TCollectionItem)
  private
    FUsarLike: Boolean;
    FCampo   : string;
    procedure SetCampo(const Value: string);
    procedure SetUsarLike(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Campo   : string read FCampo write SetCampo;
    property UsarLike: Boolean read FUsarLike write SetUsarLike;
  end;

  TCamposBusca = class(TCollection)
  private
    FOwner: TComponent;
    function GetCamposBuscaItem(Index: Integer): TCamposBuscaItem;
    procedure SetCamposBuscaItem(Index: Integer; const Value: TCamposBuscaItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TCamposBuscaItem;
    function Insert(Index: Integer): TCamposBuscaItem;
    function LocateByCampo(aCampo: string): TCamposBuscaItem;
    property Items[Index: Integer]: TCamposBuscaItem read GetCamposBuscaItem
      write SetCamposBuscaItem;
  end;

  TNvBsSearch = class(TNvBsInput)
  private
    FConnecion     : TFDConnection;
    FQryBusca      : TpQuery;
    FDsQryBusca    : TDataSource;
    FResultDatalink: TDatalink;
    FTableOrigem   : string;
    FCamposBusca   : TCamposBusca;
    FColunas       : TColunas;
    FCampoOrdenacao: string;
    FCampoChave    : string;
    FCampoUpdate   : string;
    FResultBusca   : Variant;
    FOnBeforeSearch: TBeforeSearchEvent;
    FonSelectErro  : TNotifyEvent;
    FonSelectOk    : TNotifyEvent;
    // function GetDataSource: TDataSource;
    procedure SetConnection(const Value: TFDConnection);
    // procedure SetDataSource(const Value: TDataSource);
    procedure SetCamposBusca(const Value: TCamposBusca);
    procedure SetColunas(const Value: TColunas);
    function GetResultBusca: TpQuery;
    procedure SetCampoUpdate(const Value: string);
    procedure SetOnBeforeSearch(const Value: TBeforeSearchEvent);
  protected
    procedure DataChange(Sender: TObject); override;
    procedure UpdateData(Sender: TObject); override;
    //
    procedure BuscaLista(Key: Word; Value: string);
    procedure DoKeyUp(aEvent: TJsonObject);
    procedure DoSelect(aEvent: TJsonObject);
    function ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Nome da tabela de origem no banco
    Property TableNameOrigem: string read FTableOrigem write FTableOrigem;
    // Connection do banco de dados
    property Connection: TFDConnection read FConnecion Write SetConnection;
    // Campo da tabela de origem a ser buscado
    property CamposBusca: TCamposBusca read FCamposBusca Write SetCamposBusca;
    // campos da listagem de resultados
    property CamposLista   : TColunas read FColunas write SetColunas;
    property CampoOrdenacao: string read FCampoOrdenacao write FCampoOrdenacao;
    // campo chave para localizar o resultado selecionado na query  Resultados
    property CampoChave : string read FCampoChave Write FCampoChave;
    property CampoUpdate: string read FCampoUpdate write SetCampoUpdate;
    // property DataSourceState: TDataSource read GetDataSource write SetDataSource;
    property ResultBusca   : Variant read FResultBusca write FResultBusca;
    property QryResultBusca: TpQuery read GetResultBusca;
    // property ZebrarLista      : Boolean read FZebrarLista write FZebrarLista default True;
    // property GridLista        : TsMySdbGrid read FListtxt write FListtxt;
    // property AlturaLinhaLista : Integer read FAlturaLinhaLista write SetAlturaLinhaLista;
    // property AlturaLista      : Integer read FAlturaLista write SetAlturaLista default 265;
    // property SelecionaAoClicar: Boolean read FSelecionaAoClicar write SetSelecionaAoClicar
    // default True;
    // property OnDrawColumnCell: TDrawColumnCellEvent read GetDrawColumnCell write SetDrawColumnCell;
    // property OnGetCellParams: TGetCellParamsEvent read GetOnGetCellParams write SetOnGetCellParams;
    property onSelectOk    : TNotifyEvent read FonSelectOk write FonSelectOk;
    property onSelectErro  : TNotifyEvent read FonSelectErro write FonSelectErro;
    property OnBeforeSearch: TBeforeSearchEvent read FOnBeforeSearch write SetOnBeforeSearch;
  end;

procedure register;

implementation

uses
  Windows, SysUtils, StrUtils, firedac.Stan.Option;

procedure register;
begin
  RegisterComponents('NetVCL BS', [TNvBsSearch]);
end;

function SubstituiAcentos(Texto: string): string;
Var
  A           : Integer;
  Letra       : AnsiChar;
  AnsiStr, Ret: AnsiString;
  function TiraAcento(const AChar: AnsiChar): AnsiChar;
  begin
    case AChar of
      'à', 'á', 'ã', 'ä', 'â': Result := 'a';
      'À', 'Á', 'Ã', 'Ä', 'Â': Result := 'A';
      'è', 'é', 'ë', 'ê': Result      := 'e';
      'È', 'É', 'Ë', 'Ê': Result      := 'E';
      'ì', 'í', 'ï', 'î': Result      := 'i';
      'Ì', 'Í', 'Ï', 'Î': Result      := 'I';
      'ò', 'ó', 'õ', 'ö', 'ô': Result := 'o';
      'Ò', 'Ó', 'Õ', 'Ö', 'Ô': Result := 'O';
      'ù', 'ú', 'ü', 'û': Result      := 'u';
      'Ù', 'Ú', 'Ü', 'Û': Result      := 'U';
      'ç': Result                     := 'c';
      'Ç': Result                     := 'C';
      'ñ': Result                     := 'n';
      'Ñ': Result                     := 'N';
    else Result                       := AChar;
    end;
  end;

begin
  Result  := '';
  Ret     := '';
  AnsiStr := AnsiString(Texto);
  For A   := 1 to Length(AnsiStr) do
    begin
      Letra := TiraAcento(AnsiStr[A]);
      if not(Letra in [#32 .. #126, #13, #10, #8]) then { Letras / numeros / pontos / sinais }
        Letra := ' ';
      Ret     := Ret + Letra;
    end;
  Result := String(Ret)
End;

{ TNvBsSearch }

procedure TNvBsSearch.BuscaLista(Key: Word; Value: string);
var
  CamposBuscaSql                                    : string;
  _i, L                                             : Integer;
  SearchString                                      : string;
  SelectClause, FromClause, WhereClause, OrderClause: string;
  _Result                                           : TJsonObject;
  _Data                                             : TJsonArray;
  _Cols                                             : TJsonArray;
  _LastRec                                          : Integer;
  c                                                 : Integer;
begin
  // busca
  if FTableOrigem.IsEmpty then // verifica se foi especificaca a tabela
    raise Exception.Create('Não foi especificada a propriedade TableNameOrigem no componente ' +
      Self.Name);
  // Se não fôr especificado o Campo Busca, adiciona
  if (FCamposBusca.Count = 0) and (FColunas.Count > 0) then // Se o CampoBusca estiver vazio...
    with FCamposBusca.Add do
      begin // será a primeira coluna.
        FCampo    := FColunas.Items[0].Campo;
        FUsarLike := True;
      end;
  CamposBuscaSql := ''; // campos buscados na sql
  // Se fôr especificado o campo Busca e não o campo Chave, o campo chave será o campo Busca
  if (FCampoChave = '') and (FCamposBusca.Count > 0) then
    FCampoChave := FCamposBusca.Items[0].Campo;
  if (FCampoChave = '') and (FCamposBusca.Count = 0) then
    raise Exception.Create
      ('Não foram especidficados CampoBusca, CampoChave ou CamposLista no componente ' + Self.Name);
  // Adiciona as Colunas a Sql de busca
  for _i := 0 to FColunas.Count - 1 do // adiciona as colunas a SQL
    begin
      if CamposBuscaSql <> '' then
        CamposBuscaSql := CamposBuscaSql + ',' + FColunas.Items[_i].Campo
      else
        CamposBuscaSql := FColunas.Items[_i].Campo;
    end;
  // Se não forem Setadas colunas, os CamposBusca serão as Colunas
  if CamposBuscaSql = '' then // se não foram setadas colunas...
    begin
      for _i := 0 to FCamposBusca.Count - 1 do
        begin // adiciona os CamposBusca como colunas.
          if CamposBuscaSql <> '' then
            CamposBuscaSql := CamposBuscaSql + ', ' + FCamposBusca.Items[_i].Campo
          else
            CamposBuscaSql := FCamposBusca.Items[_i].Campo;
        end;
    end;
  // adiciona o Campo Chave no primeiro campo da SQL
  if FCampoChave <> '' then
    begin
      if SearchBuf(PChar(CamposBuscaSql), CamposBuscaSql.Length, 0, 0, FCampoChave,
        [soDown, soWholeWord]) = nil then
        begin
          if CamposBuscaSql <> '' then
            CamposBuscaSql := FCampoChave + ',' + CamposBuscaSql
          else
            CamposBuscaSql := FCampoChave
        end;
    end;
  // remove caracteres indesejados
  SearchString := Value;
  if pos('ª', SearchString) > 0 then
    SearchString[(pos('ª', SearchString))] := 'a';
  if pos('º', SearchString) > 0 then
    SearchString[(pos('º', SearchString))] := 'o';
  SearchString                             := SubstituiAcentos(SearchString);

  SelectClause := 'select ' + CamposBuscaSql;
  FromClause   := 'from ' + FTableOrigem;
  WhereClause  := '';
  if FCamposBusca.Items[0].UsarLike then
    begin
      WhereClause := FCamposBusca.Items[0].Campo + ' like ' + QuotedStr('%' + SearchString + '%');
    end
  else
    begin
      WhereClause := FCamposBusca.Items[0].Campo + ' like ' + QuotedStr(SearchString);
    end;
  if FCamposBusca.Count > 1 then
    begin
      for _i := 1 to FCamposBusca.Count - 1 do
        begin
          if FCamposBusca.Items[_i].UsarLike then
            begin
              WhereClause := WhereClause + ' or ' + FCamposBusca.Items[_i].Campo + ' like ' +
                QuotedStr('%' + SearchString + '%');
            end
          else
            begin
              WhereClause := WhereClause + ' or ' + FCamposBusca.Items[_i].Campo + ' like ' +
                QuotedStr(SearchString);
            end;
        end;
    end;
  OrderClause := '';

  if Assigned(FOnBeforeSearch) then
    FOnBeforeSearch(SelectClause, FromClause, WhereClause, OrderClause);

  // efetua a busca
  FQryBusca.Close;
  FQryBusca.SQL.Clear;
  FQryBusca.SQL.Add(SelectClause);
  FQryBusca.SQL.Add(FromClause);
  FQryBusca.SQL.Add('WHERE ' + WhereClause);
  FQryBusca.SQL.Add(OrderClause);

  if FCampoOrdenacao = '' then
    FQryBusca.IndexFieldNames := FCamposBusca.Items[0].Campo
  else
    FQryBusca.IndexFieldNames := FCampoOrdenacao;
  // FQryBusca.SortType:= stAscending;
  FQryBusca.Open;
  _Result := ControlAjaxJson.O['Result'];
  _Data   := _Result.A['Data'];
  _Cols   := _Result.A['Cols'];
  _Data.Clear;
  _Cols.Clear;

  for c := 0 to FColunas.Count - 1 do
    with _Cols.AddObject do
      begin
        s['c'] := FColunas.Items[c].FCampo;
      end;

  if FQryBusca.RecordCount > 0 then
    begin
      FResultDatalink.BufferCount := FQryBusca.RecordCount;
      // formata campos marcados como moeda
      for _i := 0 to FColunas.Count - 1 do
        begin
          if FColunas.Items[_i].Moeda then
            (FQryBusca.F[FColunas.Items[_i].Campo] as TCurrencyField).currency := True;
        end;
      _Result.B['Visible'] := True;

      _LastRec := FResultDatalink.ActiveRecord;
      try
        for _i := 0 to FQryBusca.RecordCount - 1 do
          with _Data.AddObject do
            begin
              FResultDatalink.ActiveRecord  := _i;
              s['Key']                      := FQryBusca.F[FCampoChave].Text;
              for c                         := 0 to FColunas.Count - 1 do
                s[FColunas.Items[c].FCampo] := FQryBusca.F[FColunas.Items[c].FCampo].DisplayText;
            end;
      finally
        FResultDatalink.ActiveRecord := _LastRec;
      end;
    end
  else
    begin
      _Result.B['Visible'] := False;
    end;

  Invalidate;
end;

constructor TNvBsSearch.Create(AOwner: TComponent);
begin
  inherited;
  FCamposBusca                := TCamposBusca.Create(Self);
  FColunas                    := TColunas.Create(Self);
  FQryBusca                   := TpQuery.Create(Self);
  FQryBusca.FetchOptions.Mode := TfdFetchMode.fmAll;

  FDsQryBusca         := TDataSource.Create(Self);
  FDsQryBusca.DataSet := FQryBusca;

  FResultDatalink            := TDataLink.Create;
  FResultDatalink.DataSource := FDsQryBusca;

end;

// function TNvBsSearch.GetDataSource: TDataSource;
// begin
//
// end;

procedure TNvBsSearch.DataChange(Sender: TObject);
begin
  if (DataLink.DataSource <> nil) and not DataLink.FieldName.IsEmpty //
    and not(csDesigning in ComponentState) then
    inherited;
end;

destructor TNvBsSearch.Destroy;
begin
  FResultDatalink.Free;
  inherited;
end;

procedure TNvBsSearch.DoKeyUp(aEvent: TJsonObject);
begin
  OutputDebugString(PChar(aEvent.ToJSON));
  BuscaLista(aEvent.I['keyCode'], aEvent.s['value']);
end;

procedure TNvBsSearch.DoSelect(aEvent: TJsonObject);
var
  _key: string;
begin
  _key := aEvent.S['Key'];
  if not FQryBusca.LocateEx(FCampoChave, _key) then
    raise Exception.Create('Não foi possível encontrar o registro selecionado!');

  if Not FQryBusca.F[FCampoChave].isNull then
    begin
      FResultBusca := FQryBusca.Fv[FCampoChave];
      if Assigned(DataLink.Field) then // Exibição ligada a campo
        begin
          if (Not FCampoUpdate.IsEmpty) then
            begin
              DataLink.Modified;
              try
                DataLink.UpdateRecord;
              except
                DataLink.Reset;
                raise;
              end;
            end;
        end
      else if (Not FCampoUpdate.IsEmpty) then
        Text                                   := FQryBusca.Fs[FCampoUpdate];
      ControlAjaxJson.O['Result'].B['Visible'] := False;
      if Assigned(FonSelectOk) then
        FonSelectOk(Self);
    end
  else
    begin
      if (Not FCampoUpdate.IsEmpty) and Assigned(DataLink.Field) then
        DataLink.Reset;
      FResultBusca                             := '';
      Text                                     := '';
      ControlAjaxJson.O['Result'].B['Visible'] := False;
      if Assigned(FonSelectErro) Then
        FonSelectErro(Self);
    end;

end;

function TNvBsSearch.GetResultBusca: TpQuery;
begin
  Result := FQryBusca;
end;

function TNvBsSearch.ProcessEvent(AEventName: string; aEvent: TJsonObject): Boolean;
begin
  case IndexStr(AEventName, ['keyup', 'selected']) of
    0:
      begin
        DoKeyUp(aEvent);
        Result := True;
      end;
    1:
      begin
        DoSelect(aEvent);
        Result := True;
      end;
  else Result := inherited;
  end;
end;

procedure TNvBsSearch.SetCamposBusca(const Value: TCamposBusca);
begin
  FCamposBusca.Assign(Value);
end;

procedure TNvBsSearch.SetCampoUpdate(const Value: string);
begin
  FCampoUpdate := Value;
end;

procedure TNvBsSearch.SetColunas(const Value: TColunas);
begin
  FColunas.Assign(Value);
end;

procedure TNvBsSearch.SetConnection(const Value: TFDConnection);
begin
  if FConnecion <> Value then
    begin
      FConnecion           := Value;
      FQryBusca.Connection := FConnecion;
    end;
end;

// procedure TNvBsSearch.SetDataSource(const Value: TDataSource);
// begin
//
// end;

procedure TNvBsSearch.SetOnBeforeSearch(const Value: TBeforeSearchEvent);
begin
  FOnBeforeSearch := Value;
end;

procedure TNvBsSearch.UpdateData(Sender: TObject);
begin
  ValidateValue;
  if not FCampoUpdate.IsEmpty and not FCampoChave.IsEmpty then
    DataLink.DataSet.FieldByName(FCampoUpdate).Value := FQryBusca.F[FCampoChave].Value;
end;

{ TColunaItem }

procedure TColunaItem.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source <> Self) and (Source is TColunaItem) then
    begin
      Largura     := TColunaItem(Source).Largura;
      Campo       := TColunaItem(Source).Campo;
      Moeda       := TColunaItem(Source).Moeda;
      Campo       := TColunaItem(Source).Campo;
      LarguraAuto := TColunaItem(Source).LarguraAuto;
    end;

end;

constructor TColunaItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FLarguraAuto := True;
  FLargura     := 30;
  FMoeda       := False;
end;

function TColunaItem.GetDisplayName: string;
begin
  if FCampo <> '' then
    Result := FCampo
  else
    Result := Format('Coluna %d', [Index]);
end;

procedure TColunaItem.SetCampo(const Value: string);
begin
  FCampo := Value;
end;

procedure TColunaItem.SetLargura(const Value: Integer);
begin
  FLargura := Value;
end;

procedure TColunaItem.SetLarguraAuto(const Value: Boolean);
begin
  FLarguraAuto := Value;
end;

procedure TColunaItem.SetMoeda(const Value: Boolean);
begin
  FMoeda := Value;
end;

{ TColunas }

function TColunas.Add: TColunaItem;
begin
  Result := TColunaItem(inherited Add);
end;

constructor TColunas.Create(AOwner: TComponent);
begin
  inherited Create(TColunaItem);
  FOwner := AOwner;
end;

function TColunas.GetColumnItem(Index: Integer): TColunaItem;
begin
  Result := TColunaItem(inherited Items[Index]);
end;

function TColunas.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TColunas.Insert(Index: Integer): TColunaItem;
begin
  Result := TColunaItem(inherited Insert(Index));
end;

procedure TColunas.SetColumnItem(Index: Integer; const Value: TColunaItem);
begin
  Items[Index].Assign(Value);
end;

{ TCamposBuscaItem }

procedure TCamposBuscaItem.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source <> Self) and (Source is TCamposBuscaItem) then
    begin
      Campo    := TCamposBuscaItem(Source).Campo;
      UsarLike := TCamposBuscaItem(Source).UsarLike;
    end;
end;

constructor TCamposBuscaItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FUsarLike := False;
end;

function TCamposBuscaItem.GetDisplayName: string;
begin
  if FCampo <> '' then
    Result := FCampo
  else
    Result := Format('Coluna %d', [Index]);
end;

procedure TCamposBuscaItem.SetCampo(const Value: string);
begin
  FCampo := Value;
end;

procedure TCamposBuscaItem.SetUsarLike(const Value: Boolean);
begin
  FUsarLike := Value;
end;

{ TCamposBusca }

function TCamposBusca.Add: TCamposBuscaItem;
begin
  Result := TCamposBuscaItem(inherited Add);
end;

constructor TCamposBusca.Create(AOwner: TComponent);
begin
  inherited Create(TCamposBuscaItem);
  FOwner := AOwner;
end;

function TCamposBusca.GetCamposBuscaItem(Index: Integer): TCamposBuscaItem;
begin
  Result := TCamposBuscaItem(inherited Items[Index]);
end;

function TCamposBusca.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TCamposBusca.Insert(Index: Integer): TCamposBuscaItem;
begin
  Result := TCamposBuscaItem(inherited Insert(Index));
end;

function TCamposBusca.LocateByCampo(aCampo: string): TCamposBuscaItem;
var
  I: Integer;
begin
  Result := nil;
  for I  := 0 to Count - 1 do
    if Items[I].FCampo = aCampo then
      begin
        Result := Items[I];
        Break;
      end;
end;

procedure TCamposBusca.SetCamposBuscaItem(Index: Integer; const Value: TCamposBuscaItem);
begin
  Items[Index].Assign(Value);
end;

end.
