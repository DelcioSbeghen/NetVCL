unit NV.BS.Design.TableDataEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DesignEditors, DesignIntf, NV.BS.Tables,
  Data.DB, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.DBGrids,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TFrmNvBsTableDataEditor = class(TForm)
    dbgrd1: TDBGrid;
    Panel1: TPanel;
    BtnOk: TButton;
    btnCancel: TButton;
    MemTable: TFDMemTable;
    DataSource1: TDataSource;
    procedure btnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
  private
    FSource: TNvBsTable;
  public
    procedure LoadArray(Source: TNvBsTable);
  end;

  TNvBsTableDataEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

var
  FrmNvBsTableDataEditor: TFrmNvBsTableDataEditor;

implementation

uses
  NV.JSON;

{$R *.dfm}
{ TNvBsTableDataEditor }

procedure TNvBsTableDataEditor.Edit;
begin
  inherited;

  FrmNvBsTableDataEditor := TFrmNvBsTableDataEditor.Create(nil);
  try
    FrmNvBsTableDataEditor.LoadArray(GetComponent(0) as TNvBsTable);
    if FrmNvBsTableDataEditor.ShowModal = mrOk then
      Designer.Modified;
  finally
    FrmNvBsTableDataEditor.Free;
  end;
end;

function TNvBsTableDataEditor.GetAttributes: TPropertyAttributes;
begin
  result := [paDialog];
end;

{ TFrmNvBsTableDataEditor }

procedure TFrmNvBsTableDataEditor.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFrmNvBsTableDataEditor.BtnOkClick(Sender: TObject);
var
  _Data         : TJsonArray;
  _JSonRow      : TJsonObject;
  _Field        : TField;
  F             : Integer;
  _FloatValue   : Double;
  _IntegerValue : Integer;
  _DateTimeValue: TDateTime;
begin
  _Data := TJsonArray.Create;
  try
    MemTable.First;
    while NOT MemTable.Eof do
      begin
        _JSonRow := _Data.AddObject;

        for F := 0 to MemTable.FieldCount - 1 do
          begin
            _Field := MemTable.Fields[F];
            if TryStrToFloat(_Field.AsString, _FloatValue) then
              _JSonRow.F[_Field.FieldName] := _FloatValue
            else if TryStrToInt(_Field.AsString, _IntegerValue) then
              _JSonRow.I[_Field.FieldName] := _IntegerValue
            else if TryStrToDateTime(_Field.AsString, _DateTimeValue) then
              _JSonRow.D[_Field.FieldName] := _DateTimeValue
            else
              _JSonRow.S[_Field.FieldName] := _Field.AsString;
          end;

        MemTable.Next;
      end;

    FSource.Data := _Data; // Force Invalidate;
  finally
    _Data.Free;
  end;

  ModalResult:= mrOk;
end;

procedure TFrmNvBsTableDataEditor.LoadArray(Source: TNvBsTable);
var
  I       : Integer;
  _Field  : TField;
  _Column : TNvBsTableColumn;
  _JSonRow: TJsonObject;
  F       : Integer;
begin
  FSource := Source;

  MemTable.Active := False;
  MemTable.Fields.Clear;

  for I := 0 to Source.Columns.Count - 1 do
    begin
      _Column := Source.Columns[I];
      if not _Column.FieldName.IsEmpty then
        begin
          _Field           := TStringField.Create(MemTable);
          _Field.FieldName := _Column.FieldName;
          if not _Column.Title.IsEmpty then
            _Field.DisplayLabel := _Column.Title;
          _Field.DataSet        := MemTable;
        end;
    end;

  MemTable.Active := True;

  for I := 0 to Source.Data.Count - 1 do
    begin
      _JSonRow := Source.Data[I].ObjectValue;
      MemTable.Append;
      for F := 0 to _JSonRow.Count - 1 do
        begin
          _Field := MemTable.FindField(_JSonRow.Names[F]);
          if _Field <> nil then
            _Field.AsString := _JSonRow.Values[_JSonRow.Names[F]].Value;
        end;
      MemTable.Post;
    end;
end;

end.
