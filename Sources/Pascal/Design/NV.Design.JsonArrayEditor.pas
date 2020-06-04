unit NV.Design.JsonArrayEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, NV.JSON,
  DesignEditors, DesignIntf;

type
  TFrmJsonArrayEditor = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    BtnOk: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FTempArray  : TJsonArray;
    FSourceArray: TJsonArray;
  public
    procedure LoadArray(Source: TJsonArray);
    procedure SaveArray;
  end;

  TNvJsonArrayEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

var
  FrmJsonArrayEditor: TFrmJsonArrayEditor;

implementation

{$R *.dfm}
{ TForm1 }

procedure TFrmJsonArrayEditor.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFrmJsonArrayEditor.BtnOkClick(Sender: TObject);
begin
  SaveArray;
  ModalResult := mrOk;
end;

procedure TFrmJsonArrayEditor.FormCreate(Sender: TObject);
begin
  FTempArray := TJsonArray.Create;
end;

procedure TFrmJsonArrayEditor.FormDestroy(Sender: TObject);
begin
  FTempArray.Free;
end;

procedure TFrmJsonArrayEditor.LoadArray(Source: TJsonArray);
var
  I: Integer;
begin
  FSourceArray := Source;
  FTempArray.Assign(Source);
  Memo1.Clear;
  for I := 0 to FTempArray.Count - 1 do
    Memo1.Lines.Add(FTempArray.S[I]);
end;

procedure TFrmJsonArrayEditor.SaveArray;
var
  I: Integer;
begin
  FTempArray.Clear;
  for I := 0 to Memo1.Lines.Count - 1 do
    FTempArray.Add(Memo1.Lines[I]);

  FSourceArray.Assign(FTempArray);
end;

{ TTNvJsonArrayEditor }

{ TTNvJsonArrayEditor }

procedure TNvJsonArrayEditor.Edit;
begin
  inherited;

  FrmJsonArrayEditor := TFrmJsonArrayEditor.Create(nil);
  try
    FrmJsonArrayEditor.LoadArray(TJsonArray(GetOrdValue));
    if FrmJsonArrayEditor.ShowModal = mrOk then
      Designer.Modified;
  finally
    FrmJsonArrayEditor.Free;
  end;
end;

function TNvJsonArrayEditor.GetAttributes: TPropertyAttributes;
begin
  result := [paDialog];
end;

end.
