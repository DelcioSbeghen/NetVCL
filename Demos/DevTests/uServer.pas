unit uServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, NV.Server;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    btn1: TButton;
    dlgOpen1: TOpenDialog;
    NVServer1: TNVServer;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private

  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

//type
  //Create one Main Page
 // TMainPage = class(TNVPage)
// end;

procedure TForm2.btn1Click(Sender: TObject);
begin
  dlgOpen1.InitialDir:= '..\..\';
  if dlgOpen1.Execute then
    NVServer1.AddPackage(dlgOpen1.FileName);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  (*
  //Create App1
  App1 := TNVHostApp.Create(Server);
  App1.Domain := 'localhost:888'; //set domain for App1
  App1.MainPage := TMainPage;  //Set Main Page for App1
  //Add App1 to Server
  Server.AddApp(App1); *)
  //NVHostApp1.MainPage:= TMainPage;
  //NVServer1.AddApp(NVHostApp1);
  NVServer1.Start; //Start Server
end;

end.

