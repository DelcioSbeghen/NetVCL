unit uApp1;

interface

uses
  Classes, NV.HostApplication, NV.VCL.Page, NV.Server;

type
  TApp1 = class(TNVHostApp)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPage1 = class(TNVPage)
  end;

function LoadApp(aServer: TNVServer):TNVHostApp;

exports
  LoadApp;

implementation

function LoadApp(aServer: TNVServer):TNVHostApp;
begin
  registerClass(TApp1);
  Result:= TApp1.Create(nil);
end;



{ TApp1 }

constructor TApp1.Create(AOwner: TComponent);
begin
  inherited;
  Domain := 'localhost:888';
  MainPage := TPage1;
end;

end.

