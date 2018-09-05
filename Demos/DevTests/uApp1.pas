unit uApp1;

interface

uses
  Classes, NV.HostApplication, NV.VCL.Page, NV.Server;


function LoadApp(aServer: TNVServer):TNVHostApp;

exports
  LoadApp;

implementation

function LoadApp(aServer: TNVServer):TNVHostApp;
begin
  Result:= TNVHostApp.Create(nil);
  Result.Domain := 'localhost:888';
  Result.MainPage := TNVPage;
end;


end.

