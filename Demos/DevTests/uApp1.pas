unit uApp1;

interface

uses
  Classes, NV.HostApplication, NV.VCL.Page, NV.Common.HostAppInterface;


function LoadApp(aServer: INVServer):INVHostApp;

exports
  LoadApp;

implementation

function LoadApp(aServer: INVServer):INVHostApp;
var
  _App:TNVHostApp;
begin
  _App:= TNVHostApp.Create(nil);
  _App.Domain := 'localhost:888';
  _App.MainPage := TNVPage;
  Result:= _App;
end;


end.

