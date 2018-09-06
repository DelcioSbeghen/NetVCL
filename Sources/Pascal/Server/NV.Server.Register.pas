unit NV.Server.Register;

interface

uses
  Classes;

procedure register;

implementation

uses
  NV.Server;

procedure register;
begin
  RegisterComponents('NetVCL', [TNVServer]);
end;

end.

