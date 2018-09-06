unit NV.Server.HttpServer;

{ *******************************************************************************
  NV.Server.HttpServer contains code from
  Project Indy  http://www.indyproject.org/
}

interface

uses
  Classes, IdHTTPServer, NV.Common.HostAppInterface;

type
  TNVHTTPServer = class(TIdHTTPServer, INVHTTPServer)
  private
  public
    constructor Create(AOwner: TComponent); reintroduce;
  end;

implementation

{ TNVHTTPServer }

constructor TNVHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

end.

