unit NV.Server.HttpServer;

{ *******************************************************************************
  NV.Server.HttpServer contains code from
  Project Indy  http://www.indyproject.org/
}

interface

uses
  Classes, IdHTTPServer;

type
  TNVHTTPServer = class(TIdHTTPServer)
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

