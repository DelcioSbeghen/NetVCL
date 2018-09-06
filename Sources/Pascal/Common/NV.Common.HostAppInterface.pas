unit NV.Common.HostAppInterface;

interface

uses
  Classes, NV.Request;

type
  PNVRequestTask = ^TNVRequestTask;

  INVHTTPServer = interface(IInterfaceComponentReference)
    ['{DA168C96-3C5B-49F0-91DE-E15338D0154D}']
    function EndSession(const SessionName: string; const RemoteIP: string = ''): Boolean;
  end;

  INVServer = interface(IInterfaceComponentReference)
    ['{3ECD8EE7-C0CC-48EA-83FE-266433EDFD3F}']
    procedure Log(aMsg: string);
    function GetHttpServer: INVHTTPServer;
    property HttpServer: INVHTTPServer read GetHttpServer;
  end;

  INVHostApp = interface(IInterfaceComponentReference)
    ['{FB1184F3-4D93-4748-AB40-51601B22E664}']
    function GetDomain: string;
    procedure SetDomain(const Value: string);
    procedure ProcessGet(aRequest: PNVRequestTask);
    function GetServer: INVServer;
    property Domain: string read GetDomain write SetDomain;
  end;

implementation

end.

