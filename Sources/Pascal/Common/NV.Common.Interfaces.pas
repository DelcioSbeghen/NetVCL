unit NV.Common.Interfaces;

interface

uses
  Classes, IdCustomHTTPServer, IdContext, NV.Request;

type
     // interface for all NV Controls and Containers
  INVBase = interface(IInterfaceComponentReference)
    ['{90AF0C4A-F1DF-4F5C-ABE5-B480F7A8E172}']
    function GetId: string;
    property ID: string read GetId;
  end;

  INVForm = interface(INVBase)
    ['{4A4D735D-1130-46F7-9789-0AFDB95D791C}']
  end;

  INVRequest = interface
    ['{6954B551-860C-4359-B6EE-4EE3B44BDB54}']
    function Req: TIdHTTPRequestInfo;
    function Resp: TIdHTTPResponseInfo;
    function Context: TIdContext;
  end;

 (* INVSessionThread = interface(IInterfaceComponentReference)
    ['{268FF727-84C4-4476-BC50-8EF4BD921545}']
    procedure AddDataModule(aDataModule: TDataModule);
    procedure RemoveDataModule(aDataModule: TDataModule);
    function GetCurrent(var ASessionTh): Boolean;
    procedure ProcessRequest(aRequest: INVRequest);
  end;  *)

  INVSessionApp = interface(IInterfaceComponentReference)
    ['{2F4BF6CA-3BBF-4F03-9DC3-4172AE26DD21}']
  end;

  INVHostApp = interface(IInterfaceComponentReference)
    procedure ProcessGet(aRequest: TNVRequestTask);
  end;

implementation

end.

