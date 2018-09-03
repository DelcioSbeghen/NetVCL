unit NV.Request;

interface

uses
  IdContext, IdCustomHTTPServer, SyncObjs;

type
  TNvModalRequestStatus = (rmsNone // normal request
    , rmsModalStart                // request where Modal Start
    , rmsModalStartSended          // request where Modal Start after sended
    , rmsModalStartCosed           // request where Modal Start after sended and modal closed
    , rmsModalEnd                  // request where modal Ended
    // , rmsModalEnd
);

  TNVRequestTask = class(TObject)
  strict private
    FWait: TEvent;
    FProcessed: Boolean;
  private
    FContext: TIdContext;
    FRequest: TIdHTTPRequestInfo;
    FResponse: TIdHTTPResponseInfo;
    FModalStatus: TNvModalRequestStatus;
  public
    constructor Create(aContext: TIdContext; aRequest: TIdHTTPRequestInfo; aResp: TIdHTTPResponseInfo);
    destructor Destroy; override;
    procedure WaitForDwAppProcessIt;
    procedure Processed;
    function IsProcessed: Boolean;
    function Req: TIdHTTPRequestInfo;
    function Resp: TIdHTTPResponseInfo;
    function Context: TIdContext;
    property ModalStatus: TNvModalRequestStatus read FModalStatus;
  end;

implementation

{ TNVRequestTask }

function TNVRequestTask.Context: TIdContext;
begin
  Result := FContext;
end;

constructor TNVRequestTask.Create(aContext: TIdContext; aRequest: TIdHTTPRequestInfo; aResp: TIdHTTPResponseInfo);
begin
  inherited Create;
  FProcessed := False;
  FContext := aContext;
  FRequest := aRequest;
  FResponse := aResp;
  FWait := TEvent.Create(nil, True, False, '');
  FWait.ResetEvent;
end;

destructor TNVRequestTask.Destroy;
begin
  if not FProcessed then
    Processed;
  inherited;
  FWait.Free;
end;

function TNVRequestTask.IsProcessed: Boolean;
begin
  Result := FProcessed;
end;

procedure TNVRequestTask.Processed;
begin
  FProcessed := True;
  FWait.SetEvent;
end;

function TNVRequestTask.Req: TIdHTTPRequestInfo;
begin
  Result := FRequest;
end;

function TNVRequestTask.Resp: TIdHTTPResponseInfo;
begin
  Result := FResponse;
end;

procedure TNVRequestTask.WaitForDwAppProcessIt;
begin
  FWait.WaitFor(INFINITE);
end;

end.

