unit NV.Request.Http;


interface

uses
  Classes, NV.Request;

type

  TNvHttpRequest = class(TnvRequest)
  private
    FParams  : TStrings;
    FDocument: string;
  protected
    function GetDocument: string; override;
    // function GetReferer: string; override;
    // function GetURI: string; override;
    // function GetClientIp: string; override;
    // function GetSessionId: string; override;
    // function GetRawHeaderValue(Name: string): string; override;
    function GetParams: TStrings; override;
  public
    constructor Create(aRequetTask: TNVRequestTask); override;
    destructor Destroy; override;
    procedure Initialize(Document: string);
  end;

  TNvHttpResponse = class(TNvResponse)
  private
    FText      : UTF8String;
    Fheaders   : TStringList;
    FResponseNo: Integer;
  protected
    // function GetStream: TStream; override;
    // procedure SetStream(const Value: TStream); override;
    // function GetCacheControl: string; override;
    // function GetCharSet: string; override;
    // function GetContentDisposition: string; override;
    // function GetContentType: string; override;
    // function GetLocation: string; override;
    // function GetPragma: string; override;
    function GetResponseNo: Integer; override;
    // function GetResponseText: string; override;
    function GetText: UTF8String; override;
    // function GetETag: string; override;
    procedure SetCacheControl(const Value: string); override;
    // procedure SetCharSet(const Value: string); override;
    procedure SetContentDisposition(const Value: string); override;
    // procedure SetContentType(const Value: string); override;
    // procedure SetLocation(const Value: string); override;
    // procedure SetPragma(const Value: string); override;
    procedure SetResponseNo(const Value: Integer); override;
    // procedure SetResponseText(const Value: string); override;
    procedure SetText(const Value: UTF8String); override;
    procedure WriteCookies; override;
    function GetCustomHeaderValue(Name: string): string; override;
    procedure SetCustomHeaderValue(Name: string; const Value: string); override;
    // procedure SetETag(const Value: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure WriteHeader; override;
    procedure SmartServeFile(aRequest: TNVRequestTask; aFile: string); override;
    // procedure Redirect(aNewUrl: string); override;
  end;

  TNVHttpRequestTask = class(TNVRequestTask)
  private

  protected
    procedure CreateFields; override;
  public
    constructor Create; override;
    function CheckIsvalid: Boolean; override;
  end;

function GetExtension(const AMIMEType: string): string;

function GetMimeType(const aFile: string): string;

implementation

uses
  IdCookie, IdGlobal, SysUtils, IdGlobalProtocols;

{ TNVExeRequestTask }

function TNVHttpRequestTask.CheckIsvalid: Boolean;
begin
  Result := True;
end;

constructor TNVHttpRequestTask.Create;
begin
  inherited Create;
end;

procedure TNVHttpRequestTask.CreateFields;
begin
  FRequest  := TNvHttpRequest.Create(Self);
  FResponse := TNvHttpResponse.Create;
end;

{ TNvExeRequest }

constructor TNvHttpRequest.Create(aRequetTask: TNVRequestTask);
begin
  inherited Create(aRequetTask);
  FParams := TStringList.Create;
end;

destructor TNvHttpRequest.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TNvHttpRequest.GetDocument: string;
begin
  Result := FDocument;
end;

function TNvHttpRequest.GetParams: TStrings;
begin
  Result := FParams;
end;

procedure TNvHttpRequest.Initialize(Document: string);
begin
  FDocument := Document;
end;

{ TNvExeResponse }

procedure TNvHttpResponse.Clear;
begin
  inherited;
  FText := '';
end;

constructor TNvHttpResponse.Create;
begin
  inherited Create;
  Fheaders := TStringList.Create;
end;

destructor TNvHttpResponse.Destroy;
begin
  Fheaders.Free;
  inherited;
end;

function TNvHttpResponse.GetCustomHeaderValue(Name: string): string;
begin
  Result := Fheaders.Values[Name];
end;

function TNvHttpResponse.GetResponseNo: Integer;
begin
  Result := FResponseNo;
end;

function TNvHttpResponse.GetText: UTF8String;
begin
  Result := FText;
end;

procedure TNvHttpResponse.SetCacheControl(const Value: string);
begin
  inherited;

end;

procedure TNvHttpResponse.SetContentDisposition(const Value: string);
begin
  inherited;

end;

procedure TNvHttpResponse.SetCustomHeaderValue(Name: string; const Value: string);
begin
  Fheaders.Values[Name] := Value;
end;

procedure TNvHttpResponse.SetResponseNo(const Value: Integer);
begin
  inherited;
  FResponseNo := Value;
end;

procedure TNvHttpResponse.SetText(const Value: UTF8String);
begin
  FText := Value;
end;

procedure TNvHttpResponse.SmartServeFile(aRequest: TNVRequestTask; aFile: string);
var
  LFileDate: TDateTime;
  LReqDate : TDateTime;
  _File    : TStringList;

begin
  inherited;
  if FileExists(aFile) then
    begin
      _File := TStringList.Create;
      try

        _File.LoadFromFile(aFile);

        aRequest.Resp.Text := _File.Text;

        aRequest.Resp.CustomHeaderValue['Content-Type'] := GetMimeType(aFile);

        aRequest.Resp.ResponseNo := 200;
      finally
        _File.Free;
      end
    end
  else
    aRequest.Resp.ResponseNo := 404;



  // Replace Original Indy SmartServerFile to add Etag hash
  // FIdResponse.SmartServeFile(                //
  // TNVExeRequestTask(aRequest).FIdContext, //
  // TNVExeRequestTask(aRequest).FIdRequest, //
  // aFile);

  // LFileDate := IndyFileAge(aFile);
  // if (LFileDate = 0.0) and (not FileExists(aFile)) then
  // begin
  // FIdResponse.ResponseNo := 404;
  // end
  // else
  // begin
  // LReqDate := GMTToLocalDateTime(aRequest.Req.RawHeaderValue['If-Modified-Since']);
  // { do not localize }
  // // if the file date in the If-Modified-Since header is within 2 seconds of the
  // // actual file, then we will send a 304. We don't use the ETag - offers nothing
  // // over the file date for static files on windows. Linux: consider using iNode
  //
  // // RLebeau 2/21/2011: TODO - make use of ETag. It is supposed to be updated
  // // whenever the file contents change, regardless of the file's timestamps.
  //
  // if (LReqDate <> 0) and (abs(LReqDate - LFileDate) < 2 * (1 / (24 * 60 * 60))) then
  // begin
  // FIdResponse.ResponseNo := 304;
  // end
  // else
  // begin
  // FIdResponse.Date         := Now;
  // FIdResponse.LastModified := LFileDate;
  // FIdResponse.CacheControl := 'no-cache';
  //
  // FIdResponse.ETag := //
  // FormatDatetime('yyyymmddhhnnss', FIdResponse.LastModified);
  //
  // FIdResponse.ServeFile( //
  // TNVExeRequestTask(aRequest).FIdContext, aFile);
  // end;
  // end;

  //

end;

procedure TNvHttpResponse.WriteCookies;
begin
  inherited;
  //
end;

procedure TNvHttpResponse.WriteHeader;
begin
  inherited;
  // FIdResponse.WriteHeader;
end;

var
  MimeTable: TIdMimeTable = nil;

function GetExtension(const AMIMEType: string): string;
begin
  if MimeTable = nil then
    MimeTable := TIdMimeTable.Create;
  Result      := MimeTable.GetDefaultFileExt(AMIMEType);
end;

function GetMimeType(const aFile: string): string;
begin
  if MimeTable = nil then
    MimeTable := TIdMimeTable.Create;
  Result      := MimeTable.GetFileMIMEType(aFile);
end;

initialization

finalization

MimeTable.Free;

end.
