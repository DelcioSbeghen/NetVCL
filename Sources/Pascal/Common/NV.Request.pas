unit NV.Request;

interface

uses
  Classes, SyncObjs, NV.JSON;

const
  INDEX_CLIENT_IP = 0;

type
  TNvModalRequestStatus = (rmsNone // normal request
    , rmsModalStart                // request where Modal Start
    , rmsModalStartSended          // request where Modal Start after sended
    , rmsModalStartCosed           // request where Modal Start after sended and modal closed
    , rmsModalEnd                  // request where modal Ended
    // , rmsModalEnd
    );

  TNvCookies     = class;
  TNVRequestTask = class;

  TNvRequest = class
  protected
    FRequestTask: TNVRequestTask;
    function GetDocument: string; virtual; abstract;
    function GetReferer: string; virtual; abstract;
    function GetURI: string; virtual; abstract;
    function GetClientIp: string; virtual; abstract;
    function GetSessionId: string; virtual; abstract;
    function GetRawHeaderValue(Name: string): string; virtual; abstract;
    function GetParams: TStrings; virtual; abstract;
  public
    constructor Create(aRequest: TNVRequestTask); virtual;
    property Document: string read GetDocument;
    property Referer: string read GetReferer;
    property URI: string read GetURI;
    property Params: TStrings read GetParams;
    property ClientIp: string read GetClientIp;
    property SessionId: string read GetSessionId;
    property RawHeaderValue[Name: string]: string read GetRawHeaderValue;
  end;

  TNvResponse = class
  private
  protected
    FCookies: TNvCookies;
    function GetStream: TStream; virtual; abstract;
    procedure SetStream(const Value: TStream); virtual; abstract;
    function GetCacheControl: string; virtual; abstract;
    function GetCharSet: string; virtual; abstract;
    function GetContentDisposition: string; virtual; abstract;
    function GetContentType: string; virtual; abstract;
    function GetLocation: string; virtual; abstract;
    function GetPragma: string; virtual; abstract;
    function GetResponseNo: Integer; virtual; abstract;
    function GetResponseText: string; virtual; abstract;
    function GetText: UTF8String; virtual; abstract;
    function GetETag: string; virtual; abstract;
    procedure SetCacheControl(const Value: string); virtual; abstract;
    procedure SetCharSet(const Value: string); virtual; abstract;
    procedure SetContentDisposition(const Value: string); virtual; abstract;
    procedure SetContentType(const Value: string); virtual; abstract;
    procedure SetLocation(const Value: string); virtual; abstract;
    procedure SetPragma(const Value: string); virtual; abstract;
    procedure SetResponseNo(const Value: Integer); virtual; abstract;
    procedure SetResponseText(const Value: string); virtual; abstract;
    procedure SetText(const Value: UTF8String); virtual; abstract;
    function GetCustomHeaderValue(Name: string): string; virtual; abstract;
    procedure SetCustomHeaderValue(Name: string; const Value: string); virtual; abstract;
    procedure SetETag(const Value: string); virtual; abstract;
    procedure WriteCookies; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual; abstract;
    procedure WriteHeader; virtual; abstract;
    procedure SmartServeFile(aRequest: TNVRequestTask; aFile: string); virtual; abstract;
    procedure Redirect(aNewUrl: string); virtual; abstract;
    property CustomHeaderValue[Name: string]: string read GetCustomHeaderValue
      write SetCustomHeaderValue;
    property Stream: TStream read GetStream write SetStream;
    property Cookies: TNvCookies read FCookies write FCookies;
    property Text: UTF8String read GetText write SetText;
    property ResponseNo: Integer read GetResponseNo write SetResponseNo;
    property ResponseText: string read GetResponseText write SetResponseText;
    property CacheControl: string read GetCacheControl write SetCacheControl;
    property Pragma: string read GetPragma write SetPragma;
    property ContentType: string read GetContentType write SetContentType;
    property CharSet: string read GetCharSet write SetCharSet;
    property Location: string read GetLocation write SetLocation;
    property ContentDisposition: string read GetContentDisposition write SetContentDisposition;
    property ETag: string read GetETag write SetETag;
  end;

  TNVCookie = class(TCollectionItem)
  private
    FName    : string;
    FValue   : string;
    FPath    : string;
    FDomain  : string;
    FExpires : TDateTime;
    FSecure  : Boolean;
    FHttpOnly: Boolean;
  protected
    function GetHeaderValue: string;
  public
    constructor Create(Collection: TCollection); override;
    procedure AssignTo(Dest: TPersistent); override;
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    property Domain: string read FDomain write FDomain;
    property Path: string read FPath write FPath;
    property Expires: TDateTime read FExpires write FExpires;
    property Secure: Boolean read FSecure write FSecure;
    property HeaderValue: string read GetHeaderValue;
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
  end;

  TNvCookies = class(TCollection)
  private
    {$IFNDEF FPC}[weak] {$ENDIF}
    FWebResponse: TNvResponse;
  protected
    function GetCookie(Index: Integer): TNVCookie;
    procedure SetCookie(Index: Integer; Cookie: TNVCookie);
  public
    constructor Create(WebResponse: TNvResponse; ItemClass: TCollectionItemClass);
    function Add: TNVCookie;
    property WebResponse: TNvResponse read FWebResponse;
    property Items[Index: Integer]: TNVCookie read GetCookie write SetCookie; default;
  end;

  TNVRequestTask = class(TObject)
  strict private
    FWait     : TEvent;
    FProcessed: Boolean;
  private
    FModalStatus: TNvModalRequestStatus;
    FSession    : TObject;
    procedure SetHostApplication(const Value: IInterface);
  protected
    FHostApplication: IInterface;
    FRequest        : TNvRequest;
    FResponse       : TNvResponse;
    procedure CreateFields; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CheckIsvalid: Boolean; virtual; abstract;
    procedure WaitForDwAppProcessIt;
    procedure Processed;
    function IsProcessed: Boolean;
    function Req: TNvRequest;
    function Resp: TNvResponse;
    property ModalStatus: TNvModalRequestStatus read FModalStatus;
    property Session: TObject read FSession write FSession;
    property HostApplication: IInterface read FHostApplication write SetHostApplication;
  end;

  TNVRequestTaskJson = class(TNVRequestTask)
  private
    FJson: TJsonObject;
  protected
    procedure CreateFields; override;
  public
    constructor Create(JsonStr: string); reintroduce;
    destructor Destroy; override;
    function CheckIsvalid: Boolean; override;
    function JSON: TJsonObject;
  end;

const
  sDateFormat = '"%s", dd "%s" yyyy hh":"nn":"ss';

implementation

uses
    SysUtils{$IFDEF FPC} ,httpprotocol{$ELSE}, NetEncoding{$ENDIF};

const
  // These strings are NOT to be resourced

  Months: array [1 .. 12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec');
  DaysOfWeek: array [1 .. 7] of string     = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  LongDaysOfWeek: array [1 .. 7] of string = ('Sunday', 'Monday', 'Tuesday', 'Wednesday',
    'Thursday', 'Friday', 'Saturday');

function DayOfWeekStr(DateTime: TDateTime): string;
begin
  Result := DaysOfWeek[DayOfWeek(DateTime)];
end;

function MonthStr(DateTime: TDateTime): string;
var
  Year, Month, Day: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  Result := Months[Month];
end;

{ TNVRequestTask }

constructor TNVRequestTask.Create;
begin
  inherited Create;
  FProcessed := False;
  CreateFields;
  FWait := TEvent.Create(nil, True, False, '');
  FWait.ResetEvent;
end;

procedure TNVRequestTask.CreateFields;
begin
  FRequest  := TNvRequest.Create(Self);
  FResponse := TNvResponse.Create;
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
  if FResponse <> nil then
    FResponse.WriteCookies;

  FWait.SetEvent;
end;

function TNVRequestTask.Req: TNvRequest;
begin
  Result := FRequest;
end;

function TNVRequestTask.Resp: TNvResponse;
begin
  Result := FResponse;
end;

procedure TNVRequestTask.SetHostApplication(const Value: IInterface);
begin
  FHostApplication := Value;
end;

procedure TNVRequestTask.WaitForDwAppProcessIt;
begin
  FWait.WaitFor(INFINITE);
end;

{ TNvResponse }

constructor TNvResponse.Create;
begin
  inherited Create;
  FCookies := TNvCookies.Create(Self, TNVCookie);
end;

destructor TNvResponse.Destroy;
begin
  FCookies.Free;
  inherited;
end;

{ TNVCookie }

procedure TNVCookie.AssignTo(Dest: TPersistent);
begin
  if Dest is TNVCookie then
    with TNVCookie(Dest) do
      begin
        Name     := Self.FName;
        Value    := Self.FValue;
        Domain   := Self.FDomain;
        Path     := Self.FPath;
        Expires  := Self.FExpires;
        Secure   := Self.FSecure;
        HttpOnly := Self.FHttpOnly;
      end
  else
    inherited AssignTo(Dest);
end;

constructor TNVCookie.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FExpires := 0;
end;

function TNVCookie.GetHeaderValue: string;
var
  S: string;
begin
  {$IFDEF FPC}
     S := Format('%s=%s; ', [HTTPEncode(FName), HTTPEncode(FValue)]);
   {$ELSE}
   S := Format('%s=%s; ', [TNetEncoding.URL.Encode(FName), TNetEncoding.URL.Encode(FValue)]);
  {$ENDIF}
  if Domain <> '' then
    S := S + Format('domain=%s; ', [Domain]); { do not localize }
  if Path <> '' then
    S := S + Format('path=%s; ', [Path]); { do not localize }
  if Expires > -1 then
    S := S + Format(FormatDateTime('"expires="' + sDateFormat + ' "GMT; "', Expires),
      { do not localize }
      [DayOfWeekStr(Expires), MonthStr(Expires)]);
  if Secure then
    S := S + 'secure; '; { do not localize }
  if HttpOnly then
    S := S + 'httponly'; { do not localize }
  if Copy(S, Length(S) - 1, MaxInt) = '; ' then
    SetLength(S, Length(S) - 2);
  Result := S;
end;

{ TNVCookieCollection }

function TNvCookies.Add: TNVCookie;
begin
  Result := TNVCookie(inherited Add);
end;

constructor TNvCookies.Create(WebResponse: TNvResponse; ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FWebResponse := WebResponse;
end;

function TNvCookies.GetCookie(Index: Integer): TNVCookie;
begin
  Result := TNVCookie(inherited Items[Index]);
end;

procedure TNvCookies.SetCookie(Index: Integer; Cookie: TNVCookie);
begin
  Items[Index].Assign(Cookie);
end;

{ TNvRequest }

{ TNvRequest }

constructor TNvRequest.Create(aRequest: TNVRequestTask);
begin
  inherited Create;
  FRequestTask := aRequest;
end;

{ TNVRequestTaskJson }

function TNVRequestTaskJson.CheckIsvalid: Boolean;
begin
  Result := not(FJson.Count = 0);
end;

constructor TNVRequestTaskJson.Create(JsonStr: string);
begin
  inherited Create;
  FJson := TJsonObject.Create;
  FJson.FromJSON(JsonStr);
end;

procedure TNVRequestTaskJson.CreateFields;
begin
  // inherited;
  // Dont Create Fields
end;

destructor TNVRequestTaskJson.Destroy;
begin
  FJson.Free;
  inherited;
end;

function TNVRequestTaskJson.JSON: TJsonObject;
begin
  Result := FJson;
end;

end.
