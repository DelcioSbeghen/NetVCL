unit NV.Cache;


interface

uses
  Classes, SysUtils, DB, IOUtils, IdCustomHTTPServer,
  IdContext;

type

  TCacheItemBase = class(TObject)
  protected
    // FMemoryCacheStream:TMemoryStream;
    FFilename: string;
    FOwner: TComponent;
    FContentType: string;
    // FFileCachePath:string;
  public
    constructor Create(aOwner: TComponent; aFileName: string; aContentType: string);
    destructor Destroy; override;
    function URL: string;
    function FileName: string;
  end;

  TCacheItemMemory = class(TCacheItemBase)
  protected
    FStream: TMemoryStream;
  public
    constructor Create(aOwner: TComponent; aStream: TStream; aFileName: string;
      aContentType: string);
    destructor Destroy; override;
  end;

  TCacheItemFile = class(TCacheItemBase)
  private
  protected
    FPath: string;
  public
    constructor Create(aOwner: TComponent; aStream: TStream; aFileName: string;
      aContentType: string);
    destructor Destroy; override;
    class function GetCacheFilePath(aOwner: TComponent; aFileName: string): string;
  end;

  TNVCacheList = class(TStringList)

  end;

 (*) TDWUrlHandlerCache = class(TDWUrlHandlerBase)
  public
    function Execute(aReqTask: TObject; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo):Boolean; override;
  end; *)

  TNVCache = class
  public
    class function AddStreamToGlobalMemoryCache(aOwner: TComponent; aStream: TStream;
      aFileName: string; aContentType: string): string;
    class function AddStreamToSessionMemoryCache(aOwner: TComponent; aStream: TStream;
      aFileName: string; aContentType: string): string;
    class function AddStreamToSessionFileCache(aOwner: TComponent; aStream: TStream;
      aFileName: string; aContentType: string): string;
  end;

implementation

uses NV.Utils, NV.Session, NV.Interfaces, NV.Dispatcher, NV.HostApplication;

type
  THack = class(TNVSessionApp);

  { TDWCache }

class function TNVCache.AddStreamToGlobalMemoryCache(aOwner: TComponent; aStream: TStream;
  aFileName: string; aContentType: string): string;
begin
  raise Exception.Create('Need to implement Global Cache.');
end;

class function TNVCache.AddStreamToSessionFileCache(aOwner: TComponent; aStream: TStream;
  aFileName, aContentType: string): string;
var
  LItem: TCacheItemFile;
  LUrl: string;
  LHandler:TDispatchCache;
begin
  // Create the cache item Object
  LItem := TCacheItemFile.Create(aOwner, aStream, aFileName, aContentType);
  // Get url for get item
  LUrl := LItem.URL;
  // add item to DWApplication cache list
  THack(NVSessionApp).FCacheList.AddObject(LUrl, LItem);
  // Register get handler class for item url
  LHandler:= TDispatchCache.Create;
  NVSessionApp.Router.AddRoute(LUrl, LHandler);

 // DWApplication.RegisterGetHandler(aOwner, LUrl, TDWUrlHandlerCache);
  // return the url for get item
  Result := LUrl;
end;

class function TNVCache.AddStreamToSessionMemoryCache(aOwner: TComponent; aStream: TStream;
  aFileName: string; aContentType: string): string;
var
  LItem: TCacheItemMemory;
  LUrl: string;
  OldItemindex:Integer;
  LHandler: TDispatchCache;
begin
  // Create the cache item Object
  LItem := TCacheItemMemory.Create(aOwner, aStream, aFileName, aContentType);
  // Get url for get item
  LUrl := LItem.URL;
  //remove old item with same url
  OldItemindex:= THack(NVSessionApp).FCacheList.IndexOf(LUrl);
  if OldItemindex > -1 then
    THack(NVSessionApp).FCacheList.Delete(OldItemindex);
  // add item to DWApplication cache list
  THack(NVSessionApp).FCacheList.AddObject(LUrl, LItem);
  //remove older gethandler

  OldItemindex:= NVSessionApp.Router.IndexOf(LUrl);
  //OldItemindex:= THack(DWApplication).GetHandlerList.IndexOf(LUrl);
  if OldItemindex > -1 then
    NVSessionApp.Router.Delete(OldItemindex);
  // Register get handler class for item url and return the url for get item

  LHandler:= TDispatchCache.Create;
  NVSessionApp.Router.AddRoute(LUrl, LHandler);
  Result:= LUrl;

 // Result:= DWApplication.RegisterGetHandler(aOwner, aFileName, TDWUrlHandlerCache);
end;

{ TCacheItem }

constructor TCacheItemBase.Create(aOwner: TComponent; aFileName: string; aContentType: string);
begin
  FOwner       := aOwner;
  FContentType := aContentType;
  FFilename    := aFileName;
end;

destructor TCacheItemBase.Destroy;
begin
  inherited;
end;

function TCacheItemBase.FileName: string;
begin
  Result := FFilename;
end;

function TCacheItemBase.URL: string;
var
  L_IControl: INVBase;
begin
   Result := '';
  if (FOwner <> nil) and (Supports(FOwner, INVBase, L_IControl)) and (L_IControl <> nil)
      then
    Result := '/' + L_IControl.ID + '.' + FileName
  else if (FOwner <> nil) then
    Result := '/' + FOwner.Name + '.' + FileName
  else
    Result    := '/' + FileName;
end;

{ TDWUrlHandlerCache }
 (*)
function TDWUrlHandlerCache.Execute(aReqTask: TObject; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo):Boolean;
var
  Status: string;
  LPath: string;
  LCacheList: TNVCacheList;
  Lindex: Integer;
  LCacheItem: TCacheItemBase;
begin
  Result:=False;
  Status     := '';
  LPath      := ARequestInfo.Document;
  LCacheList := THack(DWApplication).FCacheList;
  if LPath <> '' then
    begin
      Lindex := LCacheList.IndexOf(LPath);
      if Lindex > -1 then
        begin
          LCacheItem := TCacheItemBase(LCacheList.GetObject(Lindex));
         // LHeader    := 'Content-Disposition: attachment; filename="' + LCacheItem.FileName + '"';
          // if is in memory cache
          if LCacheItem is TCacheItemMemory then
            begin
              // load buffer from memory
              if AResponseInfo.ContentStream = nil then
                AResponseInfo.ContentStream:= TMemoryStream.Create;

              AResponseInfo.ContentStream.CopyFrom(TCacheItemMemory(LCacheItem).FStream, 0);

              //TCacheItemMemory(LCacheItem).FStream.SaveToFile('c:/teste.pdf');
              AResponseInfo.ContentType:= LCacheItem.FContentType;

              if ARequestInfo.Params.Values['attachment'] = 'true' then
                AResponseInfo.ContentDisposition:= 'attachment; filename="' + LCacheItem.FileName + '";';

              if ARequestInfo.Params.Values['deleteCache'] = 'true' then
                begin
                  LCacheList.Delete(Lindex);
                  DWApplication.UnRegisterGethandler(LPath);
                end;
              // send
              //AnswerStream(Status, LCacheItem.FContentType, LHeader);
            end
            // else if is in file cache
          else if LCacheItem is TCacheItemFile then
            begin
              // load buffer from File
              { TODO 1 -oDELCIO -cIMPLEMENT : !!!!! Need to implement this !!!!!!!}

             // AResponseInfo.DocStream :=
             //   TIcsBufferedFileStream.Create(TCacheItemFile(LCacheItem).FPath,
              //  fmOpenRead + fmShareDenyWrite, MAX_BUFSIZE);
              // send
              //AnswerStream(Status, LCacheItem.FContentType, LHeader);
            end
        end;
    end;
 //Finish;
end;*)

{ TCacheItemMemory }

constructor TCacheItemMemory.Create(aOwner: TComponent; aStream: TStream;
  aFileName, aContentType: string);
begin
  inherited Create(aOwner, aFileName, aContentType);
  FStream := TMemoryStream.Create;
  FStream.CopyFrom(aStream, 0);
end;

destructor TCacheItemMemory.Destroy;
begin
  FStream.Free;
  inherited;
end;

{ TCacheItemFile }

constructor TCacheItemFile.Create(aOwner: TComponent; aStream: TStream;
  aFileName, aContentType: string);
var
  LStream: TMemoryStream;
begin
  inherited Create(aOwner, aFileName, aContentType);
  LStream.Create;
  try
    FPath := GetCacheFilePath(aOwner, aFileName);
    LStream.CopyFrom(aStream, 0);
    LStream.SaveToFile(FPath);
  finally
    LStream.Free;
  end;
end;

destructor TCacheItemFile.Destroy;
begin

  inherited;
end;

class function TCacheItemFile.GetCacheFilePath(aOwner: TComponent; aFileName: string): string;
var
  _HostApp:TNVHostApp;
begin
  _HostApp:=NVSessionThread.HostApp;
  // if is global cache
  if (aOwner <> nil) and (aOwner = _HostApp) then
    // path is in base cache dir
    Result := _HostApp.DocDir + 'Cache\' + aFileName
    // else if is Session Cache with Component Owner
  else if (aOwner <> nil) then // path is in Session dir and owner dir
    Result := _HostApp.DocDir + 'Cache\' + NVSessionApp.SessionID + '\' + aOwner.Name + '\' +
      aFileName
    // else is Session Cache without component Owner
  else if aOwner = nil then
    Result := _HostApp.DocDir + 'Cache\' + NVSessionApp.SessionID + '\' + aFileName;
end;

end.
