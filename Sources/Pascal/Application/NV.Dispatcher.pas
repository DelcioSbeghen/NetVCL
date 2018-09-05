unit NV.Dispatcher;

interface

uses
  NV.Request;

type
  TNVAllowedPath = (afBeginBy, afExactMatch, afDirList);

  TDispatch = class
  public
    procedure Execute(aRequest: TNVRequestTask); virtual;
  end;

  TDispatchCache = class(TDispatch)
  public
    procedure Execute(aRequest: TNVRequestTask); override;
  end;

  TDispatchDirFiles = class(TDispatch)
  private
    FAllowedFlag: TNVAllowedPath;
    procedure SetAllowedFlag(const Value: TNVAllowedPath);
  public
    constructor Create;
    property AllowedFlag: TNVAllowedPath read FAllowedFlag write SetAllowedFlag default afExactMatch;
  end;

implementation

{ TDispatchDirFiles }

constructor TDispatchDirFiles.Create;
begin
  inherited Create;
  FAllowedFlag := afExactMatch;
end;

procedure TDispatchDirFiles.SetAllowedFlag(const Value: TNVAllowedPath);
begin
  FAllowedFlag := Value;
end;

{ TDispatchCache }

procedure TDispatchCache.Execute;
(* var
 Status: string;
  LPath: string;
  LCacheList: TNVCacheList;
  Lindex: Integer;
  LCacheItem: TCacheItemBase; *)
begin
 (*) Result:=False;
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
 //Finish; *)

end;

{ TDispatch }

procedure TDispatch.Execute;
begin

end;

end.

