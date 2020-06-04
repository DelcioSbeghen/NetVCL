unit NV.Dispatcher;

interface

uses
  SysUtils, NV.Request;

type
  TNVAllowedPath = (afBeginBy, afExactMatch, afDirList);

  TDispatch = class
  public
    function Execute(aRequest: TNVRequestTask): Boolean; virtual;
  end;

  TDispatchCache = class(TDispatch)
  public
    function Execute(aRequest: TNVRequestTask): Boolean; override;
  end;

  TDispatchDirFiles = class(TDispatch)
  private
    FAllowedFlag: TNVAllowedPath;
    procedure SetAllowedFlag(const Value: TNVAllowedPath);
  public
    constructor Create;
    function Execute(aRequest: TNVRequestTask): Boolean; override;
    property AllowedFlag: TNVAllowedPath read FAllowedFlag write SetAllowedFlag
      default afExactMatch;
  end;

implementation

uses
  NV.Utils, NV.VCL.Forms;

{ TDispatchDirFiles }

constructor TDispatchDirFiles.Create;
begin
  inherited Create;
  FAllowedFlag := afExactMatch;
end;

function TDispatchDirFiles.Execute(aRequest: TNVRequestTask): Boolean;
  function DocumentFile: string;
  begin

    Result := StringReplace(aRequest.Req.Document, 'local://screen/', Application.RootPath, []);

    Result := StringReplace(Result, '/', '\', [rfReplaceAll]);

    // if aRequest.Req.Document = '/' then
    // Result := NVSessionApp.HostApp.DocDir
    // else if (aRequest.Req.Document <> '') and (aRequest.Req.Document[1] = '/') then
    // Result := AbsolutisePath(NVSessionApp.HostApp.DocDir +
    // URLDecode(AdjustOSPathDelimiters(aRequest.Req.Document)))
    // else
    // Result := AbsolutisePath(NVSessionApp.HostApp.DocDir + PathDelim +
    // URLDecode(AdjustOSPathDelimiters(aRequest.Req.Document)));
    //
    // { Check for default document }
    // if (Length(Result) > 0) and (Result[Length(Result)] = PathDelim) and
    // (FileExists(Result + 'index.html' { FDefaultDoc } )) then
    // Result := Result + 'index.html' { FDefaultDoc }
    // else if IsDirectory(Result) and (FileExists(Result + PathDelim + 'index.html' { FDefaultDoc } ))
    // then
    // Result := Result + PathDelim + 'index.html' { FDefaultDoc };
  end;

begin
  Result := False;
  case FAllowedFlag of
    afBeginBy:
      begin
        aRequest.Resp.ContentDisposition := 'inline';
        aRequest.Resp.CacheControl       := 'private';
        aRequest.Resp.SmartServeFile(aRequest, DocumentFile);
        Result := True;
        Exit;

      end;
    afExactMatch:
      begin
        raise Exception.Create('Need to implement TDispatchDirFiles.Execute for afExactMatch');
        { if CompareText(Elem.Path, aRequestInfo.Document) = 0 then
          begin
          aResponseInfo.ContentDisposition := 'inline';
          aResponseInfo.CacheControl := 'private';
          aResponseInfo.SmartServeFile(aReqTask.FContext, aRequestInfo, DocumentFile);
          Result := True;
          Exit;
          end; }
      end;
    afDirList:
      begin
        raise Exception.Create('Need to implement TDispatchDirFiles.Execute for afDirList');
        { if CompareText(Elem.Path, aRequestInfo.Document) = 0 then
          begin
          aResponseInfo.ContentDisposition := 'inline';
          aResponseInfo.CacheControl := 'private';
          aResponseInfo.SmartServeFile(aReqTask.FContext, aRequestInfo, DocumentFile);
          Result := True;
          Exit;
          end; }
      end;
  end;

end;

procedure TDispatchDirFiles.SetAllowedFlag(const Value: TNVAllowedPath);
begin
  FAllowedFlag := Value;
end;

{ TDispatchCache }

function TDispatchCache.Execute(aRequest: TNVRequestTask): Boolean;
(* var
  Status: string;
  LPath: string;
  LCacheList: TNVCacheList;
  Lindex: Integer;
  LCacheItem: TCacheItemBase; *)
begin
  Result := False;
  (* ) Result:=False;
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

function TDispatch.Execute(aRequest: TNVRequestTask): Boolean;
begin
  Result := False;
end;

end.
