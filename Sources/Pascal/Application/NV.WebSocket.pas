unit NV.WebSocket;

interface

uses
  Classes, SysUtils, IdTCPConnection, IdCustomHTTPServer, SyncObjs, IdHashSHA,
  IdCoderMIME, IdGlobal, IdException, IdStream;

type
  TWSDataCode = (wdcNone, wdcContinuation, wdcText, wdcBinary, wdcClose, wdcPing, wdcPong);

  TNVWebSocket = class(TObject)
  private
    FApp: TObject;
    FConnection: TIdTCPConnection;
    FTerminated: Boolean;
    FResponseInfo: TMemoryStream;
    FCritical: TCriticalSection;
    FHasData: Boolean;
    FWebSocketKey: string;
    FLock: TCriticalSection;
    FLastActivityTime: TDateTime;
    function WriteData(aData: TIdBytes; aType: TWSDataCode; aFIN, aRSV1, aRSV2, aRSV3: boolean): integer;
  public
    constructor Create(aDwApp: TObject; Connection: TIdTCPConnection; WebSocketKey: string);
    destructor Destroy; override;
    procedure Execute;
    procedure Lock;
    procedure Unlock;
    function TryLock: Boolean;
    function BeginWrite: TMemoryStream;
    procedure EndWrite;
    procedure Terminate;
  end;

implementation

uses
  NV.Languages;


//frame codes
const
  C_FrameCode_Continuation = 0;
  C_FrameCode_Text = 1;
  C_FrameCode_Binary = 2;
  //3-7 are reserved for further non-control frames
  C_FrameCode_Close = 8;
  C_FrameCode_Ping = 9;
  C_FrameCode_Pong = 10 {A};
  //B-F are reserved for further control frames

{ TNVWebSocket }

function TNVWebSocket.BeginWrite: TMemoryStream;
begin
  FCritical.Acquire;
  Result := FResponseInfo;
end;

constructor TNVWebSocket.Create(aDwApp: TObject; Connection: TIdTCPConnection; WebSocketKey: string);
begin
  FApp := aDwApp;
  FConnection := Connection;
  FTerminated := False;
  FCritical := TCriticalSection.Create;
  FLock := TCriticalSection.Create;
  FResponseInfo := TMemoryStream.Create;
  FHasData := False;
  FWebSocketKey := WebSocketKey;
end;

destructor TNVWebSocket.Destroy;
begin
  Terminate;
  FResponseInfo.Free;
  FCritical.Free;
  FLock.Free;
  inherited;
end;

procedure TNVWebSocket.EndWrite;
begin
  FCritical.Release;
  FHasData := True; //no need syncronization because Boolean is Atomic
  //wait socket send data
  while FHasData do
    Sleep(1);
end;

function TNVWebSocket.WriteData(aData: TIdBytes; aType: TWSDataCode; aFIN, aRSV1, aRSV2, aRSV3: boolean): integer;
var
  iByte: Byte;
  i, ioffset: Integer;
  iDataLength, iPos: Int64;
  rLength: Int64Rec;
  rMask: record
    case Boolean of
      True:
        (MaskAsBytes: array[0..3] of Byte);
      False:
        (MaskAsInt: Int32);
  end;
  strmData: TMemoryStream;
  bData: TIdBytes;
begin
  Result := 0;
  Assert(FConnection.Socket.Binding <> nil);

  strmData := TMemoryStream.Create;
  Lock;
  try
    FLastActivityTime := Now;   //sending some data
    (* 0                   1                   2                   3
       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 (nr)
       7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0 (bit)
      +-+-+-+-+-------+-+-------------+-------------------------------+
      |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
      |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
      |N|V|V|V|       |S|             |   (if payload len==126/127)   |
      | |1|2|3|       |K|             |                               |
      +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - + *)
    //FIN, RSV1, RSV2, RSV3: 1 bit each
    if aFIN then
      iByte := (1 shl 7)
    else
      iByte := 0;
    if aRSV1 then
      iByte := iByte + (1 shl 6);
    if aRSV2 then
      iByte := iByte + (1 shl 5);
    if aRSV3 then
      iByte := iByte + (1 shl 4);
    //Opcode: 4 bits
    case aType of
      wdcContinuation:
        iByte := iByte + C_FrameCode_Continuation;
      wdcText:
        iByte := iByte + C_FrameCode_Text;
      wdcBinary:
        iByte := iByte + C_FrameCode_Binary;
      wdcClose:
        iByte := iByte + C_FrameCode_Close;
      wdcPing:
        iByte := iByte + C_FrameCode_Ping;
      wdcPong:
        iByte := iByte + C_FrameCode_Pong;
    else
      raise EIdException.CreateFmt(NVLang.RsUnsupportedDataCode, [Ord(aType)]);
    end;
    strmData.Write(iByte, SizeOf(iByte));

    iByte := 0;
    //Mask: 1 bit; Note: Clients must apply a mask
   // if not IsServerSide then iByte := (1 shl 7);

    //Length: 7 bits or 7+16 bits or 7+64 bits
    if Length(aData) < 126 then            //7 bit, 128
      iByte := iByte + Length(aData)
    else if Length(aData) < 1 shl 16 then  //16 bit, 65536
      iByte := iByte + 126
    else
      iByte := iByte + 127;
    strmData.Write(iByte, SizeOf(iByte));

    //Extended payload length?
    if Length(aData) >= 126 then
    begin
      //If 126, the following 2 bytes interpreted as a 16-bit unsigned integer are the payload length
      if Length(aData) < 1 shl 16 then  //16 bit, 65536
      begin
        rLength.Lo := Length(aData);
        iByte := rLength.Bytes[1];
        strmData.Write(iByte, SizeOf(iByte));
        iByte := rLength.Bytes[0];
        strmData.Write(iByte, SizeOf(iByte));
      end
      else
      //If 127, the following 8 bytes interpreted as a 64-bit unsigned integer (the most significant bit MUST be 0) are the payload length
      begin
        rLength := Int64Rec(Int64(Length(aData)));
        for i := 7 downto 0 do
        begin
          iByte := rLength.Bytes[i];
          strmData.Write(iByte, SizeOf(iByte));
        end;
      end
    end;

    //Masking-key: 0 or 4 bytes; Note: Clients must apply a mask
   (* if not IsServerSide then
    begin
      rMask.MaskAsInt := Random(MaxInt);
      strmData.Write(rMask.MaskAsBytes[0], SizeOf(Byte));
      strmData.Write(rMask.MaskAsBytes[1], SizeOf(Byte));
      strmData.Write(rMask.MaskAsBytes[2], SizeOf(Byte));
      strmData.Write(rMask.MaskAsBytes[3], SizeOf(Byte));
    end;*)

    //write header
    strmData.Position := 0;
    TIdStreamHelper.ReadBytes(strmData, bData);

    //Mask? Note: Only clients must apply a mask
   (* if not IsServerSide then
    begin
      iPos := 0;
      iDataLength := Length(aData);
      //in place masking
      while iPos < iDataLength do
      begin
        iByte := aData[iPos] xor rMask.MaskAsBytes[iPos mod 4]; //apply mask
        aData[iPos] := iByte;
        inc(iPos);
      end;
    end; *)

    AppendBytes(bData, aData);   //important: send all at once!

    //
    FConnection.IOHandler.WriteDirect(bData, (Length(bData) - ioffset), ioffset);
   (* ioffset := 0;
    repeat
      //Result := Binding.Send(bData, ioffset);
      //
      Result :=  FConnection.IOHandler.WriteDirect(bdata, (Length(bData) - ioffset), iOffset);  //ssl compatible?
      if Result<0 then
        begin
         // IO error ; probably connexion closed by peer on protocol error ?
         {$IFDEF DEBUG_WS}
         if Debughook > 0 then
           OutputDebugString(PChar(Format('WriteError ThrID:%d, L:%d, R:%d',[getcurrentthreadid,Length(bData)-ioffset,Result])));
         {$ENDIF}
         break;
       end;
      Inc(ioffset, Result);
    until ioffset >= Length(bData); *)

//    if debughook > 0 then
//      OutputDebugString(PChar(Format('Written (TID:%d, P:%d): %s',
//                                     [getcurrentthreadid, Self.Binding.PeerPort, BytesToStringRaw(bData)])));
  finally
    Unlock;
    strmData.Free;
  end;
end;

procedure TNVWebSocket.Execute;
var
  Response: string;
  hash: TIdHashSHA1;
  guid: TGUID;
  SValue: string;
  data: TIdBytes;
begin
  FCritical.Acquire;
  try
    hash := TIdHashSHA1.Create;
    try
      SValue := TIdEncoderMIME.EncodeBytes(                   //Base64
        hash.HashString(FWebSocketKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));               //SHA1
    finally
      hash.Free;
    end;

    (* Response:= 'HTTP/1.1 101 Switching Protocols' +#13#10
     +'Upgrade: websocket' + #13#10
     +'Connection: Upgrade' + #13#10
     +'Sec-WebSocket-Accept: ' + sValue + #13#10; *)

    FConnection.IOHandler.WriteLn('HTTP/1.1 101 Switching Protocols');
    FConnection.IOHandler.WriteLn('Upgrade: websocket');
    FConnection.IOHandler.WriteLn('Connection: Upgrade');
    FConnection.IOHandler.WriteLn('Sec-WebSocket-Accept: ' + SValue);
     // HTTP headers end with a double CR+LF
    FConnection.IOHandler.WriteLn;


    (*
     FResponseInfo.Write(Response[1], ByteLength(Response));
     FHasData:=True; *)
  finally
    FCritical.Release;
  end;

  repeat
    if FHasData then
    begin
      FCritical.Acquire;
      try
        if FTerminated then
          Exit;
        data := ToBytes(TIDBytes(FResponseInfo.Memory), FResponseInfo.Size, 0);

          //FConnection.IOHandler.Write(FResponseInfo);
        WriteData(data, wdcText, True, False, False, False);

        FResponseInfo.Clear;
        FHasData := False;
      finally
        FCritical.Release;
      end;
    end;
    TThread.Sleep(1);
  until (FTerminated);

end;

procedure TNVWebSocket.Lock;
begin
  FLock.Enter;
end;

procedure TNVWebSocket.Terminate;
begin
  FCritical.Acquire;
  try
    FTerminated := True;
  finally
    FCritical.Release;
  end;
end;

function TNVWebSocket.TryLock: Boolean;
begin
  Result := FLock.TryEnter;
end;

procedure TNVWebSocket.Unlock;
begin
  FLock.Leave;
end;

end.

