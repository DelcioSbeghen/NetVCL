unit NV.HttpServerWebsockets;

interface

uses
  Classes, SysUtils,
  System.NetEncoding, IdTCPConnection, IdCustomHTTPServer;

type
  TNVWebSocket = class;

  TWebSocketMessage = record
  private
    FData : TBytes;
    FStart: Integer;
    FSize : Integer;
    procedure GetSize(Len: Integer);
    function DataSize: Int64;
    function Ping: Boolean;
    function Pong: TBytes;
    function GetData(var AData: TBytes; var IsText: Boolean): Boolean;
    procedure Consume;
    function Close: Boolean;
    procedure CloseMessage;
  public
    procedure SetText(const Str: string);
    procedure SetData(const Data: TBytes; IsText: Boolean = False);
  end;

  TWebSocketDataEvent = procedure(Socket: TNVWebSocket; const Data: TBytes; var IsText: Boolean)
    of object;
  TWebSocketMessageEvent = procedure(Socket: TNVWebSocket; const Str: string) of object;

  TNVWebSocket = class(TComponent)
  private
    FConnection: TIdTCPConnection;
    FResponse  : TIdHTTPResponseInfo;
    FOnData    : TWebSocketDataEvent;
    FOnMessage : TWebSocketMessageEvent;
  protected
    procedure ReadBytes(AData: Pointer; aSize: Integer);
    procedure write(AData: TBytes);
    procedure Disconnect;
    procedure DoDisconnected(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(const Key: string; Connection: TIdTCPConnection;
      aResponse: TIdHTTPResponseInfo); virtual;
    destructor Destroy; override;
    procedure SendMessage(const Str: string);
    procedure ReadLoop;
    procedure Close;
    function Connected: Boolean;
    class function IsWebSocket(Connection: TIdTCPConnection; aRequest: TIdHTTPRequestInfo;
      aResp: TIdHTTPResponseInfo): Boolean;
    property OnData: TWebSocketDataEvent read FOnData write FOnData;
    property OnMessage: TWebSocketMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

uses
 {$IFDEF FPC}base64, {$ENDIF}IdHashSHA, IdGlobal, NV.VCL.Forms, NV.HttpServer;

type
  THackScreen = class(TNVScreenServer);

procedure TWebSocketMessage.SetText(const Str: string);
begin
  SetData(TEncoding.UTF8.GetBytes(Str), True);
end;

procedure TWebSocketMessage.SetData(const Data: TBytes; IsText: Boolean = False);
var
  Len : Int64;
  x, c: Integer;
  i   : Integer;
begin
  Len := Length(Data);
  c   := 2 + Len;
  if Len > 125 then
    begin
      Inc(c, 2);
      if Len > 65535 then
        Inc(c, 6);
    end;
  SetLength(FData, c);
  if IsText then
    FData[0] := $81 // FIN, TEXT
  else
    FData[0] := $82; // FIN, Binary
  x          := 1;
  if Len < 126 then
    begin
      FData[x] := Len; // MASK=0|LEN=??????
      Inc(x);
    end
  else
    begin
      if Len < 65565 then
        begin
          FData[x] := 126;
          c        := 2;
        end
      else
        begin
          FData[x] := 127;
          c        := 8;
        end;
      Inc(x, c);
      for i := 0 to c - 1 do
        begin
          FData[x] := Len and $FF;
          Dec(x);
          Len := Len shr 8;
        end;
      Inc(x, c + 1);
      Len := Length(Data);
    end;
  Move(Data[0], FData[x], Len);
end;

procedure TWebSocketMessage.CloseMessage;
begin
  SetLength(FData, 2);
  FData[0] := $88; // FIN + CLOSE
  FData[1] := $00; // Empty message
end;

procedure TWebSocketMessage.GetSize(Len: Integer);
var
  Index: Integer;
begin
  FSize     := 0;
  for Index := 0 to Len - 1 do
    begin
      Assert(Length(FData) >= FStart);
      FSize := FSize shl 8 + FData[FStart];
      Inc(FStart);
    end;
end;

function TWebSocketMessage.DataSize: Int64;
begin
  Assert(Length(FData) >= 2);
  FStart := 2; // Head
  FSize  := FData[1] and $7F;
  case FSize of
    126:
      begin
        Assert(Length(FData) >= 4);
        GetSize(2);
      end;
    127:
      begin
        Assert(Length(FData) >= 10);
        GetSize(8);
      end;
  end;
  Inc(FStart, 4); // Mask
  Result := FSize;
end;

function TWebSocketMessage.Ping: Boolean;
var
  Index: Integer;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  if (FData[1] and $80) = 0 then // Not masked
    begin
      FData[0] := $88; // FIN + CLOSE
      Result   := False;
    end
  else
    begin
      // Unmask
      for Index := 0 to FSize - 1 do
        begin
          FData[FStart + Index] := FData[FStart + Index] xor FData[FStart - 4 + Index mod 4];
        end;
      Result := FData[0] = $89; // FIN + PING
    end;
end;

function TWebSocketMessage.Pong: TBytes;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  SetLength(Result, FStart - 4 + FSize);
  Result[0] := $8A;                               // Pong
  Move(FData[1], Result[1], FStart - 5);          // Size
  Result[1] := Result[1] and $7F;                 // not masked
  Move(FData[FStart], Result[FStart - 4], FSize); // Payload
  Consume;
end;

function TWebSocketMessage.GetData(var AData: TBytes; var IsText: Boolean): Boolean;
var
  Index: Integer;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  if FData[0] = $88 then // FIN + CLOSE
    Result := False
  else
    begin
      Index := Length(AData);
      SetLength(AData, Index + FSize);
      Move(FData[FStart], AData[Index], FSize);
      Result := FData[0] and $80 > 0; // FIN
      IsText := FData[0] and $7F = 1; // Text
      Consume;
    end;
end;

function TWebSocketMessage.Close: Boolean;
begin
  Result := (Length(FData) > 0) and (FData[0] = $88); // FIN + CLOSE
end;

procedure TWebSocketMessage.Consume;
begin
  Assert((FStart > 0) and (Length(FData) >= FStart + FSize));
  Delete(FData, 0, FStart + FSize);
end;

procedure TNVWebSocket.Close;
var
  M: TWebSocketMessage;
begin
  M.CloseMessage;
  try
    write(M.FData)
  except
  end;

  try
    Disconnect;
  except
  end;
end;

function TNVWebSocket.Connected: Boolean;
begin
  try
    if Assigned(FConnection) and FConnection.Connected then
      Result := True
    else
      Result := False;
  except
    Result := False;
  end;
end;

constructor TNVWebSocket.Create(const Key: string; Connection: TIdTCPConnection;
  aResponse: TIdHTTPResponseInfo);
var
  Hash: TIdHashSHA1;
  Sign: string;
begin
  inherited Create(nil);

  FConnection := Connection;
  FResponse := aResponse;
  Connection.FreeNotification(Self);
  Connection.OnDisconnected := DoDisconnected;

  Hash := TIdHashSHA1.Create;
  Sign:= Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

  {$IFDEF FPC}
  Sign:= EncodeStringBase64(BytesToString(Hash.HashString(Sign)));
  {$ELSE}
  Sign := TNetEncoding.Base64.EncodeBytesToString(Hash.HashString(Sign));
  {$ENDIF}

  Hash.Free;

  FResponse.CloseConnection:=False;
  FResponse.ResponseNo   := 101; // HTTP/1.1 101 Switching Protocols
  FResponse.ResponseText := 'Switching Protocols';
 // FResponse.CustomHeaders.Values['Server'] := 'nginx/1.25.2';
  {$IFDEF FPC}
  FResponse.Connection:= 'upgrade';
  //FResponse.ContentType := ''; // dummy
  FResponse.CustomHeaders.Values['Content-Type']        := 'text/xml';
  {$ELSE}
  FResponse.CustomHeaders.Values['Connection']           := 'upgrade';
  FResponse.ContentType := 'text/xml'; // dummy
  {$ENDIF}
  FResponse.CustomHeaders.Values['Upgrade']              := 'websocket';
  FResponse.CustomHeaders.Values['Sec-WebSocket-Accept'] := Sign;
  //FResponse.CustomHeaders.Values['X-Powered-By'] := 'Ratchet/0.4.4';
  //FResponse.ContentLength := -1;
end;

destructor TNVWebSocket.Destroy;
begin

  inherited;
end;

procedure TNVWebSocket.Disconnect;
begin
  if Connected then
    FConnection.Disconnect(False);
end;

procedure TNVWebSocket.DoDisconnected(Sender: TObject);
begin
  FConnection := nil;

end;

procedure TNVWebSocket.SendMessage(const Str: string);
var
  M: TWebSocketMessage;
begin
  M.SetText(Str);
  write(M.FData);
end;

procedure TNVWebSocket.write(AData: TBytes);
begin
  if Connected then
    FConnection.IOHandler.write(TIdBytes(AData));
end;

class function TNVWebSocket.IsWebSocket(Connection: TIdTCPConnection; aRequest: TIdHTTPRequestInfo;
  aResp: TIdHTTPResponseInfo): Boolean;
var
  Key, Ver     : string;
  _NewWebSocket: TNVWebSocket;
begin
  Key := aRequest.RawHeaders.Values['Sec-WebSocket-Key'];
  Ver := aRequest.RawHeaders.Values['Sec-WebSocket-Version'];
  if (Key <> '') and (Ver = '13') and not Application.Terminated then
    begin
      // TDWApplication(aDwApp).WebSocket := //
      // This class constructor
      // Self.

      _NewWebSocket := TNVWebSocket.Create(Key, Connection, aResp);

      with THackScreen(Screen) do
        begin
          if Assigned(FWebSochet) then
          begin
            FWebSochet.Close;
            FWebSochet.Free;
          end;
          FWebSochet := _NewWebSocket;
        end;

      aResp.WriteHeader; // send the Header

      _NewWebSocket.ReadLoop; // WebSocket data loop

      Result := True;
    end
  else
    Result := False;
end;

procedure TNVWebSocket.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FConnection) and (Operation = opRemove) then
    FConnection := nil;
end;

procedure TNVWebSocket.ReadBytes(AData: Pointer; aSize: Integer);
begin
  if Connected then
    FConnection.IOHandler.ReadBytes(TIdBytes(AData^), aSize, True);
end;

procedure TNVWebSocket.ReadLoop;
var
  M: TWebSocketMessage;
  D: TBytes;
  T: Boolean;
  s:string;
begin
  repeat
    M.FData :=  nil;
    ReadBytes(@M.FData, 6);                        // 2 head + 4 mask
    if (M.FData = nil) or (M.FData[0] = $88) then // FIN + CLOSE
      Break;

    // get size
    case M.FData[1] and $7F of
      126: ReadBytes(@M.FData, 2); // word  Size
      127: ReadBytes(@M.FData, 8); // Int64 Size
    end;

    // read data
    ReadBytes(@M.FData, M.DataSize);

    if M.Ping then
      Write(M.Pong)
    else
      begin
        if M.GetData(D, T) then
          begin
            if Assigned(FOnData) then
              FOnData(Self, D, T);
            if T and Assigned(FOnMessage) then
              FOnMessage(Self, TEncoding.UTF8.GetString(D));
            {$IFDEF FPC}
            ThackScreen(Screen).DataReceived(Pchar(BytesToString(D)));
            {$ELSE}
            ThackScreen(Screen).DataReceived(Pchar(TEncoding.UTF8.GetString(D)));
            {$ENDIF}
            D := nil;
          end;
      end;
  until M.Close; // FIN + CLOSE
end;

end.














//
// interface
// uses
// Classes, SysUtils, IdTCPConnection, IdCustomHTTPServer, SyncObjs,
// IdHashSHA, IdCoderMIME, IdGlobal, IdException, IdStream;
//
// type
//
// TWSDataCode  = (wdcNone, wdcContinuation, wdcText, wdcBinary, wdcClose, wdcPing, wdcPong);
//
// TNVWebSocket = class(TObject)
// private
// FApp:TObject;
// FConnection:TIdTCPConnection;
// FTerminated:Boolean;
// FResponseInfo:TMemoryStream;
// FCritical:TCriticalSection;
// FHasData:Boolean;
// FWebSocketKey :string;
// FLock: TCriticalSection;
// FLastActivityTime: TDateTime;
// function WriteData(aData: TIdBytes; aType: TWSDataCode; aFIN, aRSV1, aRSV2,
// aRSV3: boolean): integer;
// public
// constructor Create(aDwApp: TObject; Connection:TIdTCPConnection;
// WebSocketKey:string);
// destructor Destroy; override;
// procedure Execute;
// procedure Lock;
// procedure Unlock;
// function  TryLock: Boolean;
// function BeginWrite:TMemoryStream;
// procedure EndWrite;
// procedure Terminate;
// end;
//
//
// implementation
//
// uses
// DW.CORE.Language;
//
//
/// /frame codes
// const
// C_FrameCode_Continuation = 0;
// C_FrameCode_Text         = 1;
// C_FrameCode_Binary       = 2;
// //3-7 are reserved for further non-control frames
// C_FrameCode_Close        = 8;
// C_FrameCode_Ping         = 9;
// C_FrameCode_Pong         = 10 {A};
// //B-F are reserved for further control frames
//
// { TNVWebSocket }
//
// function TNVWebSocket.BeginWrite: TMemoryStream;
// begin
// FCritical.Acquire;
// Result:= FResponseInfo;
// end;
//
// constructor TNVWebSocket.Create(aDwApp: TObject; Connection:TIdTCPConnection;
// WebSocketKey:string);
// begin
// FApp:= aDwApp;
// FConnection:= Connection;
// FTerminated:=False;
// FCritical:= TCriticalSection.Create;
// FLock:= TCriticalSection.Create;
// FResponseInfo:= TMemoryStream.Create;
// FHasData:=False;
// FWebSocketKey:= WebSocketKey;
// end;
//
// destructor TNVWebSocket.Destroy;
// begin
// Terminate;
// FResponseInfo.Free;
// FCritical.Free;
// FLock.Free;
// inherited;
// end;
//
// procedure TNVWebSocket.EndWrite;
// begin
// FCritical.Release;
// FHasData:=True; //no need syncronization because Boolean is Atomic
// //wait socket send data
// while FHasData do
// Sleep(1);
// end;
//
// function TNVWebSocket.WriteData(aData:TIdBytes; aType:TWSDataCode; aFIN,aRSV1,aRSV2,aRSV3:boolean): integer;
// var
// iByte: Byte;
// i, ioffset: Integer;
// iDataLength, iPos: Int64;
// rLength: Int64Rec;
// rMask: record
// case Boolean of
// True : (MaskAsBytes: array[0..3] of Byte);
// False: (MaskAsInt  : Int32);
// end;
// strmData: TMemoryStream;
// bData: TIdBytes;
// begin
// Result := 0;
// Assert(FConnection.Socket.Binding <> nil);
//
// strmData := TMemoryStream.Create;
// Lock;
// try
// FLastActivityTime := Now;   //sending some data
// (* 0                   1                   2                   3
// 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 (nr)
// 7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0 (bit)
// +-+-+-+-+-------+-+-------------+-------------------------------+
// |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
// |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
// |N|V|V|V|       |S|             |   (if payload len==126/127)   |
// | |1|2|3|       |K|             |                               |
// +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - + *)
// //FIN, RSV1, RSV2, RSV3: 1 bit each
// if aFIN  then iByte :=         (1 shl 7) else iByte := 0;
// if aRSV1 then iByte := iByte + (1 shl 6);
// if aRSV2 then iByte := iByte + (1 shl 5);
// if aRSV3 then iByte := iByte + (1 shl 4);
// //Opcode: 4 bits
// case aType of
// wdcContinuation : iByte := iByte + C_FrameCode_Continuation;
// wdcText         : iByte := iByte + C_FrameCode_Text;
// wdcBinary       : iByte := iByte + C_FrameCode_Binary;
// wdcClose        : iByte := iByte + C_FrameCode_Close;
// wdcPing         : iByte := iByte + C_FrameCode_Ping;
// wdcPong         : iByte := iByte + C_FrameCode_Pong;
// else
// raise EIdException.CreateFmt(DWLang.RsUnsupportedDataCode, [Ord(aType)]);
// end;
// strmData.Write(iByte, SizeOf(iByte));
//
// iByte := 0;
// //Mask: 1 bit; Note: Clients must apply a mask
// // if not IsServerSide then iByte := (1 shl 7);
//
// //Length: 7 bits or 7+16 bits or 7+64 bits
// if Length(aData) < 126 then            //7 bit, 128
// iByte := iByte + Length(aData)
// else if Length(aData) < 1 shl 16 then  //16 bit, 65536
// iByte := iByte + 126
// else
// iByte := iByte + 127;
// strmData.Write(iByte, SizeOf(iByte));
//
// //Extended payload length?
// if Length(aData) >= 126 then
// begin
// //If 126, the following 2 bytes interpreted as a 16-bit unsigned integer are the payload length
// if Length(aData) < 1 shl 16 then  //16 bit, 65536
// begin
// rLength.Lo := Length(aData);
// iByte := rLength.Bytes[1];
// strmData.Write(iByte, SizeOf(iByte));
// iByte := rLength.Bytes[0];
// strmData.Write(iByte, SizeOf(iByte));
// end
// else
// //If 127, the following 8 bytes interpreted as a 64-bit unsigned integer (the most significant bit MUST be 0) are the payload length
// begin
// rLength := Int64Rec(Int64(Length(aData)));
// for i := 7 downto 0 do
// begin
// iByte := rLength.Bytes[i];
// strmData.Write(iByte, SizeOf(iByte));
// end;
// end
// end;
//
// //Masking-key: 0 or 4 bytes; Note: Clients must apply a mask
// (* if not IsServerSide then
// begin
// rMask.MaskAsInt := Random(MaxInt);
// strmData.Write(rMask.MaskAsBytes[0], SizeOf(Byte));
// strmData.Write(rMask.MaskAsBytes[1], SizeOf(Byte));
// strmData.Write(rMask.MaskAsBytes[2], SizeOf(Byte));
// strmData.Write(rMask.MaskAsBytes[3], SizeOf(Byte));
// end;*)
//
// //write header
// strmData.Position := 0;
// TIdStreamHelper.ReadBytes(strmData, bData);
//
// //Mask? Note: Only clients must apply a mask
// (* if not IsServerSide then
// begin
// iPos := 0;
// iDataLength := Length(aData);
// //in place masking
// while iPos < iDataLength do
// begin
// iByte := aData[iPos] xor rMask.MaskAsBytes[iPos mod 4]; //apply mask
// aData[iPos] := iByte;
// inc(iPos);
// end;
// end; *)
//
// AppendBytes(bData, aData);   //important: send all at once!
//
// //
// FConnection.IOHandler.WriteDirect(bdata, (Length(bData) - ioffset), iOffset);
// (* ioffset := 0;
// repeat
// //Result := Binding.Send(bData, ioffset);
// //
// Result :=  FConnection.IOHandler.WriteDirect(bdata, (Length(bData) - ioffset), iOffset);  //ssl compatible?
// if Result<0 then
// begin
// // IO error ; probably connexion closed by peer on protocol error ?
// {$IFDEF DEBUG_WS}
// if Debughook > 0 then
// OutputDebugString(PChar(Format('WriteError ThrID:%d, L:%d, R:%d',[getcurrentthreadid,Length(bData)-ioffset,Result])));
// {$ENDIF}
// break;
// end;
// Inc(ioffset, Result);
// until ioffset >= Length(bData); *)
//
/// /    if debughook > 0 then
/// /      OutputDebugString(PChar(Format('Written (TID:%d, P:%d): %s',
/// /                                     [getcurrentthreadid, Self.Binding.PeerPort, BytesToStringRaw(bData)])));
// finally
// Unlock;
// strmData.Free;
// end;
// end;
//
// procedure TNVWebSocket.Execute;
// var
// Response:string;
// hash: TIdHashSHA1;
// guid: TGUID;
// SValue:string;
// data:TIdBytes;
// begin
// FCritical.Acquire;
// try
// hash := TIdHashSHA1.Create;
// try
// sValue := TIdEncoderMIME.EncodeBytes(                   //Base64
// hash.HashString(FWebSocketKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11') );               //SHA1
// finally
// hash.Free;
// end;
//
// (* Response:= 'HTTP/1.1 101 Switching Protocols' +#13#10
// +'Upgrade: websocket' + #13#10
// +'Connection: Upgrade' + #13#10
// +'Sec-WebSocket-Accept: ' + sValue + #13#10; *)
//
// FConnection.IOHandler.WriteLn('HTTP/1.1 101 Switching Protocols');
// FConnection.IOHandler.WriteLn('Upgrade: websocket');
// FConnection.IOHandler.WriteLn('Connection: Upgrade');
// FConnection.IOHandler.WriteLn('Sec-WebSocket-Accept: ' + sValue);
// FConnection.IOHandler.WriteLn('Content-Type: text/xml');
//
// // HTTP headers end with a double CR+LF
// FConnection.IOHandler.WriteLn;
//
//
// (*
// FResponseInfo.Write(Response[1], ByteLength(Response));
// FHasData:=True; *)
// finally
// FCritical.Release;
// end;
//
//
// repeat
// if FHasData then
// begin
// FCritical.Acquire;
// try
// if FTerminated then
// Exit;
// data := ToBytes(TIdBytes(FResponseInfo.Memory), FResponseInfo.Size, 0);
//
// //FConnection.IOHandler.Write(FResponseInfo);
// WriteData(data, wdcText, True, False, False, False);
//
// FResponseInfo.Clear;
// FHasData:= False;
// finally
// FCritical.Release;
// end;
// end;
// TThread.Sleep(1);
// until (FTerminated);
//
// end;
//
// procedure TNVWebSocket.Lock;
// begin
// FLock.Enter;
// end;
//
// procedure TNVWebSocket.Terminate;
// begin
// FCritical.Acquire;
// try
// FTerminated:=True;
// finally
// FCritical.Release;
// end;
// end;
//
// function TNVWebSocket.TryLock: Boolean;
// begin
// Result:= FLock.TryEnter;
// end;
//
// procedure TNVWebSocket.Unlock;
// begin
// FLock.Leave;
// end;
//
// end.

//  end.
