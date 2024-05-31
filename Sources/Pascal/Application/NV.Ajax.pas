unit NV.Ajax;

interface

uses
  Classes, Controls, NV.Json, Generics.Collections;

type
  TNvAjax = class;

  TReqType = (reqModule, reqJs, reqCss);

  TNvObjChangeList = class(TObjectList<TObject>)
  protected
    procedure ProcessChanges;

  end;

  TNvAjax = class(TObject)
  private
    FPage       : TWinControl;
    FIncludes   : TJsonObject;
    FJson       : TJsonObject;
    FUpdateCount: Integer;
    FChangeList : TNvObjChangeList;
  public
    constructor Create(aPage: TWinControl); reintroduce;
    destructor Destroy; override;
    procedure AddInclude(aName: string; aType: TReqType; aSrc: string);
    // procedure AddUpdate(aId: string; aProp: string; aValue: string); overload;
    // procedure AddUpdate(aId: string; aProp: string; aValue: Boolean); overload;
    function GetControlJson(aId: string; FirstRender: Boolean = False): TJsonObject;
    procedure AddDestruction(aId: string);
    procedure AddCallFunction(aId, aFunction, aParam: string); overload;
    procedure AddCallFunction(aId, aFunction: string; aParams: TJsonArray); overload;
    procedure Invalidate;
    function ExecJson: TJsonArray;
    procedure BeginUpdate;
    procedure EndUpdate;
    function InUpdate: Boolean; inline;
    property Includes: TJsonObject read FIncludes;
    property Json: TJsonObject read FJson;
    property Page: TWinControl read FPage;
    // new implementation
    property ChangeList: TNvObjChangeList read FChangeList;
  end;

function TypeReqToStr(aType: TReqType): string;

implementation

uses
  NV.Utils, NV.Controls, NV.VCL.Forms;


// type
// ThackNvMo

function TypeReqToStr(aType: TReqType): string;
begin
  case aType of
    reqModule: Result := 'jsm';
    reqJs: Result     := 'js';
    reqCss: Result    := 'css';
  end;
end;

{ TNvAjax }

procedure TNvAjax.AddCallFunction(aId, aFunction, aParam: string);
begin
  with GetControlJson(aId).A['Call'].AddObject do
    begin
      S['function'] := aFunction;
      S['params']   := aParam;
    end;
end;

procedure TNvAjax.AddCallFunction(aId, aFunction: string; aParams: TJsonArray);
begin
  with GetControlJson(aId).A['Call'].AddObject do
    begin
      S['function'] := aFunction;
      A['params']   := aParams;
    end;
end;

procedure TNvAjax.AddDestruction(aId: string);
var
  _Json: TJsonObject;
begin
  _Json := GetControlJson(aId);

  if _Json.Contains('Change') then
    _Json.Remove('Change');

  if _Json.Contains('New') then
    _Json.Remove('New');

  _Json.S['Del'] := aId;
end;

procedure TNvAjax.AddInclude(aName: string; aType: TReqType; aSrc: string);
var
  _Inc: TJsonObject;
begin
  _Inc := FIncludes.O[aName];

  if _Inc.S['Url'] = '' then
    begin
      _Inc.S['Type'] := TypeReqToStr(aType);
      _Inc.S['Url']  := aSrc;
      FJson.A['Reqs'].AddObject.Assign(_Inc);
    end;
end;

procedure TNvAjax.BeginUpdate;
begin
  inc(FUpdateCount);
end;

{ procedure TNvAjax.AddUpdate(aId, aProp, aValue: string);
  var
  _Updates: TJsonArray;
  I: Integer;
  begin
  _Updates := Json.A['Comps'];

  for I := 0 to _Updates.Count - 1 do
  begin
  if _Updates[I].S['Id'] = aId then
  begin
  _Updates[I].S[aProp] := aValue;
  Exit;
  end;
  end;

  with _Updates.AddObject do
  begin
  S['Id'] := aId;
  S['Op'] := 'Change';
  S[aProp] := aValue;
  end;
  end;

  procedure TNvAjax.AddUpdate(aId, aProp: string; aValue: Boolean);
  var
  _Updates: TJsonArray;
  I: Integer;
  begin

  for I := 0 to _Updates.Count - 1 do
  begin
  if _Updates[I].S['Id'] = aId then
  begin
  _Updates[I].B[aProp] := aValue;
  Exit;
  end;
  end;

  with _Updates.AddObject do
  begin
  S['Id'] := aId;
  S['Op'] := 'Change';
  B[aProp] := aValue;
  end;

  end; }

constructor TNvAjax.Create(aPage: TWinControl);
begin
  inherited Create;
  FPage       := aPage;
  FIncludes   := TJsonObject.Create;
  FJson       := TJsonObject.Create;
  FChangeList := TNvObjChangeList.Create(False);
end;

destructor TNvAjax.Destroy;
begin
  FIncludes.Free;
  FJson.Free;
  FChangeList.Free;
  inherited;
end;

procedure TNvAjax.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Invalidate;
end;

function TNvAjax.ExecJson: TJsonArray;
begin
  Result := FJson.A['Exec'];
end;

function TNvAjax.GetControlJson(aId: string; FirstRender: Boolean = False): TJsonObject;
var
  _Updates: TJsonArray;
  I       : Integer;
begin
  _Updates := FJson.A['Comps'];

  for I := 0 to _Updates.Count - 1 do
    begin
      if (_Updates[I].S['Id'] = aId) then
        begin
          Result := _Updates[I].ObjectValue;
          if FirstRender and Result.Contains('Change') then
            Result.Remove('Change');

          Exit;
        end;
    end;

  Result := _Updates.AddObject;
  if Not FirstRender then
    Result.S['Change'] := aId;
  Result.S['Id']       := aId;
end;

function TNvAjax.InUpdate: Boolean;
begin
  Result := FUpdateCount <> 0;
end;

procedure TNvAjax.Invalidate;
begin
  if FUpdateCount <> 0 then
    Exit;

  if ((Json.Count > 0) or (FChangeList.Count > 0)) and Application.Running and (Screen <> nil) and
    Screen.Active then
    begin
      // ProcessChanges can be create new changes
      BeginUpdate;
      try
        //
        repeat
          FChangeList.ProcessChanges;
        until FChangeList.Count = 0;

        Screen.UpdateScreen(Json.ToJSON);
        Json.Clear;

      finally
        EndUpdate;
      end;
    end;
end;

{ TNvObjChangeList }

procedure TNvObjChangeList.ProcessChanges;
var
  _Obj: TObject;
begin
  for _Obj in Self do
    begin
      if _Obj is TNvControl then
        TNvControl(_Obj).RenderChanges
      else if _Obj is TNvWinControl then
        TNvWinControl(_Obj).RenderChanges;
    end;
end;

end.
