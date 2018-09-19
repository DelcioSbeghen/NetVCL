unit NV.Router;

interface

uses
  Classes, SysUtils, NV.Types, NV.Dispatcher, NV.Request;

type
  TNVRouteMethod = class
  private
    FNVRouteMethod: TNvRequestMethod;
  public
    constructor Create(aMethod: TNvRequestMethod);
  end;

  TNVRouter = class(TStringList)
  public
    function AddRoute(aRoute: string; aDest: TObject): TNVRouter; overload;
    function AddRoute(aRoute: string; aDest: TNvRequestMethod): TNVRouter; overload;
    function AddRoute(aRoute: string; aDest: TDispatch): TNVRouter; overload;
    function AddSubRoute(aRoute: string; aDest: TNVRouter = nil): TNVRouter;
    function Route(aRoutePath: TStrings; aRequest: TNVRequestTask): Boolean;
    class function CreateRouteStringList: TStringList; overload;
    class function CreateRouteStringList(aRoute: string): TStringList; overload;
  end;

implementation

uses
  NV.VCL.Controls, NV.VCL.Container, NV.Utils;

{ TNVRouter }

function TNVRouter.AddRoute(aRoute: string; aDest: TObject): TNVRouter;
var
  LMethod: TNVRouteMethod;
  _RouteList: TStringList;
  _LastRoute: TNVRouter;
begin
  _RouteList := CreateRouteStringList(aRoute);
  _LastRoute := Self;
  while _RouteList.Count > 0 do
  begin
    if _RouteList.Count = 1 then
      _LastRoute.AddObject(_RouteList[0], aDest)
    else
     _LastRoute := AddSubRoute(_RouteList[0]);

    _RouteList.Delete(0);
  end;

  Result:= _LastRoute;
end;

function TNVRouter.AddRoute(aRoute: string; aDest: TNvRequestMethod): TNVRouter;
var
  LMethod: TNVRouteMethod;
begin
  LMethod := TNVRouteMethod.Create(aDest);
  Result:= AddRoute(aRoute,TObject(LMethod));
end;

function TNVRouter.AddRoute(aRoute: string; aDest: TDispatch): TNVRouter;
begin
  Result:= AddRoute(aRoute, TObject(aDest));
end;

function TNVRouter.AddSubRoute(aRoute: string; aDest: TNVRouter): TNVRouter;
begin
  if aDest = nil then
    aDest := TNVRouter.Create;
  AddObject(aRoute, aDest);
  Result := aDest;
end;

class function TNVRouter.CreateRouteStringList(aRoute: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Delimiter := '/';
  Result.StrictDelimiter := True;
  Result.QuoteChar:= #0;
  Result.DelimitedText := RemoveDelimiters(aRoute);
end;

class function TNVRouter.CreateRouteStringList: TStringList;
begin
  Result:=CreateRouteStringList('');
end;

function TNVRouter.Route(aRoutePath: TStrings; aRequest: TNVRequestTask): Boolean;
var
  LRouteName: string;
  LIndex: Integer;
  LObject: TObject;
begin
  Result := False;
  if aRoutePath.Count < 1 then
    Exit;
  LRouteName := aRoutePath[0];
  LIndex:= IndexOf(LRouteName);
  if LIndex > -1 then
  begin
    LObject := Objects[LIndex];
    if not Assigned(LObject) then
      Exit;
    if LObject is TNVRouter then
    begin
      aRoutePath.Delete(0);
      TNVRouter(LObject).Route(aRoutePath, aRequest);
    end
    else if LObject is TDispatch then
    begin
      Result := TDispatch(LObject).Execute(aRequest);
    end
    else if LObject is TNVControl then
    begin
      aRoutePath.Delete(0);
      TNVControl(LObject).Router.Route(aRoutePath, aRequest);
      Result := True;
    end
    else if LObject is TNVContainer then
    begin
      aRoutePath.Delete(0);
      TNVContainer(LObject).Router.Route(aRoutePath, aRequest);
      Result := True;
    end
    else if LObject is TNVRouteMethod then
    begin
      TNVRouteMethod(LObject).FNVRouteMethod(aRequest);
      Result := True;
    end;
  end;
end;

{ TNVRouteMethod }

constructor TNVRouteMethod.Create(aMethod: TNvRequestMethod);
begin
  inherited Create;
  FNVRouteMethod := aMethod;
end;

end.

