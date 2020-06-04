unit NV.Router;

interface

uses
  Classes, SysUtils, NV.Types, NV.Dispatcher, NV.Request;

type
  // Objet to respond one route with an method
  TNVRouteMethod = class
  private
    FNVRouteMethod: TNvRequestMethod;
  public
    constructor Create(aMethod: TNvRequestMethod);
  end;

  { TODO -oDelcio -cRouter : Change to TDictionary to speed up }
  TNVRouter = class(TStringList)
  private
    FDefault: TObject;
  public
    constructor Create;
    // Add an rote to any object: Subroute, Dispatch, Method //or Control(no more supported)
    function AddRoute(aRoute: string; aDest: TObject): TNVRouter; overload;
    // Add an rote to respond with method
    function AddRoute(aRoute: string; aDest: TNvRequestMethod): TNVRouter; overload;
    // Add an route to respond with an TDispatch
    function AddRoute(aRoute: string; aDest: TDispatch): TNVRouter; overload;
    // Add an subroute
    function AddSubRouter(aRoute: string; aDest: TNVRouter = nil): TNVRouter;
    // Add default Object to respond for this route(for /)
    function AddDefault(aDest: TObject): TNVRouter; overload;
    // Add default Method to respond for this route(for /)
    function AddDefault(aDest: TNvRequestMethod): TNVRouter; overload;
    // Add default Dispatch to respond for this route(for /)
    function AddDefault(aDest: TDispatch): TNVRouter; overload;
    // Find next SubRoute and return this if encountered
    function FindSubRouter(aRouteList: TStrings; var aRoute: TNVRouter): Boolean;
    // find and execute an route
    function Route(aRouteList: TStrings; aRequest: TNVRequestTask): Boolean; virtual;
    class function CreateRouteStringList: TStringList; overload;
    class function CreateRouteStringList(aRoute: string): TStringList; overload;
  end;

  TNVPageRouter = class(TNVRouter)
  private
    FPage: TObject;
  public
    constructor Create(aPage: TObject);
    function Route(aRouteList: TStrings; aRequest: TNVRequestTask): Boolean; virtual;
  end;

implementation

uses
  NV.Controls, NV.Utils, NV.JSON;

{ TNVRouter }

function TNVRouter.AddRoute(aRoute: string; aDest: TObject): TNVRouter;
var
  _RouteList : TStringList;
  _LastRouter: TNVRouter;
  _Index     : Integer;
begin
  _RouteList := CreateRouteStringList(aRoute);

  // find last router in _RouteList
  _LastRouter := Self;
  while (_RouteList.Count > 0) and FindSubRouter(_RouteList, _LastRouter) do
    _RouteList.Delete(0);

  while _RouteList.Count > 0 do
    begin

      if _RouteList.Count = 1 then
        _LastRouter.AddObject(_RouteList[0], aDest)
      else
        _LastRouter := AddSubRouter(_RouteList[0]);

      _RouteList.Delete(0);
    end;

  Result := _LastRouter;
end;

function TNVRouter.AddRoute(aRoute: string; aDest: TNvRequestMethod): TNVRouter;
var
  LMethod: TNVRouteMethod;
begin
  LMethod := TNVRouteMethod.Create(aDest);
  Result  := AddRoute(aRoute, TObject(LMethod));
end;

function TNVRouter.AddDefault(aDest: TObject): TNVRouter;
begin
  Result   := Self;
  FDefault := aDest;
end;

function TNVRouter.AddDefault(aDest: TNvRequestMethod): TNVRouter;
var
  LMethod: TNVRouteMethod;
begin
  LMethod := TNVRouteMethod.Create(aDest);
  Result  := AddDefault(TObject(LMethod));
end;

function TNVRouter.AddDefault(aDest: TDispatch): TNVRouter;
begin
  Result := AddDefault(TObject(aDest));
end;

function TNVRouter.AddRoute(aRoute: string; aDest: TDispatch): TNVRouter;
begin
  Result := AddRoute(aRoute, TObject(aDest));
end;

function TNVRouter.AddSubRouter(aRoute: string; aDest: TNVRouter): TNVRouter;
begin
  if aDest = nil then
    aDest := TNVRouter.Create;
  AddObject(aRoute, aDest);
  Result := aDest;
end;

constructor TNVRouter.Create;
begin
  inherited Create;
end;

class function TNVRouter.CreateRouteStringList(aRoute: string): TStringList;
begin
  Result                 := TStringList.Create;
  Result.Delimiter       := '/';
  Result.StrictDelimiter := True;
  Result.QuoteChar       := #0;
  Result.DelimitedText   := RemoveDelimiters(aRoute);
end;

function TNVRouter.FindSubRouter(aRouteList: TStrings; var aRoute: TNVRouter): Boolean;
var
  _RouteName: string;
  _Index    : Integer;
begin
  Result := True;

  if aRouteList.Count > 0 then
    begin
      _RouteName := aRouteList[0];

      // this route
      if _RouteName = '.' then
        begin
          aRoute := Self;
          Exit;
        end;

      { TODO -oDelcio -cRoutes : Implement ".." UpRoute }

      // find for exact math
      _Index := IndexOf(_RouteName);
      if (_Index > -1) and (Objects[_Index] is TNVRouter) then
        begin
          aRoute := TNVRouter(Objects[_Index]);
          Exit;
        end;

      // not located an route with this name and has an wildcard route
      if _Index = -1 then
        begin
          // find for wildcard math
          _Index := IndexOf('*');
          if (_Index > -1) and (Objects[_Index] is TNVRouter) then
            begin
              aRoute := TNVRouter(Objects[_Index]);
              Exit;
            end;
        end;
    end;

  Result := False;
end;

class function TNVRouter.CreateRouteStringList: TStringList;
begin
  Result := CreateRouteStringList('');
end;

function TNVRouter.Route(aRouteList: TStrings; aRequest: TNVRequestTask): Boolean;
var
  LRouteName : string;
  LIndex     : Integer;
  LObject    : TObject;
  _LastRouter: TNVRouter;
begin
  Result := False;

  LObject:= nil;

  // find last router in RoutePath
  _LastRouter := Self;
  while (aRouteList.Count > 0) and FindSubRouter(aRouteList, _LastRouter) do
    aRouteList.Delete(0);

  if aRouteList.Count > 0 then
    begin
      LRouteName := aRouteList[0];
      // find for exact route
      LIndex := IndexOf(LRouteName);
      if LIndex > -1 then
        LObject := Objects[LIndex]
      else
        begin
          // find for wildcard(accept all)
          LIndex := IndexOf('*');
          if LIndex > -1 then
            LObject := Objects[LIndex];
        end;
    end
  else
    LObject := _LastRouter.FDefault; // Default route

  // no route found and not Default route set
  if not Assigned(LObject) then
    Exit;

  if LObject is TDispatch then
    begin
      if aRouteList.Count > 0 then
        aRouteList.Delete(0);
      Result := TDispatch(LObject).Execute(aRequest);
    end
    // else if LObject is TNVControl then
    // begin
    // if aRouteList.Count > 0 then
    // aRouteList.Delete(0);
    // Result := TNVControl(LObject).Router.Route(aRouteList, aRequest);
    // end
    // else if LObject is TNvWinControl then
    // begin
    // if aRouteList.Count > 0 then
    // aRouteList.Delete(0);
    // Result := TNvWinControl(LObject).Router.Route(aRouteList, aRequest);
    // end
  else if LObject is TNVRouteMethod then
    begin
      if aRouteList.Count > 0 then
        aRouteList.Delete(0);
      Result := TNVRouteMethod(LObject).FNVRouteMethod(aRequest);
    end
  else if LObject is TNVRouter then
    begin
      if aRouteList.Count > 0 then
        aRouteList.Delete(0);
      Result := TNVRouter(LObject).Route(aRouteList, aRequest);
    end;

end;

{ TNVRouteMethod }

constructor TNVRouteMethod.Create(aMethod: TNvRequestMethod);
begin
  inherited Create;
  FNVRouteMethod := aMethod;
end;

{ TNVPageRouter }

constructor TNVPageRouter.Create(aPage: TObject);
begin
  inherited Create;
  FPage := aPage;
end;

function TNVPageRouter.Route(aRouteList: TStrings; aRequest: TNVRequestTask): Boolean;
var
  J: TJsonObject;
begin
  J := TJsonObject.Create;
  try
    J.FromJSON(aRequest.Req.Params[0]);

    (FPage as TNVModuleContainer).ProcessRequest(J);

  finally
    J.Free;
  end;
  Result := inherited;
end;

end.
