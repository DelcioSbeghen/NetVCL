unit NV.Router;

interface

uses
  Classes, NV.Common.Types, NV.Dispatcher, NV.Request;

type
  TNVRouteMethod = class
  private
    FNVRouteMethod: TNvRequestMethod;
  public
    constructor Create(aMethod: TNvRequestMethod);
  end;

  TNVRouter = class(TStringList)
  public
    function AddRoute(aRoute: string; aDest: TObject): string; overload;
    function AddRoute(aRoute: string; aDest: TNvRequestMethod): string; overload;
    function AddRoute(aRoute: string; aDest: TDispatch): string; overload;
    function Route(aRoutePath: TStrings; aRequest: TNVRequestTask): Boolean;
  end;

implementation

uses
  NV.VCL.Controls, NV.VCL.Container;

{ TNVRouter }

function TNVRouter.AddRoute(aRoute: string; aDest: TObject): string;
begin
  AddObject(aRoute, aDest);
  Result := aRoute;
end;

function TNVRouter.AddRoute(aRoute: string; aDest: TNvRequestMethod): string;
var
  LMethod: TNVRouteMethod;
begin
  LMethod := TNVRouteMethod.Create(aDest);
  AddObject(aRoute, LMethod);
  Result := aRoute;
end;

function TNVRouter.AddRoute(aRoute: string; aDest: TDispatch): string;
begin

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
  if Find(LRouteName, LIndex) then
  begin
    LObject := Objects[LIndex];
    if not Assigned(LObject) then
      Exit;
    if LObject is TDispatch then
    begin
      TDispatch(LObject).Execute;
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

