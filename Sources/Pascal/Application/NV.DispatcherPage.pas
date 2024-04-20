unit NV.DispatcherPage;

interface

uses
  NV.Dispatcher, NV.VCL.Page, NV.Request;

type
  TDispatchPage = class(TDispatch)
  private
    FPage: TNVBasepage;
    FPageClass: TNVBasepage;
  public
    function Execute(aRequest: TNVRequestTask): Boolean; override;
    constructor Create(aPage: TNVBasepage);
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils, System.Classes, SysUtils, NV.JSON, IdGlobal, NV.Utils, NV.VCL.Forms;

type
 THackApplication  = class(TNvApplication);


{ TDispatchPage }

constructor TDispatchPage.Create(aPage: TNVBasepage);
begin
  inherited Create;
  FPage := aPage;
end;

destructor TDispatchPage.Destroy;
begin

  inherited;
end;

function TDispatchPage.Execute(aRequest: TNVRequestTask): Boolean;
  function Html: string;
  var
    _CssFiles: string;
    I: Integer;
  begin
    _CssFiles := '';

    // Page Css Files
    for I := 0 to FPage.CssFiles.Count - 1 do
    begin
      _CssFiles := _CssFiles + sLineBreak + //
        '    <link type="text/css" rel="stylesheet" href="' + //
        MakeValidFileUrl('', FPage.CssFiles[I]) + //
        '"> ';
    end;

    // Default CssFiles
    if FPage.CssFiles.Count > 0 then
      _CssFiles := _CssFiles + sLineBreak;

    _CssFiles := _CssFiles + //
      '    <link type="text/css" rel="stylesheet" href="./netvcl/css/nv.css"> '
      + sLineBreak; //

    Result := sLineBreak + //
      '<!DOCTYPE html> ' + //
      '<html lang="pt-br"> ' + sLineBreak + //
      ' ' + sLineBreak + //
      '<head> ' + sLineBreak + //
      '    <meta charset="utf-8"> ' + sLineBreak + //
      '    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">'
      + sLineBreak + //
      '    <meta http-equiv="cache-control"   content="no-cache" />' + //
      '    <title></title> ' + sLineBreak + //
      '    <script type="module" src="./netvcl/js/nv.forms.js"></script> ' +
      sLineBreak + //
      '    <script> ' + sLineBreak + //
      '        function DoLoad() { ' + sLineBreak + //
      '           App = new window.TApplication(""); ' + sLineBreak + //
      IfThen( //
      THackApplication(NV.VCL.Forms.Application).FDesignInstance //
      , 'App.FDesign = true;' //
      , '') + sLineBreak + //
      '           App.ParseJson(' + //
      FPage.Ajax.JSON.ToJSON + //
      '); ' + sLineBreak + //
      '' + sLineBreak + //
      '        } ' + sLineBreak + //
      '    </script> ' + sLineBreak + //
      '    <meta id="last-css"> ' + sLineBreak + //
      _CssFiles + sLineBreak + //
    // '    <link type="text/css" rel="stylesheet" href="./netvcl/css/nv.css"> ' + sLineBreak + //
        '</head> ' + sLineBreak + //
      ' ' + sLineBreak + //
      '<body onload="DoLoad()"> ' + sLineBreak + //
      '    <script src="./netvcl/js/jquery-3.3.1.min.js"></script> ' +
      sLineBreak + //
      '    <script src="./netvcl/js/bs/bootstrap.min.js"></script> ' +
      sLineBreak + //
      '    <script type="module" src="./netvcl/js/bs/nv.bs.forms.js"></script> '
      + sLineBreak + //
      ' ' + sLineBreak + //
      '        <meta id="last-js"> ' + sLineBreak + //
      '</body> ' + sLineBreak + //
      ' ' + sLineBreak + //
      '</html> ';
  end; //

var
  J: TJsonObject;
begin
  inherited Execute(aRequest);
  if aRequest.Req.Params.Values['callback'].IsEmpty then
  begin
    FPage.ReRender(False);
    FPage.Render;
    aRequest.Resp.Text := Html;
    FPage.Ajax.JSON.Clear;
  end
  else
  begin
    J := TJsonObject.Create;
    try
      J.FromJSON(aRequest.Req.Params[0]);
      FPage.ProcessRequest(J);
    finally
      J.Free;
    end;
    aRequest.Resp.Text := FPage.Ajax.JSON.ToJSON;
    FPage.Ajax.JSON.Clear;
  end;

  Result := True;
end;

end.
