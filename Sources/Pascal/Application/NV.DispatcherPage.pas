unit NV.DispatcherPage;

interface

uses
  NV.Dispatcher, NV.VCL.Container, NV.Request;

type
  TDispatchPage = class(TDispatch)
  private
    FPage: TNVModuleContainer;
  public
    function Execute(aRequest: TNVRequestTask):Boolean; override;
    constructor Create(aPage: TNVModuleContainer);
    destructor Destroy; override;
  end;

implementation

uses
  NV.VCL.Page;

{ TDispatchPage }

constructor TDispatchPage.Create(aPage: TNVModuleContainer);
begin
  inherited Create;
  FPage := aPage;
end;

destructor TDispatchPage.Destroy;
begin

  inherited;
end;

function TDispatchPage.Execute(aRequest: TNVRequestTask):Boolean;
const
  Html =  sLineBreak +//
    '<!DOCTYPE html>' + //
    '<html> ' + sLineBreak +  //
    ' ' + sLineBreak +   //
    '<head> ' + sLineBreak +  //
    '    <script src="netvcl/js/jquery.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak + //
    '    <script src="netvcl/js/bootstrap.js"> ' + sLineBreak + //
    '    </script> ' + sLineBreak + //
    '    <script src="netvcl/js/VXObject.js"> ' + sLineBreak + //
    '    </script> ' + sLineBreak +  //
    '    <script src="netvcl/js/VXMenu.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak +  //
    '    <script src="netvcl/js/VXServer.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak + //
    '    <script src="netvcl/js/VXApplication.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak +  //
    '    <script src="netvcl/js/VCL.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak +   //
    '    <script src="netvcl/js/VXComponent.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak +  //
    '    <script src="netvcl/js/VXUtils.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak + //
    '    <script src="netvcl/js/VXInputBase.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak + //
    ' ' + sLineBreak +    //
    '    <script src="netvcl/js/VXContainer.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak +    //
    '    <script src="netvcl/js/VXPage.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak +  //
    '    <script src="netvcl/js/VXDataset.js"> ' + sLineBreak + //
    '    </script> ' + sLineBreak +   //
    '    <script src="netvcl/js/VXTextBase.js"> ' + sLineBreak +    //
    '    </script> ' + sLineBreak +    //
    '    <script src="netvcl/js/VXText.js"> ' + sLineBreak +    //
    '    </script> ' + sLineBreak +   //
    '    <script src="netvcl/js/VXInput.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak +   //
    '</head> ' + sLineBreak +  //
    ' ' + sLineBreak +   //
    '<body> ' + sLineBreak +  //
    '    <div id="content"></div> ' + sLineBreak +   //
    '    <script> ' + sLineBreak +    //
    '        p = new TPage(); ' + sLineBreak +     //
    '        i = new TInput(p); ' + sLineBreak +  //
    '        i2 = new TInput(p); ' + sLineBreak +  //
    '        i3 = new TInput(p); ' + sLineBreak +  //
    '        i4 = new TInput(p); ' + sLineBreak +  //
    '        p.show(); ' + sLineBreak +  //
    '        i.ShowCaption = true; ' + sLineBreak +  //
    '        i.Caption = ''Caption''; ' + sLineBreak +   //
    '    </script> ' + sLineBreak + //
    '</body> ' + sLineBreak + //
    ' ' + sLineBreak +  //
    '</html> '; //
begin
  inherited Execute(aRequest);
  aRequest.Resp.ContentText:= Html;
  Result:= True;
end;

end.

