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
    '    <script src="netvcl/js/NVObject.js"> ' + sLineBreak + //
    '    </script> ' + sLineBreak +  //
    '    <script src="netvcl/js/NVMenu.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak +  //
    '    <script src="netvcl/js/NVServer.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak + //
    '    <script src="netvcl/js/NVApplication.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak +  //
    '    <script src="netvcl/js/VCL.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak +   //
    '    <script src="netvcl/js/NVComponent.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak +  //
    '    <script src="netvcl/js/NVUtils.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak + //
    '    <script src="netvcl/js/NVInputBase.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak + //
    ' ' + sLineBreak +    //
    '    <script src="netvcl/js/NVContainer.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak +    //
    '    <script src="netvcl/js/NVPage.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak +  //
    '    <script src="netvcl/js/NVDataset.js"> ' + sLineBreak + //
    '    </script> ' + sLineBreak +   //
    '    <script src="netvcl/js/NVTextBase.js"> ' + sLineBreak +    //
    '    </script> ' + sLineBreak +    //
    '    <script src="netvcl/js/NVText.js"> ' + sLineBreak +    //
    '    </script> ' + sLineBreak +   //
    '    <script src="netvcl/js/NVInput.js"> ' + sLineBreak +   //
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

