unit NV.DispatcherPage;

interface

uses
  NV.Dispatcher, NV.VCL.Container, NV.Request;

type
  TDispatchPage = class(TDispatch)
  private
    FPage: TNVModuleContainer;
  public
    procedure Execute(aRequest: TNVRequestTask); override;
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

procedure TDispatchPage.Execute(aRequest: TNVRequestTask);
const
  Html =  //
    '<html> ' + sLineBreak +  //
    ' ' + sLineBreak +   //
    '<head> ' + sLineBreak +  //
    '    <script src="jquery.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak + //
    '    <script src="bootstrap.js"> ' + sLineBreak + //
    '    </script> ' + sLineBreak + //
    '    <script src="VXObject.js"> ' + sLineBreak + //
    '    </script> ' + sLineBreak +  //
    '    <script src="VXMenu.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak +  //
    '    <script src="VXServer.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak + //
    '    <script src="VXApplication.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak +  //
    '    <script src="VCL.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak +   //
    '    <script src="VXComponent.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak +  //
    '    <script src="VXUtils.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak + //
    '    <script src="VXInputBase.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak + //
    ' ' + sLineBreak +    //
    '    <script src="VXContainer.js"> ' + sLineBreak +  //
    '    </script> ' + sLineBreak +    //
    '    <script src="VXPage.js"> ' + sLineBreak +   //
    '    </script> ' + sLineBreak +  //
    '    <script src="VXDataset.js"> ' + sLineBreak + //
    '    </script> ' + sLineBreak +   //
    '    <script src="VXTextBase.js"> ' + sLineBreak +    //
    '    </script> ' + sLineBreak +    //
    '    <script src="VXText.js"> ' + sLineBreak +    //
    '    </script> ' + sLineBreak +   //
    '    <script src="VXInput.js"> ' + sLineBreak +   //
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
  aRequest.Resp.ResponseText:= Html;
end;

end.

