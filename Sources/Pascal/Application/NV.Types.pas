unit NV.Types;

interface

uses
  Classes, IdCustomHTTPServer, NV.Request;

type
  TNVCloseAction = (caHide, caFree, caNone);

  TNvRequestMethod = function(aRequest: TNVRequestTask): Boolean of object;

  TNVCloseEvent = procedure(Sender: TObject; var aCloseAction: TNVCloseAction) of object;

  TNvRenderOrder = (roBefore, roAfter);

const
  TNvRenderOrderStr: array [Low(TNvRenderOrder) .. High(TNvRenderOrder)] of string = //
    ('before', 'after');

implementation

end.
