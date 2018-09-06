unit NV.VCL.Container;

interface

uses
  Controls, NV.Interfaces, NV.Router, NV.JSON;

type
  //base for all containers(panels, regions, Forms, Pages)
  TNVContainer = class(TCustomControl, INVBase{, INVControl, INVContainer})
  private
    FRouter: TNVRouter;
    function GetID: string;
  protected
    procedure DrawWeb(var Json: TJsonObject); virtual;
  public
    property ID: string read GetID;
    property Router: TNVRouter read FRouter;
  end;


    // base for all Modules Containers(Forms,Pages, dialogs)
  TNVModuleContainer = class(TNVContainer)
  end;

implementation

{ TNVContainer }

procedure TNVContainer.DrawWeb(var Json: TJsonObject);
begin

end;

function TNVContainer.GetID: string;
begin

end;

end.

