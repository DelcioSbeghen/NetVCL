unit NV.VCL.Controls;

interface

uses
  Controls, NV.Common.Interfaces, NV.Router, NV.JSON;

type
  //base for all controls
  TNVControl = class(TGraphicControl, INVBase {, INVControl, INVContainer})
  private
    FRouter: TNVRouter;
    function GetID: string;
  protected
    procedure DrawWeb(var Json: TJsonObject); virtual;
  public
    property ID: string read GetID;
    property Router: TNVRouter read FRouter;
  end;

  //base for all control with user input
  TNVInputControl = class(TNVControl)
  end;

implementation


{ TNVControl }

procedure TNVControl.DrawWeb(var Json: TJsonObject);
begin

end;

function TNVControl.GetID: string;
begin

end;

end.

