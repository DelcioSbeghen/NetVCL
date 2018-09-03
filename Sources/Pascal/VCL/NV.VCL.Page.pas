unit NV.VCL.Page;

interface

uses
  NV.VCL.Container;

type
  TNVPageClass = class of TNVBasepage;

  //base for all pages
  TNVBasePage = class(TNVModuleContainer)
  private
    FRouteName: string;
    procedure SetRouteName(const Value: string);
  public
    property RouteName: string read FRouteName write SetRouteName;
  end;

  TNVPage = class(TNVBasepage)
  end;

implementation

{ TNVBasePage }

procedure TNVBasePage.SetRouteName(const Value: string);
begin
  FRouteName := Value;
end;

end.

