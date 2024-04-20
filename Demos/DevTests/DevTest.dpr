// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program DevTest;

uses
  NV.VCl.Forms,
  NV.VCl.Charts,
  uIndex in 'uIndex.pas' {NVPage1: TNVPage},
  Comp.Alerts in 'Comp.Alerts.pas' {FrameAlerts: TNVPage},
  Unit3 in 'Unit3.pas' {NVPage3: TNVPage},
  Unit4 in 'Unit4.pas' {NVPage4: TNVPage},
  demo in 'demo.pas' {NVPage7: TNVPage},
  Documentation in 'Documentation.pas' {FrameDocumentation: TNVFrame},
  Introduction in 'Introduction.pas' {FrameIntro: TNVFrame},
  CompToasts in 'CompToasts.pas' {NVFrame1: TNVFrame},
  CompImageList in 'CompImageList.pas' {NVFrame7: TNVFrame},
  CompAccordions in 'CompAccordions.pas' {NVFrame9: TNVFrame},
  CompButtonGroup in 'CompButtonGroup.pas' {NVFrame10: TNVFrame},
  Comp.Cards in 'Comp.Cards.pas' {FrameCards: TNVFrame},
  Comp.Inputs in 'Comp.Inputs.pas' {FrameCompInputs: TNVFrame},
  Comp.Report in 'Comp.Report.pas';

{$R *.res}

var
  Chart: TNvChart;

begin
  Application.Initialize;
  //Application.CssFile := Application.RootPath + 'material-dashboard.css';


  Application.CreateForm(TNVPage1, NVPage1);
//  Application.CreateForm(TNVFrame7, NVFrame7);
//    Chart:= TNvChart.Create(NVPage1);
//  Chart.Parent:= NVPage1.NvBsColumn2;
//
//  Chart.Labels.Add('label1');
//  Chart.Labels.Add('label2');
//  Chart.Labels.Add('label3');
//
//  with Chart.Series.Add as TNvChartSerieItem do
//  begin
//    SerieType:= dwstLine;
//  end;

  //

  // Application.MainFormOnTaskbar := True;
  Application.Run;

end.
