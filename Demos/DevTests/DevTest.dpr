program DevTest;

uses
  NV.VCl.Forms,
  NV.VCl.Charts,
  uIndex in 'uIndex.pas' {NVPage1: TNVPage} ,
  Unit2 in 'Unit2.pas' {NVPage2: TNVPage} ,
  Unit3 in 'Unit3.pas' {NVPage3: TNVPage} ,
  Unit4 in 'Unit4.pas' {NVPage4: TNVPage} ,
  demo in 'demo.pas' {NVPage7: TNVPage} ,
  Documentation in 'Documentation.pas' {FrameDocumentation: TNVFrame} ,
  Introduction in 'Introduction.pas' {FrameIntro: TNVFrame} ,
  CompToasts in 'CompToasts.pas' {NVFrame1: TnvFrame} ,
  CompImageList in 'CompImageList.pas' {NVFrame7: TnvFrame} ,
  CompAccordions in 'CompAccordions.pas' {NVFrame9: TnvFrame} ,
  CompButtonGroup in 'CompButtonGroup.pas' {NVFrame10: TnvFrame} ,
  Comp.Cards in 'Comp.Cards.pas' {FrameCards: TnvFrame} ,
  Comp.Inputs in 'Comp.Inputs.pas' {FrameCompInputs: TnvFrame};

{$R *.res}

var
  Chart: TNvChart;

begin
  Application.Initialize;
  //Application.CssFile := Application.RootPath + 'material-dashboard.css';
  Application.CreateForm(TNVPage1, NVPage1);

//  Chart:= TNvChart.Create(NVPage1);
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
