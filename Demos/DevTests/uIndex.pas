unit uIndex;

interface

uses
  NV.VCL.Forms, NV.BS.Navbar, System.Classes, VCL.Controls, NV.Controls,
  NV.BS.Controls, NV.BS.Containers, NV.BS.Buttons, NV.JSON, NV.VCL.Charts,
  NV.BS.Alerts, NV.VCL.Images, NV.BS.HtmlControls, NV.BS.Cards, NV.BS.Inputs,
  NV.BS.Tables;

type
  TNVPage1 = class(TNVForm)
    NvBsRow2: TNvBsRow;
    SideBar: TNvBsColumn;
    Content: TNvBsColumn;
    SideBarHeader: TNvBsFormRow;
    NvBsColumn2: TNvBsColumn;
    NvBsNav2: TNvBsNav;
    BsNavItemLink1: TBsNavItemLink;
    NvSvgImageList1: TNvSvgImageList;
    NvBsNavBar2: TNvBsNavBar;
    BsNavBarBrand1: TBsNavBarBrand;
    BsNavBarContent1: TBsNavBarContent;
    NvBsButtonToolbar2: TNvBsButtonToolbar;
    NvBsButton2: TNvBsButton;
    NvBsButton3: TNvBsButton;
    NvBsButton4: TNvBsButton;
    NvBsButton5: TNvBsButton;
    NvBsRow3: TNvBsRow;
    NvBsColumn3: TNvBsColumn;
    NvBsColumn4: TNvBsColumn;
    NvBsColumn5: TNvBsColumn;
    NvBsColumn6: TNvBsColumn;
    NvBsRow4: TNvBsRow;
    NvBsColumn7: TNvBsColumn;
    NvBsColumn8: TNvBsColumn;
    NvBsColumn9: TNvBsColumn;
    NvBSText9: TNvBSText;
    BsNavItemLink2: TBsNavItemLink;
    NvBsInput2: TNvBsInput;
    NvBsColumn10: TNvBsColumn;
    BsNavItemLink3: TBsNavItemLink;
    NvBsRow5: TNvBsRow;
    NvBsColumn13: TNvBsColumn;
    NvBsColumn14: TNvBsColumn;
    NvBsCardStats5: TNvBsCardStats;
    NvBsCardHeader: TNvBsCardHeader;
    NvBSText2: TNvBSText;
    NvBsCardFooter1: TNvBsCardFooter;
    NvBSText3: TNvBSText;
    NvBsCardStats6: TNvBsCardStats;
    NvBsCardHeader3: TNvBsCardHeader;
    NvBSText4: TNvBSText;
    NvBsCardFooter3: TNvBsCardFooter;
    NvBSText5: TNvBSText;
    NvBsCardStats7: TNvBsCardStats;
    NvBsHeaderBeaut: TNvBsCardHeader;
    NvBSText7: TNvBSText;
    NvBsCardFooter4: TNvBsCardFooter;
    NvBSText16: TNvBSText;
    NvBsCardStats8: TNvBsCardStats;
    NvBsCardHeader5: TNvBsCardHeader;
    NvBSText17: TNvBSText;
    NvBsCardFooter5: TNvBsCardFooter;
    NvBSText18: TNvBSText;
    NvBsCardChart2: TNvBsCardChart;
    NvBsCardHeader1: TNvBsCardHeader;
    NvChart1: TNvChart;
    NvBsCardBody1: TNvBsCardBody;
    NvBsCardTitle1: TNvBsCardTitle;
    NvBsCardSubtitle1: TNvBsCardSubtitle;
    NvBsCardFooter2: TNvBsCardFooter;
    NvBSText1: TNvBSText;
    NvBsCardChart3: TNvBsCardChart;
    NvBsCardHeader2: TNvBsCardHeader;
    NvChart2: TNvChart;
    NvBsCardBody2: TNvBsCardBody;
    NvBsCardTitle2: TNvBsCardTitle;
    NvBsCardSubtitle2: TNvBsCardSubtitle;
    NvBsCardFooter6: TNvBsCardFooter;
    NvBSText6: TNvBSText;
    NvBsCardChart4: TNvBsCardChart;
    NvBsCardHeader4: TNvBsCardHeader;
    NvChart3: TNvChart;
    NvBsCardBody3: TNvBsCardBody;
    NvBsCardTitle3: TNvBsCardTitle;
    NvBsCardSubtitle3: TNvBsCardSubtitle;
    NvBsCardFooter7: TNvBsCardFooter;
    NvBSText8: TNvBSText;
    NvBsCardTable2: TNvBsCardTable;
    NvBsCardHeader6: TNvBsCardHeader;
    NvBSText10: TNvBSText;
    NvBsCardBody4: TNvBsCardBody;
    NvBsTable1: TNvBsTable;
    NvBsCardTable3: TNvBsCardTable;
    NvBsCardHeader7: TNvBsCardHeader;
    NvBSText11: TNvBSText;
    NvBsCardBody5: TNvBsCardBody;
    NvBsTable2: TNvBsTable;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  NVPage1: TNVPage1;

implementation

uses
  NV.utils;

{$R *.dfm}


end.
