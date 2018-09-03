unit NV.Common.Types;

interface

uses
  Classes, IdCustomHTTPServer, NV.Request;

type
  TDWRect = record
    Top: Integer;
    Botton: Integer;
    Left: Integer;
    Rigth: Integer;
  end;

  TNVSize = (bsszDefault, bsszLg, bsszMd, bsszSm, bsszXs);


{$REGION 'REGION TYPES'}
  TDWAlertStyle = (bsasSuccess, bsasInfo, bsasWarning, bsasDanger);

  TDWAlertPosition = (bsapDefault, bsapRightTop, bsapRightCenter, bsapRightBottom);

  TNVRegionBack = (bsrbDefault, bsrbPrimary, bsrbSuccess, bsrbInfo, bsrbWarning, bsrbDanger);

const
  aNVRegionBack: array[bsrbDefault..bsrbDanger] of string = ('default', 'primary', 'success', 'info', 'warning', 'danger');

type
  TNVRegionType = (bsrtNone, bsrtContainer, bsrtContainerFluid, bsrtRow, bsrtColumn, bsrtDropDown, bsrtDropDownMenu, bsrtFormGroup, bsrtJumbotron, bsrtPageHeader, bsrtWell, bsrtWellLarge, bsrtWellSmall, bsrtButtonToolbar, bsrtListGroup, bsrtModalContent, bsrtModalHeader, bsrtModalBody, bsrtModalFooter, bsrtPanelGroup, bsrtPanel, bsrtPanelBody, bsrtPanelHeading, bsrtPanelFooter, bsrtCard, bsrtCardBlock, bsrtCardHeader, bsrtCardFooter, bsrtCardGroup);

const
  aNVRegionType: array[bsrtNone..bsrtCardGroup] of string = ('', 'container', 'container-fluid', 'row', 'column', 'dropdown', 'dropdown-menu', 'form-group', 'jumbotron', 'page-header', 'well', 'well well-lg', 'well well-sm', 'btn-toolbar', 'list-group', 'modal-content', 'modal-header', 'modal-body', 'modal-footer', 'panel-group', 'panel', 'panel-body', 'panel-heading', 'panel-footer', 'card', 'card-block', 'card-header', 'card-footer', 'card-group');

type
  TNVRegionTagType = (bsttDiv, bsttH1, bsttH2, bsttH3, bsttH4, bsttH5, bsttH6, bsttP);

const
  aNVRegionTagType: array[bsttDiv..bsttP] of string = ('div', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'p');

{$ENDREGION}
{$REGION 'PROCEDURE TYPES'}

type
  TNvRequestMethod = procedure(aRequest: TNVRequestTask) of object;

  // Procedure for Control Async Events
  TDWAsyncProcedure = procedure(Sender: TObject; aParams: TStringList) of object;
  // Async Events Anonnymous procedures

  TDWAsyncEventProc = reference to procedure(Sender: TObject; EventParams: TStringList);

  //TDWOnHtmlTagProcedure = procedure(aTag: TDWCustomElement) of object;
  TDWRequestProc = procedure(aParams: TStrings; aResponse: TIdHTTPResponseInfo) of object;

{$ENDREGION}

implementation

end.

