unit NV.Languages;

interface

uses
  Classes;

type
  TNVLangRes = class
  public
    Name: string;

    // *** COMMON **************************************************************
    RsOk: string;     // 'Ok';
    RsCancel: string;  // 'Cancel';
    RsMessage: string; // 'Message';
    RsClose: string;   // 'Close';

    // *** DWApplication *******************************************************
    RsMainFormClassNotSet: string;     // 'DwApplication.MainFormClass not set.';
    RsUserSessionClassNotSet: string;  // 'UserSessionClass need to be set.';
    RsDWApplicationNotStarted: string; // 'DWApplication Not Started!';

    // *** MiniFyCss & MinifyJs ************************************************
    SErrUnterminatedComment: string;       // 'Unterminated comment.';
    SErrUnterminatedStringLiteral: string; // 'Unterminated string literal.';
    SErrUnterminatedSection: string;       // 'Unterminated secton header.';
    SErrUnterminatedSetInRegexp: string;   // 'Unterminated set in Regular Expression literal.';
    SerrUnterminatedRegexp: string;        // 'Unterminated Regular Expression literal.';

    // *** DWHttpServer
    RsLogFileCreatedOn: string; // 'This Logfile was created on ';

    // *** DW.CORE.Server ******************************************************
    RsYourSessionEnded: string; // 'Your Session Ended!';

    // *** WebSocket ***********************************************************
    RsUnsupportedDataCode: string; // 'Unsupported data code: %d';

    // *** jSonData ************************************************************
    RsUnsupportedFileEncoding: string; // 'File encoding is not supported';
    RsUnexpectedEndOfFile: string;     // 'Unexpected end of file where %s was expected';
    RsUnexpectedToken: string;         // 'Expected %s but found %s';
    RsInvalidHexNumber: string;        // 'Invalid hex number "%s"';
    RsTypeCastError: string;           // 'Cannot cast %s into %s';
    RsMissingClassInfo: string;
    // 'Class "%s" doesn''t have type information. {$M+} was not specified';
    RsInvalidJsonPath: string;           // 'Invalid JSON path "%s"';
    RsJsonPathContainsNullValue: string; // 'JSON path contains null value ("%s")';
    RsJsonPathIndexError: string;        // 'JSON path index out of bounds (%d) "%s"';
    RsVarTypeNotSupported: string;       // 'VarType %d is not supported';

    // *** Accordion ***********************************************************
    RsAccordionItemsPaddingError: string; // 'ItemsPadding must be of the format: 1px or 1%';
    RsAccordionNotSetOnItem: string;      // 'Accordion Property for Item %s is not set.';

    // *** Buttons *************************************************************
    RsButtonAsyncClickError: string; // 'Only execute click in Async Calls';

    // *** DateRangePicker *****************************************************
    { TODO 1 -oDELCIO -cIMPLEMENT : Translate DateRangePicker }

    // *** InputForm ***********************************************************
    RsInputFormsNestedError: string; // 'Forms can not be nested, you try to put %s inside %s.';

    // *** InputMoney **********************************************************
    { TODO 1 -oDELCIO -cIMPLEMENT : Translate InputMoney - symbols and formats }

    // *** JqGrid **************************************************************
    { TODO 1 -oDELCIO -cIMPLEMENT : Translate JQGrid }

    // *** MemoHTML ************************************************************
    { TODO 1 -oDELCIO -cIMPLEMENT : Translate MemoHTML }

    // *** Select2 ***************************************************************:string; //
    RsSelect2Placeholder: string; // '"Select one option"'; // '"Selecione Uma Opção"';
    { TODO 1 -oDELCIO -cIMPLEMENT : Translate Select2 - add js localization scripts }

    // *** Switch **************************************************************
    RsSwitchOn: string;  // 'ON';
    RsSwitchOff: string; // 'OFF';
    { TODO 1 -oDELCIO -cIMPLEMENT : Translate Switch - verify localized js }

    // *** DW.VCL.TreeView.pas *************************************************
    RsDeleteNode: string;       // 'Delete %s ?';
    RsDeleteNode2: string;      // 'Delete %s (with all children) ?';
    RsMasterFieldError: string; // '"MasterField" must be integer type';
    RsDetailFieldError: string; // '"DetailField" must be integer type';
    RsItemFieldError: string;   // '"ItemField" must be string, date or integer type';
    RsIconFieldError: string;   // '"IconField" must be integer type';
    RsMasterFieldEmpty: string; // '"MasterField" property must be filled';
    RsDetailFieldEmpty: string; // '"DetailField" property must be filled';
    RsItemFieldEmpty: string;   // '"ItemField" property must be filled';

    RsEMoveToModeError: string;          // 'Invalid move mode for JvDBTreeNode';
    RsMasterDetailFieldError: string;    // '"MasterField" and "DetailField" must be of same type';
    RsEDataSetNotActive: string;         // 'DataSet not active';
    RsEErrorValueForDetailValue: string; // 'error value for DetailValue';

    //*** DWUpload *************************************************************
    RsDWUploadLangFile: string;
    RsDWUploadlang: string;
  end;

//external of class to avoid alocate memory in all DwApplications;
procedure SetLangEnUS;

procedure SetLangPtBr;

procedure FreeLang;

function NVLang: TNVLangRes;

implementation

uses
  SysUtils;

threadvar
  _Lang: TNVLangRes;

(*
  // *** COMMON ******************************************************************=


*)

{ TDWLang }

function NVLang: TNVLangRes;
begin
  if not Assigned(_Lang) then
  begin
     // ShowMessage(InttoStr(TThread.Current.ThreadID));
    _lang := TNVLangRes.Create;
  end;
  Result := _Lang;
end;

procedure FreeLang;
begin
  if Assigned(_Lang) then
    _Lang.Free;
end;



{ TDWLangRes }

procedure SetLangEnUS;
begin
  with NVLang do
  begin
    Name := 'EN-US';
    RsOk := 'Ok';
    RsCancel := 'Cancel';
    RsMessage := 'Message';
    RsClose := 'Close';

      // *** DWApplication ************************************************************
    RsMainFormClassNotSet := 'DwApplication.MainFormClass not set.';
    RsUserSessionClassNotSet := 'UserSessionClass need to be set.';
    RsDWApplicationNotStarted := 'DWApplication Not Started!';

      // *** MiniFyCss & MinifyJs ***************************************************************:=
    SErrUnterminatedComment := 'Unterminated comment.';
    SErrUnterminatedStringLiteral := 'Unterminated string literal.';
    SErrUnterminatedSection := 'Unterminated secton header.';
    SErrUnterminatedSetInRegexp := 'Unterminated set in Regular Expression literal.';
    SerrUnterminatedRegexp := 'Unterminated Regular Expression literal.';

      // *** DWHttpServer
    RsLogFileCreatedOn := 'This Logfile was created on ';

      // *** DW.CORE.Server *********************************************************
    RsYourSessionEnded := 'Your Session Ended!';

      // *** WebSocket ************************************************************:=:=
    RsUnsupportedDataCode := 'Unsupported data code: %d';

      // *** jSonData ***************************************************************
    RsUnsupportedFileEncoding := 'File encoding is not supported';
    RsUnexpectedEndOfFile := 'Unexpected end of file where %s was expected';
    RsUnexpectedToken := 'Expected %s but found %s';
    RsInvalidHexNumber := 'Invalid hex number "%s"';
    RsTypeCastError := 'Cannot cast %s into %s';
    RsMissingClassInfo := 'Class "%s" doesn''t have type information. {$M+} was not specified';
    RsInvalidJsonPath := 'Invalid JSON path "%s"';
    RsJsonPathContainsNullValue := 'JSON path contains null value ("%s")';
    RsJsonPathIndexError := 'JSON path index out of bounds (%d) "%s"';
    RsVarTypeNotSupported := 'VarType %d is not supported';

      // *** Accordion ************************************************************:=:=
    RsAccordionItemsPaddingError := 'ItemsPadding must be of the format: 1px or 1%';
    RsAccordionNotSetOnItem := 'Accordion Property for Item %s is not set.';

      // *** Buttons ***************************************************************:=
    RsButtonAsyncClickError := 'Only execute click in Async Calls';

      // *** DateRangePicker ******************************************************:=:=
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate DateRangePicker }

      // *** InputForm ************************************************************:=:=
    RsInputFormsNestedError := 'Forms can not be nested, you try to put %s inside %s.';

      // *** InputMoney ************************************************************:=
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate InputMoney - symbols and formats }

      // *** JqGrid ***************************************************************:=:=
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate JQGrid }

      // *** MemoHTML ***************************************************************
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate MemoHTML }

      // *** Select2 ***************************************************************:=
    RsSelect2Placeholder := '"Select one option"'; // '"Selecione Uma Opção"';
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate Select2 - add js localization scripts }

      // *** Switch ***************************************************************:=:=
    RsSwitchOn := 'ON';
    RsSwitchOff := 'OFF';
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate Switch - verify localized js }

      // *** DW.VCL.TreeView.pas ******************************************************:=
    RsDeleteNode := 'Delete %s ?';
    RsDeleteNode2 := 'Delete %s (with all children) ?';
    RsMasterFieldError := '"MasterField" must be integer type';
    RsDetailFieldError := '"DetailField" must be integer type';
    RsItemFieldError := '"ItemField" must be string, date or integer type';
    RsIconFieldError := '"IconField" must be integer type';
    RsMasterFieldEmpty := '"MasterField" property must be filled';
    RsDetailFieldEmpty := '"DetailField" property must be filled';
    RsItemFieldEmpty := '"ItemField" property must be filled';

    RsEMoveToModeError := 'Invalid move mode for JvDBTreeNode';
    RsMasterDetailFieldError := '"MasterField" and "DetailField" must be of same type';
    RsEDataSetNotActive := 'DataSet not active';
    RsEErrorValueForDetailValue := 'error value for DetailValue';

      //*** DWUpload *************************************************************
    RsDWUploadLangFile := '';
    RsDWUploadlang := '';
  end;
end;

procedure SetLangPtBr;
begin
  with NVLang do
  begin
    Name := 'PT-BR';
    RsOk := 'Ok';
    RsCancel := 'Cancel';
    RsMessage := 'Message';
    RsClose := 'Close';

      // *** DWApplication ************************************************************
    RsMainFormClassNotSet := 'DwApplication.MainFormClass not set.';
    RsUserSessionClassNotSet := 'UserSessionClass need to be set.';
    RsDWApplicationNotStarted := 'DWApplication Not Started!';

      // *** MiniFyCss & MinifyJs ***************************************************************:=
    SErrUnterminatedComment := 'Unterminated comment.';
    SErrUnterminatedStringLiteral := 'Unterminated string literal.';
    SErrUnterminatedSection := 'Unterminated secton header.';
    SErrUnterminatedSetInRegexp := 'Unterminated set in Regular Expression literal.';
    SerrUnterminatedRegexp := 'Unterminated Regular Expression literal.';

      // *** DWHttpServer
    RsLogFileCreatedOn := 'This Logfile was created on ';

      // *** DW.CORE.Server *********************************************************
    RsYourSessionEnded := 'Your Session Ended!';

      // *** WebSocket ************************************************************:=:=
    RsUnsupportedDataCode := 'Unsupported data code: %d';

      // *** jSonData ***************************************************************
    RsUnsupportedFileEncoding := 'File encoding is not supported';
    RsUnexpectedEndOfFile := 'Unexpected end of file where %s was expected';
    RsUnexpectedToken := 'Expected %s but found %s';
    RsInvalidHexNumber := 'Invalid hex number "%s"';
    RsTypeCastError := 'Cannot cast %s into %s';
    RsMissingClassInfo := 'Class "%s" doesn''t have type information. {$M+} was not specified';
    RsInvalidJsonPath := 'Invalid JSON path "%s"';
    RsJsonPathContainsNullValue := 'JSON path contains null value ("%s")';
    RsJsonPathIndexError := 'JSON path index out of bounds (%d) "%s"';
    RsVarTypeNotSupported := 'VarType %d is not supported';

      // *** Accordion ************************************************************:=:=
    RsAccordionItemsPaddingError := 'ItemsPadding must be of the format: 1px or 1%';
    RsAccordionNotSetOnItem := 'Accordion Property for Item %s is not set.';

      // *** Buttons ***************************************************************:=
    RsButtonAsyncClickError := 'Only execute click in Async Calls';

      // *** DateRangePicker ******************************************************:=:=
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate DateRangePicker }

      // *** InputForm ************************************************************:=:=
    RsInputFormsNestedError := 'Forms can not be nested, you try to put %s inside %s.';

      // *** InputMoney ************************************************************:=
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate InputMoney - symbols and formats }

      // *** JqGrid ***************************************************************:=:=
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate JQGrid }

      // *** MemoHTML ***************************************************************
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate MemoHTML }

      // *** Select2 ***************************************************************:=
    RsSelect2Placeholder := '"Select one option"'; // '"Selecione Uma Opção"';
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate Select2 - add js localization scripts }

      // *** Switch ***************************************************************:=:=
    RsSwitchOn := 'ON';
    RsSwitchOff := 'OFF';
      { TODO 1 -oDELCIO -cIMPLEMENT : Translate Switch - verify localized js }

      // *** DW.VCL.TreeView.pas ******************************************************:=
    RsDeleteNode := 'Delete %s ?';
    RsDeleteNode2 := 'Delete %s (with all children) ?';
    RsMasterFieldError := '"MasterField" must be integer type';
    RsDetailFieldError := '"DetailField" must be integer type';
    RsItemFieldError := '"ItemField" must be string, date or integer type';
    RsIconFieldError := '"IconField" must be integer type';
    RsMasterFieldEmpty := '"MasterField" property must be filled';
    RsDetailFieldEmpty := '"DetailField" property must be filled';
    RsItemFieldEmpty := '"ItemField" property must be filled';

    RsEMoveToModeError := 'Invalid move mode for JvDBTreeNode';
    RsMasterDetailFieldError := '"MasterField" and "DetailField" must be of same type';
    RsEDataSetNotActive := 'DataSet not active';
    RsEErrorValueForDetailValue := 'error value for DetailValue';

    //*** DWUpload *************************************************************
    RsDWUploadLangFile := '/<dwlibpath>/fileinput/js/locales/pt-BR.js';
    RsDWUploadlang := 'pt-BR';
  end;
end;

end.

