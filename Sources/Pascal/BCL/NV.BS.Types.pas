unit NV.BS.Types;

interface

type
  TBsSize = (bssNoSet, bssXs, bssSM, bssMD, bssLG, bssXL);

  { TODO -oDelcio -cBackgrounds : implement bootstrap gradients $enable-gradients }
  TBsBackground = (bsbgNone, bsbgInfo, bsbgPrimary, bsbgSecondary, bsbgSuccess, bsbgDanger,
    bsbgWarning, bsbgDark, bsbgLight, bsbgWhite, bsbgTransparent);

  TBsShadow = (bssNone, bssSmall, bssNormal, bssLarger);

  TBsRoundType = (bsrNone, bsrTop, bsrRight, bsrBottom, bsrLeft, bsrCircle, bsrPill, bsrRemove);

  TBsRoundSize = (bsrsSmall, bsrsNormal, bsrsLarger);

  TBsTextAlign = (bstaNone, bstaJustify, bstaLeft, bstaCenter, bstaRight);

  TBsTextWrap = (bstwNone, bstwWrap, bstwNoWrap, bstwTruncate);

  TBsTextTransform = (bsttNone, bsttLowerCase, bsttUpperCase, bsttCapitalize);

  TBsTextWeight = (bsttwBolder, bsttwBold, bsttwNormal, bsttwLight, bsttwLigher);

  TBSDisplay = (bsdNull, bsdNone, bsdInline, bsdInlineBlock, bsdBlock, bsdTable, bsdTableCell,
    bsdTableRow, bsdFlex, bsdInlineFlex);

  TBSDirection = (bsDirNull, bsDirRow, bsDirRowReverse, bsDirColumn, bsDirColumnReverse);

  TBSJustifyContent = (bsJustNull, bsJustStart, bsJustEnd, bsJustCenter, bsJustBetween,
    bsJustAround);

  TBSAlign = (bsaNull, bsaStart, bsaEnd, bsaCenter, bsaBaseline, bsaStretch);

  TBSAutoMargin = (bsamNull, bsamRight, bsamLeft);

  TBSWrap = (bswNull, bswNoWrap, bswWrap, bswWrapReverse);

  TBs12Range = -1 .. 12;

  TBsAlignContent = (bsacNull, bsacStart, bsacEnd, bsacCenter, bsacAround, bsacSretch);

  TBsFloat = (bsfNull, bsfNone, bsfRight, bsfLeft);

  TBsSpacing = (bssNull, bssAuto, bss0, bss1, bss2, bss3, bss4, bss5);

  TBsNavbarColor = (bsnavcLight, bsnavcDark);

  TNvBsTableStyle  = (sttBordered, sttBorderless, sttHover, sttStriped, sttDark, sttSm);
  TNvBsTableStyles = set of TNvBsTableStyle;

  TNvBsTHeaderStyle = (sthUndefined, sthLight, sthDark);

const
  TBsSizeStr: array [low(TBsSize) .. High(TBsSize)] of string = ('', 'xs', 'sm', 'md', 'lg', 'xl');

  TBsBackgroundStr: array [Low(TBsBackground) .. High(TBsBackground)] of string = //
    ('', 'info', 'primary', 'secondary', 'success', 'danger', 'warning', 'dark', 'light', 'white',
    'transparent');

  TBsShadowStr: array [Low(TBsShadow) .. High(TBsShadow)] of string = //
    ('', 'sm', 'md', 'lg');

  TBsRoundTypeStr: array [Low(TBsRoundType) .. High(TBsRoundType)] of string = //
    ('', 'top', 'right', 'bottom', 'left', 'circle', 'pill', '0');

  TBsRoundSizeStr: array [Low(TBsRoundSize) .. High(TBsRoundSize)] of string = //
    ('sm', '', 'lg');

  TBsTextAlignStr: array [Low(TBsTextAlign) .. High(TBsTextAlign)] of string = //
    ('', 'justify', 'left', 'center', 'right');

  TBsTextWrapStr: array [Low(TBsTextWrap) .. High(TBsTextWrap)] of string = //
    ('', 'wrap', 'no-wrap', 'truncate');

  TBsTextTransformStr: array [Low(TBsTextTransform) .. High(TBsTextTransform)] of string = //
    ('', 'lowercase', 'uppercase', 'capitalize');

  TBsTextWeightStr: array [Low(TBsTextWeight) .. High(TBsTextWeight)] of string = //
    ('bolder', 'bold', 'normal', 'light', 'ligher');

  TBsNavbarColorStr: array [Low(TBsNavbarColor) .. High(TBsNavbarColor)] of string = //
    ('light', 'dark');

  TBSDirectionStr: array [Low(TBSDirection) .. High(TBSDirection)] of string = //
    ('', 'row', 'row-reverse', 'column', 'column-reverse');

  TBSDisplayStr: array [Low(TBSDisplay) .. High(TBSDisplay)] of string = //
    ('', 'none', 'inline', 'inline-block', 'block', 'table', 'table-cell', 'table-row', 'flex',
    'inline-flex');

  TBSJustifyContentStr: array [Low(TBSJustifyContent) .. High(TBSJustifyContent)] of string = //
    ('', 'start', 'end', 'center', 'between', 'around');

  TBSAlignStr: array [Low(TBSAlign) .. High(TBSAlign)] of string = //
    ('', 'start', 'end', 'center', 'baseline', 'stretch');

  TBSAutoMarginStr: array [Low(TBSAutoMargin) .. High(TBSAutoMargin)] of string = //
    ('', 'mr-auto', 'ml-auto');

  TBSWrapStr: array [Low(TBSWrap) .. High(TBSWrap)] of string = //
    ('', 'nowrap', 'wrap', 'reverse');

  TBsAlignContentStr: array [Low(TBsAlignContent) .. High(TBsAlignContent)] of string = //
    ('', 'start', 'end', 'center', 'around', 'stretch');

  TBsFloatStr: array [Low(TBsFloat) .. High(TBsFloat)] of string = //
    ('', 'none', 'right', 'left');

  TNvBsTableStyleStr: array [Low(TNvBsTableStyle) .. High(TNvBsTableStyle)] of string = //
    ('bordered', 'borderless', 'hover', 'striped', 'dark', 'sm');

  TNvBsTHeaderStyleStr :array [Low(TNvBsTHeaderStyle) .. High(TNvBsTHeaderStyle)] of string = //
    ('', 'thead-light', 'thead-dark');


implementation

end.
