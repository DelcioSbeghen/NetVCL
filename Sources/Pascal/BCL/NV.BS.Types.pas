unit NV.BS.Types;

interface

type
  TBsSize = (bssNoSet, bssXs, bssSM, bssMD, bssLG, bssXL, bssXXL);

  { TODO -oDelcio -cBackgrounds : implement bootstrap gradients $enable-gradients }
  TBsBackground = (bsbgNone, bsbgInfo, bsbgPrimary, bsbgSecondary, bsbgSuccess, bsbgDanger,
    bsbgWarning, bsbgDark, bsbgLight, bsbgWhite, bsbgTransparent, bsbgBlack, bsbgBody,
    bsbgBodySecondary, bsbgBodyTertiary, bsbgPrimarySubtle, bsbgSecondarySubtle, bsbgSucessSubtle,
    bsbgInfoSubtle, bsbgWarningSubtle, bsbgDangerSubtle, bsbgLightSubtle, bsbgDarkSubtle);

  TBsShadow = (bssNone, bssSmall, bssNormal, bssLarger);

  TBsRoundType = (bsrNone, bsrTop, bsrStart, bsrBottom, bsrEnd, bsrCircle, bsrPill, bsrRemove);

  TBsRoundSize = (bsrsSmall, bsrsNormal, bsrsLarger);

  TBsTextAlign = (bstaNone, bstaJustify, bstaStart, bstaCenter, bstaEnd);

  TBsTextWrap = (bstwNone, bstwWrap, bstwNoWrap, bstwTruncate);

  TBsTextTransform = (bsttNone, bsttLowerCase, bsttUpperCase, bsttCapitalize);

  TBsTextWeight = (bsttwBolder, bsttwBold, bsttwNormal, bsttwLight, bsttwLigher);

  TBSDisplay = (bsdNull, bsdNone, bsdInline, bsdInlineBlock, bsdBlock, bsdTable, bsdTableCell,
    bsdTableRow, bsdFlex, bsdInlineFlex, bsdGrid, bsdInlineGrid);

  // https://getbootstrap.com/docs/5.3/helpers/position/
  TBsViewportPos = (bsvppNull, bsvppFixedTop, bsvppFixedBottom, bsvppStickyTop, bsvppStickyBottom);

  // https://getbootstrap.com/docs/5.3/utilities/position/
  TBsDispPosition = (bsdpNull, bsdpStatic, bsdpRelative, bsdpAbsolute, bsdpFixed, bsdpSticky);

  TBSDirection = (bsDirNull, bsDirRow, bsDirRowReverse, bsDirColumn, bsDirColumnReverse);

  TBSJustifyContent = (bsJustNull, bsJustStart, bsJustEnd, bsJustCenter, bsJustBetween,
    bsJustAround);

  TBSAlign = (bsaNull, bsaStart, bsaEnd, bsaCenter, bsaBaseline, bsaStretch);

  TBSWrap = (bswNull, bswNoWrap, bswWrap, bswWrapReverse);

  TBs12Range = -1 .. 12;

  TBsAlignContent = (bsacNull, bsacStart, bsacEnd, bsacCenter, bsacAround, bsacSretch);

  TBsFloat = (bsfNull, bsfNone, bsfEnd, bsfStart);
  // NetVClCustom css float pos
  TNvFloatPos = (fposNull, fposTopLeft, fposTopCenter, fposTopRight, fposCenterLeft, fposCenter,
    fposCenterRight, fposBottomLeft, fposBottomCenter, fposBottomRight);

  TBsSpacing = (bssNull, bssAuto, bss0, bss1, bss2, bss3, bss4, bss5);

  TBsWidth = (bswdNull, bswdAuto, bswd25, bswd50, bswd75, bswd100);

  TBsPosValue = (bspvNull, bspv0, bspv50, bspv100);

  TNvBsTableStyle  = (sttBordered, sttBorderless, sttHover, sttStriped, sttDark, sttSm);
  TNvBsTableStyles = set of TNvBsTableStyle;

  TNvBsTHeaderStyle = (sthUndefined, sthLight, sthDark);

  TNvBsDataToggle = (bsdtNull, bsdtdropdown, bsdtButton, bsdtCollapse, bsdtModal);

  TNvBsLinkColor = (bslcNone, bslcPrimary, bslcSecondary, bslcSuccess, bslcDanger, bslcWarning,
    bslcInfo, bslcLight, bslcDark, bslcBodyEmphasis);

const
  TBsSizeStr: array [low(TBsSize) .. High(TBsSize)] of string = ('', 'xs', 'sm', 'md', 'lg',
    'xl', 'xxl');

  TBsBackgroundStr: array [Low(TBsBackground) .. High(TBsBackground)] of string = //
    ('', 'info', 'primary', 'secondary', 'success', 'danger', 'warning', 'dark', 'light', 'white',
    'transparent', 'black', 'body', 'body-secondary', 'body-tertiary', 'primary-subtle',
    'secondary-subtle', 'success-subtle', 'info-subtle', 'warning-subtle', 'danger-subtle',
    'light-subtle', 'dark-subtle');

  TBsShadowStr: array [Low(TBsShadow) .. High(TBsShadow)] of string = //
    ('', 'sm', 'md', 'lg');

  TBsRoundTypeStr: array [Low(TBsRoundType) .. High(TBsRoundType)] of string = //
    ('', 'top', 'start', 'bottom', 'end', 'circle', 'pill', '0');

  TBsRoundSizeStr: array [Low(TBsRoundSize) .. High(TBsRoundSize)] of string = //
    ('sm', '', 'lg');

  TBsTextAlignStr: array [Low(TBsTextAlign) .. High(TBsTextAlign)] of string = //
    ('', 'justify', 'start', 'center', 'end');

  TBsTextWrapStr: array [Low(TBsTextWrap) .. High(TBsTextWrap)] of string = //
    ('', 'wrap', 'no-wrap', 'truncate');

  TBsTextTransformStr: array [Low(TBsTextTransform) .. High(TBsTextTransform)] of string = //
    ('', 'lowercase', 'uppercase', 'capitalize');

  TBsTextWeightStr: array [Low(TBsTextWeight) .. High(TBsTextWeight)] of string = //
    ('bolder', 'bold', 'normal', 'light', 'ligher');

  TBSDirectionStr: array [Low(TBSDirection) .. High(TBSDirection)] of string = //
    ('', 'row', 'row-reverse', 'column', 'column-reverse');

  TBSDisplayStr: array [Low(TBSDisplay) .. High(TBSDisplay)] of string = //
    ('', 'none', 'inline', 'inline-block', 'block', 'table', 'table-cell', 'table-row', 'flex',
    'inline-flex', 'grid', 'inline-grid');

  TBsViewportPosStr: array [Low(TBsViewportPos) .. High(TBsViewportPos)] of string = //
    ('', 'fixed-top', 'fixed-bottom', 'sticky-top', 'sticky-bottom');

  TBsDispPositionStr: array [Low(TBsDispPosition) .. High(TBsDispPosition)] of string = //
    ('', 'static', 'relative', 'absolute', 'fixed',
    'sticky');

  TBSJustifyContentStr: array [Low(TBSJustifyContent) .. High(TBSJustifyContent)] of string = //
    ('', 'start', 'end', 'center', 'between', 'around');

  TBSAlignStr: array [Low(TBSAlign) .. High(TBSAlign)] of string = //
    ('', 'start', 'end', 'center', 'baseline', 'stretch');

  TBSWrapStr: array [Low(TBSWrap) .. High(TBSWrap)] of string = //
    ('', 'nowrap', 'wrap', 'reverse');

  TBsAlignContentStr: array [Low(TBsAlignContent) .. High(TBsAlignContent)] of string = //
    ('', 'start', 'end', 'center', 'around', 'stretch');

  TBsFloatStr: array [Low(TBsFloat) .. High(TBsFloat)] of string = //
    ('', 'none', 'end', 'start');

  TNvBsTableStyleStr: array [Low(TNvBsTableStyle) .. High(TNvBsTableStyle)] of string = //
    ('bordered', 'borderless', 'hover', 'striped', 'dark', 'sm');

  TNvBsTHeaderStyleStr: array [Low(TNvBsTHeaderStyle) .. High(TNvBsTHeaderStyle)] of string = //
    ('', 'thead-light', 'thead-dark');

  TBsButtonDataToggleStr: array [Low(TNvBsDataToggle) .. High(TNvBsDataToggle)] of string = //
    ('', 'dropdown', 'button', 'collapse', 'modal');

  TBsSpacingStr: array [Low(TBsSpacing) .. High(TBsSpacing)] of string = //
    ('', 'auto', '0', '1', '2', '3', '4', '5');

  TBsWidthStr: array [Low(TBsWidth) .. High(TBsWidth)] of string = //
    ('', 'auto', '25', '50', '75', '100');

  TBsPosValueStr: array [Low(TBsPosValue) .. High(TBsPosValue)] of string = //
    ('', '0', '50', '100');

  TNvBsLinkColorStr: array [Low(TNvBsLinkColor) .. High(TNvBsLinkColor)] of string = //
    ('', 'primary', 'secondary', 'success', 'danger', 'warning', 'info', 'light', 'dark',
    'body-emphasis');

  TNvFloatPosStr: array [Low(TNvFloatPos) .. High(TNvFloatPos)] of string = //
    ('', 'top-left', 'top-center', 'top-right', 'center-left', 'center', 'center-right',
    'bottom-left', 'bottom-center', 'bottom-right');

implementation

end.
