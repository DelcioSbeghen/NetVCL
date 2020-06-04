unit NV.VCL.Graphics;

interface

uses
  UITypes;

// canvas types
type
  TDWCanvasLineCap         = (dwclcButt, dwclcRound, dwclcSquare);
  TDWCanvasLineDashPattern = array of Integer;
  TDWCanvasLineJoin        = (dwcljBevel, dwcljRound, dwcljMiter);

const
  TDWCanvasLineCapStr: array [low(TDWCanvasLineCap) .. High(TDWCanvasLineCap)
    ] of string = ('butt', 'round', 'square');

  TDWCanvasLineJoinStr: array [low(TDWCanvasLineJoin) .. High(TDWCanvasLineJoin)
    ] of string = ('bevel', 'round', 'miter');

  // get rgba color of an TAlphacolor
function AlphaColorToRGBA(aColor: TAlphaColor): string;

implementation

uses
  SysUtils;

function AlphaColorToRGBA(aColor: TAlphaColor): string;
begin
  Result := 'rgba(' + IntToStr(TAlphaColorRec(aColor).R) + ',' + IntToStr(TAlphaColorRec(aColor).G)
    + ',' + IntToStr(TAlphaColorRec(aColor).B) + ',' + FloatToStr(TAlphaColorRec(aColor).A / 255,
    TFormatSettings.Create('en-US')) + ')';
end;

end.
