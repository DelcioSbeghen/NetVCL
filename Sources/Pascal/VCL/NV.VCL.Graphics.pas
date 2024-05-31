unit NV.VCL.Graphics;

interface

uses
  System.UITypes;

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
{$IFDEF FPC} function AlphaColorToColor(const Color: TAlphaColor): TColor;     {$ENDIF}

implementation

uses
  SysUtils;

{$IFDEF FPC}
type
   PAlphaColorRec = ^TAlphaColorRec;

  { TAlphaColorRec }

  TAlphaColorRec = record
  const
    Alpha = TAlphaColor($FF000000);
    Aliceblue = TAlphaColor(Alpha or $F0F8FF);
    Antiquewhite = TAlphaColor(Alpha or $FAEBD7);
    Aqua = TAlphaColor(Alpha or $00FFFF);
    Aquamarine = TAlphaColor(Alpha or $7FFFD4);
    Azure = TAlphaColor(Alpha or $F0FFFF);
    Beige = TAlphaColor(Alpha or $F5F5DC);
    Bisque = TAlphaColor(Alpha or $FFE4C4);
    Black = TAlphaColor(Alpha or $000000);
    Blanchedalmond = TAlphaColor(Alpha or $FFEBCD);
    Blue = TAlphaColor(Alpha or $0000FF);
    Blueviolet = TAlphaColor(Alpha or $8A2BE2);
    Brown = TAlphaColor(Alpha or $A52A2A);
    Burlywood = TAlphaColor(Alpha or $DEB887);
    Cadetblue = TAlphaColor(Alpha or $5F9EA0);
    Chartreuse = TAlphaColor(Alpha or $7FFF00);
    Chocolate = TAlphaColor(Alpha or $D2691E);
    Coral = TAlphaColor(Alpha or $FF7F50);
    Cornflowerblue = TAlphaColor(Alpha or $6495ED);
    Cornsilk = TAlphaColor(Alpha or $FFF8DC);
    Crimson = TAlphaColor(Alpha or $DC143C);
    Cyan = TAlphaColor(Alpha or $00FFFF);
    Darkblue = TAlphaColor(Alpha or $00008B);
    Darkcyan = TAlphaColor(Alpha or $008B8B);
    Darkgoldenrod = TAlphaColor(Alpha or $B8860B);
    Darkgray = TAlphaColor(Alpha or $A9A9A9);
    Darkgreen = TAlphaColor(Alpha or $006400);
    Darkgrey = TAlphaColor(Alpha or $A9A9A9);
    Darkkhaki = TAlphaColor(Alpha or $BDB76B);
    Darkmagenta = TAlphaColor(Alpha or $8B008B);
    Darkolivegreen = TAlphaColor(Alpha or $556B2F);
    Darkorange = TAlphaColor(Alpha or $FF8C00);
    Darkorchid = TAlphaColor(Alpha or $9932CC);
    Darkred = TAlphaColor(Alpha or $8B0000);
    Darksalmon = TAlphaColor(Alpha or $E9967A);
    Darkseagreen = TAlphaColor(Alpha or $8FBC8F);
    Darkslateblue = TAlphaColor(Alpha or $483D8B);
    Darkslategray = TAlphaColor(Alpha or $2F4F4F);
    Darkslategrey = TAlphaColor(Alpha or $2F4F4F);
    Darkturquoise = TAlphaColor(Alpha or $00CED1);
    Darkviolet = TAlphaColor(Alpha or $9400D3);
    Deeppink = TAlphaColor(Alpha or $FF1493);
    Deepskyblue = TAlphaColor(Alpha or $00BFFF);
    Dimgray = TAlphaColor(Alpha or $696969);
    Dimgrey = TAlphaColor(Alpha or $696969);
    Dodgerblue = TAlphaColor(Alpha or $1E90FF);
    Firebrick = TAlphaColor(Alpha or $B22222);
    Floralwhite = TAlphaColor(Alpha or $FFFAF0);
    Forestgreen = TAlphaColor(Alpha or $228B22);
    Fuchsia = TAlphaColor(Alpha or $FF00FF);
    Gainsboro = TAlphaColor(Alpha or $DCDCDC);
    Ghostwhite = TAlphaColor(Alpha or $F8F8FF);
    Gold = TAlphaColor(Alpha or $FFD700);
    Goldenrod = TAlphaColor(Alpha or $DAA520);
    Gray = TAlphaColor(Alpha or $808080);
    Green = TAlphaColor(Alpha or $008000);
    Greenyellow = TAlphaColor(Alpha or $ADFF2F);
    Grey = TAlphaColor(Alpha or $808080);
    Honeydew = TAlphaColor(Alpha or $F0FFF0);
    Hotpink = TAlphaColor(Alpha or $FF69B4);
    Indianred = TAlphaColor(Alpha or $CD5C5C);
    Indigo = TAlphaColor(Alpha or $4B0082);
    Ivory = TAlphaColor(Alpha or $FFFFF0);
    Khaki = TAlphaColor(Alpha or $F0E68C);
    Lavender = TAlphaColor(Alpha or $E6E6FA);
    Lavenderblush = TAlphaColor(Alpha or $FFF0F5);
    Lawngreen = TAlphaColor(Alpha or $7CFC00);
    Lemonchiffon = TAlphaColor(Alpha or $FFFACD);
    Lightblue = TAlphaColor(Alpha or $ADD8E6);
    Lightcoral = TAlphaColor(Alpha or $F08080);
    Lightcyan = TAlphaColor(Alpha or $E0FFFF);
    Lightgoldenrodyellow = TAlphaColor(Alpha or $FAFAD2);
    Lightgray = TAlphaColor(Alpha or $D3D3D3);
    Lightgreen = TAlphaColor(Alpha or $90EE90);
    Lightgrey = TAlphaColor(Alpha or $D3D3D3);
    Lightpink = TAlphaColor(Alpha or $FFB6C1);
    Lightsalmon = TAlphaColor(Alpha or $FFA07A);
    Lightseagreen = TAlphaColor(Alpha or $20B2AA);
    Lightskyblue = TAlphaColor(Alpha or $87CEFA);
    Lightslategray = TAlphaColor(Alpha or $778899);
    Lightslategrey = TAlphaColor(Alpha or $778899);
    Lightsteelblue = TAlphaColor(Alpha or $B0C4DE);
    Lightyellow = TAlphaColor(Alpha or $FFFFE0);
    LtGray = TAlphaColor(Alpha or $C0C0C0);
    MedGray = TAlphaColor(Alpha or $A0A0A0);
    DkGray = TAlphaColor(Alpha or $808080);
    MoneyGreen = TAlphaColor(Alpha or $C0DCC0);
    LegacySkyBlue = TAlphaColor(Alpha or $F0CAA6);
    Cream = TAlphaColor(Alpha or $F0FBFF);
    Lime = TAlphaColor(Alpha or $00FF00);
    Limegreen = TAlphaColor(Alpha or $32CD32);
    Linen = TAlphaColor(Alpha or $FAF0E6);
    Magenta = TAlphaColor(Alpha or $FF00FF);
    Maroon = TAlphaColor(Alpha or $800000);
    Mediumaquamarine = TAlphaColor(Alpha or $66CDAA);
    Mediumblue = TAlphaColor(Alpha or $0000CD);
    Mediumorchid = TAlphaColor(Alpha or $BA55D3);
    Mediumpurple = TAlphaColor(Alpha or $9370DB);
    Mediumseagreen = TAlphaColor(Alpha or $3CB371);
    Mediumslateblue = TAlphaColor(Alpha or $7B68EE);
    Mediumspringgreen = TAlphaColor(Alpha or $00FA9A);
    Mediumturquoise = TAlphaColor(Alpha or $48D1CC);
    Mediumvioletred = TAlphaColor(Alpha or $C71585);
    Midnightblue = TAlphaColor(Alpha or $191970);
    Mintcream = TAlphaColor(Alpha or $F5FFFA);
    Mistyrose = TAlphaColor(Alpha or $FFE4E1);
    Moccasin = TAlphaColor(Alpha or $FFE4B5);
    Navajowhite = TAlphaColor(Alpha or $FFDEAD);
    Navy = TAlphaColor(Alpha or $000080);
    Oldlace = TAlphaColor(Alpha or $FDF5E6);
    Olive = TAlphaColor(Alpha or $808000);
    Olivedrab = TAlphaColor(Alpha or $6B8E23);
    Orange = TAlphaColor(Alpha or $FFA500);
    Orangered = TAlphaColor(Alpha or $FF4500);
    Orchid = TAlphaColor(Alpha or $DA70D6);
    Palegoldenrod = TAlphaColor(Alpha or $EEE8AA);
    Palegreen = TAlphaColor(Alpha or $98FB98);
    Paleturquoise = TAlphaColor(Alpha or $AFEEEE);
    Palevioletred = TAlphaColor(Alpha or $DB7093);
    Papayawhip = TAlphaColor(Alpha or $FFEFD5);
    Peachpuff = TAlphaColor(Alpha or $FFDAB9);
    Peru = TAlphaColor(Alpha or $CD853F);
    Pink = TAlphaColor(Alpha or $FFC0CB);
    Plum = TAlphaColor(Alpha or $DDA0DD);
    Powderblue = TAlphaColor(Alpha or $B0E0E6);
    Purple = TAlphaColor(Alpha or $800080);
    Red = TAlphaColor(Alpha or $FF0000);
    Rosybrown = TAlphaColor(Alpha or $BC8F8F);
    Royalblue = TAlphaColor(Alpha or $4169E1);
    Saddlebrown = TAlphaColor(Alpha or $8B4513);
    Salmon = TAlphaColor(Alpha or $FA8072);
    Sandybrown = TAlphaColor(Alpha or $F4A460);
    Seagreen = TAlphaColor(Alpha or $2E8B57);
    Seashell = TAlphaColor(Alpha or $FFF5EE);
    Sienna = TAlphaColor(Alpha or $A0522D);
    Silver = TAlphaColor(Alpha or $C0C0C0);
    Skyblue = TAlphaColor(Alpha or $87CEEB);
    Slateblue = TAlphaColor(Alpha or $6A5ACD);
    Slategray = TAlphaColor(Alpha or $708090);
    Slategrey = TAlphaColor(Alpha or $708090);
    Snow = TAlphaColor(Alpha or $FFFAFA);
    Springgreen = TAlphaColor(Alpha or $00FF7F);
    Steelblue = TAlphaColor(Alpha or $4682B4);
    Tan = TAlphaColor(Alpha or $D2B48C);
    Teal = TAlphaColor(Alpha or $008080);
    Thistle = TAlphaColor(Alpha or $D8BFD8);
    Tomato = TAlphaColor(Alpha or $FF6347);
    Turquoise = TAlphaColor(Alpha or $40E0D0);
    Violet = TAlphaColor(Alpha or $EE82EE);
    Wheat = TAlphaColor(Alpha or $F5DEB3);
    White = TAlphaColor(Alpha or $FFFFFF);
    Whitesmoke = TAlphaColor(Alpha or $F5F5F5);
    Yellow = TAlphaColor(Alpha or $FFFF00);
    Yellowgreen = TAlphaColor(Alpha or $9ACD32);
    Null = TAlphaColor($00000000);
    constructor Create(const Color: TAlphaColor);
    class var ColorToRGB: function (Color: TAlphaColor): Longint;
    case Cardinal of
      0:
        (Color: TAlphaColor);
      2:
        (HiWord, LoWord: Word);
      3:
{$IFDEF BIGENDIAN}
        (A, R, G, B: System.Byte);
{$ELSE}
        (B, G, R, A: System.Byte);
{$ENDIF BIGENDIAN}
  end;

constructor TAlphaColorRec.Create(const Color: TAlphaColor);
begin
  Self := TAlphaColorRec(Color);
end;

function AlphaColorToColor(const Color: TAlphaColor): TColor;
begin
  Result := TColor((TAlphaColorRec(Color).R shl 16) or (TAlphaColorRec(Color).G shl 8) or TAlphaColorRec(Color).B);
end;

{$ENDIF FPC}



function AlphaColorToRGBA(aColor: TAlphaColor): string;
begin
  Result := 'rgba(' //
  + IntToStr(TAlphaColorRec(aColor).R) + ',' //
  + IntToStr(TAlphaColorRec(aColor).G)  + ',' //
  + IntToStr(TAlphaColorRec(aColor).B) + ',' //
  + FloatToStr(TAlphaColorRec(aColor).A / 255,  {$IFDEF FPC} DefaultFormatSettings {$ELSE} TFormatSettings.Create('en-US') {$ENDIF} ) + ')';
end;

end.
