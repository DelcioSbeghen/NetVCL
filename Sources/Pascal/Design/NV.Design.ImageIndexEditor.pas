unit NV.Design.ImageIndexEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs{$IFDEF FPC} , PropEdits {$ELSE} , DesignEditors, DesignIntf {$ENDIF}, NV.Desktop, NV.Vcl.Images;

type
  TFrmImgIdxEditor = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TNvImageIndexEditor = class(TIntegerProperty)
  private
    Browser: TNvScreenBrowser;
    procedure ExecuteCallback(const Value: {$IFDEF FPC} ustring {$ELSE} PChar {$ENDIF});
    function GetHtml(ImgList: TNvCustomImageList): string;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
  // unit MijnPropertyEditorReg;
  //
  // interface
  //
  // uses
  // Classes, Types, SysUtils, Graphics, Math, ImgList, DesignIntf, DesignEditors,
  // VCLEditors, TypInfo, MijnTTextBulletUnit;
  //
  // type
  // { TTextBulletImageIndexProperty shows a dropdown list with images for the
  // ImageIndex property of TTextBullet. It works similar to
  // TPersistentImageIndexPropertyEditor, which is declared in the implementation
  // section of StdReg.pas (package dclstd). }
  //
  // TTextBulletImageIndexProperty = class(TIntegerProperty,
  // ICustomPropertyListDrawing)
  // private
  // procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
  // var AWidth: Integer);
  // procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
  // var AHeight: Integer);
  // procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
  // const ARect: TRect; ASelected: Boolean);
  // public
  // function GetAttributes: TPropertyAttributes; override;
  // function GetImageListAt(Index: Integer): TCustomImageList; virtual;
  // procedure GetValues(Proc: TGetStrProc); override;
  // end;
  //
  // procedure Register;
  //
  // implementation
  //
  // procedure TTextBulletImageIndexProperty.ListMeasureHeight(const Value: string;
  // ACanvas: TCanvas; var AHeight: Integer);
  // var
  // ImgList: TCustomImageList;
  // begin
  // ImgList := GetImageListAt(0);
  // AHeight := ACanvas.TextHeight(Value) + 2;
  // if Assigned(ImgList) then
  // AHeight := Max(AHeight, ImgList.Height + 4);
  // end;
  //
  // procedure TTextBulletImageIndexProperty.ListMeasureWidth(const Value: string;
  // ACanvas: TCanvas; var AWidth: Integer);
  // var
  // ImgList: TCustomImageList;
  // begin
  // ImgList := GetImageListAt(0);
  // AWidth := ACanvas.TextWidth(Value) + 4;
  // if Assigned(ImgList) then
  // AWidth := Max(AWidth, ImgList.Width + 4);
  // end;
  //
  // procedure TTextBulletImageIndexProperty.ListDrawValue(const Value: string;
  // ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  // var
  // ImgList: TCustomImageList;
  // R: TRect;
  // begin
  // ImgList := GetImageListAt(0);
  // ACanvas.FillRect(ARect);
  // R := ARect;
  // Inc(R.Left, 2);
  // if Assigned(ImgList) then
  // begin
  // ImgList.Draw(ACanvas, R.Left, R.Top + 2, StrToInt(Value));
  // Inc(R.Left, ImgList.Width + 2);
  // end;
  // DefaultPropertyListDrawValue(Value, ACanvas, R, ASelected);
  // end;
  //
  // function TTextBulletImageIndexProperty.GetAttributes: TPropertyAttributes;
  // begin
  // Result := [paMultiSelect, paValueList, paRevertable];
  // end;
  //
  // function TTextBulletImageIndexProperty.GetImageListAt(Index: Integer):
  // TCustomImageList;
  // var
  // APersistent: TPersistent;
  // begin
  // APersistent := GetComponent(Index);
  // if APersistent is TTextBullet then
  // Result := TTextBullet(APersistent).Images
  // else
  // Result := nil;
  // end;
  //
  // procedure TTextBulletImageIndexProperty.GetValues(Proc: TGetStrProc);
  // var
  // ImgList: TCustomImageList;
  // i: Integer;
  // begin
  // ImgList := GetImageListAt(0);
  // if Assigned(ImgList) then
  // for i := 0 to ImgList.Count - 1 do
  // Proc(IntToStr(i));
  // end;
  //
  // procedure Register;
  // begin
  // RegisterPropertyEditor(TypeInfo(TImageIndex), TTextBullet, '',
  // TTextBulletImageIndexProperty);
  // end;

var
  FrmImgIdxEditor: TFrmImgIdxEditor;

implementation

{$R *.dfm}
{ TNvImageIndexEditor }

procedure TNvImageIndexEditor.Edit;
var
  _ImgLink: TNVImageListLink;
  _ImgList: TNvCustomImageList;
begin
  inherited;
  _ImgList := nil;
  _ImgLink := GetComponent(0) as TNVImageListLink;
  if Assigned(_ImgLink) then
    _ImgList := _ImgLink.Images;

  if not Assigned(_ImgList) then
    Raise Exception.Create('Can''t access ImageList');

  Browser := TNvScreenBrowser.Create(nil);
  Browser.CreateScreenBrowser;
  Browser.ResizeBrowser(500, 200);
  Browser.LoadHtml(GetHtml(_ImgList));
end;

procedure TNvImageIndexEditor.ExecuteCallback(const Value:  {$IFDEF FPC} ustring {$ELSE} PChar {$ENDIF});
var
  Idx     : Integer;
  _Browser: TNvScreenBrowser;
begin
  Idx := StrToIntDef(Value, -1);

  if Browser <> nil then
    FreeAndnil(Browser);

  if Idx >= 0 then
    SetValue(Idx.ToString);
end;

function TNvImageIndexEditor.GetAttributes: TPropertyAttributes;
begin
  result := [paDialog];
end;

function TNvImageIndexEditor.GetHtml(ImgList: TNvCustomImageList): string;
var
  i: Integer;
begin
  result :=             //
    '<!DOCTYPE html>' + //
    '<head>' +          //
    '<script type="text/javascript">function doLoad(){LocalHandler = new TNvLocalhandler();};</script>'
    +                                 //
    '<style>' +                       //
    ' body {' +                       //
    '   background-color: #c3c3c3;' + //
    ' }' +                            //
    '' +                              //
    '  .container {' +                //
    '    display: flex;' +            //
    '    flex-wrap: wrap;' +          //
    '  }' +                           //
    '' +                              //
    '    ul.img-list {' +             //
    '    list-style: none;' +         //
    '    display: flex;' +            //
    '    flex-wrap: wrap;' +          //
    '    padding: 0;' +               //
    '  }' +                           //
    '' +                              //
    '    li.img-item {' +             //
    '    flex-wrap: wrap;' +          //
    '    width: 50px;' +              //
    '    height: 70px;' +             //
    '    margin: 5px;' +              //
    '    background: #666766;' +      //
    '    border-radius: 6px;' +       //
    '    overflow: hidden;' +         //
    '    text-align: center;' +       //
    '    white-space: nowrap;' +      //
    '    text-overflow: clip;' +      //
    '  }' +                           //
    '' +                              //
    '  a.img-link {' +                //
    '     text-decoration: none;' +   //
    '  }' +                           //
    '' +                              //
    ' .img-link svg {' +              //
    '    height: 2em;' +              //
    '    width: 2em;' +               //
    '    fill: aliceblue;' +          //
    '     margin-top: 3px;' +         //
    ' }' +                            //
    '' +                              //
    ' spam.img-title {' +             //
    '    font-size: 0.8em;' +         //
    '    display: block;' +           //
    '    color: black;' +             //
    '  }' +                           //
    '' +                              //
    ' spam.img-index {' +             //
    '    font-size: 0.8em;' +         //
    '    display: block;' +           //
    '    font-weight: 900;' +         //
    '    color: black;' +             //
    '  }' +                           //
    '</style>' +                      //
    '</head>' +                       //
    '<html>' +                        //
    '<body onload="doLoad();">' +     //
    '<div  class="container">' +      //
    '<ul class="img-list">';          //

  for i := 0 to ImgList.Count - 1 do
    begin
      result := result +          //
        '<li class="img-item">' + //
        '<a class="img-link" href="#" onclick=LocalHandler.SendData("' + ImgList.Images[i].
        Index.ToString + '") >' +          //
        ImgList.Render(i) +                //
        '<spam class="img-title">' +       //
        ImgList.Images[i].Name +           //
        '</spam>' +                        //
        '<spam class="img-index">' +       //
        ImgList.Images[i].Index.ToString + //
        '</spam>' +                        //
        '</a>' +                           //
        '</li>';                           //
    end;

  result := result + //
    '</ul>' +        //
    '</div>' +       //
    '</body>' +      //
    '</html>';       //
end;

end.
