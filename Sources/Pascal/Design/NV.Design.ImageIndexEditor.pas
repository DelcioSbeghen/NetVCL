unit NV.Design.ImageIndexEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DesignEditors, DesignIntf, NV.Browser, NV.Vcl.Images;

type
  TFrmImgIdxEditor = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TNvImageIndexEditor = class(TIntegerProperty)
  private
    Browser: TNvBrowser;
    procedure ExecuteCallback(const Value: string);
    function GetHtml(ImgList: TNvCustomImageList): string;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

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
  _ImgLink := GetComponent(0) as TNVImageListLink;
  if Assigned(_ImgLink) then
    _ImgList := _ImgLink.Images;

  if not Assigned(_ImgList) then
    Raise Exception.Create('Can''t access ImageList');

  Browser := TNvBrowser.Create(nil);
  Browser.CreateScreenBrowser(ExecuteCallback);
  Browser.ResizeBrowser(500, 200);
  Browser.LoadHtml(GetHtml(_ImgList));
end;

procedure TNvImageIndexEditor.ExecuteCallback(const Value: string);
var
  Idx: Integer;
begin
  Idx := StrToIntDef(Value, -1);

  if Idx >= 0 then
    SetValue(Idx.ToString);

  if Browser <> nil then
    FreeAndNil(Browser);
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
