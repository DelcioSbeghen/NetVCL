unit NV.Design.ImagelistEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Graphics,
  Controls, Forms, Dialogs, DesignEditors, ExtCtrls, Vcl.Buttons, Vcl.ComCtrls, NV.Vcl.Images,
  ImageList, ImgList, NV.Browser, DesignIntf;

type
  TFrmImgListEditor = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    BtnAddOne: TSpeedButton;
    BtnAddMultpl: TSpeedButton;
    BtnReplace: TSpeedButton;
    BtnDelete: TSpeedButton;
    OpenDialog1: TOpenDialog;
    FakeList: TImageList;
    ListView1: TPanel;
    SpeedButton1: TSpeedButton;
    BtnCancel: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnAddOneClick(Sender: TObject);
    procedure BtnAddMultplClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1Resize(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnReplaceClick(Sender: TObject);
  private
    ImgList      : TNvSvgImageList;
    SourceList   : TNvSvgImageList;
    Browser      : TNvBrowser;
    BrowserActive: Boolean;
    Selected     : TNvImageItem;
    procedure ExecuteCallback(const Value: PChar);
  protected
    procedure RefillListView;

  public
    procedure InitFromImgList(aList: TNvSvgImageList);
  end;

  TNvSvgImageListEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

var
  FrmImgListEditor: TFrmImgListEditor;

implementation

uses
  StrUtils, NV.Utils;

{$R *.dfm}
{ TNvSvgImageListEditor }

procedure TNvSvgImageListEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0:
      begin
        Application.CreateForm(TFrmImgListEditor, FrmImgListEditor);
        FrmImgListEditor.InitFromImgList(Component as TNvSvgImageList);
        if FrmImgListEditor.ShowModal = mrOk then
          if Designer <> nil then
            Designer.Modified;

        FreeAndNil(FrmImgListEditor);
      end;
  end;
end;

function TNvSvgImageListEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&ImageList editor...';
    // 1: Result := '';
  end;
end;

function TNvSvgImageListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TFrmImgListEditor }

procedure TFrmImgListEditor.BtnAddMultplClick(Sender: TObject);
begin
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
  if OpenDialog1.Execute then
    begin
      ImgList.AddFromFiles(OpenDialog1.Files);
      RefillListView;
    end;
end;

procedure TFrmImgListEditor.BtnAddOneClick(Sender: TObject);
begin
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
  if OpenDialog1.Execute then
    begin
      ImgList.AddFromFile(OpenDialog1.FileName);
      RefillListView;
    end;
end;

procedure TFrmImgListEditor.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmImgListEditor.BtnDeleteClick(Sender: TObject);
begin
  if Selected <> nil then
    begin
      ImgList.Images.Delete(Selected.Index);
      Selected := nil;
      RefillListView;
    end;
end;

procedure TFrmImgListEditor.BtnReplaceClick(Sender: TObject);
begin
  if Selected <> nil then
    begin
      OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
      if OpenDialog1.Execute then
        begin
          ImgList.ReplaceFromFile(Selected.Index, OpenDialog1.FileName);
          RefillListView;
        end;

      RefillListView;
    end;
end;

procedure TFrmImgListEditor.ExecuteCallback(const Value: PChar);
var
  Idx: Integer;
begin
  if Not(csDestroying in ComponentState) then
    begin
  Idx := StrToIntDef(Value, -1);

  if Idx >= 0 then
    Selected := ImgList.Images[Idx]
  else
    Selected := nil;

  if Selected <> nil then
    Caption := ImgList.Name + '-' + Selected.Name
  else
    Caption := ImgList.Name + '-';

  BtnReplace.Enabled := Selected <> nil;
  BtnDelete.Enabled  := ImgList.Count > 0;
    end;
end;

procedure TFrmImgListEditor.FormCreate(Sender: TObject);
begin
  ImgList := TNvSvgImageList.Create(Self);
  Browser := TNvBrowser.Create(Self);
end;

procedure TFrmImgListEditor.FormDestroy(Sender: TObject);
begin
  // Browser.Free;
end;

procedure TFrmImgListEditor.FormShow(Sender: TObject);
begin
  RefillListView;
  if DebugHook <> 0 then
    Browser.ShowDevTools(Mouse.CursorPos);
end;

procedure TFrmImgListEditor.InitFromImgList(aList: TNvSvgImageList);
begin
  SourceList := aList;
  ImgList.Assign(aList);

  RefillListView;
end;

procedure TFrmImgListEditor.ListView1Resize(Sender: TObject);
begin
  if BrowserActive then
    begin
      // Browser.SetParent(ListView1);
      Browser.ResizeBrowser(ListView1.Height, ListView1.Width);
    end;
end;

procedure TFrmImgListEditor.RefillListView;
var
  i    : Integer;
  _Html: string;
begin

  _Html :=              //
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
      _Html := _Html +            //
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

  _Html := _Html + //
    '</ul>' +      //
    '</div>' +     //
    '</body>' +    //
    '</html>';     //

  if not BrowserActive then
    Browser.CreateScreenBrowser(ExecuteCallback);

  Browser.SetParent(ListView1);
  Browser.LoadHtml(_Html);

  BrowserActive := True;

  Browser.ResizeBrowser(ListView1.Height, ListView1.Width);
end;

procedure TFrmImgListEditor.SpeedButton1Click(Sender: TObject);
begin
  SourceList.Assign(ImgList);
  ModalResult := mrOk;
end;

end.
