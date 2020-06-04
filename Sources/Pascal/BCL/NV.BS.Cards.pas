unit NV.BS.Cards;

interface

uses
  NV.BS.Containers, NV.BS.Types, NV.BS.HtmlControls, System.Classes;

type

  TNvBsCardHeader = class(TNVBsContainer)
  protected
    class function SupportImage: Boolean; override;
  published
    property Background stored IsNotBgDefault;
    property Border;
    property ImageListLink;
    property Shadow default bssNone;
    property TextProps;
    property Text;
  end;

  TNvBsCardBody = class(TNVBsContainer)
  published
    property Background stored IsNotBgDefault;
    property Border;
    property Shadow default bssNone;
    property TextProps;
  end;

  TNvBsCardFooter = class(TNVBsContainer)
  published
    property Background stored IsNotBgDefault;
    property Border;
    property Shadow default bssNone;
    property TextProps;
  end;

  TNvBsCard = class(TNVBsContainer)
  protected
    procedure AfterDesignDrop; override;
  published
    property Background stored IsNotBgDefault;
    property Border;
    property Shadow default bssNone;
    property TextProps;
  end;

  TNvBsCardTitle = class(TNvBSText)
  protected
    class function DefaultHtmlTag: string; override;
  end;

  TNvBsCardSubtitle = class(TNvBSText)
  protected
    class function DefaultHtmlTag: string; override;
  end;

  TNvBsCardLink = class(TNvBsLink)

  end;

  TNvBsCardText = class(TNvBSText)
  protected
    class function DefaultHtmlTag: string; override;
  end;

  TNvBsCardStats = class(TNvBsCard)
  protected
    procedure AfterDesignDrop; override;
  end;

  TNvBsCardChart = class(TNvBsCard)
  protected
    procedure AfterDesignDrop; override;
  end;

  TNvBsCardTable = class(TNvBsCard)
  protected
    procedure AfterDesignDrop; override;
  end;

implementation

uses
  SysUtils, NV.VCL.Charts, VCL.Graphics, NV.BS.Tables;

{ TNvBsCardSubtitle }

class function TNvBsCardSubtitle.DefaultHtmlTag: string;
begin
  Result := 'h6';
end;

{ TNvBsCardTitle }

class function TNvBsCardTitle.DefaultHtmlTag: string;
begin
  Result := 'h5';
end;

{ TNvBsCardText }

class function TNvBsCardText.DefaultHtmlTag: string;
begin
  Result := 'p';
end;

{ TNvBsCardHeader }

class function TNvBsCardHeader.SupportImage: Boolean;
begin
  Result := True;
end;

{ TNvBsCardStats }

procedure TNvBsCardStats.AfterDesignDrop;
var
  _Header: TNvBsCardHeader;
  _Footer: TNvBsCardFooter;
begin
  Background     := bsbgInfo;
  _Header        := TNvBsCardHeader.Create(Owner);
  _Header.Parent := Self;
  _Header.Text   := 'Value';
  with TNvBSText.Create(Owner) do
    begin
      Parent  := _Header;
      TagHtml := 'span';
      Text    := 'Description';
    end;

  _Footer        := TNvBsCardFooter.Create(Owner);
  _Footer.Parent := Self;
  with TNvBSText.Create(Owner) do
    begin
      Parent  := _Footer;
      TagHtml := 'span';
      Text    := 'What should be done';
    end;
end;

{ TNvBsCardChart }

procedure TNvBsCardChart.AfterDesignDrop;
var
  _Header: TNvBsCardHeader;
  _Body  : TNvBsCardBody;
  _Footer: TNvBsCardFooter;
begin
  _Header        := TNvBsCardHeader.Create(Owner);
  _Header.Parent := Self;
  with TNvChart.Create(Owner) do
    begin
      Parent         := _Header;
      Responsive     := True;
      Legend.Visible := False;
      with Series.Add as TNvChartSerieItem do
        begin
          SerieType                                         := dwstLine;
          Serie.Data.CommaText                              := '2,5,10,8';
          Labels.CommaText                                  := 'One,Two,Three,Four';
          Serie.BackGroundColor                             := $00000000;
          Serie.BorderColor                                 := $FFDDDDDD;
          Serie.BorderWidth                                 := 4;
          (Serie as TNvChartSerieLine).PointBackgroundColor := $FFDDDDDD;
          (Serie as TNvChartSerieLine).PointBorderColor     := $FFDDDDDD;
        end;
    end;

  _Body        := TNvBsCardBody.Create(Owner);
  _Body.Parent := Self;
  with TNvBsCardTitle.Create(Owner) do
    begin
      Parent := _Body;
      Text   := 'Data Description';
    end;
  with TNvBsCardSubtitle.Create(Owner) do
    begin
      Parent := _Body;
      Text   := 'What happened';
    end;

  _Footer        := TNvBsCardFooter.Create(Owner);
  _Footer.Parent := Self;
  with TNvBSText.Create(Owner) do
    begin
      Parent  := _Footer;
      TagHtml := 'span';
      Text    := 'Updated in ' + DateTimeToStr(Now);
    end;

end;

{ TNvBsCard }

procedure TNvBsCard.AfterDesignDrop;
var
  _Header: TNvBsCardHeader;
  _Body  : TNvBsCardBody;
  _Footer: TNvBsCardFooter;
begin
  _Header        := TNvBsCardHeader.Create(Owner);
  _Header.Parent := Self;
  _Header.Text   := 'Card Header';

  _Body        := TNvBsCardBody.Create(Owner);
  _Body.Parent := Self;
  with TNvBsCardTitle.Create(Owner) do
    begin
      Parent := _Body;
      Text   := 'Card Title';
    end;
  with TNvBsCardSubtitle.Create(Owner) do
    begin
      Parent := _Body;
      Text   := 'Card Subtitle';
    end;

  _Footer        := TNvBsCardFooter.Create(Owner);
  _Footer.Parent := Self;
  with TNvBSText.Create(Owner) do
    begin
      Parent  := _Footer;
      TagHtml := 'span';
      Text    := 'Card Footer';
    end;
end;

{ TNvBsCardTable }

procedure TNvBsCardTable.AfterDesignDrop;
var
  _Header: TNvBsCardHeader;
  _Body  : TNvBsCardBody;
  _Table : TNvBsTable;
  _Col   : TNvBsTableColumn;
  C      : Integer;
begin
  _Header        := TNvBsCardHeader.Create(Owner);
  _Header.Parent := Self;
  _Header.Text   := 'Data Title';
  with TNvBSText.Create(Owner) do
    begin
      Parent  := _Header;
      TagHtml := 'span';
      Text    := 'Data Description';
    end;

  _Body        := TNvBsCardBody.Create(Owner);
  _Body.Parent := Self;

  _Table := TNvBsTable.Create(Owner);
  _Table.Parent:= _Body;
  _Table.Styles:= _Table.Styles - [sttBordered];

  _Col           := TNvBsTableColumn.Create(_Table.Columns);
  _Col.FieldName := 'id';
  _Col.Title     := 'ID';

  _Col           := TNvBsTableColumn.Create(_Table.Columns);
  _Col.FieldName := 'name';
  _Col.Title     := 'Name';

  _Col           := TNvBsTableColumn.Create(_Table.Columns);
  _Col.FieldName := 'salary';
  _Col.Title     := 'Salary';

  _Col           := TNvBsTableColumn.Create(_Table.Columns);
  _Col.FieldName := 'country';
  _Col.Title     := 'Country';

  for C := 1 to 4 do
    with _Table.Data.AddObject do
      begin
        I['id']      := C;
        S['name']    := 'Person Name ' + C.ToString;
        S['salary']  := '$' + Round(5).ToString + '.' + Round(2).ToString;
        S['country'] := 'Country of Person ' + C.ToString;
      end;

end;

end.
