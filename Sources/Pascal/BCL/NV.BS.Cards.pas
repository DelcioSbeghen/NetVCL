unit NV.BS.Cards;

interface

uses
  NV.BS.Containers, NV.BS.Types, NV.BS.HtmlControls, System.Classes;

type

  TNvBsCardHeader = class(TNVBsContainer)
  protected
    class function SupportImage: Boolean; override;
    class function DefaultClassCss: string; override;
    class function DefaultRenderText: Boolean; override;
  published
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property ImageListLink;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Text;
    property TextVisible;
    property Width;
  end;

  TNvBsCardBody = class(TNVBsContainer)
  protected
    class function DefaultClassCss: string; override;
  published
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width;
  end;

  TNvBsCardFooter = class(TNVBsContainer)
  protected
    class function DefaultClassCss: string; override;
  published
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width;
  end;

  TNvBsCard = class(TNVBsContainer)
  protected
    class function DefaultClassCss: string; override;
  protected
    procedure AfterDesignDrop; override;
    procedure RenameSubComponents(NewName: string); override;
  published
    property Background stored IsNotBgDefault;
    property Border;
    property ClassCss;
    property Position;
    property Shadow default bssNone;
    property TextProps;
    property Width;
  end;

  TNvBsCardTitle = class(TNvBSText)
  protected
    class function DefaultHtmlTag: string; override;
    class function DefaultClassCss: string; override;
  end;

  TNvBsCardSubtitle = class(TNvBSText)
  protected
    class function DefaultHtmlTag: string; override;
    class function DefaultClassCss: string; override;
  end;

  TNvBsCardLink = class(TNvBsLink)
  protected
    class function DefaultClassCss: string; override;
  end;

  TNvBsCardText = class(TNvBSText)
  protected
    class function DefaultHtmlTag: string; override;
    class function DefaultClassCss: string; override;
  end;

  TNvBsCardStats = class(TNvBsCard)
  protected
    class function DefaultClassCss: string; override;
    class function BackgroundDefault: TBsBackground; override;
  protected
    procedure AfterDesignDrop; override;
    procedure RenameSubComponents(NewName: string); override;
  end;

  TNvBsCardChart = class(TNvBsCard)
  protected
    class function DefaultClassCss: string; override;
    class function BackgroundDefault: TBsBackground; override;
  protected
    procedure AfterDesignDrop; override;
    procedure RenameSubComponents(NewName: string); override;
  end;

  TNvBsCardTable = class(TNvBsCard)
  protected
    class function DefaultClassCss: string; override;
    class function BackgroundDefault: TBsBackground; override;
  protected
    procedure AfterDesignDrop; override;
    procedure RenameSubComponents(NewName: string); override;
  end;

implementation

uses
  SysUtils, NV.VCL.Charts, VCL.Graphics, NV.BS.Tables;

{ TNvBsCardSubtitle }

class function TNvBsCardSubtitle.DefaultClassCss: string;
begin
  Result := 'card-subtitle';
end;

class function TNvBsCardSubtitle.DefaultHtmlTag: string;
begin
  Result := 'h6';
end;

{ TNvBsCardTitle }

class function TNvBsCardTitle.DefaultClassCss: string;
begin
  Result := 'card-title';
end;

class function TNvBsCardTitle.DefaultHtmlTag: string;
begin
  Result := 'h5';
end;

{ TNvBsCardText }

class function TNvBsCardText.DefaultClassCss: string;
begin
  Result := 'card-text';
end;

class function TNvBsCardText.DefaultHtmlTag: string;
begin
  Result := 'p';
end;

{ TNvBsCardHeader }

class function TNvBsCardHeader.DefaultClassCss: string;
begin
  Result := 'card-header';
end;

class function TNvBsCardHeader.DefaultRenderText: Boolean;
begin
  Result := True;
end;

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

class function TNvBsCardStats.BackgroundDefault: TBsBackground;
begin
  Result := bsbgPrimary;
end;

class function TNvBsCardStats.DefaultClassCss: string;
begin
  Result := 'card card-stats';
end;

procedure TNvBsCardStats.RenameSubComponents(NewName: string);
var
  I, J: Integer;
begin
  inherited;

  for I := 0 to ControlCount - 1 do
    if Controls[I] is TNvBsCardHeader then
      begin
        with Controls[I] as TNvBsCardHeader do
          begin
            Name := NewName + 'Header';

            for J := 0 to ControlCount - 1 do
              begin
                if Controls[J] is TNvBSText then
                  begin
                    Controls[J].Name := NewName + 'HeaderText';
                    Break;
                  end;
              end;
          end;

        Break;
      end;

  for I := 0 to ControlCount - 1 do
    if Controls[I] is TNvBsCardFooter then
      begin
        with Controls[I] as TNvBsCardFooter do
          begin
            Name := NewName + 'Footer';

            for J := 0 to ControlCount - 1 do
              begin
                if Controls[J] is TNvBSText then
                  begin
                    Controls[J].Name := NewName + 'FooterText';
                    Break;
                  end;
              end;
          end;

        Break;
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

class function TNvBsCardChart.BackgroundDefault: TBsBackground;
begin
  Result := bsbgPrimary;
end;

class function TNvBsCardChart.DefaultClassCss: string;
begin
  Result := 'card card-chart';
end;

procedure TNvBsCardChart.RenameSubComponents(NewName: string);
var
  I, J: Integer;
begin
  inherited;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TNvBsCardHeader then
      begin
        with Controls[I] as TNvBsCardHeader do
          begin
            for J := 0 to ControlCount - 1 do
              begin
                if Controls[J] is TNvChart then
                  begin
                    Controls[J].Name := NewName + 'Chart';
                    Break;
                  end;
              end;
          end;

        Break;
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

class function TNvBsCard.DefaultClassCss: string;
begin
  Result := 'card';
end;

procedure TNvBsCard.RenameSubComponents(NewName: string);
var
  I, J: Integer;
begin
  inherited;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TNvBsCardHeader then
      begin
        Controls[I].Name := NewName + 'Header';
        Break;
      end;

  for I := 0 to ControlCount - 1 do
    if Controls[I] is TNvBsCardBody then
      begin
        with Controls[I] as TNvBsCardBody do
          begin
            Name := NewName + 'Body';

            for J := 0 to ControlCount - 1 do
              begin
                if Controls[J] is TNvBsCardTitle then
                  begin
                    Controls[J].Name := NewName + 'Title';
                    Break;
                  end;
              end;

            for J := 0 to ControlCount - 1 do
              begin
                if Controls[J] is TNvBsCardSubtitle then
                  begin
                    Controls[J].Name := NewName + 'SubTitle';
                    Break;
                  end;
              end;
          end;

        Break;
      end;

  for I := 0 to ControlCount - 1 do
    if Controls[I] is TNvBsCardFooter then
      begin
        with Controls[I] as TNvBsCardFooter do
          begin
            Name := NewName + 'Footer';

            for J := 0 to ControlCount - 1 do
              begin
                if Controls[J] is TNvBSText then
                  begin
                    Controls[J].Name := NewName + 'Text';
                    Break;
                  end;
              end;
          end;

        Break;
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

  _Table        := TNvBsTable.Create(Owner);
  _Table.Parent := _Body;
  _Table.Styles := _Table.Styles - [sttBordered];

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

class function TNvBsCardTable.BackgroundDefault: TBsBackground;
begin
  Result := bsbgPrimary;
end;

class function TNvBsCardTable.DefaultClassCss: string;
begin
  Result := 'card card-table';
end;

procedure TNvBsCardTable.RenameSubComponents(NewName: string);
var
  I, J: Integer;
begin
  inherited;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TNvBsCardHeader then
      begin
        with Controls[I] as TNvBsCardHeader do
          begin
            for J := 0 to ControlCount - 1 do
              begin
                if Controls[J] is TNvBSText then
                  begin
                    Controls[J].Name := NewName + 'HeaderText';
                    Break;
                  end;
              end;
          end;

        Break;
      end;

  for I := 0 to ControlCount - 1 do
    if Controls[I] is TNvBsCardHeader then
      begin
        with Controls[I] as TNvBsCardHeader do
          begin
            for J := 0 to ControlCount - 1 do
              begin
                if Controls[J] is TNvBsTable then
                  begin
                    Controls[J].Name := NewName + 'Table';
                    Break;
                  end;
              end;
          end;

        Break;
      end;
end;

{ TNvBsCardBody }

class function TNvBsCardBody.DefaultClassCss: string;
begin
  Result := 'card-body';
end;

{ TNvBsCardFooter }

class function TNvBsCardFooter.DefaultClassCss: string;
begin
  Result := 'card-footer';
end;

{ TNvBsCardLink }

class function TNvBsCardLink.DefaultClassCss: string;
begin
  Result := 'card-link';
end;

end.
