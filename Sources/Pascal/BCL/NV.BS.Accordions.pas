unit NV.BS.Accordions;

interface

uses
  Windows, Classes, Controls, NV.BS.Containers;

type

  TNvBsAccordionItem = class(TNvBsContainer)
  protected
    class function SupportImage: Boolean; override;
    class function DefaultRenderText: Boolean; override;
  private
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
  protected
    procedure Click; override;
  published
    procedure Hide;
    procedure Show;

    property Action;
    property Caption;
    property CaptionVisible;
    property ImageListLink;
  end;

  TNvBsAccordionBody = class(TNvBsContainer)

  end;

  TNvBsAccordion = class(TNVBsContainer)
  protected
    class function DefaultClassCss: string; override;
  private
    FActiveItem: TNvBsAccordionItem;
    procedure SetActiveItem(const Value: TNvBsAccordionItem);
  public
    function AddNewItem: TNvBsAccordionItem;
  published
    property ClassCss;
    property ActiveItem: TNvBsAccordionItem read FActiveItem write SetActiveItem;
  end;

implementation

uses
  NV.VCL.Forms;

{ TNvBsAccordionItem }

procedure TNvBsAccordionItem.Click;
begin
  inherited;
  if (csDesigning in ComponentState) then
    begin
      if NeedSendChange then
        Screen.Ajax.AddCallFunction(ID, 'Show', '');
      Invalidate;
    end;
end;

procedure TNvBsAccordionItem.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  if (csDesigning in ComponentState) then
    Message.Result := HTCLIENT;
end;

class function TNvBsAccordionItem.DefaultRenderText: Boolean;
begin
  Result := True;
end;

procedure TNvBsAccordionItem.Hide;
begin
  if NeedSendChange then
    Screen.Ajax.AddCallFunction(ID, 'Hide', '');
  Invalidate;
end;

procedure TNvBsAccordionItem.Show;
begin
  if NeedSendChange then
    Screen.Ajax.AddCallFunction(ID, 'Show', '');
  Invalidate;
end;

class function TNvBsAccordionItem.SupportImage: Boolean;
begin
  Result := True;
end;

{ TNvBsAccordion }

function TNvBsAccordion.AddNewItem: TNvBsAccordionItem;
begin
  Result        := TNvBsAccordionItem.Create(Self.Owner);
  Result.Parent := Self;
  with TNvBsAccordionBody.Create(Self.Owner) do
    Parent := Result;
end;

class function TNvBsAccordion.DefaultClassCss: string;
begin
  Result := 'accordion'
end;

procedure TNvBsAccordion.SetActiveItem(const Value: TNvBsAccordionItem);
begin
  if Value <> FActiveItem then
    begin
      if Assigned(FActiveItem) then
        FActiveItem.hide;
      FActiveItem := Value;
      if Assigned(Value) then
        Value.Show;
    end;
end;

end.
