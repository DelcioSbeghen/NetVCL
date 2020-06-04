unit NV.BS.Accordions;

interface

uses
  NV.BS.Containers, NV.BS.Cards;

type

  TNvBsAccordionItem = class(TNvBsCard)
  protected
    class function SupportImage: Boolean; override;
  published
    property Caption;
    property ImageListLink;
  end;

  TNvBsAccordionBody = class(TNvBsCardBody)

  end;

  TNvBsAccordion = class(TNVBsContainer)
  public
    function AddNewItem: TNvBsAccordionItem;
  end;

implementation

{ TNvBsAccordionItem }

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

end.
