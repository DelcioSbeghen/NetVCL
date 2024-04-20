unit NV.BS.Register;

interface

uses
  Classes, DB, DesignEditors, DesignIntf, ToolsAPI, PaletteAPI;

type
  TNVBsNavEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    // procedure Edit; override;
  end;

  TNVBsNavBarEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    // procedure Edit; override;
  end;

  TNVBsDropdownMenuEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    // procedure Edit; override;
  end;

  TNVBsCardEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    // procedure Edit; override;
  end;

  TNVBsCardBodyEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    // procedure Edit; override;
  end;

  TNVBsTabsEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    // procedure Edit; override;
  end;

  TNvBsAccordionEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TNvColFormatterProperty = class(TPropertyEditor)
  protected
    // function FilterFunc(const ATestEditor: IProperty): Boolean;
    // function GetComponentReference: TComponent; virtual;
    // function ShowReferenceProperty: Boolean; virtual;
    // function GetSelections: IDesignerSelections; virtual;
  public
    // function AllEqual: Boolean; override;
    // procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    // procedure GetProperties(Proc: TGetPropProc); override;
    // function GetEditLimit: Integer; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

uses SysUtils, StrUtils, Dialogs, NV.BS.Containers, NV.BS.ScrollFocus, NV.BS.Alerts, NV.BS.Buttons,
  NV.BS.Inputs, NV.BS.Navbar, NV.BS.HtmlControls, NV.BS.Cards, NV.BS.Tabs, NV.BS.Accordions,
  NV.BS.Tables, NV.JSON, NV.BS.Design.TableDataEditor, NV.Design.ActionEditor;

const
  PALLETE_PAGE      = 'NetVCL BS';
  PALLETE_PAGE_DASH = 'NetVCL BS Dashboard';

procedure Register;
begin

  { TODO -oDelcio -cRegister : Property categories in Object inspector with RegisterPropertiesInCategory }

  // RegisterComponents('SRP', [TSRPInput, TNvrPanel]);
  RegisterComponents(PALLETE_PAGE, [ //
    TNvBsRow, TNvBsFormRow, TNvBsColumn, TNvBsScrollFocus, TNvBsListGroup, TNvBsListItem,
    // containers
    TNvBSText,                            // HtmlControls
    TNvBsAlert, TNvBsToast, TNvBsSpinner, // Alerts, Toasts, Spinners
    TNvBsButton, TNvBsButtonGroup, TNvBsButtonToolbar, TNvBsButtonDropdown, // Buttons and bars
    TNvBsInput, TNvBsInputDate, TNvBsDateRange, // inputs
    TNvBsInputDateTime, TNvBsSelect, TNvBsMemo, TNvBsRange, TNvBsCheckBox, TNvBsSwitch,
    TNvBsLookupSelect, TNvBsInputGroup, TNvBsInputAddonIcon, TNvBsInputAddonText,
    TNvBsInputAddonAction]);

  UnlistPublishedProperty(TNvBsCheckBox, 'Value');
  UnlistPublishedProperty(TNvBsButtonDropdown, 'Menu.OnShow');
  UnlistPublishedProperty(TNvBsButtonDropdown, 'Menu.OnHide');

  // DropdownMenu
  RegisterClasses([TNvBsDropdownMenu, TNvBsDropDownItemLink, TNvBsButtonDropdown,
    TNvBsDropdownContainer]);
  RegisterComponentEditor(TNvBsDropdownMenu, TNVBsDropdownMenuEditor);
  RegisterComponents(PALLETE_PAGE, [TNvBsDropDown]);

  // Nav
  RegisterComponents(PALLETE_PAGE, [TNvBsNav]);
  RegisterClasses([TBsNavItemLink, TBsNavItemDropdown, TBsNavItemCollapse]);
  RegisterComponentEditor(TNvBsNav, TNVBsNavEditor);

  // Navbar
  RegisterComponents(PALLETE_PAGE, [TNvBsNavBar]);
  RegisterClasses([TBsNavBarContainer, TBsNavBarBrand, TBsNavBarToggler, TBsNavBarContent,
    TBsNavBarNav, TBsNavBarItemLink, TBsNavBarItemDropdown]);
  RegisterComponentEditor(TNvBsNavBar, TNVBsNavBarEditor);

  // Sidebar
  RegisterComponents(PALLETE_PAGE, [TNvBsSideBar]);

  // Cards
  RegisterComponents(PALLETE_PAGE, [TNvBsCard]);
  RegisterComponents(PALLETE_PAGE_DASH, [TNvBsCardStats, TNvBsCardChart, TNvBsCardTable]);
  RegisterClasses([TNvBsCardHeader, TNvBsCardBody, TNvBsCardFooter, TNvBsCardTitle,
    TNvBsCardSubtitle, TNvBsCardLink, TNvBsCardText]);
  RegisterComponentEditor(TNvBsCard, TNVBsCardEditor);
  RegisterComponentEditor(TNvBsCardBody, TNVBsCardBodyEditor);

  // Accordion
  RegisterComponents(PALLETE_PAGE, [TNvBsAccordion]);
  RegisterClasses([TNvBsAccordionItem, TNvBsAccordionBody]);
  RegisterComponentEditor(TNvBsAccordion, TNvBsAccordionEditor);

  // Tabs
  RegisterComponents(PALLETE_PAGE, [TNvBsTabControl, TNvBsTDI]);
  RegisterClasses([TNvBsTabs, TNvBsTabHeaderLink, TNvBsTab, TNvBsTabContent]);
  RegisterComponentEditor(TNvBsTabControl, TNVBsTabsEditor);

  // Tables
  RegisterComponents(PALLETE_PAGE, [TNvBsTable, TNvBsDbTable]);
  RegisterPropertyEditor(TypeInfo(TJsonArray), TNvBsTable, 'Data', TNvBsTableDataEditor);
  RegisterClasses([TNvBsTableColumns, TNvBsTableColumn, TNvBsFmtAction]);
  RegisterPropertyEditor(TypeInfo(TNvBsFormatterClass), TNvBsTableColumn, 'FormaterClass',
    TNvColFormatterProperty);
  RegisterPropertyEditor(TypeInfo(TBasicAction), TNvBsFmtAction, 'Action', TNvActionProperty);

end;

{ TNVBsNavEditor }

procedure TNVBsNavEditor.ExecuteVerb(Index: Integer);
var
  ListItem        : TBsNavItem;
  ListItemDropdown: TBsNavItemDropdown;
  DropDownMenu    : TNvBsDropdownMenu;
begin
  inherited;
  case Index of
    0:
      begin
        ListItem        := TBsNavItemLink.Create(TNvBsNav(Component).Owner);
        ListItem.Parent := TNvBsNav(Component);
        // ListItem.Name   := Designer.UniqueName(ListItem.ClassName);
      end;
    1:
      begin
        DropDownMenu := TNvBsDropdownMenu.Create(TNvBsNav(Component).Owner);
        // DropDownMenu.Name := Designer.UniqueName(DropDownMenu.ClassName);

        ListItemDropdown := TBsNavItemDropdown.Create(TNvBsNav(Component).Owner, DropDownMenu);
        ListItemDropdown.Parent := TNvBsNav(Component);
        // ListItemDropdown.Name := Designer.UniqueName(ListItemDropdown.ClassName);
      end;
    2:
      begin
        ListItem        := TBsNavItemCollapse.Create(TNvBsNav(Component).Owner);
        ListItem.Parent := TNvBsNav(Component);
        // ListItem.Name   := Designer.UniqueName(ListItem.ClassName);
      end;

  end;
end;

function TNVBsNavEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add Nav Item Link';
    1: Result := 'Add Nav Item Dropdown';
    2: Result := 'Add Nav Item Collapse';
  end;
end;

function TNVBsNavEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TNVBsNavBarEditor }

procedure TNVBsNavBarEditor.ExecuteVerb(Index: Integer);
var
  ListItem        : TBsNavBarItemLink;
  ListItemDropdown: TBsNavBarItemDropdown;
  DropDownMenu    : TNvBsDropdownMenu;

begin
  inherited;
  case Index of
    0:
      begin
        ListItem := TBsNavBarItemLink.Create(TNvBsNavBar(Component).Owner);
        // ListItem.Name   := Designer.UniqueName(ListItem.ClassName);
        ListItem.Parent := TNvBsNavBar(Component).Content.Nav;
      end;
    1:
      begin
        DropDownMenu := TNvBsDropdownMenu.Create(TNvBsNavBar(Component).Owner);
        // DropDownMenu.Name := Designer.UniqueName(DropDownMenu.ClassName);

        ListItemDropdown := TBsNavBarItemDropdown.Create(TNvBsNavBar(Component).Owner,
          DropDownMenu);
        // ListItemDropdown.Name   := Designer.UniqueName(ListItemDropdown.ClassName);
        ListItemDropdown.Parent := TNvBsNavBar(Component).Content.Nav;
      end;
  end;
end;

function TNVBsNavBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add Menu Item Link';
    1: Result := 'Add Menu Item Dropdown';
  end;
end;

function TNVBsNavBarEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TNVBsDropdownMenuEditor }

procedure TNVBsDropdownMenuEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:
      begin
        TNvBsDropDownItemLink                                       //
          .Create(Component.Owner, Component as TNvBsDropdownMenu); //
        // .Name := Designer.UniqueName(TNvBsDropDownItemLink.ClassName);
      end;
  end;
end;

function TNVBsDropdownMenuEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add DropDown Item link';
    // 1: Result := 'Add Menu Item Dropdown';
  end;
end;

function TNVBsDropdownMenuEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TNVBsCardEditor }

procedure TNVBsCardEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0: { TODO -oDelcio -CardEditor : Check if already added Header/Body/Footer }
      with TNvBsCardHeader.Create(Component.Owner) do
        begin
          Parent := Component as TNvBsCard;
          // Name   := Designer.UniqueName(ClassName);
        end;
    1:
      with TNvBsCardBody.Create(Component.Owner) do
        begin
          Parent := Component as TNvBsCard;
          // Name   := Designer.UniqueName(ClassName);
        end;
    2:
      with TNvBsCardFooter.Create(Component.Owner) do
        begin
          Parent := Component as TNvBsCard;
          // Name   := Designer.UniqueName(ClassName);
        end;
  end;
end;

function TNVBsCardEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add Card Header';
    1: Result := 'Add Card Body';
    2: Result := 'Add Card Footer';
  end;
end;

function TNVBsCardEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TNVBsCardBodyEditor }

procedure TNVBsCardBodyEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:
      with TNvBsCardTitle.Create(Component.Owner) do
        begin
          Parent := Component as TNvBsCardBody;
          // Name   := Designer.UniqueName(ClassName);
        end;
    1:
      with TNvBsCardSubtitle.Create(Component.Owner) do
        begin
          Parent := Component as TNvBsCardBody;
          // Name   := Designer.UniqueName(ClassName);
        end;
    2:
      with TNvBsCardText.Create(Component.Owner) do
        begin
          Parent := Component as TNvBsCardBody;
          // Name   := Designer.UniqueName(ClassName);
        end;
    3:
      with TNvBsCardLink.Create(Component.Owner) do
        begin
          Parent := Component as TNvBsCardBody;
          // Name   := Designer.UniqueName(ClassName);
        end;

  end;
end;

function TNVBsCardBodyEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add Card Title';
    1: Result := 'Add Card Subtitle';
    2: Result := 'Add Card Text';
    3: Result := 'Add Card Link';
  end;
end;

function TNVBsCardBodyEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

{ TNVBsTabsEditor }

procedure TNVBsTabsEditor.ExecuteVerb(Index: Integer);
var
  DropDownMenu: TNvBsDropdownMenu;
  _TabControl : TNvBsTabControl;
begin
  inherited;
  case Index of
    0: (Component as TNvBsTabControl).AddNewTab;
    // begin
    // ((Component as TNvBsTabs).Parent as TNvBsTabControl).AddNewTabLink;
    //
    //
    // with TNvBsTabLink.Create(Component.Owner) do
    // begin
    // Parent := Component as TNvBsTabs;
    // // Name   := Designer.UniqueName(ClassName);
    // end;
    // _TabControl := (Component as TNvBsTabs).Parent as TNvBsTabControl;
    //
    // with TNvBsTabPane.Create(Component.Owner) do
    // begin
    // Parent := _TabControl.Content;
    // // Name   := Designer.UniqueName(ClassName);
    // end;
    // end;
    1: (Component as TNvBsTabControl).AddNewTab;
  // begin
  // DropDownMenu      := TNvBsDropdownMenu.Create(Component.Owner);
  // // DropDownMenu.Name := Designer.UniqueName(DropDownMenu.ClassName);
  //
  // with TBsNavBarItemDropdown.Create(Component.Owner, DropDownMenu) do
  // begin
  // // Name   := Designer.UniqueName(ClassName);
  // Parent := DropDownMenu;
  // end;
  //
  // with TNvBsTabDropdown.Create(Component.Owner, DropDownMenu) do
  // begin
  // Parent := Component as TNvBsTabs;
  // //  Name   := Designer.UniqueName(TNvBsDropDownItemLink.ClassName);
  // end;
  // end;
end;
end;

function TNVBsTabsEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add New Tab';
    1: Result := 'Add New Dropdown Tab';
  end;
end;

function TNVBsTabsEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TNvBsAccordionEditor }

procedure TNvBsAccordionEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: (Component as TNvBsAccordion).AddNewItem;
end;
end;

function TNvBsAccordionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add New Item';
  end;
end;

function TNvBsAccordionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TNvColFormatterProperty }

function TNvColFormatterProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TNvColFormatterProperty.GetValue: string;
begin
  if TClass(GetOrdValue) <> nil then
    Result := TClass(GetOrdValue).ClassName;
end;

procedure TNvColFormatterProperty.GetValues(Proc: TGetStrProc);
var
  Formatters: TStrings;
  I         : Integer;
begin
  Formatters := GetColFormatterList;
  for I      := 0 to Formatters.Count - 1 do
    Proc(Formatters[I]);
end;

procedure TNvColFormatterProperty.SetValue(const Value: string);
var
  Formatter : TNvBsFormatterClass;
  Formatters: TStrings;
  I         : Integer;
begin
  Formatters := GetColFormatterList;
  I          := Formatters.IndexOf(Value);
  if I > -1 then
    begin
      Formatter := TNvBsFormatterClass(Formatters.Objects[I]);
      SetOrdValue(LongInt(Formatter));
    end;
end;

end.
