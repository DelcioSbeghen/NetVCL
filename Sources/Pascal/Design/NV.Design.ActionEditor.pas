unit NV.Design.ActionEditor;

interface

uses
  DesignEditors, SysUtils,
  Classes, Actions, ActionEditors, Vcl.ImgList, DesignIntf;

type
  TNvActionListEditor = class(TComponentEditor)
  private
    //procedure ShowActionListEditor;
    procedure AddNewAction;
   // procedure ShowActionListEditor;
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
  end;


   TNvActionProperty = class(TComponentProperty)
//  private type
//    TRegisteredAction = record
//      Category: string;
//      ActionClass: TBasicActionClass
//    end;
  private
  //  FActionListView: TActionListView;
   // FHost: IPropertyHost;
  //  FRegisteredActionList: TList<TRegisteredAction>;
   // procedure CreateNewAction(Sender: TObject; const Category: string;
  //    ActionClass: TContainedActionClass; ActionList: TContainedActionList);
  //  procedure SelectAction(Sender: TObject; Action: TContainedAction);
  // procedure AddAction(const Category: string; ActionClass: TBasicActionClass; Info: Pointer);
  public
    //destructor Destroy; override;
    // IProperty80
    //procedure Edit(const Host: IPropertyHost; DblClick: Boolean); reintroduce; overload;
    //function GetAttributes: TPropertyAttributes; override;
  end;



  // interface
  //
  // uses
  // Types, SysUtils, Classes, Graphics, ActnList, dxActions, cxGraphics, cxControls, dxUIGenerator,
  //
  // ImgList;
  //
  // {$IFDEF DELPHIXE3}
  // type
  //
  // {$IFDEF DELPHIXE8}
  // TNvIDEActionsImageLink = TObject;
  // {$ELSE}
  // TNvIDEActionsImageLink = TChangeLink;
  // {$ENDIF}

  { TNvIDEActions }

  TNvIDEActions = class(TIDEActions)
  public
    class var OldIDEActionsClass: TIDEActionsClass;
    class procedure AssignAction(Source, Destination: TBasicAction); override;
    class function BaseActionClass: TContainedActionClass; override;
    class function BaseActionListClass: TContainedActionListClass; override;
    class procedure CopyImageIfAvailable(const NewAction: TContainedAction;
      const ActionList: TContainedActionList); override;
    class function CreateImageList(ActionList: TContainedActionList): TCustomImageList; override;
    class function DefaultActionClass: TContainedActionClass; override;
    class procedure RegisterImageLink(const ActionList: TContainedActionList;
       const ImageLink: TObject); override;
    class procedure UnregisterImageLink(const ActionList: TContainedActionList;
       const ImageLink: TObject); override;
  end;

implementation

uses
  Actnedit, NV.Design.IOTAUtils, NV.VCL.ActnList;

procedure TNvActionListEditor.AddNewAction;
var
  act:TNvAction;
begin
  act:= TNvAction.Create(Component.Owner{Form});
  act.ActionList:= Component as TNvActionList;
end;

procedure TNvActionListEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
   // 0: ShowActionListEditor;
    0: AddNewAction;
  else raise ENotImplemented.Create('TNvActionListEditor has only one verb (index = 0) supported.');
  end;
end;

function TNvActionListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TNvActionListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    //0: Result := 'Show ActionList Editor';
    0: Result := 'Add New Action';
  else raise ENotImplemented.Create('TNvActionListEditor has only one verb (index = 0) supported.');
  end;
end;

//procedure TNvActionListEditor.ShowActionListEditor;
//begin
//  Actnedit.ShowActionListDesigner(Designer, Component as TContainedActionList);
//end;

//type
 // TdxBasicActionAccess = class(TNvCustomAction);


  { TNvIDEActions }

class procedure TNvIDEActions.AssignAction(Source, Destination: TBasicAction);
// var
// ABitmap: TcxBitmap32;
// ADestination: TdxBasicActionAccess;
// AImages: TcxImageList;
// ASource: TNvCustomAction;
begin
  OldIDEActionsClass.AssignAction(Source, Destination);
  //
  // if (Source is TNvCustomAction) and (Destination is TNvCustomAction) then
  // begin
  // ASource := TdxBasicActionAccess(Source);
  // if not (ASource.Images is TcxImageList) then
  // Exit;
  // ADestination := TdxBasicActionAccess(Destination);
  // end
  // else
  // Exit;
  //
  // AImages := TcxImageList(ASource.Images);
  // if IsImageAssigned(AImages, ASource.ImageIndex) then
  // begin
  // FreeAndNil(ADestination.FImage);
  // FreeAndNil(ADestination.FMask);
  //
  // ABitmap := TcxBitmap32.CreateSize(16, 16);
  // AImages.GetImage(ASource.ImageIndex, ABitmap);
  // ADestination.FImage := ABitmap;
  //
  // end;
end;

class function TNvIDEActions.BaseActionClass: TContainedActionClass;
begin
  if IsNvFormModule then
    Result := TNvCustomAction
  else
    Result := OldIDEActionsClass.BaseActionClass;
end;

class function TNvIDEActions.BaseActionListClass: TContainedActionListClass;
begin
  if IsNvFormModule then
    Result := TNvCustomActionList
  else
    Result := OldIDEActionsClass.BaseActionListClass;
end;

class procedure TNvIDEActions.CopyImageIfAvailable(const NewAction: TContainedAction;
  const ActionList: TContainedActionList);
var
 // AAction: TdxBasicActionAccess;
  AImages: TCustomImageList;
begin
  if NewAction is TNvCustomAction then
    //
  else
    OldIDEActionsClass.CopyImageIfAvailable(NewAction, ActionList);
end;

class function TNvIDEActions.CreateImageList(ActionList: TContainedActionList): TCustomImageList;
begin
  if ActionList is TNvCustomActionList then
    //
  else
    Result := OldIDEActionsClass.CreateImageList(ActionList);
end;

class function TNvIDEActions.DefaultActionClass: TContainedActionClass;
begin
  if IsNvFormModule then
    Result := TNvAction
  else
    Result := OldIDEActionsClass.DefaultActionClass;
end;

class procedure TNvIDEActions.RegisterImageLink(const ActionList: TContainedActionList;
   const ImageLink: TObject);
begin
  if ActionList is TNvCustomActionList then
    //ImageLink:= nil
  else
  OldIDEActionsClass.RegisterImageLink(ActionList, ImageLink);
end;

class procedure TNvIDEActions.UnregisterImageLink(const ActionList: TContainedActionList;
   const ImageLink: TObject);
begin
  if ActionList is TNvCustomActionList then
    //ImageLink:= nil
  else
  OldIDEActionsClass.UnregisterImageLink(ActionList, ImageLink);
end;

//initialization
//
//if TNvIDEActions.OldIDEActionsClass = nil then
//  begin
//    TNvIDEActions.OldIDEActionsClass := GetIDEActions('VCL');
//    if (TNvIDEActions.OldIDEActionsClass <> nil) and
//      (TNvIDEActions.OldIDEActionsClass <> TNvIDEActions) then
//      begin
//        UnregisterActionsInFramework('VCL');
//        RegisterActionsInFramework('VCL', TNvIDEActions);
//      end
//    else
//      TNvIDEActions.OldIDEActionsClass := nil;
//  end;
//
//finalization
//
//if TNvIDEActions.OldIDEActionsClass <> nil then
//  begin
//    UnregisterActionsInFramework('VCL');
//    RegisterActionsInFramework('VCL', TNvIDEActions.OldIDEActionsClass);
//    TNvIDEActions.OldIDEActionsClass := nil;
//  end;


end.
