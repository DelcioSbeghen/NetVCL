unit NV.VCL.ActnList;

interface

uses
  Classes, SysUtils, UITypes, {$IFDEF FPC} Controls, actnlist, LCLType, {$ELSE} Actions,{$ENDIF} NV.VCL.Images;

type

  TNvCustomActionList = class({$IFDEF FPC} TCustomActionList {$ELSE} TContainedActionList{$ENDIF})
  private
    // FImageChangeLink: TChangeLink;
    // FImageListLink: TNVImageListLink;
    // procedure ImageListChange(Sender: TObject);
  protected
    // FImageListChanging: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    // procedure SetImageListLink(Value: TNVImageListLink); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // function IsShortCut(var Message: TWMKey): Boolean;
    // property ImageListLink: TNVImageListLink read FImageListLink write SetImageListLink;
  end;

  /// <summary> The usual list of actions (with published properties) in VCL </summary>
  TNvActionList = class(TNvCustomActionList)
  published
    // property ImageListLink;
    property State;
    property OnChange;
    property OnExecute;
    {$IFNDEF FPC}
    property OnStateChange;
    {$ENDIF}
    property OnUpdate;
  end;

  /// <summary> This class is designed to communicate with some of the object in VCL </summary>

  { TNvActionLink }

  TNvActionLink = class({$IFDEF FPC} TControlActionLink {$ELSE} TContainedActionLink{$ENDIF})
  protected
    function IsImageNameLinked: Boolean; virtual;
  //  {$IFDEF FPC}
  //  procedure SetAutoCheck(Value: Boolean); virtual;
  //  procedure SetCaption(const Value: string); virtual;
  //  procedure SetChecked(Value: Boolean); virtual;
  //  procedure SetEnabled(Value: Boolean); virtual;
  //  procedure SetGroupIndex(Value: Integer); virtual;
  //  procedure SetHelpContext(Value: THelpContext); virtual;
  //  procedure SetHelpKeyword(const Value: string); virtual;
  //  procedure SetHelpType(Value: THelpType); virtual;
  //  procedure SetHint(const Value: string); virtual;
  //  procedure SetImageIndex(Value: Integer); virtual;
  //  procedure SetShortCut(Value: TShortCut); virtual;
  //  procedure SetVisible(Value: Boolean); virtual;
  //public
  //  function IsCaptionLinked: Boolean; virtual;
  //  function IsCheckedLinked: Boolean; virtual;
  //  function IsEnabledLinked: Boolean; virtual;
  //  function IsGroupIndexLinked: Boolean; virtual;
  //  function IsHelpContextLinked: Boolean; virtual;
  //  function IsHelpLinked: Boolean; virtual;
  //  function IsHintLinked: Boolean; virtual;
  //  function IsImageIndexLinked: Boolean; virtual;
  //  function IsShortCutLinked: Boolean; virtual;
  //  function IsVisibleLinked: Boolean; virtual;
  //  {$ENDIF}
  end;

  TNvActionLinkClass = class of TNvActionLink;

  /// <summary> List of additional combinations of hot keys in VCL </summary>
  TNvShortCutList = class({$IFDEF FPC} TShortCutList {$ELSE} TCustomShortCutList{$ENDIF})
  public
    function Add(const S: String): Integer; override;
  end;

  /// <summary> The usual action (without published properties) in VCL </summary>

  { TNvCustomAction }

  TNvCustomAction = class({$IFDEF FPC} TCustomAction {$ELSE} TContainedAction {$ENDIF})
  private
    FImageListLink: TNVImageListLink;
    FActionList   : {$IFDEF FPC} TCustomActionList {$ELSE} TContainedActionList{$ENDIF};
    // FImageName: TImageName;
    FImageIndexChanging: Boolean;
    //{$IFDEF FPC}
    //FAutoCheck: Boolean;
    //FCaption: TTranslateString;
    //FChecked: Boolean;
    //FChecking: Boolean;
    //FGrayed: Boolean;
    //FDisableIfNoHandler: Boolean;
    //FEnabled: Boolean;
    //FGroupIndex: Integer;
    //FHelpContext: THelpContext;
    //FHelpKeyword: string;
    //FHelpType: THelpType;
    //FHint: TTranslateString;
    //FSecondaryShortCuts: TShortCutList;// nil as default
    //FShortCut: TShortCut;
    //FVisible: Boolean;
    //FOnHint: THintEvent;
    //procedure SetAutoCheck(Value: Boolean);
    //procedure SetCaption(const Value: TTranslateString);
    //procedure SetChecked(Value: Boolean);
    //procedure SetEnabled(Value: Boolean);
    //procedure SetGroupIndex(const Value: Integer);
    //procedure SetHelpContext(Value: THelpContext); virtual;
    //procedure SetHelpKeyword(const Value: string); virtual;
    //procedure SetHelpType(Value: THelpType);
    //procedure SetHint(const Value: TTranslateString);
    //procedure SetShortCut(Value: TShortCut);
    //procedure SetVisible(Value: Boolean);
    //function GetSecondaryShortCuts: TShortCutList;
    //procedure SetSecondaryShortCuts(const Value: TShortCutList);
    //function IsSecondaryShortCutsStored: Boolean;
    //{$ENDIF}
    // procedure SetImageName(const Value: TImageName);
    function GetCustomActionList: TNvCustomActionList; inline;
    procedure SetCustomActionList(const Value: TNvCustomActionList); inline;
  protected
    // FImage: TObject;
    // FMask: TObject;
    procedure AssignTo(Dest: TPersistent); override;
    // procedure SetImageIndex(Value: System.UITypes.TImageIndex); override;
    procedure Change; override;
    // function GetImages: TCustomImageList; virtual;
    procedure SetImageListLink(Value: TNVImageListLink); virtual;
    procedure Loaded; override;
    protected
    {$IFDEF FPC}
    //property AutoCheck: Boolean read FAutoCheck write SetAutoCheck default False;
    //property Checked: Boolean read FChecked write SetChecked default False;
    //property Grayed: Boolean read FGrayed write FGrayed;
    //property DisableIfNoHandler: Boolean read FDisableIfNoHandler
    //                                    write FDisableIfNoHandler default False;
    //property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    //property HelpContext: THelpContext
    //                           read FHelpContext write SetHelpContext default 0;
    //property HelpKeyword: string read FHelpKeyword write SetHelpKeyword;
    //property HelpType: THelpType
    //                         read FHelpType write SetHelpType default htContext;
    //property Hint: TTranslateString read FHint write SetHint;
    //property SecondaryShortCuts: TShortCutList read GetSecondaryShortCuts
    //              write SetSecondaryShortCuts stored IsSecondaryShortCutsStored;
    //property ShortCut: TShortCut read FShortCut write SetShortCut default 0;
    //property Visible: Boolean read FVisible write SetVisible default True;
    //property OnHint: THintEvent read FOnHint write FOnHint;
   {$ELSE}
   function CreateShortCutList:  TCustomShortCutList; override;
   {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    function Update: Boolean; override;
    property ImageListLink: TNVImageListLink read FImageListLink write SetImageListLink;
    // property ImageName: TImageName read FImageName write SetImageName;
    property ActionList: TNvCustomActionList read GetCustomActionList write SetCustomActionList;
   // {$IFDEF FPC}
   // property Caption: TTranslateString read FCaption write SetCaption;
   // property Enabled: Boolean read FEnabled write SetEnabled default True;
   //{$ENDIF}
  end;

  /// <summary> The usual action (with published properties) in VCL </summary>
  TNvAction = class(TNvCustomAction)
  private
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCheck;
    property Caption;
    property Checked;
    property Enabled;
    property GroupIndex;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    // property ImageIndex;
    // property ImageName;
    property ImageListLink;
    property SecondaryShortCuts;
    property ShortCut default 0;
    property Visible;
    property OnExecute;
    property OnHint;
    property OnUpdate;
  end;

implementation

uses
  Windows, Nv.VCL.Forms, NV.Controls;

{ TCustomVCLActionList }

procedure TNvCustomActionList.Change;
begin
  inherited;
  if {$IFNDEF FPC}ActionsCreated and {$ENDIF} //
  (csDesigning in ComponentState) then
    begin
      if (Owner is TNVForm) and (TNVForm(Owner).Designer <> nil) then
        TNVForm(Owner).Designer.Modified;
    end;
end;

constructor TNvCustomActionList.Create(AOwner: TComponent);
begin
  inherited;
  // FImageChangeLink := TChangeLink.Create;
  // FImageChangeLink.OnChange := ImageListChange;
end;

destructor TNvCustomActionList.Destroy;
begin
  // FreeAndNil(FImageChangeLink);
  inherited;
end;

// procedure TNvCustomActionList.ImageListChange(Sender: TObject);
// begin
// if Sender = Images then
// begin
// FImageListChanging := True;
// try
// Change;
// finally
// FImageListChanging := False;
// end;
// end;
// end;

// {$IFDEF CLR}[UIPermission(SecurityAction.LinkDemand, Window=UIPermissionWindow.AllWindows)]{$ENDIF}
// function TNvCustomActionList.IsShortCut(var Message: TWMKey): Boolean;
// var
// I: Integer;
// ShortCut: TShortCut;
// ShiftState: TShiftState;
// Action: TContainedAction;
// CustAction: TNvCustomAction;
// begin
// {$IF NOT DEFINED(CLR)}
// Result := False;
// if Vcl.Menus.IsAltGRPressed then Exit;
// {$ENDIF}
// ShiftState := KeyDataToShiftState(Message.KeyData);
// ShortCut := Vcl.Menus.ShortCut(Message.CharCode, ShiftState);
// if ShortCut <> scNone then
// for I := 0 to ActionCount - 1 do
// begin
// Action := Actions[I];
// if Action is TNvCustomAction then
// begin
// CustAction := TNvCustomAction(Action);
// if (CustAction.ShortCut = ShortCut) or (Assigned(CustAction.SecondaryShortCuts) and
// (CustAction.SecondaryShortCuts.IndexOfShortCut(ShortCut) <> -1)) then
// begin
// Result := CustAction.HandleShortCut;
// Exit;
// end;
// end;
// end;
// {$IF DEFINED(CLR)}
// Result := False;
// {$ENDIF}
// end;

procedure TNvCustomActionList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  // if Operation = opRemove then
  // if AComponent = Images then
  // Images := nil;
end;

// procedure TNvCustomActionList.SetImages(Value: TCustomImageList);
// begin
// if Images <> nil then
// Images.UnRegisterChanges(FImageChangeLink);
// FImages := Value;
// if Images <> nil then
// begin
// Images.RegisterChanges(FImageChangeLink);
// Images.FreeNotification(Self);
// end;
// end;

{ TNvShortCutList }

function TNvShortCutList.Add(const S: String): Integer;
begin
  Result := inherited Add(S);
  // Objects[Result] := TObject(TextToShortCut(S));
end;

{ TNvActionLink }
function TNvActionLink.IsImageNameLinked: Boolean;
begin
  Result := Action is TNvCustomAction;
end;

//{$IFDEF FPC}
//procedure TNvActionLink.SetAutoCheck(Value: Boolean);
//begin
//
//end;
//
//procedure TNvActionLink.SetCaption(const Value: string);
//begin
//
//end;
//
//procedure TNvActionLink.SetChecked(Value: Boolean);
//begin
//
//end;
//
//procedure TNvActionLink.SetEnabled(Value: Boolean);
//begin
//
//end;
//
//procedure TNvActionLink.SetGroupIndex(Value: Integer);
//begin
//
//end;
//
//procedure TNvActionLink.SetHelpContext(Value: THelpContext);
//begin
//
//end;
//
//procedure TNvActionLink.SetHelpKeyword(const Value: string);
//begin
//
//end;
//
//procedure TNvActionLink.SetHelpType(Value: THelpType);
//begin
//
//end;
//
//procedure TNvActionLink.SetHint(const Value: string);
//begin
//
//end;
//
//procedure TNvActionLink.SetImageIndex(Value: Integer);
//begin
//
//end;
//
//procedure TNvActionLink.SetShortCut(Value: TShortCut);
//begin
//
//end;
//
//procedure TNvActionLink.SetVisible(Value: Boolean);
//begin
//
//end;
//
//function TNvActionLink.IsCaptionLinked: Boolean;
//begin
// Result := Action is TCustomAction;
//end;
//
//function TNvActionLink.IsCheckedLinked: Boolean;
//begin
//  Result := Action is TCustomAction;
//end;
//
//function TNvActionLink.IsEnabledLinked: Boolean;
//begin
//  Result := Action is TCustomAction;
//end;
//
//function TNvActionLink.IsGroupIndexLinked: Boolean;
//begin
//  Result := Action is TCustomAction;
//end;
//
//function TNvActionLink.IsHelpContextLinked: Boolean;
//begin
//  Result := Action is TCustomAction;
//end;
//
//function TNvActionLink.IsHelpLinked: Boolean;
//begin
//  Result := Action is TCustomAction;
//end;
//
//function TNvActionLink.IsHintLinked: Boolean;
//begin
// Result := Action is TCustomAction;
//end;
//
//function TNvActionLink.IsImageIndexLinked: Boolean;
//begin
//  Result := Action is TCustomAction;
//end;
//
//function TNvActionLink.IsShortCutLinked: Boolean;
//begin
//  Result := Action is TCustomAction;
//end;
//
//function TNvActionLink.IsVisibleLinked: Boolean;
//begin
//  Result := Action is TCustomAction;
//end;
//{$ENDIF}

{ TNvCustomAction }

constructor TNvCustomAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageListLink:= TNVImageListLink.Create(nil);
end;

{$IFDEF FPC}
//procedure TNvCustomAction.SetAutoCheck(Value: Boolean);
//var
//  I: Integer;
//begin
//  if Value = FAutoCheck then exit;
//  for I := 0 to FClients.Count - 1 do
//    TNvActionLink(FClients[I]).SetAutoCheck(Value);
//  FAutoCheck := Value;
//  Change;
//end;
//
//procedure TNvCustomAction.SetCaption(const Value: TTranslateString);
//var
//  I: Integer;
//begin
//  if Value = FCaption then exit;
//  for I := 0 to FClients.Count - 1 do
//    TNvActionLink(FClients[I]).SetCaption(Value);
//  FCaption := Value;
//  Change;
//end;
//
//procedure TNvCustomAction.SetChecked(Value: Boolean);
//var
//  I: Integer;
//  Action: TContainedAction;
//begin
//  if FChecking then exit;
//  if (Value=FChecked) and not FGrayed then exit;
//  FChecking := True;
//  try
//    for I := 0 to FClients.Count - 1 do
//      TNvActionLink(FClients[I]).SetChecked(Value);
//    FChecked := Value;
//    if (FGroupIndex > 0) and FChecked then
//      for I := 0 to ActionList.ActionCount - 1 do
//      begin
//        Action := ActionList[I];
//        if (Action <> Self)
//        and (Action is TNvCustomAction)
//        and (TNvCustomAction(Action).FGroupIndex = FGroupIndex) then
//          TCustomAction(Action).Checked := False;
//      end;
//    Change;
//  finally
//    FChecking := False;
//  end;
//end;
//
//procedure TNvCustomAction.SetEnabled(Value: Boolean);
//var
//  I: Integer;
//begin
//  if Value = FEnabled then exit;
//  if ActionList<>nil then
//  begin
//    if ActionList.State = asSuspended then
//    begin
//      FEnabled := Value;
//      exit;
//    end;
//    if ActionList.State = asSuspendedEnabled then
//    begin
//      // enable for Delphi compatibility
//      Value := True;
//    end;
//  end;
//  for I := 0 to FClients.Count - 1 do
//    TNvActionLink(FClients[I]).SetEnabled(Value);
//  FEnabled := Value;
//  Change;
//end;
//
//procedure TNvCustomAction.SetGroupIndex(const Value: Integer);
//var
//  I: Integer;
//begin
//  if Value = FGroupIndex then exit;
//  FGroupIndex := Value;
//  for I := 0 to FClients.Count - 1 do
//    TNvActionLink(FClients[I]).SetGroupIndex(Value);
//  Change;
//end;
//
//procedure TNvCustomAction.SetHelpContext(Value: THelpContext);
//var
//  I: Integer;
//begin
//  if Value = FHelpContext then exit;
//  for I := 0 to FClients.Count - 1 do
//    TNvActionLink(FClients[I]).SetHelpContext(Value);
//  FHelpContext := Value;
//  Change;
//end;
//
//procedure TNvCustomAction.SetHelpKeyword(const Value: string);
//var
//  I: Integer;
//begin
//  if Value = FHelpKeyword then exit;
//  for I := 0 to FClients.Count -1 do
//    TNvActionLink(FClients[I]).SetHelpKeyword(Value);
//  FHelpKeyword := Value;
//  Change;
//end;
//
//procedure TNvCustomAction.SetHelpType(Value: THelpType);
//var
//  I: Integer;
//begin
//  if Value = FHelpType then exit;
//  for I := 0 to FClients.Count -1 do
//    TNvActionLink(FClients[I]).SetHelpType(Value);
//  FHelpType := Value;
//  Change;
//end;
//
//procedure TNvCustomAction.SetHint(const Value: TTranslateString);
//var
//  I: Integer;
//begin
//  if Value = FHint then exit;
//  for I := 0 to FClients.Count - 1 do
//    TNvActionLink(FClients[I]).SetHint(Value);
//  FHint := Value;
//  Change;
//end;
//
//procedure TNvCustomAction.SetShortCut(Value: TShortCut);
//var
//  I: Integer;
//begin
//  if Value = FShortCut then exit;
//  for I := 0 to FClients.Count - 1 do
//    TNvActionLink(FClients[I]).SetShortCut(Value);
//  FShortCut := Value;
//  Change;
//end;
//
//procedure TNvCustomAction.SetVisible(Value: Boolean);
//var
//  I: Integer;
//begin
//  if Value = FVisible then exit;
//  for I := 0 to FClients.Count - 1 do
//    TNvActionLink(FClients[I]).SetVisible(Value);
//  FVisible := Value;
//  Change;
//end;
//
//function TNvCustomAction.GetSecondaryShortCuts: TShortCutList;
//begin
//  if FSecondaryShortCuts = nil then
//    FSecondaryShortCuts := TShortCutList.Create;
//  Result := FSecondaryShortCuts;
//end;
//
//procedure TNvCustomAction.SetSecondaryShortCuts(const Value: TShortCutList);
//begin
//  if FSecondaryShortCuts = nil then begin
//    if (Value=nil) or (Value.Count=0) then exit;
//    FSecondaryShortCuts := TShortCutList.Create;
//  end;
//  FSecondaryShortCuts.Assign(Value);
//end;
//
//function TNvCustomAction.IsSecondaryShortCutsStored: Boolean;
//begin
//  Result := Assigned(FSecondaryShortCuts) and (FSecondaryShortCuts.Count > 0);
//end;
//
{$ELSE}
function TNvCustomAction.CreateShortCutList: TCustomShortCutList;
begin
  Result := TNvShortCutList.Create;
end;
{$ENDIF}

destructor TNvCustomAction.Destroy;
begin
  // FImage.Free;
  // FMask.Free;
  FImageListLink.Free;
  inherited Destroy;
end;

procedure TNvCustomAction.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TNvCustomAction then
    begin
      TNvCustomAction(Dest).FImageListLink.Assign(FImageListLink);
    end;
end;

function TNvCustomAction.Execute: Boolean;
begin
  Result := False;
  {$IFNDEF FPC}
  if Suspended then
    exit;
  {$ENDIF}
  Update;
  if Enabled and AutoCheck then
    if not Checked or Checked and (GroupIndex = 0) then
      Checked := not Checked;
  Result      := Enabled;
  if Result then
    begin
{$IF DEFINED(CLR)}
      Result := ((ActionList <> nil) and ActionList.ExecuteAction(Self)) or
        (Application.ExecuteAction(Self)) or (inherited Execute);
      if (not Result) and (Assigned(Application)) then
        Result := Application.DispatchAction(True, self, False);
{$ELSE}
      Result := ((ActionList <> nil) and ActionList.ExecuteAction(Self)) or
        (Application.ExecuteAction(Self)) or (inherited Execute) { or
        (SendAppMessage(CM_ACTIONEXECUTE, 0, LPARAM(Self)) = 1) };
{$ENDIF}
    end;
end;

function TNvCustomAction.GetCustomActionList: TNvCustomActionList;
begin
  Result := TNvCustomActionList(inherited ActionList);
end;

// procedure TNvCustomAction.SetImageIndex(Value: System.UITypes.TImageIndex);
// begin
// if not FImageIndexChanging and (Images <> nil) and Images.IsImageNameAvailable then
// FImageName := Images.GetNameByIndex(Value);
// inherited;
// end;

// procedure TNvCustomAction.SetImageName(const Value: TImageName);
// begin
// if FImageName <> Value then
// begin
// FImageName := Value;
// if (Images <> nil) and Images.IsImageNameAvailable then
// begin
// FImageIndexChanging := True;
// try
// ImageIndex := Images.GetIndexByName(FImageName);
// finally
// FImageIndexChanging := False;
// end;
// end;
// end;
// end;

procedure TNvCustomAction.Loaded;
begin
  inherited;
  // if (Images <> nil) and Images.IsImageNameAvailable and (ImageIndex >= 0) and (FImageName = '') then
  // FImageName := Images.GetNameByIndex(ImageIndex);
end;

type
  TCustomActionListClass = class(TNvCustomActionList);

procedure TNvCustomAction.Change;
// var
// LIndex: Integer;
begin
  // if (ActionList <> nil) and (TCustomActionListClass(ActionList).FImageListChanging) and
  // (Images <> nil) and Images.IsImageNameAvailable and (FImageName <> '') then
  // begin
  // LIndex := Images.GetIndexByName(FImageName);
  // if LIndex <> ImageIndex then
  // begin
  // FImageIndexChanging := True;
  // try
  // ImageIndex := LIndex;
  // finally
  // FImageIndexChanging := False;
  // end;
  // end
  // else
  // inherited;
  // end
  // else
  inherited;
end;

procedure TNvCustomAction.SetCustomActionList(const Value: TNvCustomActionList);
begin
  inherited ActionList := Value;
end;

procedure TNvCustomAction.SetImageListLink(Value: TNVImageListLink);
begin
  if FImageListLink <> Value then
     FImageListLink.Assign(Value);
end;

// function TNvCustomAction.GetImages: TCustomImageList;
// begin
// if ActionList <> nil then
// Result := ActionList.Images
// else
// Result := nil;
// end;

function TNvCustomAction.Update: Boolean;
begin
{$IF DEFINED(CLR)}
  Result := (ActionList <> nil) and ActionList.UpdateAction(Self) or Application.UpdateAction(Self)
    or inherited Update;
  if not Result then
    if Assigned(Application) then
      Result := Application.DispatchAction(False, self, False);
{$ELSE}
  Result := (ActionList <> nil) and ActionList.UpdateAction(Self) or Application.UpdateAction(Self)
    or inherited Update { or
    (SendAppMessage(CM_ACTIONUPDATE, 0, LPARAM(Self)) = 1) };
{$ENDIF}
end;

{ TNvAction }

constructor TNvAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisableIfNoHandler := True;
end;

{$IF NOT DEFINED(CLR)}

//initialization
//
//StartClassGroup(TNvControl);
//ActivateClassGroup(TNvControl);
//GroupDescendentsWith(TNvCustomActionList, TNvControl);
//GroupDescendentsWith(TNvCustomAction, TNvControl);
////
//StartClassGroup(TNvWinControl);
//ActivateClassGroup(TNvWinControl);
//GroupDescendentsWith(TNvCustomActionList, TNvWinControl);
//GroupDescendentsWith(TNvCustomAction, TNvWinControl);

{$ENDIF}

end.
