unit NV.VCL.ActnList;

interface

uses
  Classes, SysUtils, UITypes, Actions, NV.VCL.Images;

type

  TNvCustomActionList = class(TContainedActionList)
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
    property OnStateChange;
    property OnUpdate;
  end;

  /// <summary> This class is designed to communicate with some of the object in VCL </summary>
  TNvActionLink = class(TContainedActionLink)
  protected
    function IsImageNameLinked: Boolean; virtual;
  end;

  TNvActionLinkClass = class of TNvActionLink;

  /// <summary> List of additional combinations of hot keys in VCL </summary>
  TNvShortCutList = class(TCustomShortCutList)
  public
    function Add(const S: String): Integer; override;
  end;

  /// <summary> The usual action (without published properties) in VCL </summary>
  TNvCustomAction = class(TContainedAction)
  private
    FImageListLink: TNVImageListLink;
    FActionList   : TContainedActionList;
    // FImageName: TImageName;
    FImageIndexChanging: Boolean;
    // procedure SetImageName(const Value: TImageName);
    function GetCustomActionList: TNvCustomActionList; inline;
    procedure SetCustomActionList(const Value: TNvCustomActionList); inline;
  protected
    // FImage: TObject;
    // FMask: TObject;
    procedure AssignTo(Dest: TPersistent); override;
    function CreateShortCutList: TCustomShortCutList; override;
    // procedure SetImageIndex(Value: System.UITypes.TImageIndex); override;
    procedure Change; override;
    // function GetImages: TCustomImageList; virtual;
    procedure SetImageListLink(Value: TNVImageListLink); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    function Update: Boolean; override;
    property ImageListLink: TNVImageListLink read FImageListLink write SetImageListLink;
    // property ImageName: TImageName read FImageName write SetImageName;
    property ActionList: TNvCustomActionList read GetCustomActionList write SetCustomActionList;

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
  if ActionsCreated and (csDesigning in ComponentState) then
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

{ TNvCustomAction }

constructor TNvCustomAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageListLink:= TNVImageListLink.Create(nil);
end;

function TNvCustomAction.CreateShortCutList: TCustomShortCutList;
begin
  Result := TNvShortCutList.Create;
end;

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
  if Suspended then
    exit;
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
