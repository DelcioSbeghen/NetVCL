unit NV.Messages;

interface

type
  TNVMessage = class
  public
    Msg: Cardinal;
    Obj: Pointer;
  end;

const
  DM_REQUEST = 1; // pass TNVAppRequestTask on TNVMessage.Obj
  DM_INVALIDATE = 2; // pass Control on TNVMessage.Obj
  DM_UPDATE_LANG = 3; // Update DWApplication language

implementation

end.

