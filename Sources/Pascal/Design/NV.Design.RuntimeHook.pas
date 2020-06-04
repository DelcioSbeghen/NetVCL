unit NV.Design.RuntimeHook;

interface
  uses
  ToolsAPI;


implementation


function _GetDesignExename:string;
begin
  Result:= getActiveProject.ProjectOptions.TargetName;
end;




initialization
  //  GetDesignExename:= _GetDesignExename;

finalization
  //  GetDesignExename:= nil;


end.
