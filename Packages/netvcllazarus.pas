{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NetVCLLazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  NV.Ajax, NV.Cache, NV.Classes, NV.Desktop, NV.Dispatcher, 
  NV.DispatcherDesign, NV.DispatcherPage, NV.HttpServer, 
  NV.HttpServerWebsockets, NV.Interfaces, NV.MergeSort, NV.Messages, 
  NV.Request.Exe, NV.Request.Http, NV.Router, NV.Types, NV.UserSession, 
  NV.Utils, NV.BS.Accordions, NV.BS.Alerts, NV.BS.Badges, NV.BS.Buttons, 
  NV.BS.Cards, NV.BS.Containers, NV.BS.Controls, NV.BS.DBCtrls, 
  NV.BS.HtmlControls, NV.BS.Inputs, NV.BS.Navbar, NV.BS.Register, 
  NV.BS.ScrollFocus, NV.BS.Tables, NV.BS.Tabs, NV.BS.Types, NV.Json, 
  NV.Languages, NV.Request, NV.Design.ActionEditor, NV.Design.CopyFiles, 
  NV.Design.ImageIndexEditor, NV.Design.ImagelistEditor, 
  NV.Design.JsonArrayEditor, NV.Controls, NV.VCL.ActnList, NV.VCL.Charts, 
  NV.VCL.Dashboards, NV.VCL.DBCtrls, NV.VCL.Dialogs, NV.VCL.Forms, 
  NV.VCL.Frame, NV.VCL.Graphics, NV.VCL.Images, NV.VCL.Page, 
  NV.Design.Lazarus.Register, NV.Design.Lazarus.Designer, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('NV.BS.Register', @NV.BS.Register.Register);
  RegisterUnit('NV.Design.Lazarus.Register', 
    @NV.Design.Lazarus.Register.Register);
end;

initialization
  RegisterPackage('NetVCLLazarus', @Register);
end.
