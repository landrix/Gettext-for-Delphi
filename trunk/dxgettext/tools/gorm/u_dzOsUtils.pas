///<summary> This is an extract from u_dzOsUtils which is part of dzlib available from sourceforge
///          under the MPL 1.1 </summary>
unit u_dzOsUtils;

interface

function ShellExecEx(const Filename: string; const Parameters: string;
  const Verb: string; CmdShow: Integer; _ShowAssociateDialog: Boolean = False): Boolean;

implementation

uses
  ShellAPI;

function ShellExecEx(const Filename: string; const Parameters: string;
  const Verb: string; CmdShow: Integer; _ShowAssociateDialog: Boolean = False): Boolean;
var
  Sei: TShellExecuteInfo;
begin
  FillChar(Sei, SizeOf(Sei), #0);
  Sei.cbSize := SizeOf(Sei);
  Sei.FMask := SEE_MASK_DOENVSUBST;
  if not _ShowAssociateDialog then
    Sei.FMask := Sei.FMask or SEE_MASK_FLAG_NO_UI;
  Sei.lpFile := PChar(Filename);
  if Parameters <> '' then
    Sei.lpParameters := PChar(Parameters)
  else
    Sei.lpParameters := nil;
  if Verb <> '' then
    Sei.lpVerb := PChar(Verb)
  else
    Sei.lpVerb := nil;
  Sei.nShow := CmdShow;
  Result := ShellExecuteEx(@Sei);
end;

end.
