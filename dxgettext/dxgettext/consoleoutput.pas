unit consoleoutput;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl                          *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dxgettext.po.dk/ for more information          *)
(*                                                            *)
(**************************************************************)

interface

// This writes to standard console output.
// On Linux, and if no language is set, system.write() will be used.
// Otherwise, the Windows API will be used, with Unicode if possible
procedure Write (const ws:string);
procedure Writeln (const ws:string='');



implementation

{$ifdef MSWINDOWS}
uses
  windows, sysutils,
  gnugettext;
{$endif}

{$ifdef MSWINDOWS}
function ConWriteW(con:THandle;const outstr:String):Boolean;
var
  len,written:Cardinal;
begin
  len:=length(outstr);
  if len>0 then
    WriteConsoleW(con, PWideChar(@outstr[1]), len, written, nil);
  result:=written=len;
end;

function ConWriteA(con:THandle;const outstr:Ansistring):Boolean;
var
  len,written:Cardinal;
begin
  len:=length(outstr);
  if len>0 then
  WriteConsole(con, PChar(@outstr[1]), len, written, nil);
  result:=written=len;
end;

var
  output: THandle;  //cache handle

procedure Write(const ws:string);
var
  lang:string;
  success:boolean;
begin
  lang:=lowercase(GetCurrentLanguage);
  if (lang='') or (lang='c') or (lang='en') then begin
    system.write (ws);
  end
  else if (output = 0) then
  begin
    success:=false;
    // First, try Unicode output to screen
    output := CreateFileW('CONOUT$', GENERIC_READ or GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if output = INVALID_HANDLE_VALUE then CloseHandle(output);
    if output <> INVALID_HANDLE_VALUE then
    //try
      success:=ConWriteW(output, ws);
    //finally
      //CloseHandle(output);
    //end;
    if not success then begin
      output := CreateFile('CONOUT$', GENERIC_READ or GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
      if output <> INVALID_HANDLE_VALUE then
      if output = INVALID_HANDLE_VALUE then CloseHandle(output);
      //try
        success:=ConWriteW(output, ws);
      //finally
        //CloseHandle(output);
      //end;
    end;
    if not success then begin
      // Output failed or not implemented - using writeln() instead
      system.write (ws);
    end;
  end
  else
  begin
    success:=ConWriteW(output, ws);
    if not success then
      // Output failed or not implemented - using writeln() instead
      system.write (ws);
  end;
end;
{$endif}

procedure Writeln (const ws:string='');
begin
  Write(ws+sLinebreak);
end;

end.
