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
// On Linux system.write() will be used.
// On Windows,
//    in default console mode, the Windows API WriteConsoleW will be used (unicode)
//    when output is piped or redirected, normal system.write is used (otherwise no output is generated)
procedure Write   (const ws:string);
procedure Writeln (const ws:string='');


implementation

{$ifdef MSWINDOWS}
uses
  windows, sysutils,
  gnugettext;
{$endif}

{$ifdef MSWINDOWS}
procedure Write(const ws:string);
var
  hOut: THandle;
  charsWritten: DWORD;
  fileType: DWORD;
  isConsole: Boolean;
begin
  hOut := GetStdHandle( STD_OUTPUT_HANDLE );
  if hOut = INVALID_HANDLE_VALUE then Exit;

  // Check if current standard-output is a console
  fileType := GetFileType( hOut );
  if (fileType = FILE_TYPE_CHAR) then
    isConsole := GetConsoleMode(hOut, charsWritten)
  else
    isConsole := False;

  if isConsole then
  begin
    // Direct UTF-16 output
    Windows.WriteConsoleW(hOut, PWideChar(ws), Length(ws), charsWritten, nil);
  end
  else
  begin
    // Fallback for redirected/piped output
    System.Write(ws);
  end;
end;

//function ConWriteW(con:THandle;const outstr:String):Boolean;
//var
//  len,written:Cardinal;
//begin
//  len:=length(outstr);
//  if len>0 then
//    WriteConsoleW(con, PWideChar(@outstr[1]), len, written, nil);
//  result:=written=len;
//end;
//
//function ConWriteA(con:THandle;const outstr:Ansistring):Boolean;
//var
//  len,written:Cardinal;
//begin
//  len:=length(outstr);
//  if len>0 then
//  WriteConsole(con, PChar(@outstr[1]), len, written, nil);
//  result:=written=len;
//end;
//
//var
//  output: THandle;  //cache handle
//
//procedure Write(const ws:string);
//var
//  lang:string;
//  success:boolean;
//begin
//  lang:=lowercase(GetCurrentLocaleName);
//  if (lang='') or (lang='c') or (lang='en') then begin
//    system.write (ws);
//  end
//  else if (output = 0) then
//  begin
//    success:=false;
//    // First, try Unicode output to screen
//    output := CreateFileW('CONOUT$', GENERIC_READ or GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
//    if output = INVALID_HANDLE_VALUE then CloseHandle(output);
//    if output <> INVALID_HANDLE_VALUE then
//    //try
//      success:=ConWriteW(output, ws);
//    //finally
//      //CloseHandle(output);
//    //end;
//    if not success then begin
//      output := CreateFile('CONOUT$', GENERIC_READ or GENERIC_WRITE, FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
//      if output <> INVALID_HANDLE_VALUE then
//      if output = INVALID_HANDLE_VALUE then CloseHandle(output);
//      //try
//        success:=ConWriteW(output, ws);
//      //finally
//        //CloseHandle(output);
//      //end;
//    end;
//    if not success then begin
//      // Output failed or not implemented - using writeln() instead
//      system.write (ws);
//    end;
//  end
//  else
//  begin
//    success:=ConWriteW(output, ws);
//    if not success then
//      // Output failed or not implemented - using writeln() instead
//      system.write (ws);
//  end;
//end;
{$endif}

procedure Writeln (const ws:string='');
begin
  Write(ws+sLinebreak);
end;

end.
