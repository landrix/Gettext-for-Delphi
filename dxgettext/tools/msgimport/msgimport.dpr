{$APPTYPE CONSOLE}
program msgmkignore;

uses
  SysUtils,
  gnugettext in '..\..\sample\gnugettext.pas',
  poparser in '..\..\dxgettext\poparser.pas',
  appconsts in '..\..\dxgettext\appconsts.pas',
  consoleoutput in '..\..\dxgettext\consoleoutput.pas',
  u_dzQuicksort in '..\..\dxgettext\u_dzQuicksort.pas',
  assembleengine in '..\assemble\assembleengine.pas',
  consoleapp in 'consoleapp.pas',
  msgimportengine in 'msgimportengine.pas',
  ConsoleAppHandler in '..\..\dxgettext\ConsoleAppHandler.pas',
  xgettexttools in '..\..\dxgettext\xgettexttools.pas';

var
  conapp:TConsoleApp;
begin
  try
    textdomain('dxgettext');
    AddDomainForResourceString('delphi');
    AddDomainForResourceString('kylix');
    conapp:=TConsoleApp.Create;
    try
      conapp.Execute;
    finally
      FreeAndNil (conapp);
    end;
  except
    on e:Exception do begin
      writeln (e.Message);
      ExitCode:=1;
    end;
  end;
  if DebugHook <> 0 then begin
    Write('press <enter>');
    Readln;
  end;
end.
