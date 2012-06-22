{$APPTYPE CONSOLE}
program msgmkignore;

uses
  SysUtils,
  {$ifdef mswindows}
  gnugettext in '..\..\sample\gnugettext.pas',
  poparser in '..\..\dxgettext\poparser.pas',
  appconsts in '..\..\dxgettext\appconsts.pas',
  consoleoutput in '..\..\dxgettext\consoleoutput.pas',
  ignoredetector in '..\..\dxgettext\ignoredetector.pas',
  u_dzQuicksort in '..\..\dxgettext\u_dzQuicksort.pas',
  assembleengine in '..\assemble\assembleengine.pas',
  msgimportengine in '..\msgimport\msgimportengine.pas',
  {$else}
  gnugettext in '../../sample/gnugettext.pas',
  poparser in '../../dxgettext/poparser.pas',
  appconsts in '../../dxgettext/appconsts.pas',
  consoleoutput in '../../dxgettext/consoleoutput.pas',
  ignoredetector in '../../dxgettext/ignoredetector.pas',
  u_dzQuicksort in '../../dxgettext/u_dzQuicksort.pas',
  assembleengine in '../assemble/assembleengine.pas',
  msgimportengine in '../msgimport/msgimportengine.pas',
  {$endif}
  consoleapp in 'consoleapp.pas',
  msgmkignoreengine in 'msgmkignoreengine.pas';

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
end.
