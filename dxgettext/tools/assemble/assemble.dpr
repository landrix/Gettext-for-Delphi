{$APPTYPE CONSOLE}
program assemble;

uses
  SysUtils,
  {$ifdef MSWINDOWS}
  gnugettext in '..\..\sample\gnugettext.pas',
  appconsts in '..\..\dxgettext\appconsts.pas',
  consoleoutput in '..\..\dxgettext\consoleoutput.pas',
  {$else}
  gnugettext in '../../sample/gnugettext.pas',
  appconsts in '../../dxgettext/appconsts.pas',
  consoleoutput in '../../dxgettext/consoleoutput.pas',
  {$endif}
  consoleapp in 'consoleapp.pas',
  assembleengine in 'assembleengine.pas';

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
      consoleoutput.writeln (e.Message);
      ExitCode:=1;
    end;
  end;
end.
