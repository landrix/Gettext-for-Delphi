program GetResourceStringTest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  gnugettext in '..\..\..\dxgettext\sample\gnugettext.pas',
  GetResourceStringTestMain in 'GetResourceStringTestMain.pas';

begin
  try
    Main;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
