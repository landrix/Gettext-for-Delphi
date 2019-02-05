program MultithreadedResourceStringTest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  gnugettext in '..\..\..\dxgettext\sample\gnugettext.pas',
  MultithreadedResourceStringTestMain in 'MultithreadedResourceStringTestMain.pas';

begin
  try
    Main;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
