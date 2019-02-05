program MultithreadedGetResourceStringTest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  gnugettext in '..\..\..\dxgettext\sample\gnugettext.pas',
  MultithreadedGetResourceStringTestMain in 'MultithreadedGetResourceStringTestMain.pas';

begin
  try
    Main;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
