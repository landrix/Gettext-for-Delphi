program extractChangedTranslations;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  gnugettext in '..\..\sample\gnugettext.pas',
  appconsts in '..\..\dxgettext\appconsts.pas',
  consoleoutput in '..\..\dxgettext\consoleoutput.pas',
  ectConsole in 'ectConsole.pas',
  poparser in '../../dxgettext/poparser.pas',
  u_dzQuicksort in '../../dxgettext/u_dzQuicksort.pas',
  msgmergedxengine in '..\ggmerge\msgmergedxengine.pas';

var
  lConsoleApplication: TConsoleApplication;

begin
  try
    { TODO -oUser -cConsole Main : Code hier einfügen }
    textdomain('dxgettext');
    AddDomainForResourceString('delphi');
    AddDomainForResourceString('kylix');

    lConsoleApplication := TConsoleApplication.Create;
    try
      lConsoleApplication.Execute;
    finally
      FreeAndNil (lConsoleApplication);
    end;
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
