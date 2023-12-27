program ggmerge;

uses
  gnugettext in '..\..\sample\gnugettext.pas',
  poparser in '..\..\dxgettext\poparser.pas',
  appconsts in '..\..\dxgettext\appconsts.pas',
  Forms,
  uRun in 'uRun.pas' {FormRun},
  msgmergedxengine in 'msgmergedxengine.pas',
  consoleoutput in '..\..\dxgettext\consoleoutput.pas',
  u_dzQuicksort in '..\..\dxgettext\u_dzQuicksort.pas',
  xgettexttools in '..\..\dxgettext\xgettexttools.pas',
  ConsoleAppHandler in '..\..\dxgettext\ConsoleAppHandler.pas';

{$R *.res}

begin
  AddDomainForResourceString('delphi');
  textdomain('dxgettext');
  Application.Initialize;
  Application.CreateForm(TFormRun, FormRun);
  Application.Run;
end.
