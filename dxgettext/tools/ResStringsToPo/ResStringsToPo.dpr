program ResStringsToPo;

uses
  Forms,
  w_ResStringsToPo in 'w_ResStringsToPo.pas' {f_ResStringsToPo},
  poparser in '..\..\dxgettext\poparser.pas',
  gnugettext in '..\..\sample\gnugettext.pas',
  u_dzQuicksort in '..\..\dxgettext\u_dzQuicksort.pas',
  ConsoleAppHandler in '..\..\dxgettext\ConsoleAppHandler.pas',
  xgettexttools in '..\..\dxgettext\xgettexttools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tf_ResStringsToPo, f_ResStringsToPo);
  Application.Run;
end.
