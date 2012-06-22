program ConvertTranslateDb;

uses
  Forms,
  w_ConvertTranslateDb in 'w_ConvertTranslateDb.pas' {f_ConvertTranslateDb},
  u_TranslationDBAccess in '..\..\gorm\u_TranslationDBAccess.pas',
  u_dzAdoDbUniqueId in '..\..\gorm\u_dzAdoDbUniqueId.pas',
  PoTools in '..\..\gorm\PoTools.pas',
  poparser in '..\..\..\dxgettext\poparser.pas',
  gnugettext in '..\..\..\sample\gnugettext.pas',
  u_dzQuicksort in '..\..\..\dxgettext\u_dzQuicksort.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tf_ConvertTranslateDb, f_ConvertTranslateDb);
  Application.Run;
end.
