program Gorm;

uses
  Forms,
  EditorUi in 'EditorUi.pas' {FormEditor},
  gnugettext in '..\..\sample\gnugettext.pas',
  poparser in '..\..\dxgettext\poparser.pas',
  EditLabel in 'EditLabel.pas' {FormLabel},
  LabelApplicationByRulesUi in 'LabelApplicationByRulesUi.pas' {FormLabelApplicationByRules},
  GoogleTranslate in 'GoogleTranslate.pas',
  PoTools in 'PoTools.pas',
  PreferencesUi in 'PreferencesUi.pas' {FormPreferences},
  AppSettings in 'AppSettings.pas',
  ConsoleAppHandler in '..\..\dxgettext\ConsoleAppHandler.pas',
  ToolsAutoUpgrader in 'ToolsAutoUpgrader.pas',
  utils in 'utils.pas',
  AutoTranslateOptions in 'AutoTranslateOptions.pas' {FormAutoTranslateOptions},
  GoogleTranslateSettings in 'GoogleTranslateSettings.pas' {GoogleTranslationSettings},
  w_MicrosoftTranslateSettings in 'w_MicrosoftTranslateSettings.pas' {f_MicrosoftTranslationSettings},
  u_TranslatorEngineGoogle in 'u_TranslatorEngineGoogle.pas',
  u_TranslatorEngineMicrosoft in 'u_TranslatorEngineMicrosoft.pas',
  u_TranslatorEngine in 'u_TranslatorEngine.pas',
  u_TranslatorEngineGetText in 'u_TranslatorEngineGetText.pas',
  u_TranslatorEnginePoFile in 'u_TranslatorEnginePoFile.pas',
  w_TextFilter in 'w_TextFilter.pas' {f_Textfilter},
  u_TranslationDBAccess in 'u_TranslationDBAccess.pas',
  w_TranslationDb in 'w_TranslationDb.pas' {f_TranslationDb},
  u_TranslationRepository in 'u_TranslationRepository.pas',
  w_EditHeader in 'w_EditHeader.pas' {f_EditHeader},
  u_Languages in 'u_Languages.pas',
  w_TranslationDbNew in 'w_TranslationDbNew.pas' {f_TranslationDbNew},
  w_dzProgress in 'w_dzProgress.pas' {f_dzProgress},
  u_TranslatorEngineRepository in 'u_TranslatorEngineRepository.pas',
  u_dzOsUtils in 'u_dzOsUtils.pas',
  u_dzVclUtils in 'u_dzVclUtils.pas',
  u_dzClassUtils in 'u_dzClassUtils.pas',
  TranslationsMemory in 'TranslationsMemory.pas',
  w_IgnoreLoad in 'w_IgnoreLoad.pas' {f_IgnoreLoad},
  w_IgnoreImport in 'w_IgnoreImport.pas' {f_IgnoreImport},
  w_IgnoreSave in 'w_IgnoreSave.pas' {f_IgnoreSave},
  u_dzGoogleTranslate in 'u_dzGoogleTranslate.pas',
  u_dzMicrosoftTranslate in 'u_dzMicrosoftTranslate.pas',
  u_dzStringUtils in 'u_dzStringUtils.pas',
  w_TranslationDbLearnOptions in 'w_TranslationDbLearnOptions.pas' {f_TranslationDbLearnOptions},
  u_dzAdoDbUniqueId in 'u_dzAdoDbUniqueId.pas',
  PoDiffHtml in 'PoDiffHtml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Gorm editor for PO files';
  Application.CreateForm(TFormEditor, FormEditor);
  Application.Run;
end.
