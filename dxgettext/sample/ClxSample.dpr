program ClxSample;

uses
  gnugettext in 'gnugettext.pas',
  QForms,
  SysUtils,
  ClxSampleForm in 'ClxSampleForm.pas' {FormMain};

{$R *.res}

begin
  AddDomainForResourceString('delphi');
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
