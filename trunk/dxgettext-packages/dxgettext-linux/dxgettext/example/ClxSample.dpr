program ClxSample;

uses
  QForms,
  SysUtils,
  gnugettext in 'gnugettext.pas',
  ClxSampleForm in 'ClxSampleForm.pas' {FormMain};

{$R *.res}

begin
  AddDomainForResourceString('delphi');
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
