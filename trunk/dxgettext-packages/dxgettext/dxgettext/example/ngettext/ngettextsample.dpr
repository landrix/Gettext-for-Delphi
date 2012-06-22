program ngettextsample;

uses
  QForms,
  uMain in 'uMain.pas' {FormNgettext},
  gnugettext in '..\gnugettext.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormNgettext, FormNgettext);
  Application.Run;
end.
