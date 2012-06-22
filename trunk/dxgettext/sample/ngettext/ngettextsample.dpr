program ngettextsample;

uses
  gnugettext in '..\gnugettext.pas',
  Forms,
  uMain in 'uMain.pas' {FormNgettext};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormNgettext, FormNgettext);
  Application.Run;
end.
