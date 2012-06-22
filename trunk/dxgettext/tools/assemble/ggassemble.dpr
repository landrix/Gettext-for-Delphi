program ggassemble;

uses
  gnugettext in '..\..\sample\gnugettext.pas',
  Forms,
  Dialogs,
  umain in 'umain.pas' {Form1},
  assembleengine in 'assembleengine.pas',
  appconsts in '..\..\dxgettext\appconsts.pas';

{$R *.res}

begin
  AddDomainForResourceString('delphi');
  TextDomain ('dxgettext');
  if paramcount<>1 then begin
    ShowMessage (_('This program needs a filename as parameter.'));
    exit;
  end;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
