program CustTransDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  {$IFDEF VER130}
  gnugettext in '..\..\packages\dxgettext\dxgettext\delphi5\gnugettext.pas',
  gnugettextd5 in '..\..\packages\dxgettext\dxgettext\delphi5\gnugettextd5.pas'
  {$ELSE}
  gnugettext in '..\..\sample\gnugettext.pas'
  {$ENDIF}
  ;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
