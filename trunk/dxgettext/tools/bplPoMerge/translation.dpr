program translation;

{
See translation-readme.html for info & help

License:
This tool is licensed under MPL 1.1 (Mozilla Public License).
See OSI for complete text of the license before using this tool.

Author:
(c) 2003 XAN
www.xan.de
info@xan.de
}

uses
  Forms,
  main_translation in 'main_translation.pas' {frmMain},
  gnugettext in '..\..\sample\gnugettext.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

