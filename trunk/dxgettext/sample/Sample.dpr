program Sample;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl                          *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)


uses
  gnugettext in 'gnugettext.pas',
  gginitializer in 'gginitializer.pas',
  Windows,
  Forms,
  Graphics,
  SampleForm in 'SampleForm.pas' {FormMain};

{$R *.res}

begin
  // This is the list of ignores for this project. The list of
  // ignores has to come before the first call to TranslateComponent().
  TP_GlobalIgnoreClass(TFont);

  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

