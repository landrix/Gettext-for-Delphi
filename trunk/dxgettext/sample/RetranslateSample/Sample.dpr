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
  gnugettext in '..\gnugettext.pas',
  Forms,
  SampleForm in 'SampleForm.pas' {FormMain};

{$R *.res}

begin
  // Use delphi.mo for runtime library translations, if it is there
  AddDomainForResourceString('delphi');
  
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
