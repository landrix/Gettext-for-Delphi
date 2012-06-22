unit gginitializer;
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

interface

implementation

uses
  gnugettext;
  
initialization
  // Use delphi.mo for runtime library translations, if it is there
  // by putting this line into this separate unit, we can execute it
  // before the initialization sections of the other units are executed.
  AddDomainForResourceString('delphi');


end.

