program ggdxgettext;
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
  Forms,
  Dialogs,
  uconfig in 'uconfig.pas' {FormConfig},
  xgettexttools in 'xgettexttools.pas',
  appconsts in 'appconsts.pas',
  poparser in 'poparser.pas',
  xgettext in 'xgettext.pas',
  gnugettext in '..\sample\gnugettext.pas',
  ExeImage in 'ExeImage.pas',
  rxtypes in 'rxtypes.pas',
  uWork in 'uWork.pas' {FormWork},
  runxgettext in 'runxgettext.pas',
  ConsoleAppHandler in 'ConsoleAppHandler.pas',
  ignoredetector in 'ignoredetector.pas';

{$R *.res}

begin
  AddDomainForResourceString('delphi');
  textdomain('dxgettext');
  if paramcount<>1 then begin
    ShowMessage (_('This program needs one parameter, which must be a directory path or a file name.'));
    exit;
  end;
  Application.Initialize;
  Application.CreateForm(TFormWork, FormWork);
  Application.Run;
end.
