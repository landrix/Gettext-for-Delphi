unit HelpUi;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl                            *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dxgettext.po.dk/ for more information            *)
(*                                                              *)
(****************************************************************)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormHelp = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses gnugettext;

{$R *.dfm}

procedure TFormHelp.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
end;

end.