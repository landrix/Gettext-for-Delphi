unit SampleForm;
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

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormMain = class(TForm)
    ButtonTestGettext: TButton;
    ButtonTestResourcestring: TButton;
    procedure ButtonTestGettextClick(Sender: TObject);
    procedure ButtonTestResourcestringClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  gnugettext;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
end;

resourcestring
  MessageToUser='Thank you for clicking this button';

procedure TFormMain.ButtonTestResourcestringClick(Sender: TObject);
begin
  // This is a demonstration of automatic resourcestring translation
  MessageDlg (MessageToUser,mtInformation,[mbOK],0);
end;

procedure TFormMain.ButtonTestGettextClick(Sender: TObject);
begin
  // This is a demo of the _() syntax
  MessageDlg (_('Thank you for clicking this button'),mtInformation,[mbOK],0);
end;

end.

