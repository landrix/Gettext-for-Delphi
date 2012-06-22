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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, 
  Dialogs, gnugettext, TntForms, TntStdCtrls, StdCtrls;

type
  TFormMain = class(TTntForm)
    ButtonTestGettext: TTntButton;
    ButtonTestResourcestring: TTntButton;
    ListBoxLanguage: TListBox;
    procedure ButtonTestGettextClick(Sender: TObject);
    procedure ButtonTestResourcestringClick(Sender: TObject);
    procedure ListBoxLanguageClick(Sender: TObject);
    procedure TntFormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.TntFormCreate(Sender: TObject);
begin
  TranslateComponent (self);
end;

resourcestring
  MessageToUser='Thank you for clicking this button';

procedure TFormMain.ButtonTestResourcestringClick(Sender: TObject);
begin
  // This way is always Unicode enabled
  MessageBoxW(Handle,PWideChar(LoadResStringW(@MessageToUser)),'Information',MB_OK);
end;

procedure TFormMain.ButtonTestGettextClick(Sender: TObject);
begin
  // This is a demo of the _() syntax
  // It works perfectly with Unicode
  MessageBoxW(Handle,PWideChar(_('Thank you for clicking this button')),'Information',MB_OK);
end;

procedure TFormMain.ListBoxLanguageClick(Sender: TObject);
const // This table corresponds to the listbox
  LangCode:array[0..13] of string=('','ca','zh','hr','da_DK','nl','en','et', 'fr','de','ja','no','ru','sv');
var
  i:integer;
begin
  // Store the state of the listbox, since it will change during retranslation
  i:=ListBoxLanguage.ItemIndex;

  // Change language
  UseLanguage(LangCode[i]);

  // Retranslate all translated properties on this form
  RetranslateComponent (self);

  // Restore the previous state of the listbox
  ListBoxLanguage.ItemIndex:=i;
end;

end.

