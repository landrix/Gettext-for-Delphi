unit LabelApplicationByRulesUi;
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
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormLabelApplicationByRules = class(TForm)
    MemoExplanation: TMemo;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    EditFilename: TLabeledEdit;
    ButtonSelectFilename: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSelectFilenameClick(Sender: TObject);
    procedure EditFilenameChange(Sender: TObject);
  private
  public
  end;

implementation

uses gnugettext;

{$R *.dfm}

procedure TFormLabelApplicationByRules.ButtonSelectFilenameClick(
  Sender: TObject);
var
  od:TOpenDialog;
begin
  od:=TOpenDialog.Create(self);
  try
    od.FileName:=EditFilename.Text;
    if od.Execute then begin
      EditFilename.Text:=od.FileName;
    end;
  finally
    FreeAndNil (od);
  end;
end;

procedure TFormLabelApplicationByRules.EditFilenameChange(Sender: TObject);
begin
  ButtonOK.Enabled:=EditFilename.Text<>'';
end;

procedure TFormLabelApplicationByRules.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);

  MemoExplanation.Lines.Clear;
  MemoExplanation.Lines.Add(_('Specify a utf-8 file that contains rules for application of labels.'));
  MemoExplanation.Lines.Add('');
  MemoExplanation.Lines.Add(_('The file has the following format, where the first word in each line is a label:'));
  MemoExplanation.Lines.Add('');

  MemoExplanation.Lines.Add('qvt file qvt*'); // do not translate
  MemoExplanation.Lines.Add('qvt file ToolsQvt.pas'); // do not translate
  MemoExplanation.Lines.Add('special msgid *micro*'); // do not translate
  MemoExplanation.Lines.Add('special msgid *degrees Celsius*'); // do not translate
end;

end.