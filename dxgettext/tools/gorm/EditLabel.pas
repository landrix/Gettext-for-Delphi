unit EditLabel;
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
  TFormLabel = class(TForm)
    EditLabelName: TLabeledEdit;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure EditLabelNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    class function Execute(AOwner: TComponent; var ALabel: string): boolean; static;
  end;

implementation

uses gnugettext;

{$R *.dfm}

class function TFormLabel.Execute(AOwner: TComponent; var ALabel: string): boolean;
var
  frm: TFormLabel;
begin
  frm:=TFormLabel.Create(AOwner);
  try
    frm.EditLabelName.Text := ALabel;
    Result := (frm.ShowModal=mrOK);
    if Result then begin
      ALabel := frm.EditLabelName.Text;
      Result := Result and (ALabel <> '');
    end;
  finally
    FreeAndNil(frm);
  end;
end;

procedure TFormLabel.EditLabelNameChange(Sender: TObject);
var
  i,p:integer;
  s:string;
begin
  s:=lowercase(EditLabelName.Text);

  for i:=length(s) downto 1 do begin
    case s[i] of
      'a'..'z':;
    else
      delete (s,i,1);
    end;
  end;

  if s<>EditLabelName.Text then begin
    p:=EditLabelName.SelStart;
    EditLabelName.Text:=s;
    EditLabelName.SelStart:=p;
  end;
end;

procedure TFormLabel.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
end;

end.