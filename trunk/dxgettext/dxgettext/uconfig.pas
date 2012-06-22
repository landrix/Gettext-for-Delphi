unit uconfig;
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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XPMan, StdCtrls, ExtCtrls;

type
  TFormConfig = class(TForm)
    EditMask: TLabeledEdit;
    CheckBoxRecurse: TCheckBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    EditBasepath: TLabeledEdit;
    CheckBoxSaveSettings: TCheckBox;
    CBCreateIgnore: TCheckBox;
    CBRemoveIgnore: TCheckBox;
    CheckBoxAllowNonAscii: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  inifiles, gnugettext, appconsts;

{$R *.dfm}

procedure TFormConfig.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
  Caption:=Caption+' (ggdxgettext '+Version+')';
end;

procedure TFormConfig.ButtonOKClick(Sender: TObject);
var
  filename:string;
  ini:TIniFile;
begin
  if CheckBoxSaveSettings.Checked then begin
    filename:=ExtractFilePath(IncludeTrailingPathDelimiter(ExpandFileName(EditBasePath.Text)))+'dxgettext.ini';
    ini:=TIniFile.Create (filename);
    try
      ini.WriteBool('ggdxgettext','recurse',CheckBoxRecurse.Checked);
      ini.WriteString('dxgettext','mask',EditMask.Text);
      ini.WriteBool('ggdxgettext','updateignore',CBCreateIgnore.Checked);
      ini.WriteBool('ggdxgettext','useignore',CBRemoveIgnore.Checked);
      ini.WriteBool('ggdxgettext','allownonascii',CheckBoxAllowNonAscii.Checked);
    finally
      FreeAndNil (ini);
    end;
  end;
end;

end.
