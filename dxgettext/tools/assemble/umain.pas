unit umain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, assembleengine, XPMan, CheckLst;

type
  TForm1 = class(TForm)
    ListBoxTranslations: TCheckListBox;
    LabelExplanation: TLabel;
    LabelExeHeading: TLabel;
    LabelExeName: TLabel;
    PanelButtons: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    XPManifest: TXPManifest;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    { Private declarations }
    engine:Tassembleengine;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses gnugettext, appconsts;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  filename:string;
  i:integer;
begin
  TranslateComponent (self);
  filename:=ExpandFileName(paramstr(1));
  engine:=Tassembleengine.Create;
  engine.exefilename:=filename;
  engine.SetGnuGettextPatchCode;
  engine.filemask:='*.mo';
  engine.PrepareFileList;
  ListBoxTranslations.Items.Text:=engine.filelist.Text;
  for i:=0 to ListBoxTranslations.Items.Count-1 do 
    ListBoxTranslations.Checked[i]:=True;
  LabelExeName.Caption:=filename;
  if ListBoxTranslations.Items.Count=0 then begin
    ListBoxTranslations.Items.Text:=_('No translations (.mo files) found.');
    ButtonOK.Enabled:=False;
  end;
  ListBoxTranslations.ItemIndex:=0;
  Caption:=Caption+' (ggassemble '+version+')';
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  PanelButtons.Left:=(Width-PanelButtons.Width) div 2;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil (engine);
end;

procedure TForm1.ButtonOKClick(Sender: TObject);
var
  i:integer;
begin
  screen.cursor:=crHourglass;
  try
    for i:=0 to ListBoxTranslations.Items.Count-1 do begin
      if not ListBoxTranslations.Checked[i] then
        engine.SkipFile (ListBoxTranslations.Items.Strings[i]);
    end;
    engine.Execute;
  finally
    screen.cursor:=crDefault;
  end;
  Close;
end;

procedure TForm1.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

end.
