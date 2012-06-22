unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QGrids;

type
  TFormNgettext = class(TForm)
    StringGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
    procedure DoColumn(col: integer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNgettext: TFormNgettext;

implementation

uses gnugettext;

{$R *.xfm}

procedure TFormNgettext.DoColumn (col:integer);
var
  i:integer;
begin
  for i:=1 to StringGrid.RowCount-1 do
    StringGrid.Cells[col,i]:=Format(dngettext('plurals','%d file','%d files',i-1),[i-1]);
end;

procedure TFormNgettext.FormCreate(Sender: TObject);
var
  i:integer;
begin
  TranslateComponent (self);
  for i:=1 to StringGrid.RowCount-1 do
    StringGrid.Cells[0,i]:=IntToStr(i-1);
  StringGrid.Cells[1,0]:='English';
  StringGrid.Cells[2,0]:='German';
  StringGrid.Cells[3,0]:='French';
  StringGrid.Cells[4,0]:='Polish';
  DoColumn (1);
  try
    UseLanguage('de');
    DoColumn (2);
    UseLanguage('fr');
    DoColumn (3);
    UseLanguage('pl');
    DoColumn (4);
  finally
    // Switch back to default language
    UseLanguage('C');
  end;
end;

end.
