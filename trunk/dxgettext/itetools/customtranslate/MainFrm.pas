unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    ListView1: TListView;
    btnExtract: TButton;
    PageControl1: TPageControl;
    tabExplain: TTabSheet;
    RichEdit1: TRichEdit;
    tabExtract: TTabSheet;
    RichEdit2: TRichEdit;
    btnUseSV: TButton;
    procedure btnExtractClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnUseSVClick(Sender: TObject);
  private
    procedure TranslateListView(Obj: TObject);
    procedure TranslateTreeView(Obj: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  
resourcestring
  // NB: these are the strings used in the treeview, so we don't need to
  // do anything special for dxgettext to extract them into the PO file
  // (we don't even have to assign the strings to the treenodes in code)
  // We still need to provide a custom translator for treeviews, however, since
  // TranslateProperties can't translate TTreeNodes (they have no published properties)
  // Also, notice that "General" only appears once here but several times in the treeview
  // but stil gets translated for all uses of "General". This is due to how TranslateProperties works.
  
  SEnvironment = 'Environment';
  SGeneral = 'General';
  SViewer = 'Viewer';
  SGantt = 'Gantt';
  SFontsAndColors = 'Fonts and Colors';
  SResources = 'Resources';
  STasks = 'Tasks';
  SToolTips = 'ToolTips';
  SOrders = 'Orders';
  SColors = 'Colors';

implementation
uses
  gnugettext;

{$R *.dfm}

procedure ExtractListViewStrings(AListView:TListView;AStrings:TStrings);
var i,j:integer;li:TListItem;
begin
   for i := 0 to AListView.Columns.Count - 1 do
   begin
     AStrings.Add(Format('#. %s.Columns',[AListView.Name]));
     AStrings.Add('#: MainFrm.pas');
     AStrings.Add(Format('msgid "%s"',[AListView.Columns[i].Caption]));
     AStrings.Add('msgstr ""');
     AStrings.Add('');
   end;
   for i := 0 to AListView.Items.Count - 1 do
   begin
     li := AListView.Items[i];
     AStrings.Add(Format('#. %s.Items',[AListView.Name]));
     AStrings.Add('#: MainFrm.pas');
     AStrings.Add(Format('msgid "%s"',[li.Caption]));
     AStrings.Add('msgstr ""');
     AStrings.Add('');
     for j := 0 to li.SubItems.Count - 1 do
     begin
       AStrings.Add(Format('#. %s.SubItems',[AListView.Name]));
       AStrings.Add('#: MainFrm.pas');
       AStrings.Add(Format('msgid "%s"',[li.SubItems[j]]));
       AStrings.Add('msgstr ""');
       AStrings.Add('');
     end;
   end;
end;

procedure TForm1.TranslateTreeView(Obj:TObject);
var N:TTreeNode;
begin
  if Obj is TTreeView then
  begin
    N := TTreeView(Obj).Items.GetFirstNode;
    while Assigned(N) do
    begin
      N.Text := _(N.Text);
      N := N.GetNext;
    end;
  end;
end;

procedure TForm1.TranslateListView(Obj:TObject);
var i,j:integer;li:TListITem;
begin
  if Obj is TListView then
  begin
    // We need to handle the columns as well since TranslateProperties
    // skips this class entirely.
    for i := 0 to TListView(Obj).Columns.Count - 1 do
      TListView(Obj).Columns[i].Caption := _(TListView(Obj).Columns[i].Caption);

    for i := 0 to TListView(Obj).Items.Count - 1 do
    begin
      li := TListView(Obj).Items[i];
      li.Caption := _(li.Caption);
      for j := 0 to li.SubItems.Count - 1 do
        li.SubItems[j] := _(li.SubItems[j]);
    end;
  end;
end;

procedure TForm1.btnExtractClick(Sender: TObject);
begin
  RichEdit2.Lines.Clear;
  // Extract the strings in the listview (COlumns and Items): these strings must later be manually
  // added to the PO file created by dxgettext and then translated before you can use the translations
  // in this project
  ExtractListViewStrings(ListView1,RichEdit2.Lines);
  PageControl1.ActivePageIndex := 1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RichEdit1.WordWrap := true;
  Treeview1.FullExpand;
  TP_GlobalHandleClass(TTreeView,TranslateTreeView);
  TP_GlobalHandleClass(TListView,TranslateListView);
  // make sure we start up untranslated on any locale
  UseLanguage('en');
  TranslateProperties(self);
end;

procedure TForm1.btnUseSVClick(Sender: TObject);
begin
  // switch language
  UseLanguage('sv');
  TranslateProperties(self);
end;

end.


