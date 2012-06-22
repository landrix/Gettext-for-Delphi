unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, StrParser, ActnList, ExtCtrls, Menus,
  POUtils, ITEUtils, ImgList, Tabs, TntComCtrls;

type
  TfrmMain = class(TForm)
    alMain: TActionList;
    acNewPO: TAction;
    acOpenPO: TAction;
    acSavePO: TAction;
    acSaveAsPO: TAction;
    acImportDFN: TAction;
    acImportXML: TAction;
    acLangAdd: TAction;
    acLangUpdate: TAction;
    acMergePO: TAction;
    Panel1: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    tvFiles: TTreeView;
    acExtractStrings: TAction;
    mmMain: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Tools1: TMenuItem;
    Help1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    acExit: TAction;
    Exit1: TMenuItem;
    ExtractStrings1: TMenuItem;
    ImportDFN1: TMenuItem;
    ImportXML1: TMenuItem;
    tcSources: TTabControl;
    OpenPO: TOpenDialog;
    il16: TImageList;
    acCloseFile: TAction;
    AddLanguage1: TMenuItem;
    UpdateLanguage1: TMenuItem;
    acAbout: TAction;
    About1: TMenuItem;
    pnlDesign: TPanel;
    lvDesign: TTntListView;
    reMsgID: TTntRichEdit;
    reMsgstr: TTntRichEdit;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    acNext: TAction;
    acPrev: TAction;
    acSettings: TAction;
    N2: TMenuItem;
    Settings1: TMenuItem;
    acCopyMsgIdMsgStr: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acOpenPOExecute(Sender: TObject);
    procedure tvFilesDblClick(Sender: TObject);
    procedure tcSourcesChange(Sender: TObject);
    procedure tvFilesCollapsed(Sender: TObject; Node: TTreeNode);
    procedure tvFilesExpanded(Sender: TObject; Node: TTreeNode);
    procedure acCloseFileExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure lvDesignData(Sender: TObject; Item: TListItem);
    procedure lvDesignAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure acExitExecute(Sender: TObject);
    procedure lvDesignChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure reMsgstrExit(Sender: TObject);
    procedure acNextExecute(Sender: TObject);
    procedure acPrevExecute(Sender: TObject);
    procedure acCopyMsgIdMsgStrExecute(Sender: TObject);
  private
    FModified: boolean;
    FFilename, FBasePath: string;
    FPOFile: TPOFile;
    FLastItem:TListItem;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure CreateEverything;
    procedure FreeEverything;
    function GetNodePath(ANode: TTreeNode): string;
    function AddLanguageFolders(BaseRoot: TTreeNode; const BaseFolder: string): boolean;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure CheckSaveFile;
    procedure SaveFile;
    property Modified: boolean read FModified write FModified;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}
const
  cClosedImage = 4;
  cOpenImage = 5;
  cPoImage = 12;

procedure TfrmMain.LoadFromFile(const Filename: string);
var
  i: integer;
begin
  lvDesign.Items.Count := 0;

  if Filename = '' then Exit;
  FFilename := Filename;
  i := tcSources.Tabs.IndexOf(ExtractRelativePath(FBasePath,Filename));
  if i < 0 then
    i := tcSources.Tabs.Add(ExtractRelativePath(FBasePath,Filename));
  FPOFile.LoadFromFile(Filename);
  tcSources.TabIndex := i;
  lvDesign.Items.Count := FPOFile.Count;
end;

procedure TfrmMain.SaveToFile(const Filename: string);
begin
  // TODO
end;

procedure TfrmMain.SaveFile;
begin
  // TODO
end;

procedure TfrmMain.CheckSaveFile;
begin
  // TODO
  Modified := false;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  CreateEverything;
  LoadSettings;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CheckSaveFile;
  SaveSettings;
  FreeEverything;
end;

procedure TfrmMain.FreeEverything;
begin
  FPOFile.Free;
end;

procedure TfrmMain.LoadSettings;
begin

end;

procedure TfrmMain.SaveSettings;
begin

end;

procedure TfrmMain.CreateEverything;
begin
  FPOFile := TPOFile.Create;
end;

procedure TfrmMain.acOpenPOExecute(Sender: TObject);
begin
  CheckSaveFile;
  OpenPO.Filename := FFilename;
  if OpenPO.Execute then
  begin
    tvFiles.Items.BeginUpdate;
    try
      tvFiles.Items.Clear;
      FBasePath := ExtractFilePath(OpenPO.Filename);
      AddLanguageFolders(tvFiles.Items.AddChild(nil, ExcludeTrailingPathDelimiter(ExtractFilePath(OpenPO.Filename))),
        FBasePath);
      LoadFromFile(ExtractFileName(OpenPO.Filename));

      tvFiles.Items.GetFirstNode.Expand(false);
    finally
      tvFiles.Items.EndUpdate;
    end;
  end;
end;

function TfrmMain.AddLanguageFolders(BaseRoot: TTreeNode; const BaseFolder: string): boolean;
var
  F: TSearchRec;
  N: TTreeNode;
begin
  Result := false;
  if FindFirst(BaseFolder + '*.po', faAnyFile, F) = 0 then
  try
    repeat
      if (F.Attr and faDirectory = 0) then
      begin
        with tvFiles.Items.AddChild(BaseRoot, F.Name) do
        begin
          ImageIndex := cPoImage;
          SelectedIndex := cPoImage;
        end;
        Result := true;
      end;
    until FindNext(F) <> 0;
  finally
    FindClose(F);
  end;

  if FindFirst(BaseFolder + '*.*', faDirectory, F) = 0 then
  try
    repeat
      if (F.Attr and faDirectory = faDirectory) and (F.Name[1] <> '.') then
      begin
        N := tvFiles.Items.AddChild(BaseRoot, F.Name);
        if not AddLanguageFolders(N, BaseFolder + F.Name + '\') then
          tvFiles.Items.Delete(N)
        else
        begin
          N.ImageIndex := cClosedImage;
          N.SelectedIndex := cClosedImage;
          Result := true;
        end;
      end;
    until FindNext(F) <> 0;
  finally
    FindClose(F);
  end;
end;

procedure TfrmMain.tvFilesDblClick(Sender: TObject);
var
  S: string;
begin
  S := GetNodePath(tvFiles.Selected);
  if FileExists(S) then
    LoadFromFile(S);
end;

function TfrmMain.GetNodePath(ANode: TTreeNode): string;
begin
  Result := '';
  while ANode <> nil do
  begin
    Result := ANode.Text + '\' + Result;
    ANode := ANode.Parent;
  end;
  Result := ExcludeTrailingPathDelimiter(Result);
end;

procedure TfrmMain.tcSourcesChange(Sender: TObject);
begin
  if tcSources.TabIndex >= 0 then
    LoadFromFile(FBasePath + tcSources.Tabs[tcSources.TabIndex]);
end;


procedure TfrmMain.tvFilesCollapsed(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := cClosedImage;
  Node.SelectedIndex := cClosedImage;
end;

procedure TfrmMain.tvFilesExpanded(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := cOpenImage;
  Node.SelectedIndex := cOpenImage;
end;

procedure TfrmMain.acCloseFileExecute(Sender: TObject);
var i:integer;
begin
  i := tcSources.TabIndex;
  if i >= 0 then
  begin
    tcSources.Tabs.Delete(i);
    while i >= tcSources.Tabs.Count do Dec(i);
    if (i < tcSources.Tabs.Count) and (i > -1) then
    begin
      tcSources.TabIndex := i;
      LoadFromFile(FBasePath + tcSources.Tabs[tcSources.TabIndex]);
    end
    else
      LoadFromFile('');
    tcSources.Invalidate;
  end;
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  ShowMessage(Application.Title);
end;

procedure TfrmMain.lvDesignData(Sender: TObject; Item: TListItem);
begin
  TTNTListItem(Item).Caption := UTF8Decode(FPOFile.Items[Item.Index].MsgID);
  TTNTListItem(Item).SubItems.Add(UTF8Decode(FPOFile.Items[Item.Index].MsgStr));
  TTNTListItem(Item).SubItems.Add(UTF8Decode(FPOFile.Items[Item.Index].Comments.Text));
end;

procedure TfrmMain.lvDesignAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  if (Item.SubItems.Count > 0) and (Item.SubItems[0] <> '') then
    lvDesign.Canvas.Brush.Color := $00EFEFEF;
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.lvDesignChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Item <> nil then
  begin
    reMsgID.Lines.Text := UTF8Decode(FPoFile.Items[Item.Index].MsgID);
    reMsgStr.Lines.Text := UTF8Decode(FPoFile.Items[Item.Index].MsgStr);
    reMsgStr.Modified := false;
  end;
end;

procedure TfrmMain.reMsgstrExit(Sender: TObject);
begin
  if reMsgStr.Modified and (lvDesign.Selected <> nil) then
    FPoFile.Items[lvDesign.Selected.Index].MsgStr := trim(UTF8Encode(reMsgStr.Lines.Text));
end;

procedure TfrmMain.acNextExecute(Sender: TObject);
begin
  reMsgstrExit(Sender);
  if lvDesign.Items.Count > 0 then
  begin
    if (lvDesign.Selected = nil) then
      lvDesign.Selected := lvDesign.Items[0]
    else if lvDesign.Selected.Index < lvDesign.Items.Count - 1 then
      lvDesign.Selected := lvDesign.Items[lvDesign.Selected.Index + 1];
    if lvDesign.Selected <> nil then
      lvDesign.Selected.MakeVisible(false);
  end;
  reMsgstr.SetFocus;
end;

procedure TfrmMain.acPrevExecute(Sender: TObject);
begin
  reMsgstrExit(Sender);
  if lvDesign.Items.Count > 0 then
  begin
    if (lvDesign.Selected = nil) then
      lvDesign.Selected := lvDesign.Items[0]
    else if lvDesign.Selected.Index > 0 then
      lvDesign.Selected := lvDesign.Items[lvDesign.Selected.Index - 1];
    if lvDesign.Selected <> nil then
      lvDesign.Selected.MakeVisible(false);
  end;
  reMsgstr.SetFocus;
end;

procedure TfrmMain.acCopyMsgIdMsgStrExecute(Sender: TObject);
begin
  if lvDesign.Selected <> nil then
    with lvDesign.Selected do
    begin
      FPOFile[Index].MsgStr := FPOFile[Index].MsgID;
      lvDesignChange(Sender,lvDesign.Selected,ctState);
      lvDesign.Invalidate;
//      SubItems[0]           := UTF8Decode(FPOFile[Index].MsgID);
    end;
end;

end.

