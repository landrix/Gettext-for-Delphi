unit main_translation;

{
See translation-readme.html for info & help

License:
This tool is licensed under MPL 1.1 (Mozilla Public License).
See OSI for complete text of the license before using this tool.

Author:
(c) 2003 XAN
www.xan.de
info@xan.de
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, Buttons, Grids, ValEdit, ComCtrls, ExtCtrls, Menus, Dialogs, ShellAPI;

type
  TfrmMain = class(TForm)
    panel2: TGroupBox;
    btMergePO: TButton;
    l_PoFile: TLabel;
    edPO: TEdit;
    progressBusy: TProgressBar;
    chkUTF8: TCheckBox;
    SaveDialog: TSaveDialog;
    listboxMerge: TListBox;
    OpenDialog: TOpenDialog;
    btCreatePO: TButton;
    TheMainMenu: TMainMenu;
    Files1: TMenuItem;
    Extractions1: TMenuItem;
    POfiles1: TMenuItem;
    Exit1: TMenuItem;
    Extract1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    Merge1: TMenuItem;
    Create1: TMenuItem;
    N1: TMenuItem;
    Help1: TMenuItem;
    splitMain: TSplitter;
    panelClient: TPanel;
    Values: TListView;
    panel1: TGroupBox;
    l_BplDir: TLabel;
    l_LanguageExtension: TLabel;
    lbLoadInfo: TLabel;
    lbEntriesInfo: TLabel;
    edBPL: TEdit;
    edLang: TEdit;
    btExtract: TBitBtn;
    progressLoad: TProgressBar;
    p_Bottom: TPanel;
    b_SelectBplSourceDir: TButton;
    b_SelectPo: TButton;
    procedure btExtractClick(Sender: TObject);
    procedure btMergePOClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btCreatePOClick(Sender: TObject);
    procedure ValuesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure Exit1Click(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure b_SelectBplSourceDirClick(Sender: TObject);
    procedure b_SelectPoClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FFilename: String;
    FColumn: Integer;
    function CorrectStringToPO(strIn:String) : String;
    procedure LoadValues(sFilename:String;nCol:Integer);
    function DeleteQuotes(strIn:String) : String;
    procedure AddMsgStr(list:TStrings;strIDName,strText:AnsiString);
    procedure FormResourceCallback(hModule:THandle;lpszType:PChar;lpszName:PChar);
    procedure StringResourceCallback(hModule:THandle;lpszType:PChar;lpszName:PChar);
    procedure ParseBinaryObject(Input:TStream;Name:String);
    procedure MergeToValues(sID:String;strText:WideString;nCol:Integer);
  public
    { Public-Deklarationen }
  end;

const
  colBPL  = 0;
  colLang = 1;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses StrUtils, gnugettext, FileCtrl;

// taken from https://forums.embarcadero.com/thread.jspa?threadID=112112

type
  TUTF8NoBOMEncoding = class(TUTF8Encoding)
  public
    function GetPreamble: TBytes; override;
  end;

function TUTF8NoBOMEncoding.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

function TfrmMain.CorrectStringToPO(strIn:String) : String;
begin
  //replace special chars
  Result:=strIn;
  Result:=AnsiReplaceStr(Result,'\','\\');
  Result:=AnsiReplaceStr(Result,#13#10,'\n');
  Result:=AnsiReplaceStr(Result,#9,'\t');
  Result:=AnsiReplaceStr(Result,#13,'\r');
  Result:=AnsiReplaceStr(Result,#10,'\n');
  Result:=AnsiReplaceStr(Result,'"','\"');
end;

function EnumResourceNames_Callback(hModule:THandle;lpszType:PChar;lpszName:PChar;lParam:LongWord):Bool; stdcall;
var
  Form: TfrmMain;
begin
  Form:=TfrmMain(lParam);
  if lpszType=RT_RCDATA then
    Form.FormResourceCallback(hModule,lpszType,lpszName)
  else if lpszType=RT_STRING then
    Form.StringResourceCallback(hModule,lpszType,lpszName);
  Result := True;
end;

procedure TfrmMain.ParseBinaryObject(Input:TStream;Name:String);
var
  SaveSeparator: Char;
  Reader: TReader;
  ObjectName, PropName: string;

  procedure ConvertHeader;
  var
    ClassName: string;
    Flags: TFilerFlags;
    Position: Integer;
  begin
    Reader.ReadPrefix(Flags, Position);
    ClassName := Reader.ReadStr;
    ObjectName := Reader.ReadStr;
    if ObjectName = '' then
      ObjectName := ClassName;  // save for error reporting
  end;

  procedure ConvertBinary;
  var
    Count: Longint;
    Buffer: array of Char;
  begin
    Reader.ReadValue;
    Reader.Read(Count, SizeOf(Count));
    SetLength(Buffer,Count);
    Reader.Read(Buffer[0],Count);
  end;

  procedure ConvertProperty(Name:String); forward;

  procedure ConvertValue(Name:String);
  var
    L: Integer;
    S: string;
    W: WideString;
    nCount: Integer;
  begin
    case Reader.NextValue of
      vaList:
        begin
          Reader.ReadValue;
          nCount:=0;
          while not Reader.EndOfList do begin
            ConvertValue(Name+'.'+IntToStr(nCount));
            Inc(nCount);
          end;
          Reader.ReadListEnd;
        end;
      vaInt8, vaInt16, vaInt32:
        Reader.ReadInteger;
      vaExtended:
        Reader.ReadFloat;
      vaSingle:
        Reader.ReadSingle;
      vaCurrency:
        Reader.ReadCurrency;
      vaDate:
        Reader.ReadDate;
      vaWString, vaUTF8String:
        begin
          W := Reader.ReadWideString;
          L := Length(W);
if L>0 then
MergeToValues(Format('%s_%s',[ChangeFileExt(ExtractFilename(FFilename),''),Name]),W,FColumn);
        end;
      vaString, vaLString:
        begin
          S := Reader.ReadString;
          L := Length(S);
if L>0 then
MergeToValues(Format('%s_%s',[ChangeFileExt(ExtractFilename(FFilename),''),Name]),S,FColumn);
        end;
      vaIdent, vaFalse, vaTrue, vaNil, vaNull:
        Reader.ReadIdent;
      vaBinary:
        ConvertBinary;
      vaSet:
        begin
          Reader.ReadValue;
          while True do begin
            S := Reader.ReadStr;
            if S = '' then Break;
          end;
        end;
      vaCollection:
        begin
          nCount:=0;
          Reader.ReadValue;
          while not Reader.EndOfList do begin
            if Reader.NextValue in [vaInt8, vaInt16, vaInt32] then
              ConvertValue(Name+'.Item');
            Reader.CheckValue(vaList);
            while not Reader.EndOfList do
              ConvertProperty(Name+'.Item'+IntToStr(nCount));
            Reader.ReadListEnd;
            Inc(nCount);
          end;
          Reader.ReadListEnd;
        end;
      vaInt64:
        Reader.ReadInt64;
    else
      raise EReadError.Create('Parse error in Properties');
    end;
  end;

  procedure ConvertProperty(Name:String);
  begin
    PropName := Reader.ReadStr;
    ConvertValue(Name+'.'+PropName);
  end;

  procedure ConvertObject(Name:String);
  begin
    ConvertHeader;
    Name:=Name+'.'+ObjectName;
    while not Reader.EndOfList do ConvertProperty(Name);
    Reader.ReadListEnd;
    while not Reader.EndOfList do ConvertObject(Name);
    Reader.ReadListEnd;
  end;

begin
  Reader := TReader.Create(Input, 4096);
  SaveSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    try
      Reader.ReadSignature;
      ConvertObject(Name);
    finally
    end;
  finally
    DecimalSeparator := SaveSeparator;
    Reader.Free;
  end;
end;

procedure TfrmMain.MergeToValues(sID:String;strText:WideString;nCol:Integer);
// add to listview
var Item: TListItem;
begin
  Item:=Values.FindCaption(0,sID,False,True,False);
  if not Assigned(Item) then begin
    Item:=Values.Items.Add;
    Item.Caption:=sID;
    Item.SubItems.Add('');
    Item.SubItems.Add('');
  end;
  Item.SubItems[FColumn]:=CorrectStringToPO(strText);
end;

procedure TfrmMain.FormResourceCallback(hModule:THandle;lpszType:PChar;lpszName:PChar);
var Buffer: array of Byte;
    streamRes: TResourceStream;
begin
  streamRes:=TResourceStream.Create(hModule,lpszName,lpszType);
  SetLength(Buffer,4);
  streamres.Read(Buffer[0],SizeOf(Buffer));
  // is it a DFM?
  if String(Buffer)='TPF0' then begin
    streamRes.Seek(0,0);
    ParseBinaryObject(streamRes,lpszName);
  end;
  streamRes.Free;
end;

procedure TfrmMain.StringResourceCallback(hModule:THandle;lpszType:PChar;lpszName:PChar);
var Buffer: array of Byte;
    streamRes: TResourceStream;
    nIdx: Word;
    nstrSize: Integer;
begin
  streamRes:=TResourceStream.CreateFromID(hModule,LongWord(lpszName),lpszType);
  try
    streamRes.Seek(0,0);
    nIdx:=0;
    while streamRes.Position<streamRes.Size do begin
      // make the work visible
      progressLoad.Position:=(progressLoad.Position+1) mod 100;
      SetLength(Buffer,2);
      streamRes.Read(Buffer[0],2);  // Size of next string
      nStrSize:=Buffer[0]+Buffer[1]*256;
      if nStrSize=0 then
        break;
      SetLength(Buffer,nStrSize*2);  // *2 because of WideString
      streamRes.Read(Buffer[0],nStrSize*2);
      MergeToValues(Format('%s_STRING_%d',[ChangeFileExt(ExtractFilename(FFilename),''),(LongWord(lpszName)-1)*16+nIdx]),WideString(Buffer),FColumn);
      Inc(nIdx);
    end;
  finally
    streamRes.Free;
  end;
end;

procedure TfrmMain.LoadValues(sFilename:String;nCol:Integer);
const nMaxStrLen = 2000;
var lwInstance: THandle;
begin
  lwInstance:=LoadLibraryEx(PChar(sFilename),0,LOAD_LIBRARY_AS_DATAFILE);
  try
    if (lwInstance=0) then
      exit; //==>
    FFilename:=sFilename;
    FColumn:=nCol;
    // enum ressourcestrings
    EnumResourceNames(lwInstance,RT_STRING,@EnumResourceNames_Callback,LongWord(Self));
    // enum forms
    EnumResourceNames(lwInstance,RT_RCDATA,@EnumResourceNames_Callback,LongWord(Self));
  finally
    FreeLibrary(lwInstance);
  end;
end;

procedure TfrmMain.btExtractClick(Sender: TObject);
var
  sr: TSearchRec;
  dir: string;
  fn: string;
  FullFn: string;
  LangExt: string;
begin
  btExtract.Enabled:=False;

  Values.Items.BeginUpdate;
  Values.Items.Clear;
  Values.Items.EndUpdate;

  dir :=IncludeTrailingPathDelimiter(ExtractFilePath(edBPL.Text));
  LangExt := edLang.Text;
  if FindFirst(edBPL.Text,faAnyFile,sr)=0 then begin
    repeat
      Values.Items.BeginUpdate;
      try
        fn := sr.Name;
        lbLoadInfo.Caption:=Format(_('Loading %s ...'),[fn]);
        Application.ProcessMessages;
        fullFn := ExpandFilename(dir + fn);
        LoadValues(fn, colBPL);

        lbLoadInfo.Caption:=Format(_('Loading %s ...'),[ChangeFileExt(fn,LangExt)]);
        Application.ProcessMessages;
        FullFn := ChangeFileExt(FullFn, LangExt);
        LoadValues(FullFn,colLang);

        lbEntriesInfo.Caption:=Format(_('%d translations available'),[Values.Items.Count]);
      finally
        Values.Items.EndUpdate;
      end;
    until FindNext(sr)<>0;
    FindClose(sr);
  end;
  lbLoadInfo.Caption:=_('done');
  progressLoad.Position:=0;
  btExtract.Enabled:=True;
end;

function TfrmMain.DeleteQuotes(strIn:String) : String;
begin
  Result:='';
  if (Length(strIn)>=2) and (LeftStr(strIn,1)='"') and (RightStr(strIn,1)='"') then
    Result:=copy(strIn,2,Length(strIn)-2);
end;

procedure TfrmMain.AddMsgStr(list:TStrings;strIDName,strText:AnsiString);
var nCharCount: Integer;
begin
  if pos('\n',strText)=0 then begin
    // single line
    list.Add(Format('%s "%s"',[strIDName,strText]));
    end
  else begin
    // multi line
    list.Add(Format('%s ""',[strIDName]));
    while Length(strText)>0 do begin
      if pos('\n',strText)>0 then
        nCharCount:=pos('\n',strText)+1
      else
        nCharCount:=Length(strText);
      list.Add(Format('"%s"',[copy(strText,1,nCharCount)]));
      Delete(strText,1,nCharCount);
    end;
  end;
end;

procedure TfrmMain.btMergePOClick(Sender: TObject);
var tmpList:TStringList;
    outList:TStringList;
    n,n1: Integer;
    msgid,msgstr: String;
    Item: TListItem;
    State: Integer;
    strRow: AnsiString;
    Encoding: TEncoding;
begin
  btMergePO.Enabled:=False;

  listboxMerge.Items.Clear;

  Encoding := nil;
  tmpList:=nil;
  outList:=TStringList.Create;
  try
    tmpList:=TStringList.Create;
    if chkUTF8.Checked then
      Encoding := TUTF8NoBOMEncoding.Create
    else
      Encoding := TMBCSEncoding.Create(CP_ACP, 0, 0);
    try
      tmpList.LoadFromFile(edPO.Text, Encoding);
      // if file exists -> backup file
      DeleteFile(edPO.Text+'.bak');
      RenameFile(edPO.Text,edPO.Text+'.bak');
    except
    end;
    n:=0;
    progressBusy.Min:=0;
    progressBusy.Max:=tmpList.Count;
    State:=0;  // 0:neutral, 1:msgid, 2:msgstr
    while (n<tmpList.Count) or (State = 2) do begin
      if n >= tmpList.Count then begin
        // this is so we can write the last msgstr
        strRow := '';
      end else begin
        strRow:=tmpList[n];
      end;
      case State of
      0: begin
          outList.Add(strRow);
          if pos('msgid ',strRow)>0 then begin
            State:=1;
            msgid:=DeleteQuotes(copy(strRow,7,Length(strRow)-6));
          end;
          Inc(n);
         end;
      1: begin
          if pos('msgstr ',strRow)>0 then begin
            State:=2;
            msgstr:=DeleteQuotes(copy(strRow,8,Length(strRow)-7));
            end
          else begin
            outList.Add(strRow);
            msgid:=msgid+DeleteQuotes(strRow);
          end;
          Inc(n);
         end;
      2: begin
          if pos('"',strRow)>0 then begin
            msgstr:=msgstr+DeleteQuotes(strRow);
            Inc(n);
            end
          else begin
            if Length(msgstr)>0 then begin
              // allready filled -> do not replace it
              end
            else begin
              Item:=nil;
              for n1:=0 to Values.Items.Count-1 do begin
                if Values.Items[n1].SubItems[0]=msgid then begin
                  Item:=Values.Items[n1];
                  break;
                end;
              end;
              if Item<>nil then begin
                msgstr:=Item.SubItems[1];
                listboxMerge.Items.Add(Format(_('row %d: %s -> %s -> %s'),[n,Item.Caption,Item.SubItems[0],Item.SubItems[1]]));
              end;
            end;
            AddMsgStr(outList,'msgstr',msgstr);
            State:=0;
          end;
         end;
      end;
      progressBusy.Position:=n;
      Application.ProcessMessages;
    end;
    progressBusy.Position:=0;
    outList.SaveToFile(edPO.Text, Encoding);
  finally
    Encoding.Free;
    tmpList.Free;
    outList.Free;
  end;
  btMergePO.Enabled:=True;
end;

procedure TfrmMain.b_SelectBplSourceDirClick(Sender: TObject);
var
  dir: string;
begin
  dir := ExtractFileDir(edBPL.Text);
  if SelectDirectory(_('Select directory containing translation bpl files'), '', dir, [sdNewUI], Self) then
    edBPL.Text := IncludeTrailingPathDelimiter(dir) + '*.bpl';
end;

procedure TfrmMain.b_SelectPoClick(Sender: TObject);
begin
  OpenDialog.Filter := 'PO files (*.po)|*.po';
  OpenDialog.FileName := edPO.Text;
  if OpenDialog.Execute(Self.Handle) then
    edPO.Text := OpenDialog.FileName;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  lbLoadInfo.Caption := '';
  lbEntriesInfo.Caption := '';
end;

procedure TfrmMain.btCreatePOClick(Sender: TObject);
var outList:TStringList;
    n: Integer;
    strMsgID,strMsgStr: String;
  Encoding: TEncoding;
begin
  btCreatePO.Enabled:=False;
  listboxMerge.Items.Clear;
  outList:=TStringList.Create;
  try
    outList.LoadFromFile(edPO.Text);
    // if file exists -> backup file
    DeleteFile(edPO.Text+'.bak');
    RenameFile(edPO.Text,edPO.Text+'.bak');
    DeleteFile(edPO.Text);
  except
  end;
  Values.AlphaSort;
  outList.Clear;
  outList.Add('# SOME DESCRIPTIVE TITLE.');
  outList.Add('# Copyright (C) YEAR THE PACKAGE''S COPYRIGHT HOLDER');
  outList.Add('# This file is distributed under the same license as the PACKAGE package.');
  outList.Add('# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.');
  outList.Add('#');
  strMsgID:='';
  strMsgStr:='Project-Id-Version: \n'+
             'POT-Creation-Date: '+FormatDateTime('yyyy-mm-dd hh:nn',Now())+'\n'+
             'PO-Revision-Date: \n'+
             'Last-Translator: \n'+
             'Language-Team: \n'+
             'MIME-Version: 1.0\n'+
             'Content-Type: text/plain; charset=';
  if chkUTF8.Checked then
    strMsgStr:=strMsgStr+'utf-8\n'
  else
    strMsgStr:=strMsgStr+'iso-8859-1\n';
  strMsgStr:=strMsgStr+'Content-Transfer-Encoding: 8bit\n';
  progressBusy.Min:=0;
  progressBusy.Max:=Values.Items.Count;
  for n:=0 to Values.Items.Count-1 do begin
    if strMsgID<>Values.Items[n].SubItems[0] then begin
      AddMsgStr(outList,'msgid',strMsgID);
      AddMsgStr(outList,'msgstr',strMsgStr);
      outList.Add('');
      strMsgID:=Values.Items[n].SubItems[0];
      strMsgStr:=Values.Items[n].SubItems[1];
    end;
    outlist.Add('#. '+Values.Items[n].Caption);
    progressBusy.Position:=n;
    Application.ProcessMessages;
  end;
  AddMsgStr(outList,'msgid',strMsgID);
  AddMsgStr(outList,'msgstr',strMsgStr);
  if chkUTF8.Checked then
    for n:=0 to outList.Count-1 do
      outList[n]:=AnsiToUTF8(outList[n]);

  if chkUTF8.Checked then
    Encoding := TUTF8NoBOMEncoding.Create
  else
    Encoding := TMBCSEncoding.Create(CP_ACP, 0, 0);
  try
    outList.SaveToFile(edPO.Text, Encoding);
  finally
    Encoding.Free;
  end;
  outList.Free;
  progressBusy.Position:=0;
  btCreatePO.Enabled:=True;
end;

procedure TfrmMain.ValuesCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  Compare:=CompareStr(Item1.SubItems[0],Item2.SubItems[0]);
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.Load1Click(Sender: TObject);
var n:Integer;
    tmpList: TStrings;
    strRow: String;
begin
  OpenDialog.FileName := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
    + 'resources.txt';
  OpenDialog.Filter := 'Text files (*.txt)|*.txt';
  if OpenDialog.Execute then begin
    tmpList:=TStringList.Create;
    tmpList.LoadFromFile(OpenDialog.Filename);
    Values.Items.BeginUpdate;
    try
      Values.Items.Clear;
      for n:=0 to tmpList.Count-1 do begin
        strRow:=tmpList[n];
        with Values.Items.Add do begin
          Caption:=copy(strRow,1,Pos(#9,strRow)-1);
          System.Delete(strRow,1,Pos(#9,strRow));
          SubItems.Add(copy(strRow,1,Pos(#9,strRow)-1));
          System.Delete(strRow,1,Pos(#9,strRow));
          SubItems.Add(strRow);
        end;
      end;
    finally
      Values.Items.EndUpdate;
    end;
    tmpList.Free;
  end;
end;

procedure TfrmMain.Save1Click(Sender: TObject);
var n:Integer;
    tmpList: TStrings;
begin
  SaveDialog.FileName := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
    + 'resources.txt';
  SaveDialog.Filter := 'Text files (*.txt)|*.txt';
  if SaveDialog.Execute then begin
    tmpList:=TStringList.Create;
    for n:=0 to Values.Items.Count-1 do
      tmpList.Add(Values.Items[n].Caption+#9+Values.Items[n].SubItems[0]+#9+Values.Items[n].SubItems[1]);
    tmpList.SaveToFile(SaveDialog.Filename);
    tmpList.Free;
  end;
end;

procedure TfrmMain.Help1Click(Sender: TObject);
begin
  ShellExecute(Handle,'open',PChar(ExpandFilename(ExtractFilePath(Application.ExeName)+'/translation-readme.html')),nil,nil,SW_SHOW);
end;

end.
