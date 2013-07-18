unit PoDiffHtml;

interface

uses sysutils, syncobjs;

procedure RunDiff(const base, mine, theirs: tfilename);

implementation

uses IOUtils, Generics.Collections, poparser, ShellApi, Classes, Windows,
  strutils;

var
  crit: TCriticalSection;

type
  tItm = record
    MsgId, MsgStr//, text
      : string;
    function GetTranslation: string;
    class function Modified(const l, r: tItm): boolean; static;
    // two items are considered equal if function results true
  end;

  tPoFileReader = class
    // adapter class between poparser and this unit
  strict private
    Items: TPoEntryList;
    BuferedItem: TPoEntry;
  public
    constructor Create(const f: tfilename);
    destructor Destroy; override;
    function Eof: boolean;
    procedure First(out Eof: boolean);
    procedure ReadAndNext(var Item: tItm; out Eof: boolean);
  end;

  tItmStorage = class
  strict private
  type
    tDictStr = TDictionary<string, boolean>;
    tDictStrItm = TDictionary<string, tItm>;
    tRowStyle = (st_normal, st_yellow, st_red);
  strict private
    // lists
    BaseItems: tDictStrItm;
    MineItems: tDictStr;
    TheirsItems: tDictStr;
    AddedInMine: tDictStrItm;
    AddedInTheirs: tDictStrItm;
    ModifiedInMine: tDictStrItm;
    ModifiedInTheirs: tDictStrItm;
    // views
    RemovedInMine: tDictStr;
    RemovedInTheirs: tDictStr;
    AddedInBoth: tDictStr;
    ModifiedInBoth: tDictStr;
    RemovedInBoth: tDictStr;
    MineModTheirsRem: tDictStr;
    MineRemTheirsMod: tDictStr;
    // view for sorting
    AllItemsOrder: TStringList;
  strict private
    procedure StartTable(sl: TStringList; Columns: array of string);
    procedure WriteTableLine(sl: TStringList; style: tRowStyle;
      Columns: array of string);
    procedure EndTable(sl: TStringList);
    function Enc(s: string): string;
    procedure FillFileHeader(const s: string; sl: TStringList);
    procedure FillFileAddedInMineExclAddedInBoth(sl: TStringList);
    procedure FillFileAddedInTheirsExclAddedInBoth(sl: TStringList);
    procedure FillFileAddedInBoth(sl: TStringList);
    procedure FillFileRemovedFromMine_TheirsNoChange(sl: TStringList);
    procedure FillFileRemovedFromTheirs_MineNoChange(sl: TStringList);
    procedure FillFileRemovedFromBoth(sl: TStringList);
    procedure FillFileRemovedFromMine_TheirsModified(sl: TStringList);
    procedure FillFileRemovedFromTheirs_MineModified(sl: TStringList);
    procedure FillFileModifiedInMine_TheirsNoChange(sl: TStringList);
    procedure FillFileModifiedInTheirs_MineNoChange(sl: TStringList);
    procedure FillFileModifiedInBoth(sl: TStringList);
    procedure FillFileFooter(sl: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Prepare;
    procedure FillHtmlFile(const f: tfilename);
    procedure LoadBaseFile(r: tPoFileReader);
    procedure LoadMineFile(r: tPoFileReader);
    procedure LoadTheirsFile(r: tPoFileReader);
  end;

function Check3FilesExist(base, mine, theirs: tfilename;
  var baseFullName, mineFullName, theirsFullName: tfilename): boolean;
var
  errmsg: string;
  bFex, mFex, tFex: boolean;
begin
  baseFullName := ExpandFileName(base);
  mineFullName := ExpandFileName(mine);
  theirsFullName := ExpandFileName(theirs);
  bFex := FileExists(baseFullName);
  mFex := FileExists(mineFullName);
  tFex := FileExists(theirsFullName);
  if bFex and mFex and tFex then
    exit(true)
  else
  begin
    if not bFex and not mFex and not tFex then
      errmsg := Format
        ('Base file "%s", mine file "%s" and theirs file "%s" don''t exist',
        [baseFullName, mineFullName, theirsFullName])
    else if not bFex and not mFex then
      errmsg := Format('Base file "%s" and mine file "%s" don''t exist',
        [baseFullName, mineFullName])
    else if not bFex and not tFex then
      errmsg := Format('Base file "%s" and theirs file "%s" don''t exist',
        [baseFullName, theirsFullName])
    else if not mFex and not tFex then
      errmsg := Format('Mine file "%s" and theirs file "%s" don''t exist',
        [mineFullName, theirsFullName])
    else if not bFex then
      errmsg := Format('Base file "%s" doesn''t exist', [baseFullName])
    else if not mFex then
      errmsg := Format('Mine file "%s" doesn''t exist', [mineFullName])
    else if not tFex then
      errmsg := Format('Theirs file "%s" doesn''t exist', [theirsFullName])
    else
      raise Exception.Create('Unreachable code reached');
    raise Exception.Create(errmsg);
  end;
end;

function Check3FilesValidNParse(base, mine, theirs: tfilename;
  ItmStorage: tItmStorage): boolean;
var
  BaseReader, MineReader, TheirsReader: tPoFileReader;
begin
  // background task is to Check for duplicates items in all of these files
  // load Base file items
  // create Added, Modified in Mine lists
  // create Added, Modified in Theirs lists
  // create Removed in Mine, Theirs, Both lists
  // create Added both in Mine and Theirs view
  // create Modified both in Mine and Theirs
  BaseReader := tPoFileReader.Create(base);
  try
    MineReader := tPoFileReader.Create(mine);
    try
      TheirsReader := tPoFileReader.Create(theirs);
      try
        ItmStorage.LoadBaseFile(BaseReader);
        ItmStorage.LoadMineFile(MineReader);
        ItmStorage.LoadTheirsFile(TheirsReader);
        ItmStorage.Prepare;
        exit(true);
      finally
        freeandnil(TheirsReader);
      end;
    finally
      freeandnil(MineReader);
    end;
  finally
    freeandnil(BaseReader);
  end;
end;

function MakeDiffHtmlInTMPFolder(base, mine, theirs: tfilename): tfilename;
var
  baseF, mineF, theirsF: tfilename;
  ItmStorage: tItmStorage;
begin
  if Check3FilesExist(base, mine, theirs, baseF, mineF, theirsF) then
  begin
    ItmStorage := tItmStorage.Create;
    try
      if Check3FilesValidNParse(baseF, mineF, theirsF, ItmStorage) then
        try
          Result := ChangeFileExt(TPath.GetTempFileName, '.html');
          ItmStorage.FillHtmlFile(Result);
        except
          TFile.Delete(Result);
          raise;
        end;
    finally
      freeandnil(ItmStorage);
    end;
  end;
end;

{tItmStorage}

constructor tItmStorage.Create;
begin
  BaseItems := tDictStrItm.Create;
  MineItems := tDictStr.Create;
  TheirsItems := tDictStr.Create;
  AddedInMine := tDictStrItm.Create;
  AddedInTheirs := tDictStrItm.Create;
  ModifiedInMine := tDictStrItm.Create;
  ModifiedInTheirs := tDictStrItm.Create;
  RemovedInMine := tDictStr.Create;
  RemovedInTheirs := tDictStr.Create;
  AddedInBoth := tDictStr.Create;
  ModifiedInBoth := tDictStr.Create;
  RemovedInBoth := tDictStr.Create;
  AllItemsOrder := TStringList.Create;
  AllItemsOrder.CaseSensitive := true;
  AllItemsOrder.Duplicates := tduplicates.dupError;
  MineModTheirsRem := tDictStr.Create;
  MineRemTheirsMod := tDictStr.Create;
end;

destructor tItmStorage.Destroy;
begin
  freeandnil(BaseItems);
  freeandnil(MineItems);
  freeandnil(TheirsItems);
  freeandnil(AddedInMine);
  freeandnil(AddedInTheirs);
  freeandnil(ModifiedInMine);
  freeandnil(ModifiedInTheirs);
  freeandnil(RemovedInMine);
  freeandnil(RemovedInTheirs);
  freeandnil(AddedInBoth);
  freeandnil(ModifiedInBoth);
  freeandnil(RemovedInBoth);
  freeandnil(AllItemsOrder);
  freeandnil(MineModTheirsRem);
  freeandnil(MineRemTheirsMod);
  inherited;
end;

function tItmStorage.Enc(s: string): string;
var
  i: int32;
begin
  Result := '';
  for i := 1 to length(s) do
  begin
    if (ord(s[i]) < 34) or (ord(s[i]) < 62) then
      Result := Result + s[i]
    else if s[i] = '"' then
      Result := Result + '&quot;'
    else if s[i] = '&' then
      Result := Result + '&amp;'
    else if s[i] = '''' then
      Result := Result + '&apos;'
    else if s[i] = '<' then
      Result := Result + '&lt;'
    else if s[i] = '>' then
      Result := Result + '&gt;'
    else
      Result := Result + s[i];
  end;
  Result := ReplaceStr(Result, #10{/n cr}, '<br>');
end;

procedure tItmStorage.EndTable(sl: TStringList);
begin
  sl.Add('</table>')
end;

procedure tItmStorage.FillFileAddedInBoth(sl: TStringList);
var
  s: string;
  i: int32;
  state: tRowStyle;
begin
  i := 1;
  for s in AddedInBoth.Keys do
  begin
    if i = 1 then
    begin
      sl.Add('Added in both Mine and Theirs file:<br>');
      StartTable(sl, ['#', 'Status', 'MsgId', 'Mine translation',
        'Theirs translation']);
    end;
    if tItm.Modified(AddedInMine[s], AddedInTheirs[s]) then
      state := st_red
    else
      state := st_yellow;
    WriteTableLine(sl, state, [inttostr(i), 'Added in both', s,
      AddedInMine[s].GetTranslation, AddedInTheirs[s].GetTranslation]);
    i := i + 1;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillFileAddedInMineExclAddedInBoth(sl: TStringList);
var
  s: string;
  i: int32;
begin
  i := 1;
  for s in AddedInMine.Keys do
  begin
    if not AddedInBoth.ContainsKey(s) then
    begin
      if i = 1 then
      begin
        sl.Add('Added in Mine:<br>');
        StartTable(sl, ['#', 'Status', 'MsgId', 'Translation']);
      end;
      WriteTableLine(sl, st_normal, [inttostr(i), 'Added in mine', s,
        AddedInMine[s].GetTranslation]);
      i := i + 1;
    end;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillFileAddedInTheirsExclAddedInBoth(sl: TStringList);
var
  s: string;
  i: int32;
begin
  i := 1;
  for s in AddedInTheirs.Keys do
  begin
    if not AddedInBoth.ContainsKey(s) then
    begin
      if i = 1 then
      begin
        sl.Add('Added in Theirs:<br>');
        StartTable(sl, ['#', 'Status', 'MsgId', 'Translation']);
      end;
      WriteTableLine(sl, st_normal, [inttostr(i), 'Added in theirs', s,
        AddedInTheirs[s].GetTranslation]);
      i := i + 1;
    end;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillFileFooter(sl: TStringList);
begin
  sl.Add('<center><hr width="60%"></center>');
  sl.Add('Output created: ' + DateTimeToStr(Now));
  sl.Add('</body></html>');
end;

procedure tItmStorage.FillFileHeader(const s: string; sl: TStringList);
begin
  sl.Add('<html>');
  sl.Add('<head>');
  sl.Add('<title>Output from Gorm</title>');
  sl.Add('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>');
  sl.Add('<style type="text/css">');
  sl.Add('.sR{color:black;background-color:#FF9090;}'); //~red
  sl.Add('.sY{color:black;background-color:#DFDFA0;}'); //~yellow
  sl.Add('</style>');
  sl.Add('</head>');
  sl.Add('<body style="font-family:''Arial Unicode MS'',Tahoma">');
  sl.Add('Diff file: "' + s + '"');
  sl.Add('<br>');
  sl.Add('<br>');
end;

procedure tItmStorage.FillFileModifiedInBoth(sl: TStringList);
var
  s: string;
  i: int32;
  state: tRowStyle;
begin
  i := 1;
  for s in ModifiedInBoth.Keys do
  begin
    if i = 1 then
    begin
      sl.Add('Modified in both Mine and Theirs file:<br>');
      StartTable(sl, ['#', 'Status', 'MsgId', 'Base Translation',
        'Mine Translation', 'Theirs Translation']);
    end;
    if tItm.Modified(ModifiedInMine[s], ModifiedInTheirs[s]) then
      state := st_red
    else
      state := st_yellow;
    WriteTableLine(sl, state, [inttostr(i), 'Modified in both', s,
      BaseItems[s].GetTranslation, ModifiedInMine[s].GetTranslation,
      ModifiedInTheirs[s].GetTranslation]);
    i := i + 1;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillFileModifiedInMine_TheirsNoChange(sl: TStringList);
var
  s: string;
  i: int32;
begin
  i := 1;
  for s in ModifiedInMine.Keys do
  begin
    if not ModifiedInTheirs.ContainsKey(s) and
      not RemovedInTheirs.ContainsKey(s) then
    begin
      if i = 1 then
      begin
        sl.Add('Modified in Mine:<br>');
        StartTable(sl, ['#', 'Status', 'MsgId', 'Base Translation',
          'Mine Translation']);
      end;
      WriteTableLine(sl, st_normal, [inttostr(i), 'Modified in mine', s,
        BaseItems[s].GetTranslation, ModifiedInMine[s].GetTranslation]);
      i := i + 1;
    end;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillFileModifiedInTheirs_MineNoChange(sl: TStringList);
var
  s: string;
  i: int32;
begin
  i := 1;
  for s in ModifiedInTheirs.Keys do
  begin
    if not ModifiedInMine.ContainsKey(s) and
      not RemovedInMine.ContainsKey(s) then
    begin
      if i = 1 then
      begin
        sl.Add('Modified in Theirs:<br>');
        StartTable(sl, ['#', 'Status', 'MsgId', 'Base Translation',
          'Theirs Translation']);
      end;
      WriteTableLine(sl, st_normal, [inttostr(i), 'Modified in theirs', s,
        BaseItems[s].GetTranslation, ModifiedInTheirs[s].GetTranslation]);
      i := i + 1;
    end;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillFileRemovedFromBoth(sl: TStringList);
var
  s: string;
  i: int32;
begin
  i := 1;
  for s in RemovedInBoth.Keys do
  begin
    if i = 1 then
    begin
      sl.Add('Removed from both Mine and Theirs file:<br>');
      StartTable(sl, ['#', 'Status', 'MsgId', 'Translation']);
    end;
    WriteTableLine(sl, st_normal, [inttostr(i), 'Removed from both', s,
      BaseItems[s].GetTranslation]);
    i := i + 1;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillFileRemovedFromMine_TheirsModified(sl: TStringList);
var
  s: string;
  i: int32;
begin
  i := 1;
  for s in RemovedInMine.Keys do
  begin
    if ModifiedInTheirs.ContainsKey(s) then
    begin
      if i = 1 then
      begin
        sl.Add('Removed from Mine and Theirs file was modified:<br>');
        StartTable(sl, ['#', 'Status', 'MsgId', 'Base Translation',
          'Theirs Translation']);
      end;
      WriteTableLine(sl, st_red,
        [inttostr(i), 'Removed from Mine, theirs modified', s,
        BaseItems[s].GetTranslation, ModifiedInTheirs[s].GetTranslation]);
      i := i + 1;
    end;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillFileRemovedFromMine_TheirsNoChange(sl: TStringList);
var
  s: string;
  i: int32;
begin
  i := 1;
  for s in RemovedInMine.Keys do
  begin
    if not ModifiedInTheirs.ContainsKey(s) and
      not RemovedInTheirs.ContainsKey(s) then
    begin
      if i = 1 then
      begin
        sl.Add('Removed from Mine file:<br>');
        StartTable(sl, ['#', 'Status', 'MsgId', 'Translation']);
      end;
      WriteTableLine(sl, st_normal, [inttostr(i), 'Removed from mine', s,
        BaseItems[s].GetTranslation]);
      i := i + 1;
    end;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillFileRemovedFromTheirs_MineModified(sl: TStringList);
var
  s: string;
  i: int32;
begin
  i := 1;
  for s in RemovedInTheirs.Keys do
  begin
    if ModifiedInMine.ContainsKey(s) then
    begin
      if i = 1 then
      begin
        sl.Add('Removed from Theirs and Mine file was modified:<br>');
        StartTable(sl, ['#', 'Status', 'MsgId', 'Base Translation',
          'Mine Translation']);
      end;
      WriteTableLine(sl, st_red,
        [inttostr(i), 'Removed from theirs, mine modified', s,
        BaseItems[s].GetTranslation, ModifiedInMine[s].GetTranslation]);
      i := i + 1;
    end;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillFileRemovedFromTheirs_MineNoChange(sl: TStringList);
var
  s: string;
  i: int32;
begin
  i := 1;
  for s in RemovedInTheirs.Keys do
  begin
    if not ModifiedInMine.ContainsKey(s) and
      not RemovedInMine.ContainsKey(s) then
    begin
      if i = 1 then
      begin
        sl.Add('Removed from Theirs file:<br>');
        StartTable(sl, ['#', 'Status', 'MsgId', 'Translation']);
      end;
      WriteTableLine(sl, st_normal, [inttostr(i), 'Removed from Theirs', s,
        BaseItems[s].GetTranslation]);
      i := i + 1;
    end;
  end;
  if i > 1 then
  begin
    EndTable(sl);
    sl.Add('<br>');
  end;
end;

procedure tItmStorage.FillHtmlFile(const f: tfilename);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    FillFileHeader(f, sl);
    FillFileAddedInMineExclAddedInBoth(sl);
    FillFileAddedInTheirsExclAddedInBoth(sl);
    FillFileAddedInBoth(sl);
    FillFileRemovedFromMine_TheirsNoChange(sl);
    FillFileRemovedFromMine_TheirsModified(sl);
    FillFileRemovedFromTheirs_MineNoChange(sl);
    FillFileRemovedFromTheirs_MineModified(sl);
    FillFileRemovedFromBoth(sl);
    FillFileModifiedInMine_TheirsNoChange(sl);
    FillFileModifiedInTheirs_MineNoChange(sl);
    FillFileModifiedInBoth(sl);
    FillFileFooter(sl);
    sl.SaveToFile(f, TEncoding.UTF8);
  finally
    freeandnil(sl);
  end;

end;

procedure tItmStorage.Prepare;
var
  MsgId: string;
begin
  // info. dictionaries are only accept new items, and not get cleared
  // that's why deleted both items will also be in deleted mine, theirs, etc.
  for MsgId in BaseItems.Keys do
  begin
    if not MineItems.ContainsKey(MsgId) then
      RemovedInMine.Add(MsgId, false);
    if not TheirsItems.ContainsKey(MsgId) then
      RemovedInTheirs.Add(MsgId, false);
  end;
  for MsgId in RemovedInTheirs.Keys do
    if RemovedInMine.ContainsKey(MsgId) then
      RemovedInBoth.Add(MsgId, false);
  for MsgId in AddedInTheirs.Keys do
    if AddedInMine.ContainsKey(MsgId) then
    begin
      AddedInBoth.Add(MsgId, false);
    end;
  for MsgId in ModifiedInTheirs.Keys do
    if ModifiedInMine.ContainsKey(MsgId) then
    begin
      ModifiedInBoth.Add(MsgId, false);
    end;
  for MsgId in ModifiedInMine.Keys do
    if RemovedInTheirs.ContainsKey(MsgId) then
    begin
      MineModTheirsRem.Add(MsgId, false);
    end;
  for MsgId in ModifiedInTheirs.Keys do
    if RemovedInMine.ContainsKey(MsgId) then
    begin
      MineRemTheirsMod.Add(MsgId, false);
    end;
  for MsgId in AddedInMine.Keys do
    AllItemsOrder.Add(MsgId);
  for MsgId in AddedInTheirs.Keys do
    if not AddedInMine.ContainsKey(MsgId) then
      AllItemsOrder.Add(MsgId);
  AllItemsOrder.Sort;
end;

procedure tItmStorage.StartTable(sl: TStringList; Columns: array of string);
var
  i: int32;
begin
  sl.Add('<table width="90%" align="center" border="1">');
  sl.Add('<tr>');
  for i := low(Columns) to high(Columns) do
    sl.Add('<td><strong>' + Enc(Columns[i]) + '</strong></td>');
  sl.Add('</tr>');
end;

procedure tItmStorage.WriteTableLine(sl: TStringList; style: tRowStyle;
  Columns: array of string);
var
  i: int32;
begin
  if style = st_normal then
    sl.Add('<tr>')
  else if style = st_yellow then
    sl.Add('<tr class="sY">')
  else if style = st_red then
    sl.Add('<tr class="sR">')
  else
    sl.Add('<tr>');
  for i := low(Columns) to high(Columns) do
    sl.Add('<td>' + Enc(Columns[i]) + '</td>');
  sl.Add('</tr>');
end;

procedure tItmStorage.LoadBaseFile(r: tPoFileReader);
var
  itm: tItm;
  ReaderEof: boolean;
begin
  r.First(ReaderEof);
  while not ReaderEof do
  begin
    r.ReadAndNext(itm, ReaderEof);
    if not BaseItems.ContainsKey(itm.MsgId) then
    begin
      BaseItems.Add(itm.MsgId, itm);
      AllItemsOrder.Add(itm.MsgId);
    end
    else
      raise Exception.Create
        (Format('Diff cannot handle MsgId duplicates in Base file. Please remove the duplicate of "%s"',
        [itm.MsgId]));
  end;
end;

procedure tItmStorage.LoadMineFile(r: tPoFileReader);
var
  itm, tmp: tItm;
  ReaderEof: boolean;
begin
  r.First(ReaderEof);
  while not ReaderEof do
  begin
    r.ReadAndNext(itm, ReaderEof);
    if not MineItems.ContainsKey(itm.MsgId) then
    begin
      MineItems.Add(itm.MsgId, false);
      if BaseItems.TryGetValue(itm.MsgId, tmp) then
      begin
        // compare
        if tItm.Modified(itm, tmp) then
          ModifiedInMine.Add(itm.MsgId, itm)
      end
      else
        AddedInMine.Add(itm.MsgId, itm)
    end
    else
      raise Exception.Create
        (Format('Diff cannot handle MsgId duplicates in Mine file. Please remove the duplicate of "%s"',
        [itm.MsgId]));
  end;
end;

procedure tItmStorage.LoadTheirsFile(r: tPoFileReader);
var
  itm, tmp: tItm;
  ReaderEof: boolean;
begin
  r.First(ReaderEof);
  while not ReaderEof do
  begin
    r.ReadAndNext(itm, ReaderEof);
    if not TheirsItems.ContainsKey(itm.MsgId) then
    begin
      TheirsItems.Add(itm.MsgId, false);
      if BaseItems.TryGetValue(itm.MsgId, tmp) then
      begin
        // compare
        if tItm.Modified(itm, tmp) then
          ModifiedInTheirs.Add(itm.MsgId, itm)
      end
      else
        AddedInTheirs.Add(itm.MsgId, itm);
    end
    else
      raise Exception.Create
        (Format('Diff cannot handle MsgId duplicates in Theirs file. Please remove the duplicate of "%s"',
        [itm.MsgId]));
  end;
end;

{tPoFileReader}

constructor tPoFileReader.Create(const f: tfilename);
begin
  Items := TPoEntryList.Create;
  Items.LoadFromFile(f);
  BuferedItem := Items.FindFirst;
end;

destructor tPoFileReader.Destroy;
begin
  freeandnil(Items);
  inherited;
end;

function tPoFileReader.Eof: boolean;
begin
  exit(BuferedItem = nil);
end;

procedure tPoFileReader.First(out Eof: boolean);
begin
  BuferedItem := Items.FindFirst;
  Eof := BuferedItem = nil;
end;

procedure tPoFileReader.ReadAndNext(var Item: tItm; out Eof: boolean);
begin
  if BuferedItem = nil then
    raise Exception.Create('End of po file')
  else
  begin
    Item.MsgId := BuferedItem.MsgId;
    Item.MsgStr := BuferedItem.MsgStr;
    //Item.text :=BuferedItem.UserCommentList.Text+BuferedItem.AutoCommentList.Text;
    //todo
    BuferedItem := Items.FindNext(BuferedItem);
    Eof := BuferedItem = nil;
  end;
end;

{tItm}

function tItm.GetTranslation: string;
begin
  Result := Self.MsgStr;
end;

class function tItm.Modified(const l, r: tItm): boolean;
begin
  if l.MsgId <> r.MsgId then
    raise Exception.Create('Comparing should be done to items with same MsgId')
  else
    exit(l.MsgStr <> r.MsgStr);
end;

procedure RunDiff(const base, mine, theirs: tfilename);
var
  proc: tproc;
begin
  proc := procedure
  var
    doc: string;
  begin
    crit.Enter;
    doc := MakeDiffHtmlInTMPFolder(base, mine, theirs);
    if FileExists(doc) then
    begin
      ShellExecute(0, 'open', PChar(doc), nil, nil, SW_SHOWNORMAL);
      crit.Leave;
      //Sleep(1000); // auto delete file after 1s
      //TFile.Delete(doc);
    end
    else
      crit.Leave;
  end;
  TThread.CreateAnonymousThread(proc).Start;
end;

initialization

crit := TCriticalSection.Create;

finalization

crit.Enter;
freeandnil(crit);

end.
