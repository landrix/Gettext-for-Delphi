unit poparser;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl                            *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dybdahl.dk/dxgettext/ for more information       *)
(*                                                              *)
(****************************************************************)

interface

uses
  Classes;

const
  PluralSplitter=#12;

const // use these for Get/SetPoHeaderEntry calls
  PO_HEADER_PROJECT_ID_VERSION = 'Project-Id-Version:';
  PO_HEADER_LANGUAGE_TEAM = 'Language-Team:';
  PO_HEADER_LAST_TRANSLATOR = 'Last-Translator:';
  PO_HEADER_CONTENT_TYPE = 'Content-Type:';
  PO_HEADER_LANGUAGE = 'X-Poedit-Language:';
  PO_HEADER_Poedit_BasePath = 'X-Poedit-Basepath:';

type
  TObjectPascalFormat = (opfTrue, opfFalse, opfUndefined);
  TPoEntry=
    class
    private
      FIndex: integer; // remember the entry's index in the original file so we can
                       // store them in the same order as we read them
    public
      UserCommentList:TStringList;   // Entire lines
      AutoCommentList:TStringList;   // Entire lines
      MsgId:string;              // singular and plural are separated by PluraSPlitter, if plural form is present
      MsgStr:string;             // plural forms are separated by PluraSPlitter, if present
      Fuzzy:boolean;                 // If true, msgstr is not the translation, but just a proposal for a translation
      IsObjectPascalFormat: TObjectPascalFormat;
      constructor Create;
      destructor Destroy; override;
      procedure Assign (po:TPoEntry);
      procedure Clear;
      procedure WriteToStream (str:TStream; AWidth: integer = 70);  // Adds an empty line afterwards.
      function IsPluralForm:boolean;   // Returns true if MsgId is a plural form
    end;
  TPoEntryList=
    class
    private
      list:TStringList; // Strings are searchkeys, objects are TList of TPoEntries
      function GetSearchKey (MsgId:string):string;
    function GetHeaderEntry(const _Label: string): string;
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadFromFile (filename:string);
      procedure SaveToFile (filename:string; AWidth: integer = 70);
      procedure Clear;
      function Find (MsgId:string):TPoEntry;
      function Delete (MsgId:string):boolean;  // True if found and deleted, false if not found
      procedure Add (entry:TPoEntry); // Will fail if MsgId exists. Entry is copied.
      function Count:integer;
      function Language: string; // returns the X-Poedit-Language entry in the '' translation
      function ProjectAndVersion: string; // returns the Project-Id-Version entry in the '' translation
      function BasePath: String;

      // Iterate through all items. When nil is returned, no more elements are there.
      function FindFirst:TPoEntry;
      function FindNext (po:TPoEntry):TPoEntry;
    end;
  // Easy to use parser for .po files. Just put all the lines into AddLine(),
  // and each time a translation has been found, it turns true once.
  // Always end your parsing by putting an empty line into Addline.
  TPoParser=
    class
    private
      LineNumber:Integer;
      IsMsgId:boolean;
      entry:TPoEntry;  // This is the return value if last AddLine returned True
      entryhasdata:boolean;
    public
      constructor Create;
      destructor Destroy; override;

      // Put all your lines into AddLine(). It will return nil most
      // of the times, but it will return an entry each time the whitespace
      // after an entry has been reached. The entry is only valid until the next
      // call to AddLine().
      function AddLine (line:string):TPoEntry;

      // Read a couple of lines from file and return next TPoEntry. Returns nil if no more entries.
      function ReadNextEntry (var tf:TextFile):TPoEntry;
      property CurrentLineNumber:integer read LineNumber;
    end;

function GetPoHeaderEntry(const _Header: string; const _Label: string): string;
procedure SetPoHeaderEntry(var _Header: string; const _Label: string; const _Value: string);

// These use utf-8 when writing!
procedure StreamWrite (s:TStream;const line:string);
procedure StreamWriteln (s:TStream;const line:string='');
procedure StreamWriteMinimumPoHeader (s:TStream;const appname:string);
procedure StreamWriteDefaultPoTemplateHeader (s:TStream;const appname:string);

implementation

uses
  Math, SysUtils, gnugettext, u_dzQuicksort, StrUtils;

function GetPoHeaderEntry(const _Header: string; const _Label: string): string;
var
  sl: TStringList;
  s: string;
begin
  Result := '';
  sl := TStringList.Create;
  try
    sl.Text := _Header;
    for s in sl do begin
      if StartsText(_Label, s) then begin
        Result := Trim(Copy(s, Length(_Label) + 1));
        exit;
      end;
    end;
  finally
    FreeAndNil(sl);
  end;
end;

procedure SetPoHeaderEntry(var _Header: string; const _Label: string; const _Value: string);
var
  i: Integer;
  s: string;
  sl: TStringList;
  Found: boolean;
begin
  sl := TStringList.Create;
  try
    sl.Text := _Header;
    Found := false;
    for i := 0 to sl.Count - 1 do begin
      s := sl[i];
      if StartsText(_Label, s) then begin
        Found := true;
        sl[i] := _Label + ' ' + _Value;
        break;
      end;
    end;
    if not Found then
      sl.Add(_Label + ' ' + _Value);
    _Header := sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;

procedure StreamWriteMinimumPoHeader (s:TStream;const appname:string);
begin
  StreamWriteln(s, '#, fuzzy');
  StreamWriteln(s, 'msgid ""');
  StreamWriteln(s, 'msgstr ""');
  StreamWriteln(s, '"POT-Creation-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + '\n"');
  StreamWriteln(s, '"PO-Revision-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + '\n"');
  StreamWriteln(s, '"Last-Translator: Somebody <your.email@address.com>\n"');
  StreamWriteln(s, '"MIME-Version: 1.0\n"');
  StreamWriteln(s, '"Content-Type: text/plain; charset=UTF-8\n"');
  StreamWriteln(s, '"Content-Transfer-Encoding: 8bit\n"');
  StreamWriteln(s, '"X-Generator: ' + appname + '\n"');
  Streamwriteln(s, '');
end;

procedure StreamWriteDefaultPoTemplateHeader (s:TStream;const appname:string);
begin
  StreamWriteln(s, '# SOME DESCRIPTIVE TITLE.');
  StreamWriteln(s, '# Copyright (C) YEAR THE PACKAGE''S COPYRIGHT HOLDER');
  StreamWriteln(s, '# This file is distributed under the same license as the PACKAGE package.');
  StreamWriteln(s, '# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.');
  StreamWriteln(s, '# ');
  StreamWriteln(s, '#, fuzzy');
  StreamWriteln(s, 'msgid ""');
  StreamWriteln(s, 'msgstr ""');
  StreamWriteln(s, '"Project-Id-Version: PACKAGE VERSION\n"');
  StreamWriteln(s, '"POT-Creation-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + '\n"');
  StreamWriteln(s, '"PO-Revision-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + '\n"');
  StreamWriteln(s, '"Last-Translator: Somebody <your.email@address.com>\n"');
  StreamWriteln(s, '"MIME-Version: 1.0\n"');
  StreamWriteln(s, '"Content-Type: text/plain; charset=UTF-8\n"');
  StreamWriteln(s, '"Content-Transfer-Encoding: 8bit\n"');
  StreamWriteln(s, '"X-Generator: ' + appname + '\n"');
  Streamwriteln(s, '');
end;

procedure StreamWriteln (s:TStream;const line:string='');
begin
  StreamWrite (s, line);
  StreamWrite (s, sLineBreak);
end;

procedure StreamWrite (s:TStream;const line:string);
var
  len:integer;
  utf8line:utf8string;
begin
  {$ifdef UNICODE} // >=D2009
  utf8line:=utf8string(line);
  {$else}
  utf8line := UTF8Encode(line);
  {$endif}
  len:=length(utf8line);
  if len>0 then
    if s.Write(utf8line[1],len)<>len then
      raise Exception.Create (_('Error when writing to stream.'));
end;

function String2PO (const s:string):string;
// Converts a string to the syntax that is used in .po files
var
  i: integer;
  c: char;
  escnext:boolean;
begin
  Result := '';
  escnext:=False;
  for i := 1 to length(s) do begin
    c := s[i];
    case c of
      #32..#33, #35..pred('\'), succ('\')..#65535:
        begin
          // This will also support 20-bit unicode points
          if escnext then Result:=Result+'\';
          Result := Result + c;
          escnext:=False;
        end;
      '\':begin
            Result:=Result+'\\';
            escnext:=False;
          end;
      #13:; // Do nothing
      #10:begin
            Result := Result + '\n';
            escnext:=False;
          end;
      #34:begin
            Result := Result + '\"';
            escnext:=False;
          end;
      #0:begin
           Result := Result + '\0';
           escnext:=True;
         end;
      #9:begin
           Result:=Result+'\t';
           escnext:=False;
         end;
    else
      Result := Result + '\x' + IntToHex(ord(c),2);
      escnext:=True;
    end;
  end;
  Result := '"' + Result + '"';
end;

{ TPoParser }

function TPoParser.AddLine(line: string):TPoEntry;
var
  i:integer;
  value:string;
begin
  try
    Inc (LineNumber);
    line:=trim(line);
    if line<>'' then begin
      if not entryhasdata then begin
        entry.Clear;
        entryhasdata:=False;
      end;
      if copy(line,1,2)='#,' then
      begin
        if (pos(', fuzzy', line) > 0) then
        begin
          entry.Fuzzy:=True;
        end;

        if (pos(', no-object-pascal-format', line) > 0) then
        begin
          entry.IsObjectPascalFormat := opfFalse;
        end
        else
        if (pos(', object-pascal-format', line) > 0) then
        begin
          entry.IsObjectPascalFormat := opfTrue;
        end
        else
        begin
          entry.IsObjectPascalFormat := opfUndefined;
        end;
      end
      else
      if copy(line,1,2)='#~' then
        entry.AutoCommentList.Add(line)
      else
      if copy(line,1,2)='# ' then
        entry.UserCommentList.Add(line)
      else
      if copy(line,1,1)='#' then
        entry.AutoCommentList.Add(line)
      else begin
        if uppercase(copy(line,1,12))='MSGID_PLURAL' then begin
          IsMsgId:=True;
          delete (line,1,12);
          line:=trim(line);
          entry.MsgId:=entry.MsgId+PluralSplitter;
        end;
        if uppercase(copy(line,1,5))='MSGID' then begin
          IsMsgId:=True;
          delete (line,1,5);
          line:=trim(line);
        end;
        if uppercase(copy(line,1,6))='MSGSTR' then begin
          IsMsgId:=False;
          delete (line,1,6);
          if copy(line,1,1)='[' then begin
            if copy(line,2,1)<>'0' then
              entry.MsgStr:=entry.MsgStr+PluralSplitter;
            delete (line,1,3);
          end;
          line:=trim(line);
        end;
        if (copy(line,1,1)<>'"') or (copy(line,length(line),1)<>'"') then
          raise Exception.Create (Format(_('Illegal line: %s'),[line]));
        value:=copy(line,2,length(line)-2);
        i:=1;
        while i<length(value) do begin
          if value[i]='\' then begin
            delete (value,i,1);
            case value[i] of
              'n':value[i]:=#10;
              't':value[i]:=#9;
              'x':begin
                    value[i]:=char(StrToInt('$'+copy(value,i+1,2)));
                    delete (value,i+1,2);
                  end;
            else
              // Do nothing - the character was just escaped.
            end;
          end;
          inc (i);
        end;
        if IsMsgId then entry.MsgId:=entry.MsgId+value
                   else entry.MsgStr:=entry.MsgStr+value;
      end;
    end;
    if (line='') and entryhasdata then begin
      if (entry.MsgId='') and (entry.MsgStr='') then begin
        // This entry contains nothing. It's probably a deleted entry (using #~)
        Result:=nil;
      end else
        Result:=entry;
    end else begin
      Result:=nil;
    end;
    entryhasdata:=line<>'';
  except
    on e:Exception do
      raise Exception.Create (format(_('Exception %s in line %d:'+SLineBreak+'%s'),[e.ClassName,LineNumber,e.Message]));
  end;
end;

constructor TPoParser.Create;
begin
  LineNumber:=0;
  entry:=TPoEntry.Create;
end;

destructor TPoParser.Destroy;
begin
  FreeAndNil (entry);
  inherited;
end;

{$ifndef UNICODE} // <D2009
function UTF8ToUnicodeString(aText: RawByteString): string;
begin
  result := Utf8ToAnsi(aText);
end;
{$endif}

function TPoParser.ReadNextEntry(var tf: TextFile): TPoEntry;
var
  rline:RawByteString;
  line:string;
begin
  while not eof(tf) do begin
    Readln (tf, rline);
    line:= UTF8ToUnicodeString(rline);
    Result:=AddLine(line);
    if Result<>nil then
      exit;
  end;
  Result:=AddLine ('');
end;


{ TPoEntry }

procedure TPoEntry.Assign(po: TPoEntry);
begin
  UserCommentList.Assign(po.UserCommentList);
  AutoCommentList.Assign(po.AutoCommentList);

  FIndex := po.FIndex;
  MsgId     := po.MsgId;
  MsgStr    := po.MsgStr;
  Fuzzy     := po.Fuzzy;
  IsObjectPascalFormat := po.IsObjectPascalFormat;
end;

procedure TPoEntry.Clear;
begin
  UserCommentList.Clear;
  AutoCommentList.Clear;

  FIndex := -1;
  MsgId     := '';
  MsgStr    := '';
  Fuzzy     := False;
  IsObjectPascalFormat := opfUndefined;
end;

constructor TPoEntry.Create;
begin
  inherited Create;
  FIndex := -1; // invalid
  UserCommentList:=TStringList.Create;
  AutoCommentList:=TStringList.Create;
end;

destructor TPoEntry.Destroy;
begin
  FreeAndNil (UserCommentList);
  FreeAndNil (AutoCommentList);
  inherited;
end;

function TPoEntry.IsPluralForm: boolean;
begin
  Result:=pos(PluralSplitter,MsgId)>=1;
end;

function FindBestBreak (s:string;LineWidth:integer):integer;
// Returns number of characters to include in the line
var
  spacepos:integer;
  i,p:integer;
  MaxLength:integer;
begin
  if LineWidth = 0 then begin
    // no line wrapping unless there is a linefeed in the string
    LineWidth := MaxInt;
  end;
  p:=pos(#10,s);
  spacepos:=0;
  MaxLength:=min(length(s),LineWidth);
  if (p>0) and (p<MaxLength) then begin
    Result:=p;
    exit;
  end;
  i:=MaxLength;
  while i>=1 do begin
    case s[i] of
      #10:begin
            Result:=i;
            exit;
          end;
      ' ':
        if spacepos=0 then
          spacepos:=i;
    end;
    dec (i);
  end;
  if spacepos>LineWidth div 2 then begin
    Result:=spacepos;
  end else
    Result:=MaxLength;
  if (Result>=2) and (ord(s[Result])<32) and (ord(s[Result])<>10) then begin
    for i:=Result-1 downto 1 do begin
      if (ord(s[i])>=32) or (ord(s[i])=10) then begin
        Result:=i;
        exit;
      end;
    end;
  end;
end;

procedure TPoEntry.WriteToStream (str:TStream; AWidth: integer = 70);
  procedure WritePart (token:string;msg:string);
  var
    IsFirst: boolean;
    p:integer;
    part:string;
  begin
    IsFirst := true;
    StreamWrite (str, token+' ');
    while true do begin
      p:=FindBestBreak (msg,AWidth);
      part:=copy(msg,1,p);
      delete (msg,1,length(part));
      if IsFirst and (msg <> '') then begin
        // prefix any multiline strings with an empty string as the gnu tools do
        IsFirst := false;
        StreamWriteln(str, String2po(''));
      end;
      StreamWriteln (str, String2PO(part));
      if msg='' then
        break;
    end;
  end;
var
  s:string;
  p:integer;
  idx:integer;
  isplural:boolean;
  MsgStrCopy:string;
begin
  // Write comments
  s := trim(UserCommentList.Text);
  if s <> '' then
    s := s + sLineBreak;
  StreamWrite(str, s);

  s := trim(AutoCommentList.Text);
  if s <> '' then
    s := s + sLineBreak;
  StreamWrite(str, s);

  //*** when fussy or object-pascal-format is set, add a special comment line
  //    with the key-words
  if Fuzzy or
     (IsObjectPascalFormat <> opfUndefined) then
  begin
    s := '#';

    // Fuzzy?
    if Fuzzy then
      s := s + ', fuzzy';

    //*** object-pascal-Format?
    case IsObjectPascalFormat of
      opfTrue : s := s + ', object-pascal-format';
      opfFalse: s := s + ', no-object-pascal-format';
      opfUndefined:; //*** do nothing
    else
      raise Exception.Create(_('unknown State of IsObjectPascalFormat'));
    end;

    StreamWriteln(str, s);
  end;

  // Write msgid and msgstr
  p:=pos(PluralSplitter,MsgId);
  isplural:=p<>0;
  if not isplural then
    WritePart ('msgid',MsgId)
  else begin
    WritePart ('msgid',copy(MsgId,1,p-1));
    WritePart ('msgid_plural',copy(MsgId,p+1,maxint));
  end;
  p:=pos(PluralSplitter,MsgStr);
  if (p=0) and (not isplural) then
    WritePart ('msgstr',MsgStr)
  else begin
    idx:=0;
    MsgStrCopy := MsgStr;
    while true do begin
      if p<>0 then begin
        WritePart ('msgstr['+IntToStr(idx)+']',copy(MsgStr,1,p-1));
        delete (MsgStr,1,p);
      end else begin
        WritePart ('msgstr['+IntToStr(idx)+']',MsgStr);
        Msgstr := MsgStrCopy;
        break;
      end;
      inc (idx);
      p:=pos(PluralSplitter,MsgStr);
    end;
  end;

  // Write empty line
  StreamWrite (str, sLineBreak);
end;

{ TPoEntryList }

procedure TPoEntryList.Add(entry: TPoEntry);
var
  p:Integer;
  l:TList;
  idx:integer;
  po:TPoEntry;
  searchkey:string;
begin
  searchkey:=GetSearchKey(entry.MsgId);
  if list.Find(searchkey,idx) then begin
    l:=list.Objects[idx] as TList;
    for p:=0 to l.count-1 do begin
      po:=TObject(l.Items[p]) as TPoEntry;
      if po.MsgId=entry.MsgId then
        raise Exception.Create (Format(_('This list of translations cannot handle MsgId duplicates. Please remove the duplicate of "%s".'),[po.MsgId]));
    end;
  end else begin
    l:=TList.Create;
    list.AddObject(searchkey,l);
  end;
  po:=TPoEntry.Create;
  po.Assign (entry);
  l.Add(po);
end;

function TPoEntryList.BasePath: String;
begin
  Result := GetHeaderEntry( PO_HEADER_Poedit_BasePath);
end;

procedure TPoEntryList.Clear;
var
  i,j:integer;
  l:TList;
begin
  for i:=0 to list.count-1 do begin
    l:=list.Objects[i] as TList;
    for j:=0 to l.count-1 do 
      TObject(l.Items[j]).Free;
    l.Free;
  end;
  list.Clear;
end;

function TPoEntryList.Count: integer;
begin
  Result:=list.Count;
end;

constructor TPoEntryList.Create;
begin
  list:=TStringList.Create;
  list.Duplicates:=dupError;
  list.CaseSensitive:=True;
  list.Sorted:=True;
end;

function TPoEntryList.Delete(MsgId: string): boolean;
var
  p:Integer;
  l:TList;
  idx:integer;
  po:TPoEntry;
begin
  Result:=False;
  if list.Find(GetSearchKey(MsgId),idx) then begin
    l:=list.Objects[idx] as TList;
    for p:=0 to l.count-1 do begin
      po:=TObject(l.Items[p]) as TPoEntry;
      if po.MsgId=MsgId then begin
        po.Free;
        l.Delete (p);
        Result:=True;
        if l.Count=0 then begin
          l.Free;
          list.Delete (idx);
        end;
        exit;
      end;
    end;
  end;
end;

destructor TPoEntryList.Destroy;
begin
  Clear;
  FreeAndNil (list);
  inherited;
end;

function TPoEntryList.Find(MsgId: string):TPoEntry;
var
  p:Integer;
  l:TList;
  idx:integer;
begin
  if list.Find(GetSearchKey(MsgId),idx) then begin
    l:=list.Objects[idx] as TList;
    for p:=0 to l.count-1 do begin
      Result:=TObject(l.Items[p]) as TPoEntry;
      if Result.MsgId=MsgId then
        exit;
    end;
  end;
  Result:=nil;
end;

function TPoEntryList.FindFirst: TPoEntry;
var
  l:TList;
begin
  if list.Count=0 then begin
    Result:=nil;
    exit;
  end;
  l:=list.Objects[0] as TList;
  if l.Count=0 then
    raise Exception.Create (_('Internal error in TPoEntryList data structure. Sublist for searchkey was empty.'));
  Result:=TObject(l.Items[0]) as TPoEntry;
end;

function TPoEntryList.FindNext(po: TPoEntry): TPoEntry;
var
  p:Integer;
  l:TList;
  idx:integer;
begin
  Result:=Nil;
  if list.Find(GetSearchKey(po.MsgId),idx) then begin
    l:=list.Objects[idx] as TList;
    p:=l.IndexOf(po);
    if p=-1 then
      raise Exception.Create (_('Error: Specified TPoEntry was not found in list.'));
    if p=l.Count-1 then begin
      if idx=list.Count-1 then begin
        Result:=nil;
      end else begin
        l:=list.Objects[idx+1] as TList;
        if l.Count=0 then
          raise Exception.Create (_('Internal error in TPoEntryList data structure. Sublist for searchkey was empty.'));
        Result:=TObject(l.Items[0]) as TPoEntry;
      end;
    end else
      Result:=TObject(l.Items[p+1]) as TPoEntry;
  end;
end;

function TPoEntryList.GetSearchKey(MsgId: string): string;
var
  p:integer;
begin
  p:=pos(#10,MsgId);
  if p<>0 then begin
    Result:=copy(MsgId,1,p-1);
  end else begin
    Result:=MsgId;
  end;
end;

function TPoEntryList.GetHeaderEntry(const _Label: string): string;
var
  Entry: TPoEntry;
begin
  Result := '';
  Entry :=  Find('');
  if not Assigned(Entry) then
    exit;

  Result := GetPoHeaderEntry(Entry.MsgStr, _Label);
end;

function TPoEntryList.ProjectAndVersion: string;
begin
  Result := GetHeaderEntry(PO_HEADER_PROJECT_ID_VERSION);
end;

function TPoEntryList.Language: string;
begin
  Result := GetHeaderEntry(PO_HEADER_LANGUAGE);
end;

procedure TPoEntryList.LoadFromFile(filename: string);
var
  tf:TextFile;
  pop:TPoParser;
  pe:TPoEntry;
  Idx: integer;
begin
  FileMode:=fmOpenRead;
  AssignFile (tf,filename);
  Reset (tf);
  try
    Idx := 0;
    pop:=TPoParser.Create;
    try
      while true do begin
        pe:=pop.ReadNextEntry(tf);
        if pe=nil then
          break;
        pe.FIndex := Idx;
        Inc(Idx);
        Add (pe);
      end;
    finally
      FreeAndNil (pop);
    end;
  finally
    CloseFile (tf);
  end;
end;

type
  TPoEntrySorter = class
  private
    FMaxIdx: integer;
    FLst: TList;
    function doCompare(AIdx1, AIdx2: integer): integer;
    procedure doSwap(AIdx1, AIdx2: integer);
    function GetItems(AIdx: integer): TPoEntry;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Ape: TPoEntry);
    procedure Sort;
    function Count: integer;
    property Items[AIdx: integer]: TPoEntry read GetItems; default;
  end;

function TPoEntrySorter.Count: integer;
begin
  Result := FLst.Count;
end;

constructor TPoEntrySorter.Create;
begin
  inherited Create;
  FLst := TList.Create;
end;

destructor TPoEntrySorter.Destroy;
begin
  FreeAndNil(FLst);
  inherited;
end;

procedure TPoEntrySorter.Add(Ape: TPoEntry);
begin
  if Ape.FIndex > FMaxIdx then
    FMaxIdx := Ape.FIndex;
  Flst.Add(Ape);
end;

procedure TPoEntrySorter.Sort;
var
  i: integer;
  pe: TPoEntry;
begin
  for i := 0 to Flst.Count - 1 do begin
    pe := Flst[i];
    if pe.FIndex = -1 then begin
      Inc(FMaxIdx);
      pe.FIndex := FMaxIdx;
    end;
  end;
  u_dzQuicksort.QuickSort(0, Flst.Count - 1, doCompare, doSwap);
end;

function TPoEntrySorter.doCompare(AIdx1, AIdx2: integer): integer;
var
  pe1: TPoEntry;
  pe2: TPoEntry;
begin
  pe1 := FLst[AIdx1];
  pe2 := FLst[AIdx2];
  Result := CompareValue(pe1.FIndex, pe2.FIndex);
end;

procedure TPoEntrySorter.doSwap(AIdx1, AIdx2: integer);
begin
  FLst.Exchange(AIdx1, AIdx2);
end;

function TPoEntrySorter.GetItems(AIdx: integer): TPoEntry;
begin
  Result := FLst[AIdx];
end;

procedure TPoEntryList.SaveToFile (filename:string; AWidth: integer = 70);
var
  outfile:TFileStream;
  pe:TPoEntry;
  i: Integer;
  Sorter: TPoEntrySorter;
begin
  outfile := nil;
  Sorter := TPoEntrySorter.Create;
  try
    outfile:=TFileStream.Create (filename, fmCreate);
    // Write header
    pe:=Find('');
    if pe<>nil then
      Sorter.Add(pe);

    // Write the rest
    pe:=FindFirst;
    while pe<>nil do begin
      if pe.MsgId<>'' then
        Sorter.Add(pe);
      pe:=FindNext (pe);
    end;

    Sorter.Sort;

    for i := 0 to Sorter.Count - 1 do begin
      pe := Sorter[i];
      if pe.MsgId='' then
        // always wrap the header
        pe.WriteToStream(outfile, 70)
      else
        pe.WriteToStream(outfile, AWidth);
    end;
  finally
    FreeAndNil (outfile);
    FreeAndNil(Sorter);
  end;
end;

end.

