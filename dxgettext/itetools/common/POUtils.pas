{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Peter Thornqvist [peter3@peter3.com]
Portions created by Peter Thornqvist are Copyright (C) 2003 Peter Thornqvist. All Rights Reserved.

You may retrieve the latest version of this file at dxgettext's home page,
located at http://dybdahl.dk/dxgettext/

(************************************************************************
  Unicode Bugfixes in this unit by Arthur Hoornweg
*************************************************************************)
-----------------------------------------------------------------------------}
{$I JEDI.INC}
{$IFDEF COMPILER7_UP}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

unit POUtils;

interface
uses
  Windows, SysUtils, Classes;

type
  // one item in a PO file
  EPOFileError = class(Exception);
  EMOFileError = class(Exception);
  EDFNFileError = class(Exception);
  TPOItem = class(TPersistent)
  private
    FComments: TStrings;
    FMsgID: string;
    FMsgStr: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Comments: TStrings read FComments write FComments;
    property MsgID: string read FMsgID write FMsgID;
    property MsgStr: string read FMsgStr write FMsgStr;
  end;

  // a collection of TPOItems
  TPOFile = class
  private
    FItems: TList;
    FHeader: TPOItem;
    FSorted: boolean;
    function GetCount: integer;
    function GetEntry(Index: integer): TPOItem;
  public
    constructor Create;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    destructor Destroy; override;
    function IndexOf(const AMsgID: string): integer;
    function Add: TPOItem;
    procedure Delete(Index: integer);
    procedure Clear;
    // removes duplicates. NOTE: always keeps the *last* duplicate found!
    procedure Pack;
    procedure Sort;
    // adds the items in POFile that are not already available
    procedure Merge(POFile: TPoFile);
    property Header: TPOItem read FHeader;
    property Items[Index: integer]: TPOItem read GetEntry; default;
    property Count: integer read GetCount;
    property Sorted: boolean read FSorted;
  end;

  TMOItem = class
  private
    FMsgID: string;
    FMsgStr: string;
  public
    constructor Create(const AMsgID, AMsgStr: string);
    property MsgID: string read FMsgID write FMsgID;
    property MsgStr: string read FMsgStr write FMsgStr;
  end;

  TMOFile = class
  private
    FItems: TList;
    FSwapRead: boolean;
    FStringCount, FOriginalOffset, FTranslateOffset,
      FHashSize, FHashOffset: LongWord; // 32 bit = 4 bytes

    function GetLookUp(const MsgID: string): string;
    procedure ReadSignature(Stream: TStream);
    procedure ReadStringInfo(Stream: TStream);
    procedure ReadHashTableInfo(Stream: TStream);
    procedure ReadItems(Stream: TStream);
    function GetCount: LongWord;
    procedure WriteHashTableInfo(Stream: TStream);
    procedure WriteItems(Stream: TStream);
    procedure WriteSignature(Stream: TStream);
    procedure WriteStringInfo(Stream: TStream);
    function GetItem(Index: LongWord): TMoItem;
    procedure Swap(var Value: LongWord);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Sort;
    procedure CompileFromPO(const Filename: string); overload;
    procedure CompileFromPO(POFile: TPOFile); overload;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    property LookUp[const MsgID: string]: string read GetLookUp;
    property Items[Index: LongWord]: TMoItem read GetItem; default;
    property Count: LongWord read GetCount;
  end;

function trimAllBelow(const S: string; Ch: Char = #33): string;
function trimTrailingCRLF(const S: string): string;
// replaces #13#10 with \n , #9 with \t etc
function EscapeString(const S: string): string;
// replaces \n with #13#10, \t with #9 etc
function UnescapeString(const S: string): string;

implementation

const
  cMOSignature = $950412DE;

function StripQuotes(const S: string; Ch: Char): string;
begin
  if (Length(S) > 0) and (S[1] = Ch) and (S[Length(S)] = Ch) then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

function trimTrailingCRLF(const S: string): string;
begin
  Result := S;
  Exit;
  if Copy(Result, Length(Result) - 1, 2) = #13#10 then
    SetLength(Result, Length(Result) - 2)
end;

function trimAllBelow(const S: string; Ch: Char = #33): string;
var
  right, left: Integer;
begin
  right := Length(S);
  left := 1;
  while (left <= right) and (S[left] < Ch) do
    Inc(left);
  if left > right then
    Result := ''
  else
  begin
    while S[right] < Ch do
      Dec(right);
    Result := Copy(S, left, right - left + 1);
  end;
end;

function EscapeString(const S: string): string;
var
  i, j: integer;
begin
  SetLength(Result, Length(S) * 2); // worst case - all needs escaping
  j := 0;
  i := 1;
  while i <= Length(S) do
  begin
    case S[i] of
      #9:
        begin
          Inc(j);
          Result[j] := '\';
          Inc(j);
          Result[j] := 't';
        end;
      #10:
        begin
          Inc(j);
          Result[j] := '\';
          Inc(j);
          Result[j] := 'n';
        end;
      #13: ; // do nothing
      '"':
        begin
          Inc(j);
          Result[j] := '\';
          Inc(j);
          Result[j] := '"';
        end;
    else
      begin
        Inc(j);
        Result[j] := S[i];
      end;
    end; // case
    Inc(i);
  end;
  SetLength(Result, j);
end;

function UnescapeString(const S: string): string;
var
  i, j: integer;
begin
  SetLength(Result, Length(S));
  j := 0;
  i := 1;
  while i <= Length(S) do
  begin
    case S[i] of
      '\':
        if ((i < Length(S)) and (S[i + 1] <> '\')) then
          case S[i + 1] of
            'n':
              begin
                Inc(j);
                Result[j] := #13;
                Inc(j);
                Result[j] := #10;
                Inc(i);
              end;
            'r': ; // do nothing
            't':
              begin
                Inc(j);
                Result[j] := #9;
                Inc(i);
              end;
            '"':
              begin
                Inc(j);
                Result[j] := '"';
                Inc(i);
              end;
          else
            begin
              Inc(j);
              Result[j] := S[i];
            end;
          end; // case
      #1..#31: ; // do nothing
    else
      begin
        Inc(j);
        Result[j] := S[i];
      end;
    end; // case
    Inc(i);
  end;
  SetLength(Result, j);
end;

function Swap32(AValue: LongWord): LongWord; assembler;
asm
  bswap eax;
end;

function BinaryStrCompare(const S1,S2:string):integer;
begin
  if S1 = S2 then
    Result := 0
  else if S1 < S2 then
    Result := -1
  else
    Result := +1;
end;

function POSortItems(Item1, Item2: Pointer): integer;
begin
  Result := BinaryStrCompare(TPoItem(Item1).MsgId, TPoItem(Item2).MsgId);
end;

function POBinarySearch(AList: TList; const AMsgID: string; var AIndex: integer): boolean;
var
  L, R, M: integer;
  CompareResult: integer;
begin
  L := 0;
  R := AList.Count - 1;
  while L <= R do
  begin
    M := (L + R) div 2;
    // NB: binary comparison!!!
    CompareResult := BinaryStrCompare(TPOItem(AList[M]).MsgID, AMsgID);
    if (CompareResult < 0) then
      L := M + 1
    else if (CompareResult > 0) then
      R := M - 1
    else
    begin
      AIndex := M;
      Result := true;
      Exit;
    end;
  end;
  // not found, should be located here:
  Result := false;
  AIndex := L;
end;

function MOSortItems(Item1, Item2: Pointer): integer;
begin
  Result := BinaryStrCompare(TMOItem(Item1).MsgID,TMoItem(Item2).MsgID);
end;

function MOBinarySearch(AList: TList; const AMsgID: string; var AIndex: integer): boolean;
var
  L, R, M: integer;
  CompareResult: integer;
begin
  L := 0;
  R := AList.Count - 1;
  while L <= R do
  begin
    M := (L + R) div 2;
    // NB: binary comparison!!!
    CompareResult := BinaryStrCompare(TMoItem(AList[M]).MsgID, AMSgID);
    if (CompareResult < 0) then
      L := M + 1
    else if (CompareResult > 0) then
      R := M - 1
    else
    begin
      AIndex := M;
      Result := true;
      Exit;
    end;
  end;
  // not found, should be located here:
  Result := false;
  AIndex := L;
end;

{ TPOItem }

procedure TPOItem.Assign(Source: TPersistent);
begin
  if (Source is TPOItem) then
  begin
    Comments.Assign(TPOItem(Source).Comments);
    MsgID := TPOItem(Source).FMsgID;
    MsgStr := TPOItem(Source).FMsgStr;
    Exit;
  end;
  inherited;
end;

constructor TPOItem.Create;
begin
  inherited Create;
  //  FMsgId := TStringlist.Create;
  //  FMsgStr := TStringlist.Create;
  FComments := TStringlist.Create;
end;

destructor TPOItem.Destroy;
begin
  //  FMsgId.Free;
  //  FMsgStr.Free;
  FComments.Free;
  inherited;
end;

{
function TPOItem.MsgIDText: string;
var i:integer;
begin
  Result := '';
  for i := 0 to MsgID.Count - 1 do
    Result := Result + ExpandCRLF(MsgID[i]);
end;

function TPOItem.MsgStrText: string;
var i:integer;
begin
  Result := '';
  for i := 0 to MsgStr.Count - 1 do
    Result := Result + ExpandCRLF(MsgStr[i]);
end;
}
{ TPOFile }

function TPOFile.Add: TPOItem;
begin
  Result := TPOItem.Create;
  FItems.Add(Result);
  FSorted := false;
end;

procedure TPOFile.Clear;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    TObject(FItems[i]).Free;
  FItems.Count := 0;
  FItems.Capacity := 128;
  FreeAndNil(FHeader);
end;

constructor TPOFile.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FItems.Capacity := 128;
end;

procedure TPOFile.Delete(Index: integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

destructor TPOFile.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TPOFile.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TPOFile.GetEntry(Index: integer): TPOItem;
begin
  Result := TPOItem(FItems[Index]);
end;

function TPOFile.IndexOf(const AMsgID: string): integer;
var
  tmp: string;
begin
  tmp := trimTrailingCRLF(AMsgID);
  if Sorted then
  begin
    if POBinarySearch(FItems, tmp, Result) then
      Exit;
  end
  else
    for Result := 0 to Count - 1 do
      if AnsiSameStr(trimTrailingCRLF(Items[Result].MsgId), tmp) then
        Exit;
  Result := -1;
end;

procedure TPOFile.LoadFromFile(const Filename: string);
type
  TEntryState = (esNone, esComment, esId, esStr);
var
  i: integer;
  S: TStringlist;
  AEntry: TPOItem;
  AState: TEntryState;

begin
  Clear;
  if not FileExists(Filename) then
    Exit;
  S := TStringlist.Create;
  try
    AEntry := nil;
    AState := esNone;
    S.LoadFromFile(Filename {$IFDEF unicode}, TEncoding.UTF8{$ENDIF});
    for i := 0 to S.Count - 1 do
    begin
      if AEntry = nil then
        AEntry := Add;
      if AnsiSameText(Copy(S[i], 1, 1), '#') then
      begin

        AEntry.Comments.Add(Copy(S[i], 2, MaxInt));
        AState := esComment;
      end
      else if AnsiSameText(Copy(S[i], 1, 6), 'msgid ') then
      begin
        AEntry.MsgId := {AEntry.MsgId + #13#10 + } StripQuotes(Copy(S[i], 7, MaxInt), '"');
        AState := esID;
      end
      else if AnsiSameText(Copy(S[i], 1, 7), 'msgstr ') then
      begin
        AEntry.MsgStr := {AEntry.MsgStr + #13#10 + } StripQuotes(Copy(S[i], 8, MaxInt), '"');
        AState := esStr;
      end
      else if S[i] <> '' then
        case AState of
          esID:
            AEntry.MsgId := AEntry.MsgId + StripQuotes(S[i], '"');
          esStr:
            AEntry.MsgStr := AEntry.MsgStr + StripQuotes(S[i], '"');
          else
          begin
            AEntry := nil;
            aState := esNone;
          end
        end
      else
      begin
        if (AEntry <> nil) and (AEntry.MsgID = '') then
        begin
          if FHeader = nil then
            FHeader := AEntry;
          // remove the entry even if header is assigned: there can be only one (empty)!
          FItems.Remove(AEntry);
        end;
        AEntry := nil;
        aState := esNone;
      end;
    end;
  finally
    S.Free;
  end;
  FSorted := false;
end;

procedure TPOFile.Merge(POFile: TPoFile);
var
  i: integer;
begin
  if POFile = nil then
    raise EPOFileError.Create('POFile cannot be nil');
  for i := 0 to POFile.Count - 1 do
    if IndexOf(POFile[i].MsgId) < 0 then
      Add.Assign(POFile[i]);
end;

procedure TPOFile.Pack;
var
  i, j: integer;
  tmp: boolean;
begin
  tmp := FSorted;
  try
    // this will not work if sorted, so fake unsorted
    FSorted := false;
    j := Count - 1;
    while j >= 0 do
    begin
      i := IndexOf(Items[j].MsgID);
      if (i >= 0) and (i <> j) then
      begin
        // retain comments
        Items[j].Comments.AddStrings(Items[i].Comments);
        // keep translation if we don't have one already
        if (trimTrailingCRLF(Items[j].MsgStr) = '') then
          Items[j].MsgStr := Items[i].MsgStr;
        // remove duplicate
        Delete(i);
      end;
      Dec(j);
    end;
  finally
    FSorted := tmp;
  end;
end;

procedure TPOFile.SaveToFile(const Filename: string);
var
  i, j: integer;
  S, T: TStringlist;
begin
  S := TStringlist.Create;
  T := TStringlist.Create;
  try
    if FHeader <> nil then
    begin
      for j := 0 to FHeader.Comments.Count - 1 do
        S.Add(Format('#%s', [FHeader.Comments[j]]));
      S.Add('msgid ""');
      T.Text := StringReplace(FHeader.MsgStr, '\n', '\n'#13#10, [rfReplaceAll]);
      if T.Count = 0 then
        S.Add('msgstr ""')
      else if T.Count > 1 then
      begin
        S.Add('msgstr ""');
        for j := 0 to T.Count - 1 do
          S.Add(Format('"%s"', [T[j]]));
      end
      else
        S.Add(Format('msgstr "%s"', [T[0]]));
      S.Add('');
    end;
    for i := 0 to Count - 1 do
    begin
      if Items[i].MsgId = '' then Continue;

      for j := 0 to Items[i].Comments.Count - 1 do
        S.Add(Format('#%s', [Items[i].Comments[j]]));
      T.Text := StringReplace(Items[i].MsgId, '\n', '\n'#13#10, [rfReplaceAll]);
      if T.Count = 0 then
        S.Add('msgid ""')
      else if T.Count > 1 then
      begin
        S.Add('msgid ""');
        for j := 0 to T.Count - 1 do
          S.Add(Format('"%s"', [T[j]]));
      end
      else
        S.Add(Format('msgid "%s"', [T[0]]));
      T.Text := StringReplace(Items[i].MsgStr, '\n', '\n'#13#10, [rfReplaceAll]);
      if T.Count = 0 then
        S.Add('msgstr ""')
      else if T.Count > 1 then
      begin
        S.Add('msgstr ""');
        for j := 0 to T.Count - 1 do
          S.Add(Format('"%s"', [T[j]]));
      end
      else
        S.Add(Format('msgstr "%s"', [T[0]]));
      // empty line between entries
      S.Add('');
    end;
{$IFDEF unicode}
    S.WriteBOM := false;
    S.SaveToFile(Filename, TEncoding.UTF8);
{$ELSE}
    S.SaveToFile(Filename);
{$ENDIF}
  finally
    S.Free;
    T.Free;
  end;
end;

procedure TPOFile.Sort;
begin
  if not FSorted and (FItems.Count > 1) then
    FItems.Sort(POSortItems);
  FSorted := true;
end;

{ TMOItem }

constructor TMOItem.Create(const AMsgID, AMsgStr: string);
begin
  inherited Create;
  FMsgID := AMsgID;
  FMsgStr := AMsgStr;
end;

{ TMOFile }

procedure TMOFile.CompileFromPO(POFile: TPOFile);
var
  i: integer;
begin
  Clear;
  // remove duplicates
  POFile.Pack;
  for i := 0 to POFile.Count - 1 do
    FItems.Add(TMoItem.Create(POFile[i].MsgID, POFile[i].MsgStr));
  Sort;
end;

procedure TMOFile.CompileFromPO(const Filename: string);
var
  PO: TPOFile;
begin
  PO := TPOFile.Create;
  try
    PO.LoadFromFile(Filename);
    CompileFromPO(PO);
  finally
    PO.Free;
  end;
end;

constructor TMOFile.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TMOFile.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TMOFile.GetLookUp(const MsgID: string): string;
var
  i: integer;
begin
  if MOBinarySearch(FItems, MsgID, i) then
    Result := TMoItem(FItems[i]).MsgStr;
end;

procedure TMOFile.LoadFromFile(const Filename: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TMOFile.ReadSignature(Stream: TStream);
var
  aBuffer: LongWord;
begin
  Stream.Read(ABuffer, sizeof(ABuffer));
  if (ABuffer <> $950412DE) and (ABuffer <> $DE120495) then
    raise EMOFileError.CreateFmt('Unknown signature: $%x', [ABuffer]);
  // make sure we read in correct byte order
  FSwapRead := (ABuffer = $DE120495);
  Stream.Read(ABuffer, sizeof(ABuffer));
  // only revision 0 supported
  if ABuffer <> 0 then
    raise EMOFileError.CreateFmt('Unsupported revision: %d', [ABuffer]);
end;

procedure TMOFile.ReadStringInfo(Stream: TStream);
begin
  Stream.Read(FStringCount, sizeof(FStringCount));
  Swap(FStringCount);
  Stream.Read(FOriginalOffset, sizeof(FOriginalOffset));
  Swap(FOriginalOffset);
  Stream.Read(FTranslateOffset, sizeof(FTranslateOffset));
  Swap(FTranslateOffset);
end;

procedure TMOFile.ReadHashTableInfo(Stream: TStream);
begin
  Stream.Read(FHashSize, sizeof(FHashSize));
  Swap(FHashSize);
  Stream.Read(FHashOffset, sizeof(FHashOffset));
  Swap(FHashOffset);
end;

procedure TMOFile.LoadFromStream(Stream: TStream);
begin
  Clear;
  ReadSignature(Stream);
  ReadStringInfo(Stream);
  ReadHashTableInfo(Stream);
  ReadItems(Stream);
end;

procedure TMOFile.SaveToFile(const Filename: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TMOFile.WriteSignature(Stream: TStream);
var
  ABuffer: LongWord;
begin
  ABuffer := cMOSignature;
  // signature
  Stream.Write(ABuffer, sizeof(ABuffer));
  // revision
  ABuffer := 0;
  Stream.Write(ABuffer, sizeof(ABuffer));
end;

procedure TMOFile.WriteStringInfo(Stream: TStream);
begin
  FStringCount := Count;
  Stream.Write(FStringCount, sizeof(FStringCount));
  FOriginalOffset := 28;
  Stream.Write(FOriginalOffset, sizeof(FOriginalOffset));
  FTranslateOffset := FOriginalOffset + FStringCount * sizeof(LongWord) * 2;
  Stream.Write(FTranslateOffset, sizeof(FTranslateOffset));
end;

procedure TMOFile.WriteHashTableInfo(Stream: TStream);
var
  ABuffer: LongWord;
begin
  ABuffer := 0;
  // size of hash (always 0 = no hash table)
  FHashSize := Stream.Position;
  Stream.Write(ABuffer, sizeof(ABuffer));
  // offset of hash (always 0)
  FHashOffset := Stream.Position;
  Stream.Write(ABuffer, sizeof(ABuffer));
end;

procedure TMOFile.WriteItems(Stream: TStream);
var
  ASize, AOffset: LongWord;
  i: integer;
{$IFDEF unicode}
  NullChar: AnsiChar;
  utf: Utf8String;
{$ELSE}
  NullChar: char;
  utf: string;
type
  pAnsiChar = Pchar;
{$ENDIF}
begin
  {
    Format of MO files:
    Type                                        Offset
    ===========================================================
    Magic                                       0
    Revision                                    4
    StringCount                                 8
    OriginalInfoTableOffset                    12
    TranslateInfoTableOffset                   16
    HashSize                                   20
    HashOffset                                 24
    ...                                        ...
    OriginalLength0                            OriginalInfoTableOffset
    OriginalOffset0                            OriginalInfoTableOffset + 4
    ...                                        ...
    OriginalLength_n                           OriginalInfoTableOffset + n * 8
    OriginalOffset_n                           OriginalInfoTableOffset + n * 8 + 4
    TranslateLength0                           TranslateInfoTableOffset
    TranslateOffset0                           TranslateInfoTableOffset + 4
    ...                                        ...
    TranslateLength_n                          TranslateInfoTableOffset + n * 8
    TranslateOffset_n                          TranslateInfoTableOffset + n * 8 + 4
    StartHash                                  HashOffset
    ...                                        ...
    EndHash                                    HashOffset + HashSize * 4
    OriginalString0                            OriginalOffset0
    OriginalString1                            OriginalOffset1
    ...                                        ...
    OriginalString_n                           OriginalOffset_n
    TranslateString0                           TranslateOffset0
    TranslateString1                           TranslateOffset1
    ...                                        ...
    TranslateString_n                          TranslateOffset_n
  }

  NullChar := #0;
  // first write all length and offset values. Since we don't know the offset values
  // yet, write a dummy value and return later to fill it in
  AOffset := Stream.Position;
  for i := 0 to FItems.Count - 1 do
  begin
    // just write garbage: fix later
    Stream.Write(AOffset, sizeof(AOffset));
    Stream.Write(AOffset, sizeof(AOffset));
    Stream.Write(AOffset, sizeof(AOffset));
    Stream.Write(AOffset, sizeof(AOffset));
  end;

  // TODO: write hash table entry if hash table is 0?
  //  ASize := 0;
  //  StartHash
  //  Stream.Write(ASize,sizeof(ASize));
  //  EndHash
  //  Stream.Write(ASize,sizeof(ASize));

    // write original strings
  for i := 0 to FItems.Count - 1 do
  begin
    if i = 0 then
      FOriginalOffset := Stream.Position;
    utf := TMOItem(FItems[i]).MsgID;
    ASize := Length(utf);
    if ASize > 0 then
      Stream.Write(PAnsiChar(utf)^, ASize);
    // always write NUL
    Stream.Write(NullChar, sizeof(NullChar));
  end;

  // write translated strings
  for i := 0 to FItems.Count - 1 do
  begin
    if i = 0 then
      FTranslateOffset := Stream.Position;
    utf := TMOItem(FItems[i]).MsgStr;
    ASize := Length(utf);
    if ASize > 0 then
      Stream.Write(PAnsiChar(utf)^, ASize);
    // always write NUL
    Stream.Write(NullChar, sizeof(NullChar));
  end;

  // go back and fill in the real length and offset values
  Stream.Seek(AOffset, soFromBeginning);

  // original
  for i := 0 to FItems.Count - 1 do
  begin
    utf := TMOItem(FItems[i]).MsgID;
    ASize := Length(utf);
    Stream.Write(ASize, sizeof(ASize));
    Stream.Write(FOriginalOffset, sizeof(FOriginalOffset));
    Inc(FOriginalOffset, ASize + 1); // include terminating NUL
  end;

  // translated
  for i := 0 to FItems.Count - 1 do
  begin
    utf := TMOItem(FItems[i]).MsgStr;
    ASize := Length(utf);
    Stream.Write(ASize, sizeof(ASize));
    Stream.Write(FTranslateOffset, sizeof(FTranslateOffset));
    Inc(FTranslateOffset, ASize + 1); // include terminating NUL
  end;
end;

procedure TMOFile.SaveToStream(Stream: TStream);
begin
  if Count > 0 then
  begin
    WriteSignature(Stream);
    WriteStringInfo(Stream);
    WriteHashTableInfo(Stream);
    WriteItems(Stream);
  end;
end;

function TMOFile.GetCount: LongWord;
begin
  Result := FItems.Count;
end;

procedure TMOFile.Clear;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    TMoItem(FItems[i]).Free;
  FItems.Clear;
end;

procedure TMOFile.ReadItems(Stream: TStream);
var
  ASize, ACount, AOffset: LongWord;
  AString: {$IFDEF unicode} Utf8String {$ELSE} string {$ENDIF};
begin
  ACount := 0;
  // read original
  while ACount < FStringCount do
  begin
    // go to the start of the offset
    Stream.Seek(FOriginalOffset, soFromBeginning);
    // read string length
    Stream.Read(ASize, sizeof(ASize));
    Swap(ASize);
    SetLength(AString, ASize);

    // read offset of original string
    Stream.Read(AOffset, sizeof(AOffset));
    Swap(AOffset);
    // ..go there
    Stream.Seek(AOffset, soFromBeginning);
    // ...read the string
    if ASize > 0 then
      Stream.Read(AString[1], ASize);
    FItems.Add(TMOItem.Create(AString, ''));
    Inc(ACount);
    // jump forward
    Inc(FOriginalOffset, sizeof(FOriginalOffset) * 2);
  end;

  // read translation
  ACount := 0;
  while ACount < FStringCount do
  begin
    // go to item offset
    Stream.Seek(FTranslateOffset, soFromBeginning);
    // read string length
    Stream.Read(ASize, sizeof(ASize));
    Swap(ASize);
    SetLength(AString, ASize);

    // read offset to actual string
    Stream.Read(AOffset, sizeof(AOffset));
    Swap(AOffset);
    // ..go there
    Stream.Seek(AOffset, soFromBeginning);
    if ASize > 0 then
      Stream.Read(AString[1], ASize);
    TMoItem(FItems[ACount]).MsgStr := AString;
    Inc(ACount);
    // jump forward
    Inc(FTranslateOffset, sizeof(LongWord) * 2);
  end;
  // no need to sort FItems since the MO is already sorted (or at least should be!)
  // FItems.Sort(SortMOItems);
end;

procedure TMOFile.Sort;
begin
  if FItems.Count > 1 then
    FItems.Sort(MoSortItems);
end;

function TMOFile.GetItem(Index: LongWord): TMoItem;
begin
  if (Index >= Count) then
    raise EMOFileError.CreateFmt('Index out of bounds (%d)', [Index]);
  Result := TMOItem(FItems[Index]);
end;

procedure TMOFile.Swap(var Value: LongWord);
begin
  if FSwapRead then
    Value := Swap32(Value);
end;

end.

