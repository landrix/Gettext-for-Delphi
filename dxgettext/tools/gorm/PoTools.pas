unit PoTools;
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
  Classes, CheckLst, PoParser;

type
  TSourceCodeLocation = record
    Path: String;
    LineNumber: Integer;
  end;

procedure ExtractSourcecodeLocations (Item:TPoEntry;Locations:TStrings);
function ExtractLocationPathLineNumber( const xLocationString: String): TSourceCodeLocation;
procedure ExtractProgrammerComments (Item:TPoEntry;Comments:TStrings);
procedure ExtractUserComments (Item:TPoEntry;Comments:TStrings);
procedure ExtractLabels (Item:TPoEntry;CheckListBoxLabels:TCheckListBox);
function FormatMsgIdForDisplay (const s:string):string;
function ConvertMsgStrToStorageFormat (const s:string):string;
function CountPlaceholders(const _Which, _In: string): integer;
function FindWarnings (item:TPoEntry):string;
function patternmatch (pattern,itemtext:string):boolean;
function patternmatchspacedlist (pattern,spacedlist:string):boolean;
procedure AddLabel (item:TPoEntry;const LabelName:string);
procedure RemoveLabel(item:TPoEntry;const LabelName:string);
function HasLabel(_Item: TPoEntry; const _LabelName: string): boolean;

implementation

uses
  StrUtils, SysUtils, gnugettext, Character;

procedure AddLabel (item:TPoEntry;const LabelName:string);
var
  i:integer;
  s:string;
begin
  s:='# Label: '+LabelName;
  i:=item.UserCommentList.IndexOf(s);
  if i=-1 then
    item.UserCommentList.Add(s);
end;

procedure RemoveLabel(item:TPoEntry;const LabelName:string);
var
  i:integer;
  s:string;
begin
  s:='# Label: '+LabelName;
  i:=item.UserCommentList.IndexOf(s);
  if i <> -1 then
    item.UserCommentList.Delete(i);
end;

function HasLabel(_Item: TPoEntry; const _LabelName: string): boolean;
begin
  Result := (_Item.UserCommentList.IndexOf('# Label: ' + _LabelName) <> -1);
end;

function patternmatch (pattern,itemtext:string):boolean;
var
  left,right:boolean;
begin
  // Returns true, if the itemtext matches the pattern, which allows some wildcars
  pattern  := LowerCase(pattern);
  itemtext := LowerCase(itemtext);

  left:=LeftStr(pattern,1)='*';
  right:=RightStr(pattern,1)='*';
  if left and right then begin
    Result:=pos(copy(pattern,2,length(pattern)-2),itemtext)>0;
  end else
  if left and not right then begin
    Result:=RightStr(itemtext,length(pattern)-1)=RightStr(pattern,length(pattern)-1);
  end else
  if not left and right then begin
    Result:=LeftStr(itemtext,length(pattern)-1)=LeftStr(pattern,length(pattern)-1);
  end else
    Result:=itemtext=pattern;
end;

function patternmatchspacedlist (pattern,spacedlist:string):boolean;
var
  startpos,p:integer;
begin
  Result:=False;
  spacedlist := spacedlist + ' ';
  startpos:=1;
  for p:=1 to length(spacedlist) do
  begin
    if TCharacter.IsWhiteSpace(spacedlist[p]) then begin
      if patternmatch(pattern,copy(spacedlist,startpos,p-startpos)) then begin
        Result:=True;
        break;
      end;
      startpos:=p+1;
    end;
  end;
end;

function ConvertMsgStrToStorageFormat (const s:string):string;
var
  i:integer;
begin
  i:=1;
  Result:=s;
  while i<=length(Result) do begin
    if Result[i]=#13 then
      delete (Result,i,1)
    else
      inc (i);
  end;
end;


function FormatMsgIdForDisplay (const s:string):string;
var
  i:integer;
begin
  // Formats a string so that spaces become visible
  Result:=s;
  i:=1;
  while i<=length(result) do begin
    if Result[i]=' ' then
      Result[i]:='·';
    if Result[i]=#9 then
      {$ifndef UNICODE}  // >=D2009
      Result[i]:='→';
      {$else}
      Result[i]:='»';
      {$endif}
    if Result[i]=#10 then begin
      insert ('↓',Result,i);
      inc (i);
    end;
    inc (i);
  end;
end;

procedure ExtractSourcecodeLocations (Item:TPoEntry;Locations:TStrings);
var
  s: string;
  i: Integer;
begin
  Locations.Clear;
  for i := 0 to Item.AutoCommentList.Count - 1 do
  begin
    s := Item.AutoCommentList.Strings[i];
    if LeftStr(s, 2) = '#:' then
      Locations.Add(Trim(Copy(s, 3)));
  end;
end;

function ExtractLocationPathLineNumber( const xLocationString: String): TSourceCodeLocation;
var
  lPos: Integer;
begin
  Result := Default( TSourceCodeLocation);

  if (xLocationString <> '') then
  begin
    lPos := pos( ':', xLocationString);

    if (lPos > 0) then
    begin
      Result.Path       := copy( xLocationString, 0, lPos - 1);
      Result.LineNumber := StrToIntDef( copy( xLocationString,
                                              lPos + 1,
                                              Length( xLocationString) - lPos),
                                        0);
    end
    else
    begin
      Result.Path       := xLocationString;
    end;
  end;
end;

procedure ExtractProgrammerComments (Item:TPoEntry;Comments:TStrings);
var
  s: string;
  i: Integer;
begin
  Comments.Clear;
  for i := 0 to Item.AutoCommentList.Count - 1 do
  begin
    s := Item.AutoCommentList.Strings[i];
    if LeftStr(s, 2) = '#.' then
      Comments.Add(Trim(Copy(s, 3)))
    else if LeftStr(s, 2) <> '#:' then
      Comments.Add(s);
  end;
end;

procedure ExtractUserComments (Item:TPoEntry;Comments:TStrings);
var
  i:integer;
  s:string;
begin
  Comments.Clear;
  for i:=0 to Item.UserCommentList.Count-1 do begin
    s:=Item.UserCommentList.Strings[i];
    if LeftStr(s,9)<>'# Label: ' then begin
      if LeftStr(s,2)='# ' then
        Delete (s,1,2);
      Comments.Add (s);
    end;
  end;
end;

procedure ExtractLabels (Item:TPoEntry;CheckListBoxLabels:TCheckListBox);
var
  i,idx:integer;
  s:string;
begin
  for i:=0 to CheckListBoxLabels.Count-1 do
    CheckListBoxLabels.Checked[i]:=False;
  for i:=0 to Item.UserCommentList.Count-1 do begin
    s:=Item.UserCommentList.Strings[i];
    if LeftStr(s,9)='# Label: ' then begin
      delete (s,1,9);
      idx:=CheckListBoxLabels.Items.IndexOf(s);
      if idx=-1 then
        idx:=CheckListBoxLabels.Items.Add(s);
      Assert (idx<>-1);
      CheckListBoxLabels.Checked[idx]:=True;
    end;
  end;
end;

function CountPlaceholders(const _Which, _In: string): integer;
var
  Wanted: TFormatSpecifiers;
  Found: TFormatSpecifiers;
  i: Integer;
begin
  Result := 0;
  // _Which is a specifier itself, we are only interested in its type letter
  Wanted := ParseFormatSpecifiers(_Which);
  if Length(Wanted) <> 1 then
    Exit;
  Found := ParseFormatSpecifiers(_In);
  for i := 0 to High(Found) do
    if Found[i].TypeLetter = Wanted[0].TypeLetter then
      Inc(Result);
end;

function FindWarnings (item:TPoEntry):string;

  // Counts the specifiers in _In that are exactly _Specifier, e.g. '%.1f' does not
  // count for '%1f' even though both are float specifiers.
  function CountSpecifiers(const _Specifier: string; const _In: string): integer;
  var
    Specifiers: TFormatSpecifiers;
    i: integer;
  begin
    Result := 0;
    Specifiers := ParseFormatSpecifiers(_In);
    for i := 0 to High(Specifiers) do
      if FormatSpecifierToStr(Specifiers[i]) = _Specifier then
        Inc(Result);
  end;

  // Adds the distinct specifiers of _In to _Specifiers.
  procedure CollectSpecifiers(const _In: string; _Specifiers: TStrings);
  var
    Parsed: TFormatSpecifiers;
    i: integer;
    Normalized: string;
  begin
    Parsed := ParseFormatSpecifiers(_In);
    for i := 0 to High(Parsed) do begin
      Normalized := FormatSpecifierToStr(Parsed[i]);
      if _Specifiers.IndexOf(Normalized) = -1 then
        _Specifiers.Add(Normalized);
    end;
  end;

  function HasPlaceholderMismatch(const _Which: string; item: TPoEntry; out _Error: string): boolean;
  var
    count1: integer;
    count2: integer;
  begin
    count1 := CountSpecifiers(_Which, item.MsgId);
    count2 := CountSpecifiers(_Which, item.MsgStr);
    Result := count1 <> count2;
    if Result then
      _Error := Format(_('The original text includes %d times %s, but the translation contains %d of these.'),
        [Count1, _Which, Count2]);
  end;

var
  count1,count2,i:integer;
  s: string;
  Specifiers: TStringList;
begin
  Result:='';
  if item.MsgId='' then begin
    if item.MsgStr='' then
      Result:=Result+_('Please specify a correct PO header as translation for the empty string.')+sLineBreak;
  end else
  if item.MsgStr<>'' then begin
    // Check the format specifiers of both texts. Comparing the complete specifiers
    // rather than just their type letters also finds e.g. %.1f turned into %1f.
    Specifiers := TStringList.Create;
    try
      CollectSpecifiers(item.MsgId, Specifiers);
      CollectSpecifiers(item.MsgStr, Specifiers);
      for i := 0 to Specifiers.Count - 1 do
        if HasPlaceholderMismatch(Specifiers[i], item, s) then
          Result := Result + s + sLineBreak;
    finally
      FreeAndNil(Specifiers);
    end;
    if (LeftStr(item.MsgId,1)=' ') and (LeftStr(item.MsgStr,1)<>' ') then
      Result:=Result+_('Check spaces.')+sLineBreak;
    if (LeftStr(item.MsgId,1)<>' ') and (LeftStr(item.MsgStr,1)=' ') then
      Result:=Result+_('Check spaces.')+sLineBreak;
    if (RightStr(item.MsgId,1)=' ') and (RightStr(item.MsgStr,1)<>' ') then
      Result:=Result+_('Check spaces.')+sLineBreak;
    if (RightStr(item.MsgId,1)<>' ') and (RightStr(item.MsgStr,1)=' ') then
      Result:=Result+_('Check spaces.')+sLineBreak;

    // Check line breaks
    count1:=0;
    count2:=0;
    for i:=1 to length(item.MsgId) do
      if item.MsgId[i]=#10 then
        inc (count1);
    for i:=1 to length(item.MsgStr) do
      if item.MsgStr[i]=#10 then
        inc (count2);
    if count1<>count2 then
      Result:=Result+_('The number of line breaks do not match.')+sLineBreak;

    // Check tabulators
    count1:=0;
    count2:=0;
    for i:=1 to length(item.MsgId) do
      if item.MsgId[i]=#9 then
        inc (count1);
    for i:=1 to length(item.MsgStr) do
      if item.MsgStr[i]=#9 then
        inc (count2);
    if count1<>count2 then
      Result:=Result+_('The number of tabulators do not match.')+sLineBreak;

  end;
  Result:=trim(Result);
end;

end.