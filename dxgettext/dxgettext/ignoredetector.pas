unit ignoredetector;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl, Carlos Macao              *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dxgettext.po.dk/ for more information            *)
(*                                                              *)
(****************************************************************)

interface

uses
  PoParser;

// Determines if entry is translatable. If not, it will add some comments to entry.
function IsProbablyTranslatable ( xEntry: TPoEntry;
                                  xBlackList, xWhiteList: TPoEntryList): Boolean;


implementation

uses
  SysUtils,
  {$ifdef UNICODE} // >=D2009
  Character;
  {$else}
  CharacterHelper;
  {$endif}

function LastWord(sWord,s:string):boolean;
var
  posWord:integer;
begin
  result:=false;
  posWord:=Pos('.'+sWord, s);
  if (posWord<>0) and ((posWord+length('.'+sWord)-1)=length(s)) then
    result:=true;
end;

function EndsWith(sEnd, s: string): boolean;
var
 SubLen: integer;
begin
 SubLen := Length(sEnd);
 Assert(SubLen > 0);
 Result := SameText(Copy(s, Length(s)-SubLen+1, SubLen), sEnd);
end;

function IsProbablyTranslatable ( xEntry: TPoEntry;
                                  xBlackList, xWhiteList: TPoEntryList): Boolean;
var
  i:integer;
  lHasLetters, lHasDigits, lHasWhiteSpace, lHasPunctuationMark: boolean;
  c:char;
  s2:string;
  found:boolean;
  s:string;
  lStartString: String;

function NameParam(s:string;i:integer):boolean;
var
  posFile:integer;
  f:text;
  fileName,s2,
  oldS1,oldS2,oldS3:string;
  endSearch:boolean;
  posSep,lineNum:integer;
begin
  result:=false;
  fileName:='';
  s:='';
  s2 := xEntry.AutoCommentList.Strings[i+1];
  if Copy(s2,1,2)='#:' then
    s:=Trim(Copy(s2,3,length(s2)-2));
  if s<>'' then begin
    posSep:=0;
    if Pos(':',s)<>0 then
      posSep := Pos(':',s);
    if posSep<>0 then begin
      fileName:=Copy(s,1,posSep-1);
      { TODO : filename needs to have the base directory in front }
      if fileExists(fileName) then // Search for the msgid associated file
      try
        posFile:=StrToInt(Copy(s,posSep+1,length(s)-posSep));
        try
          AssignFile(f,fileName);
          Reset(f);
          oldS1:='';
          oldS2:='';
          oldS3:='';
          endSearch:=false;
          lineNum:=1;
          if not eof(f) then begin
            repeat
              ReadLn(f,s2);
              if (lineNum=posFile) then
              begin
                if (Pos('PARAMS = <',oldS1)<>0) and (Pos('ITEM',oldS2)<>0) and (Pos('DATATYPE =',oldS3)<>0) then
                  // If it's a Param item Name
                  result:=true;
                endSearch:=true;
              end;
              oldS1:=oldS2;
              oldS2:=oldS3;
              oldS3:=uppercase(s2);
              Inc(lineNum);
            until eof(f) or endSearch;
          end;
        finally
          Close(f);
        end;
      except
      end;
    end;
  end;
end;

begin
  // Check for msgid ""
  if xEntry.MsgId='' then
  begin
    Result:=True;
    exit;
  end;

  //*** never add WhiteList entrys to ignore file
  if not ( Assigned( xWhiteList) and
           Assigned( xWhiteList.Find( xEntry.MsgId))) then
  begin
    //*** always add blacklist entrys to ignore file
    if Assigned( xBlackList) and
       Assigned( xBlackList.Find( xEntry.MsgId)) then
    begin
      xEntry.UserCommentList.Add('#  blacklist entry');
      Result:=False;
      exit;
    end;

    s2 := xEntry.msgid;

    lStartString := LowerCase(copy(s2, 1, 2));
    if (lStartString = 'l_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed label');
      Result:=False;
      exit;
    end
    else if (lStartString = 'a_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed action');
      Result:=False;
      exit;
    end
    else if (lStartString = 'b_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed button');
      Result:=False;
      exit;
    end
    else if (lStartString = 'e_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed edit');
      Result:=False;
      exit;
    end
    else if (lStartString = 'p_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed panel');
      Result:=False;
      exit;
    end;

    lStartString := LowerCase(copy(s2, 1, 3));
    if (lStartString = 'ac_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed action');
      Result:=False;
      exit;
    end
    else if (lStartString = 'bb_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed bitbutton');
      Result:=False;
      exit;
    end
    else if (lStartString = 'cb_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed combo box');
      Result:=False;
      exit;
    end
    else if (lStartString = 'gb_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed group box');
      Result:=False;
      exit;
    end
    else if (lStartString = 'tb_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed tool bar');
      Result:=False;
      exit;
    end
    else if (lStartString = 'ts_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed tab sheet');
      Result:=False;
      exit;
    end;

    lStartString := LowerCase(copy(s2, 1, 4));
    if (lStartString = 'act_') then
    begin
      xEntry.UserCommentList.Add('#  Seems like unrenamed action');
      Result:=False;
      exit;
    end;

    // Check for text types
    lHasLetters:=False;
    lHasDigits:=False;
    lHasWhiteSpace:=False;
    lHasPunctuationMark:= False;
    for i:=1 to length(s2) do
    begin
      c:=s2[i];

      {$ifdef UNICODE} // >=D2009
      if (ord(c)>=128) or CharInSet(c,['a'..'z','A'..'Z']) then
        lHasLetters:=True;
      if CharInSet(c,['0'..'9']) then
        lHasDigits:=True;
      if CharInSet(c,[#0..#32]) then
        lHasWhiteSpace:=True;
      if CharInSet(c, ['.', ',', ':', '-', '/']) then
        lHasPunctuationMark := True;
      {$else}
      if (ord(c)>=128) or (c in ['a'..'z','A'..'Z']) then
        hasletters:=True;
      if (c in ['0'..'9']) then
        hasdigits:=True;
      if (c in [#0..#32]) then
        haswhitespace:=True;
      if (c in ['.', ',', ':', '-', '/']) then
        lHasPunctuationMark := True;
      {$endif}
    end;

    if (not lHasWhiteSpace) and
       lHasLetters and
       lHasDigits and
       ( not lHasPunctuationMark) then
    begin
      xEntry.UserCommentList.Add('#  Doesn''t look like text');
      Result:=False;
      exit;
    end;

    if not lHasLetters then
    begin
      xEntry.UserCommentList.Add('#  Doesn''t have any letters');
      Result:=False;
      exit;
    end;

    // Check for font names, component names etc.
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=trim(xEntry.AutoCommentList.Strings[i]);
      if copy(s,1,2)='#.' then begin
        if uppercase(copy(s,length(s)-8,9))='FONT.NAME' then begin
          Found:=True;
        end else begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like a Font.Name extract');
      Result:=False;
      exit;
    end;

    // Check for formats
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then begin
        if LastWord('FORMAT',s) or LastWord('DISPLAYFORMAT',s) then begin
          Found:=True;
        end else
        if Found then begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like a format string');
      Result:=False;
      exit;
    end;

    // Check for tablenames
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then begin
        if LastWord('TABLENAME',s) then begin
          Found:=True;
        end else
        if Found then begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like a Database table name');
      Result:=False;
      exit;
    end;

    // Check for fieldnames
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do
    begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then
      begin
        if LastWord('DATAFIELD',s) or LastWord('FIELDNAME',s) or
           LastWord('LOOKUPFIELD',s) or
           LastWord('INDEXNAME',s) or LastWord('KEYFIELDS',s) or
           LastWord('LOOKUPKEYFIELDS',s) or
           LastWord('LOOKUPRESULTFIELD',s) or
           LastWord('KEYLINKS.STRINGS',s) or
           LastWord('LISTFIELD',s) or LastWord('KEYFIELD',s) or
           LastWord('KEYFIELDNAMES',s) or LastWord('ORIGIN',s) or LastWord('PARAMNAMES.STRINGS',s) or
           LastWord('MASTERFIELDS',s) or LastWord('GENERATORLINKS.STRINGS',s) or
           (LastWord('NAME',s) and NameParam(s, i)) or
           LastWord('DATABASENAME',s) or
           LastWord('SESSIONNAME',s) or
           LastWord('CONNECTIONSTRING', s) then
        begin
          Found := True;
        end
        else if Found then
        begin
          Found := False;
          break;
        end;
      end;
    end;
    if Found then
    begin
      xEntry.UserCommentList.Add('#  Seems like a Database field name or index field name');
      Result:=False;
      exit;
    end;

    // Check for SQL sentences
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then begin
        if LastWord('SQL.STRINGS',s) or LastWord('SELECTSQL.STRINGS',s) or LastWord('KEYRELATION',s) or LastWord('FIELDSDISPLAYLABEL.STRINGS',s) then begin
          Found:=True;
        end else begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like a SQL sentence');
      Result:=False;
      exit;
    end;

    // Check for foldernames or filenames or paths
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then begin
        if LastWord('FILENAME',s) or LastWord('REGISTRYPATH',s) then begin
          Found:=True;
        end else begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like a path, foldername or filename');
      Result:=False;
      exit;
    end;

    // Check for Default and Param Strings
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then begin
        if LastWord('DEFAULTVALUES.STRINGS',s) or LastWord('PARAMS.STRINGS',s) then begin
          Found:=True;
        end else begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like a Default or Param string Value');
      Result:=False;
      exit;
    end;

    // Check for Column Attributes
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then begin
        if LastWord('COLUMNATTRIBUTES.STRINGS',s) then begin
          Found:=True;
        end else begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like a Column Attribute');
      Result:=False;
      exit;
    end;

    // Check for PropertiesClassName
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then begin
        if LastWord('PROPERTIESCLASSNAME',s) then begin
          Found:=True;
        end else begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like an Internal ClassName');
      Result:=False;
      exit;
    end;

    // Check for Value properties
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then begin
        if LastWord('VALUE',s) or LastWord('PASSWORD',s) or LastWord('ROOT',s) then begin
          Found:=True;
        end else begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like a propertie Value');
      Result:=False;
      exit;
    end;

    // Check for font names, component names etc.
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then begin
        if (copy(s,length(s)-7,8)='....NAME') or LastWord('STOREDPROCNAME', s) then begin
          Found:=True;
        end else begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like a component name');
      Result:=False;
      exit;
    end;

    // Check for file masks and file extensions
    s:=s2;
    if (copy(s,1,1)='*') and (length(s)=5) or (length(s)=4) then begin
      if length(s)=5 then
        delete (s,1,1);
      if copy(s,1,1)='.' then begin
        delete (s,1,1);
        for i:=1 to 3 do
        begin
          {$ifdef UNICODE} // >=D2009
          found:=CharInSet(s[i],['a'..'z','A'..'Z']);
          {$else}
          found := (s[i] in ['a'..'z','A'..'Z']);
          {$endif}
          if not found then break;
        end;
        if found then begin
          xEntry.UserCommentList.Add('#  Looks like a file extension or file mask');
          Result:=False;
          exit;
        end;
      end;
    end;

     // Check for JVCL TJvAppStorage and TJvFormStorage
    found:=False;
    for i:=0 to xEntry.AutoCommentList.Count-1 do begin
      s:=uppercase(trim(xEntry.AutoCommentList.Strings[i]));
      if copy(s,1,2)='#.' then begin
        if EndsWith('STORAGEOPTIONS.BOOLEANSTRINGTRUEVALUES', s)
          or EndsWith('STORAGEOPTIONS.BOOLEANSTRINGFALSEVALUES', s)
          or EndsWith('..FORMSTORAGE..APPSTORAGEPATH', s) then begin
          Found:=True;
        end else begin
          Found:=False;
          break;
        end;
      end;
    end;
    if Found then begin
      xEntry.UserCommentList.Add('#  Seems like a JVCL TJvFormStorage/TJvAppStorage Property');
      Result:=False;
      exit;
    end;
  end;

  Result:=True;
end;

end.
