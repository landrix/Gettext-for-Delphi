{$APPTYPE CONSOLE}
program msgmergePOT;
// This tool should be used on two identical extracts from
// two source-code versions that only differ in the languages
// in the msgid, and both have msgstr blank.
// It was made for extracting the Borland translations from
// the German and French versions of the VCL.

uses
  Windows,
  SysUtils,
  Classes,
  gnugettext in '..\..\sample\gnugettext.pas',
  poparser in '..\..\dxgettext\poparser.pas',
  appconsts in '..\..\dxgettext\appconsts.pas',
  consoleoutput in '..\..\dxgettext\consoleoutput.pas',
  Math,
  u_dzQuicksort in '..\..\dxgettext\u_dzQuicksort.pas';

function StringProximity (s1,s2:string):integer;
var
  i:integer;
begin
  Result:=0;
  for i:=1 to min(length(s1),length(s2)) do begin
    if s1[i]=s2[i] then
      inc (Result,100)
    else
    if uppercase(s1[i])=uppercase(s2[i]) then
      inc (Result,80)
    else
    inc (Result,50-min(50,abs(ord(uppercase(s1)[i])-ord(uppercase(s2)[i]))));
  end;
end;

procedure Convert (pe:TPoEntry;trans:TPoEntryList);
var
  pt:TPoEntry;
  bestentry:TPoEntry;
  maxpoints,points:integer;
  fuzzy:boolean;
  i,j,idx:integer;
  s,s2:string;
begin
  if pe.MsgId='' then
    exit;
  maxpoints:=0;
  bestentry:=nil;
  fuzzy:=false;
  pt:=trans.FindFirst;
  while pt<>nil do begin
    if (pt.AutoCommentList.Text=pe.AutoCommentList.Text) then begin
      // The perfect case: The translation matches something in the other file
      bestentry:=pt;
      fuzzy:=false;
      break;
    end else begin
      // The semi-perfect case: Programmer's name matches
      points:=0;
      for i:=0 to pt.AutoCommentList.Count-1 do begin
        s:=pt.AutoCommentList.Strings[i];
        if copy(s,1,29)='#. Programmer''s name for it: ' then begin
          idx:=pe.AutoCommentList.IndexOf(s);
          if idx<>-1 then
            inc(points,10);
        end;
        if copy(s,1,2)='#:' then begin
          for j:=0 to pe.AutocommentList.Count-1 do begin
            s2:=pe.AutoCommentList.Strings[j];
            if s=s2 then begin
              inc (points,5);
              break;
            end else
              if samefilename(s,s2) then
                inc (points,2); 
          end;
        end;
      end;
      if points<>0 then
        points:=points+9000;
      if points>maxpoints then begin
        maxpoints:=points;
        bestentry:=pt;
        fuzzy:=false;
      end else begin
        // Third solution: source positions match
        points:=0;
        for i:=0 to pt.AutoCommentList.Count-1 do begin
          s:=pt.AutoCommentList.Strings[i];
          if copy(s,1,2)='#:' then begin
            idx:=pe.AutoCommentList.IndexOf(s);
            if idx<>-1 then
              inc(points);
          end;
        end;
        if points<>0 then
          points:=points+9000;
        if points>maxpoints then begin
          maxpoints:=points;
          bestentry:=pt;
          fuzzy:=false;
        end;
      end;
    end;
    pt:=trans.FindNext(pt);
  end;
  if bestentry<>nil then begin
    pe.MsgStr:=bestentry.MsgId;
    if fuzzy then
      pe.AutoCommentList.Add('#, fuzzy');
  end else begin
    writeln (Format(_('No translation for %s'),[pe.MsgId]));
  end;
end;

function samefilename (s1,s2:string):boolean;
var
  p:integer;
begin
  Result:=false;
  // Strip colon and things after it
  p:=pos(':',s1);
  if p=0 then exit;
  s1:=copy(s1,1,p-1);
  // Strip colon and things after it
  p:=pos(':',s2);
  if p=0 then exit;
  s1:=copy(s2,1,p-1);
  // Strip up to last slash
  while true do begin
    p:=pos('/',s1);
    if p=0 then break;
    s1:=copy(s1,p+1,maxint);
  end;
  // Strip up to last slash
  while true do begin
    p:=pos('/',s2);
    if p=0 then break;
    s2:=copy(s1,p+1,maxint);
  end;
  Result:=uppercase(s1)=uppercase(s2);
end;

{ Main routine }

var
  srceng:TextFile;
  dest:TFileStream;
  trans:TPoEntryList;
  pe:TPoEntry;
  pop:TPoParser;
begin
  textdomain('dxgettext');
  AddDomainForResourceString('delphi');
  AddDomainForResourceString('kylix');

  try
    if paramcount<>3 then begin
      // Messages about how to use this program
      writeln (Format(_('msgmergePOT %s'),(.version.)));
      writeln;
      writeln (_('msgmergePOT usage:'));
      writeln (_('  msgmergePOT english.po otherlanguage.po destination.po'));
      writeln;
      writeln (_('This will create a po file from two identical po extracts that'+sLineBreak+
                 'only differ by the language, for instance the German and English'+sLineBreak+
                 'runtime source code from Borland.'));
      writeln;
      exit;
    end;

    trans:=TPoEntryList.Create;
    pop:=TPoParser.Create;
    try
      trans.LoadFromFile(paramstr(2));

      AssignFile (srceng,paramstr(1));
      Reset (srceng);
      dest:=TFileStream.Create(paramstr(3), fmCreate);
      try
        while true do begin
          pe:=pop.ReadNextEntry (srceng);
          if pe=nil then
            break;
          Convert (pe,trans);
          pe.WriteToStream(dest);
        end;
      finally
        CloseFile (srceng);
        FreeAndNil (dest);
      end;
    finally
      FreeAndNil (trans);
      FreeAndNil (pop);
    end;
  except
    on e:Exception do begin
      writeln (_('Exception occured'));
      writeln (e.message);
      ExitCode:=1;
    end;
  end;
end.
