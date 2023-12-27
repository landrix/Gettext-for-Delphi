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
  u_dzQuicksort in '..\..\dxgettext\u_dzQuicksort.pas',
  ConsoleAppHandler in '..\..\dxgettext\ConsoleAppHandler.pas',
  xgettexttools in '..\..\dxgettext\xgettexttools.pas';

type
  TAutoCommentCompareType = (acctError, acctFileName, acctBaseDirAndFileName);

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

function SameAutoComment( const xCompateType: TAutoCommentCompareType;
                          const xIdComment, xTransComment: string): Boolean;
var
  p, i, lLastPathEntry, lIdPosition, lTransPosition: integer;
  lIdList, lTransList: TStringList;
  lIdFolderName, lTransFolderName, lIdFileName, lTransFileName: String;
begin
  Result := False;

  lIdFileName      := '';
  lTransFileName   := '';

  lIdFolderName    := '';
  lTransFolderName := '';

  lIdPosition    := 0;
  lTransPosition := 0;

  lIdList    := nil;
  lTransList := nil;
  try
    lIdList    := TStringList.Create;
    lTransList := TStringList.Create;

    if (ExtractStrings(['/'], ['#', ':', ' '], PChar(xIdComment)   , lIdList   ) > 0) and
       (ExtractStrings(['/'], ['#', ':', ' '], PChar(xTransComment), lTransList) > 0) then
    begin
      //lIdFolderName    := lIdList.Text;
      //lTransFolderName := lTransList.Text;

      //*** extract folder part
      lLastPathEntry := lIdList.Count - 2;

      for i := 0 to lLastPathEntry do
      begin
        lIdFolderName := lIdFolderName + '/' + lIdList[i];
      end;


      lLastPathEntry := lTransList.Count - 2;

      if xCompateType = acctBaseDirAndFileName then
      begin
        Inc(lLastPathEntry, -1);
      end;

      for i := 0 to lLastPathEntry do
      begin
        lTransFolderName := lTransFolderName + '/' + lTransList[i];
      end;


      //*** extract FileName part (last entry in list)
      // Strip colon and things after it
      p := LastDelimiter(':', lIdList[lIdList.Count - 1]);
      if p = 0 then
      begin
        exit;
      end;
      lIdFileName := copy(lIdList[lIdList.Count - 1], 1, p - 1);
      lIdPosition := StrToInt( copy( lIdList[lIdList.Count - 1], p + 1, Length(lIdList[lIdList.Count - 1]) - p));

      // Strip colon and things after it
      p := pos(':', lTransList[lTransList.Count - 1]);
      if p = 0 then
      begin
        exit;
      end;
      lTransFileName := copy(lTransList[lTransList.Count - 1], 1, p - 1);
      lTransPosition := StrToInt( copy( lTransList[lTransList.Count - 1], p + 1, Length(lTransList[lTransList.Count - 1]) - p));
    end;

    case xCompateType of
      acctFileName:
        begin
          Result := SameFileName(lIdFileName, lTransFileName) and
                    (lIdPosition = lTransPosition);
        end;
      acctBaseDirAndFileName:
        begin
          Result := SameText(lIdFolderName, lTransFolderName) and
                    SameFileName(lIdFileName, lTransFileName) and
                    (lIdPosition = lTransPosition);
        end;
    else
      raise Exception.Create('invalid type');
    end;
  finally
    FreeAndNil(lIdList);
    FreeAndNil(lTransList);
  end;
end;

procedure Convert (xPe: TPoEntry; xTrans: TPoEntryList);
var
  lPt: TPoEntry;
  lBestEntry: TPoEntry;
  lMaxPoints, lPoints: integer;
  fuzzy: boolean;
  i, j, idx: integer;
  s, s2: string;
begin
  if xPe.MsgId='' then
  begin
    exit;
  end;

  lMaxPoints := 0;
  lBestEntry := nil;
  fuzzy     := false;

  lPt := xTrans.FindFirst;
  while lPt <> nil do
  begin
    if (lPt.AutoCommentList.Text=xPe.AutoCommentList.Text) then
    begin
      // The perfect case: The translation matches something in the other file
      lBestEntry := lPt;
      fuzzy := False;

      break;
    end
    else
    begin
      // The semi-perfect case: Programmer's name matches
      lPoints := 0;

      if xPe.MsgId = ' &Images ' then
      begin
        lPoints := 0;
      end;

      for i := 0 to lPt.AutoCommentList.Count - 1 do
      begin
        s:=lPt.AutoCommentList.Strings[i];
        if copy(s , 1, 29) = '#. Programmer''s name for it: ' then
        begin
          idx := xPe.AutoCommentList.IndexOf(s);
          if idx <> -1 then
          begin
            inc(lPoints, 50);
          end;
        end;

        if (copy(s, 1, 2) = '#:') then
        begin
          for j := 0 to xPe.AutocommentList.Count - 1 do
          begin
            s2 := xPe.AutoCommentList.Strings[j];

            if (s = s2) then
            begin
              inc (lPoints, 20);
              break;
            end
            else if (copy(s2, 1, 2) = '#:') and
                    SameAutoComment(acctBaseDirAndFileName, s2, s) then
            begin
              inc (lPoints, 10);
            end;
          end;
        end;
      end;

      if lPoints <> 0 then
      begin
        lPoints := lPoints + 9000;
      end;

      if (lPoints > lMaxPoints) then
      begin
        lMaxPoints := lPoints;
        lBestEntry := lPt;
        fuzzy := false;
      end
      else
      begin
        // Third solution: source positions match
        lPoints := 0;
        for i := 0 to lPt.AutoCommentList.Count - 1 do
        begin
          s := lPt.AutoCommentList.Strings[i];

          if (copy(s, 1, 2) = '#:') then
          begin
            idx := xPe.AutoCommentList.IndexOf(s);
            if idx <> -1 then
            begin
              //*** perfect source position match
              inc(lPoints, 5);
            end
            else
            begin
              //*** match one sub folder deeper (e.g. for ...\db\XX\DBLogDlg.pas)
              for j := 0 to xPe.AutocommentList.Count - 1 do
              begin
                s2 := xPe.AutoCommentList.Strings[j];

                if (copy(s2, 1, 2) = '#:') and
                   SameAutoComment(acctBaseDirAndFileName, s2, s) then
                begin
                  inc (lPoints, 2);
                end;
              end;
            end;
          end;
        end;

        if (lPoints <> 0) then
        begin
          lPoints := lPoints + 9000;
        end;

        if lPoints > lMaxPoints then
        begin
          lMaxPoints := lPoints;
          lBestEntry := lPt;
          fuzzy := false;
        end;
      end;
    end;

    lPt := xTrans.FindNext(lPt);
  end;

  if lBestEntry <> nil then
  begin
    if (xPe.MsgId <> lBestEntry.MsgId) then
    begin
      xPe.MsgStr := lBestEntry.MsgId;

      if fuzzy then
      begin
        xPe.AutoCommentList.Add('#, fuzzy');
      end;
    end
    else
    begin
      writeln (Format(_('msgId %s: translation equal'), [xPe.MsgId]));
    end;
  end
  else
  begin
    writeln (Format(_('No translation for %s'), [xPe.MsgId]));
  end;
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

        FormatOutputWithMsgCat( dest);
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
