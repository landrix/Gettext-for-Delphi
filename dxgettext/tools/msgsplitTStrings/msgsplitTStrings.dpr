{$APPTYPE CONSOLE}
program msgsplitTStrings;
// This tool splits all multi-line translations into several
// single-line translations, if all lines end with \n

uses
  SysUtils,
  Classes,
  gnugettext in '..\..\sample\gnugettext.pas',
  poparser in '..\..\dxgettext\poparser.pas',
  appconsts in '..\..\dxgettext\appconsts.pas',
  u_dzQuicksort in '..\..\dxgettext\u_dzQuicksort.pas',
  Math,
  ConsoleAppHandler in '..\..\dxgettext\ConsoleAppHandler.pas',
  xgettexttools in '..\..\dxgettext\xgettexttools.pas';

{ Main routine }

var
  src:TextFile;
  dest:TFileStream;
  pe,pe2,pe3:TPoEntry;
  pop:TPoParser;
  pol:TPoEntryList;
  extra:TPoEntryList;
  sl1,sl2:TStringList;
  i:integer;
begin
  {$ifdef mswindows}
  // Translation disabled on Windows because it's a console program
  gnugettext.DefaultInstance.Enabled:=False;
  {$endif}
  textdomain('dxgettext');
  {$ifdef LINUX}
  AddDomainForResourceString('kylix3');
  {$endif}

  try
    if (paramcount<>3) or (uppercase(paramstr(2))<>'-O') then begin
      // Messages about how to use this program
      writeln (Format(_('msgsplitTStrings %s'),(.version.)));
      writeln;
      writeln (_('msgsplitTStrings usage:'));
      writeln (_('  msgsplitTStrings input.po -o output.po'));
      writeln;
      writeln (_('This will split all multiline translations into several single'+sLineBreak+
                 'line translations, while still keeping the multiline translations'+sLineBreak+
                 'around. Use msgmerge on the output file afterwards.'));
      writeln;
      exit;
    end;

    pol:=TPoEntryList.Create;
    extra:=TPoEntryList.Create;
    pop:=TPoParser.Create;
    sl1:=TStringList.Create;
    sl2:=TStringList.Create;
    try
      pol.LoadFromFile(paramstr(1));

      AssignFile (src,paramstr(1));
      Reset (src);
      dest:=TFileStream.Create(paramstr(3), fmCreate);
      try
        while true do begin
          pe:=pop.ReadNextEntry (src);
          if pe=nil then
            break;
          if (pe.MsgId<>'') and (copy(pe.MsgId,length(pe.MsgId),1)=#10) then begin
            sl1.Text:=pe.MsgId;
            sl2.Text:=pe.MsgStr;
            for i:=0 to sl1.Count-1 do begin
              pe2:=TPoEntry.Create;
              pe2.Assign(pe);
              pe2.MsgId:=sl1.strings[i];
              if i>=sl2.Count then
                pe2.MsgStr:=''
              else
                pe2.MsgStr:=sl2.strings[i];
              pe3:=pol.Find(pe2.MsgId);
              if pe3=nil then begin
                if extra.Find(pe2.MsgId)=nil then begin
                  pe2.AutoCommentList.Add('#, fuzzy');
                  extra.Add(pe2);
                  pe2:=nil;
                end;
              end;
              if pe3<>nil then begin
                FreeAndNil (pe2);
                if pe3.AutoCommentList.IndexOf('#, fuzzy')=-1 then
                  pe3.AutoCommentList.Add('#, fuzzy');
              end;
            end;
          end;
        end;
        pe:=pol.FindFirst;
        while pe<>nil do begin
          pe.WriteToStream (dest);
          pe:=pol.FindNext(pe);
        end;
        pe:=extra.FindFirst;
        while pe<>nil do begin
          pe.WriteToStream (dest);
          pe:=extra.FindNext(pe);
        end;

        FormatOutputWithMsgCat( dest);
      finally
        CloseFile (src);
        FreeAndNil (dest);
      end;
    finally
      FreeAndNil (pop);
      FreeAndNil (pol);
      FreeAndNil (sl1);
      FreeAndNil (sl2);
      FreeAndNil (extra);
    end;
  except
    on e:Exception do begin
      writeln (_('Exception occured'));
      writeln (e.message);
      ExitCode:=1;
    end;
  end;
end.
