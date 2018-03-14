unit msgimportengine;

interface

type
  TMsgImportEngine=
    class
    public
      inputfilename:string;
      outputfilename:string;
      IgnoreDuplicates: boolean;
      procedure Execute;
    end;

implementation

uses
  sysutils, poparser, consoleoutput, gnugettext;

{ TMsgImportEngine }

procedure TMsgImportEngine.Execute;
var
  polist:TPoEntryList;
  tf:TextFile;
  aline:RawByteString;
  line:string;
  pe:TPoEntry;
  p:integer;
  row:integer;
begin
  polist:=TPoEntryList.Create;
  try
    AssignFile (tf,inputfilename);
    Reset (tf);
    try
      row:=0;
      while not eof(tf) do begin
        readln (tf,aline);
        line:=UTF8ToUnicodeString(aline);
        inc (row);
        line:=trim(line);
        if line<>'' then begin
          // Check for utf-8 validity
          p:=pos(#9,line);
          if p<=1 then begin
            Writeln (_('Error: No tabulator in line'));
            writeln (line);
          end else begin
            pe:=TPoEntry.Create;
            pe.MsgId:=copy(line,1,p-1);
            pe.MsgStr:=copy(line,p+1,maxint);
            pe.AutoCommentList.Add('#. Row '+IntToStr(row));
            if IgnoreDuplicates and Assigned(polist.Find(pe.MsgId)) then begin
              WriteLn('ignoring duplicate MsgId ' + pe.MsgId);
              FreeAndNil(pe);
            end else
              polist.Add(pe);
          end;
        end;
      end;
    finally
      CloseFile (tf);
    end;
    pe:=TPoEntry.Create;
    pe.MsgId:='';
    pe.MsgStr:='Project-Id-Version: Imported text file'#10+
               'POT-Creation-Date: '+FormatDateTime('yyyy-mm-dd hh:nn',now)+#10+
               'PO-Revision-Date: '#10+
               'Last-Translator: Somebody <somebody@somewhere.com>'#10+
               'MIME-Version: 1.0'#10+
               'Content-Type: text/plain; charset=UTF-8'#10+
               'Content-Transfer-Encoding: 8bit'#10;
    polist.Add (pe);
    polist.SaveToFile (outputfilename);
  finally
    FreeAndNil (polist);
  end;
end;

end.

