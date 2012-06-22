program stripifdef;
// Program used to strip ifdef compiler directives out of gnugettext.pas

uses
  SysUtils;
  
var
  infile,outfile:TextFile;
  line,uline:string;
  level:integer;
  include:boolean;
  cutdirective:boolean;
begin
  AssignFile (infile,paramstr(1));
  Reset (infile);
  AssignFile (outfile,paramstr(2));
  Rewrite (outfile);
  try
    level:=0;
    Include:=True;
    CutDirective:=False;
    while not eof (infile) do begin
      Readln (infile,line);
      uline:=uppercase(trim(line));
      if copy(uline,1,4)='{$IF' then
        inc (level);
      if (copy(uline,1,11)='{$IFDEF VER') or
         (uline='{$IFDEF DELPHI5OROLDER}') then
        if level=1 then
          include:=False;
      if (uline='{$IFNDEF DELPHI5OROLDER}') then
        if level=1 then
          CutDirective:=True;

      if uline='{$IFNDEF DELPHI6OROLDER}' then begin
        writeln (outfile,'{$ifndef VER140}');
      end else
      if Include then
        if (not CutDirective) or
          ((copy(uline,1,4)<>'{$IF') and (uline<>'{$ENDIF}')) then
          Writeln (outfile,line);

      if uline='IMPLEMENTATION' then begin
        writeln (outfile);
        writeln (outfile,'{$ifndef MSWINDOWS}');
        writeln (outfile,'{$ifndef LINUX}');
        writeln (outfile,'  ''This version of gnugettext.pas is only meant to be compiled with Kylix 3,''');
        writeln (outfile,'  ''Delphi 6, Delphi 7 and later versions. If you use other versions, please''');
        writeln (outfile,'  ''get the gnugettext.pas version from the Delphi 5 directory.''');
        writeln (outfile,'{$endif}');
        writeln (outfile,'{$endif}');
      end;

      if (level<>0) and (uline='{$ENDIF}') then begin
        dec (level);
        if level=0 then begin
          include:=True;
          CutDirective:=False;
        end;
      end;
    end;
  finally
    CloseFile (infile);
    CloseFile (outfile);
  end;
end.
