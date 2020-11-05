unit consoleapp;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl                          *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dxgettext.po.dk/ for more information          *)
(*                                                            *)
(**************************************************************)

interface

uses
  xgettext;

type
  TConsoleApp=
    class
    private
      prevTask:string;
      Quiet:boolean;
      IgnoreConstReplaceWarnings:boolean;
      ProgramHeaderWritten:boolean;
      ThereAreWarnings:boolean;
      procedure WriteHelp;
      procedure Progress (const CurrentTask,CurrentFileName:string; const LineNumber:Integer);
      procedure Warning (WarningType:TWarningType;const Msg,Line,Filename:string;LineNumber:Integer);
      procedure WriteProgramHeader;
    public
      procedure WriteLine (const msg:string);
      procedure Execute;
    end;

implementation

uses
  Classes, SysUtils,
  {$ifdef mswindows}  Windows, {$endif}
  appconsts, gnugettext, xgettexttools, consoleoutput;
  
{ TConsoleApp }

procedure TConsoleApp.Execute;
var
  xgt:TXGetText;
  i:integer;
  param,uparam:string;
  tf:TextFile;
  {$ifdef LINUX}
  found:boolean;
  {$endif}
  DetectWarnings:boolean;
  w: integer;
  lTicks: Int64;
begin
  lTicks := GetTickCount;
  try
    if (paramcount=0) then begin
      WriteHelp;
      exit;
    end;

    DetectWarnings:=false;
    xgt:=TXGetText.Create;
    try
      xgt.OnWarning:=Warning;
      xgt.OnProgress:=Progress;
      i:=1;
      while i<=paramcount do begin
        param:=paramstr(i);
        uparam:=uppercase(param);
        {$ifdef mswindows}
        if uparam='--CODEPAGE' then begin
          inc (i);
          DefCP:=StrToInt(paramstr(i));
        end else
        {$endif}
        if uparam='--NONASCII' then begin
          xgt.AllowNonAscii:=True;
        end else
        if uparam='--DETECTWARNINGS' then begin
          // Warnings should return an exit code different from zero
          DetectWarnings := true;
        end else
        if uparam='-R' then begin
          xgt.recurse:=True;
        end else
        if uparam='-Q' then begin
          Quiet:=True;
        end else
        if uparam='--HELP' then begin
          WriteHelp;
          exit;
        end else
        if uparam='-B' then begin
          inc (i);
          xgt.AddBaseDirectory (ExpandFileName(paramstr(i)));
        end else
        if uparam='--USEIGNOREPO' then
        begin
          xgt.UseIgnoreFile:=True;
        end else
        if uparam='--UPDATEIGNORE' then
        begin
          xgt.UpdateIgnore:=True;
        end else
        if uparam = '--PRESERVEUSERCOMMENTS' then
        begin
          xgt.PreserveUserComments := True;
        end
        else
        if uparam='--SO' then begin
          inc (i);
          xgt.SingleOutputFilename:=ExpandFileName(paramstr(i));
        end else
        if uparam='-O' then begin
          inc (i);
          xgt.DestinationPath:=IncludeTrailingPathDelimiter(ExpandFileName(paramstr(i)));
        end else
        if uparam='-O:OCC' then begin
          xgt.OrderbyMsgid:=False;
        end else
        if uparam='-O:MSGID' then begin
          xgt.OrderbyMsgid:=True;
        end else
        if uparam='--NOWC' then begin
          xgt.NoWildcards:=True;
        end else
        if uparam='--IGNORE-CONSTREPLACE' then begin
          IgnoreConstReplaceWarnings:=True;
        end else
        if uparam='--DELPHI' then begin
          xgt.AddDelphiFilemasks;
        end else
        if Copy(uparam, 7)='--WRAP=' then begin
          if not TryStrToInt(Copy(uparam, 8), w) or (w < 10) then begin
            WriteHelp;
            exit;
          end;
          xgt.MaxWidth := w;
        end else
        if uparam='--NO-WRAP' then begin
          xgt.MaxWidth := 0;
        end else
        if uparam='--KYLIX' then begin
          xgt.AddKylixFilemasks;
        end else
        if uparam = '--NO-LINE-NUMBERS' then begin
           xgt.LineNumbers := False;
        end else
        begin
          if copy(param,1,1)='@' then begin
            FileMode:=fmOpenRead;
            AssignFile (tf,copy(param,2,maxint));
            Reset (tf);
            try
              while not eof(tf) do begin
                readln (tf,param);
                param:=trim(param);
                xgt.filemasks.Add (param);
              end;
            finally
              CloseFile (tf);
            end;
          end else
            xgt.filemasks.add (param);
        end;
        inc (i);
      end;

      if not Quiet then
        WriteProgramHeader;

      {$ifdef LINUX}
      if not xgt.NoWildcards then begin
        found:=False;
        for i:=0 to xgt.filemasks.count-1 do begin
          if (pos('*',xgt.filemasks.strings[i])<>0) or
             (pos('*',xgt.filemasks.strings[i])<>0) then
            found:=True;
        end;
        if not found then begin
          WriteLine ('');
          WriteLine (_('Warning: Remember to escape wildcards in the shell'));
          WriteLine (_('Use --nowc option to disable this message.'));
        end;
      end;
      {$endif}

      xgt.Execute;
    finally
      FreeAndNil (xgt);
      if DetectWarnings and ThereAreWarnings then
        ExitCode:=1;
    end;
  finally
    OutputDebugString(PWideChar(Format( _('Duration: %s ms'),
                                        [IntToStr(GetTickCount - lTicks)])));
  end;
end;

procedure TConsoleApp.Progress(const CurrentTask:String;const CurrentFileName: string;
  const LineNumber: Integer);
begin
  if (prevTask<>CurrentTask) then begin
    prevTask:=CurrentTask;
    if (not Quiet) then
      WriteLine(CurrentTask);
  end;
end;

procedure TConsoleApp.Warning (WarningType:TWarningType;const Msg,Line,Filename:string;LineNumber:Integer);
var
  linenrstr:string;
begin
  if not ProgramHeaderWritten then
    WriteProgramHeader;
  if IgnoreConstReplaceWarnings and (WarningType in [wtConstantReplaced]) then
    exit;
  linenrstr:=IntToStr(linenumber);
  if Quiet then begin
    if WarningType=wtUnexpectedException then begin
      WriteLine (Format(_('ERROR in %s: %s'),[Filename,msg]));
    end else begin
      WriteLine (Format(_('WARNING in %s: %s'),[Filename,msg]));
    end;
  end else begin
    if WarningType=wtUnexpectedException then begin
      WriteLine (Format(_('ERROR: %s'),[msg]));
    end else begin
      WriteLine (Format(_('WARNING: %s'),[msg]));
    end;
  end;
  WriteLine (linenrstr+': '+Line);
end;

procedure TConsoleApp.WriteHelp;
begin
  // Headline for screen output when no parameter is given
  WriteProgramHeader;
  WriteLine (_('dxgettext usage:'));
  {$ifdef MSWINDOWS}
  WriteLine ('  dxgettext *.pas *.dfm *.dpr -r');
  {$endif}
  {$ifdef LINUX}
  WriteLine ('  dxgettext \*.pas \*.dfm \*.dpr -r');
  {$endif}
  WriteLine ('  dxgettext'+_(' -b c:\source\myprogram --delphi'));
  WriteLine ('');
  // This message may not be wider than 78 characters because it must fit onto a 80x25 text mode screen.
  WriteLine (_('This will extract all texts from the specified files and save them'+sLineBreak+
             'in a file named default.po.'));
  WriteLine ('');
  WriteLine (_('Options:'));
  WriteLine ('  --delphi                 ' + _('Adds the wildcards: *.pas *.inc *.rc *.dpr *.xfm *.dfm *.fmx'));
  WriteLine ('  --kylix                  ' + _('Adds the wildcards: *.pas *.inc *.rc *.dpr *.xfm'));
  WriteLine ('  -r                       ' + _('Recurse subdirectories'));
  WriteLine ('  -b dir                   ' + _('Use directory as base directory for filenames.'));
  writeLine ('                           ' + _('You can specify several base dirs, that will all be scanned.'));
  WriteLine ('  -o:msgid                 ' + _('Order by msgid'));
  WriteLine ('  -o:occ                   ' + _('Order by occurence (default)'));
  WriteLine ('  -o dir                   ' + _('Output directory for .po files'));
  WriteLine ('  --so filename            ' + _('Outputs default domain to specified file. No other files are written.'));
  WriteLine ('  -q                       ' + _('Quiet: Reduces output to absolute minimum.'));
  WriteLine ('  --useignorepo            ' + _('Use ignore.po file from the base dir'));
  WriteLine ('  --updateignore           ' + _('Add likely ignores to ignore.po file'));
  WriteLine ('  --preserveUserComments   ' + _('preserve user comments from existing template file'));
  WriteLine ('  --nonascii               ' + _('Allow non-ascii characters.'));
  {$ifdef MSWINDOWS}
  WriteLine ('  --codepage nnn           ' + _('Assume the specified codepage. Default is CP_ACP.'));
  {$endif}
  WriteLine ('  --nowc                   ' + _('Assume wildcards to be part of filenames'));
  WriteLine ('  --ignore-constreplace    ' + _('Suppresses warnings about CRLF'));
  WriteLine ('  --detectwarnings         ' + _('Warnings will produce a nonzero exit code'));
  WriteLine ('  --wrap=<number>          ' + _('Wrap lines at <number> (default: 70, min: 10)'));
  WriteLine ('  --no-wrap                ' + _('do not wrap lines'));
  WriteLine ('');
  WriteLine (_('If a filename is preceded with @, it is assumed to contain a list of'+sLineBreak+
               'filenames or file masks.'));
end;

procedure TConsoleApp.WriteLine(const msg: string);
begin
  consoleoutput.Writeln (msg);
end;

procedure TConsoleApp.WriteProgramHeader;
begin
  WriteLine (Format(_('dxgettext %s'),[version]));
  ProgramHeaderWritten:=True;
end;

end.
