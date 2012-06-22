program ggfmt;

uses
  gnugettext in '..\..\sample\gnugettext.pas',
  Classes,
  Forms,
  SysUtils,
  Windows,
  uOutput in 'uOutput.pas' {FormOutput},
  appconsts in '..\..\dxgettext\appconsts.pas',
  assembleengine in '..\assemble\assembleengine.pas',
  consoleoutput in '..\..\dxgettext\consoleoutput.pas',
  ConsoleAppHandler in '..\..\dxgettext\ConsoleAppHandler.pas';

{$R *.res}

function shellescape (s:string):string;
var
  i:integer;
begin
  Result:='';
  for i:=1 to length(s) do begin
    if s[i]='"' then
      Result:=Result+'\"'
    else
    if s[i]='\' then
      Result:=Result+'\\'
    else
      Result:=Result+s[i];
  end;
end;

var
  AppOutput:TStringList;
  temppath,tempfilename:string;
  len:DWORD;
  cmdline:string;
  filename:string;
  apppath:string;
  programname:string;
  outtext:string;
  outputext:string;
begin
  TextDomain ('dxgettext');
  AddDomainForResourceString('delphi');

  apppath:=extractfilepath(paramstr(0));
  chdir (apppath);

  SetLength (temppath, 1000);
  len:=GetTempPath (1000,PChar(temppath));
  SetLength (temppath, len);
  tempfilename:=IncludeTrailingPathDelimiter(temppath)+'msgfmt-'+IntToStr(Application.Handle)+'.$$$';

  programname:=paramstr(1);
  filename:=ExpandFileName(paramstr(2));
  if paramcount=3 then outputext:=paramstr(3) else outputext:='';

  Application.Initialize;
  Application.CreateForm(TFormOutput, FormOutput);
  AppOutput:=TStringlist.Create;
  try
    cmdline:='"'+programname+'" "'+shellescape(filename)+'"';
    if outputext<>'' then
      cmdline:=cmdline+' -o "'+shellescape(changefileext(filename,outputext))+'"';
    cmdline:=cmdline+' 2>&1';
    cmdline:='-c "'+shellescape(cmdline)+'"';
    ExecConsoleApp('bash.exe',cmdline,AppOutput,nil);
    outtext:=AdjustLineBreaks(AppOutput.Text);
  finally
    FreeAndNil (AppOutput);
  end;
  if trim(outtext)='' then begin
    exit;
  end else begin
    // First %s will be replaced by version number, second by the name of an .exe file that has been run
    FormOutput.Caption:=Format(_('ggfmt %s running %s'),[version,programname]);
    FormOutput.MemoOutput.Lines.Text:=outtext;
    FormOutput.MemoOutput.Lines.Insert(0,Format(_('Result of running %s:'),[programname]));
  end;
  Application.Run;
end.
