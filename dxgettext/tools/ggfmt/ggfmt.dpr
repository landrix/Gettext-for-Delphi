program ggfmt;

uses
  gnugettext in '..\..\sample\gnugettext.pas',
  Classes,
  Forms,
  SysUtils,
  StrUtils,
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
  lAppOutput: TStringList;
  lTempPath,
  lTempFileName:string;
  lLen: DWORD;
  lCmdLine: string;
  lFileName: string;
  lAppPath: string;
  lProgramName: string;
  lOutText: string;
  lFileExtension: string;
begin
  TextDomain( 'dxgettext');
  AddDomainForResourceString( 'delphi');

  lAppPath := ExtractFilePath( paramstr(0));
  chdir( lAppPath);

  SetLength( lTempPath, 1000);
  lLen := GetTempPath( 1000, PChar( lTempPath));
  SetLength( lTempPath, lLen);
  lTempFileName := IncludeTrailingPathDelimiter( lTempPath) + 'msgfmt-' + IntToStr( Application.Handle) + '.$$$';

  lProgramName := ParamStr(1);
  lFileName    := ExpandFileName( ParamStr(2));
  if (ParamCount = 3) then
  begin
    lFileExtension := ParamStr(3);
  end
  else
  begin
    lFileExtension := '';
  end;

  Application.Initialize;
  Application.CreateForm( TFormOutput, FormOutput);

  lAppOutput := TStringlist.Create;
  try
    lCmdLine := '"' + lFileName + '"';
    if (lFileExtension <> '') then
    begin
      lCmdLine := lCmdLine + ' -o "' + ChangeFileExt( lFileName,
                                                      lFileExtension) + '"';
    end;
    //lCmdLine := lCmdLine + ' 2>&1';

    ExecConsoleApp( lProgramName,
                    lCmdLine,
                    lAppOutput,
                    nil);

    begin
      lOutText := Trim( AdjustLineBreaks( lAppOutput.Text));
    end;
  finally
    FreeAndNil (lAppOutput);
  end;

  if ( ( lOutText <> '') and
       not AnsiStartsText( 'Execution succes',
                           lOutText)) then
  begin
    // First %s will be replaced by version number, second by the name of an .exe file that has been run
    FormOutput.Caption := Format( _('ggfmt %s running %s'),
                                  [ version,
                                    lProgramName]);
    FormOutput.MemoOutput.Lines.Append( Format( _('Result of running %s:'),
                                                [ lProgramName]));
    FormOutput.MemoOutput.Lines.Append( Format( _('Parameter: %s'),
                                                [ lCmdLine]));
    FormOutput.MemoOutput.Lines.Text := lOutText;
  end
  else
  begin
    exit;
  end;

  Application.Run;
end.
