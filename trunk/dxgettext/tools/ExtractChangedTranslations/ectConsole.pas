unit ectConsole;

interface

uses
  SysUtils,
  gnugettext,
  appconsts,
  msgmergedxengine;

type
  TConsoleApplication = class(Tobject)
    private
      FEngine: TMsgMergeDxEngine;
    public
      constructor Create;
      destructor Destroy; override;

      procedure AnalyzeCommandline;
      procedure Execute;
      procedure WriteHelp;
    end;

implementation


procedure TConsoleApplication.AnalyzeCommandline;
var
  i:integer;
  lParam, lUParam: string;
begin
  i := 1;

  while i <= ParamCount do
  begin
    lParam  := ParamStr(i);
    lUParam := UpperCase(lParam);

    if lUParam = '-O' then
    begin
      inc (i);
      FEngine.OutputFileName := ExpandFileName(ParamStr(i));
    end
    else if (lUParam = '--IGNORECOMMENTS') then
    begin
      FEngine.IgnoreComments := True;
    end
    else
    begin
      if FEngine.TranslationFileName = '' then
      begin
        FEngine.TranslationFileName := ExpandFileName(lParam);
      end
      else
      begin
        FEngine.TemplateFileName := ExpandFileName(lParam);
      end;
    end;

    inc (i);
  end;
end;

constructor TConsoleApplication.Create;
begin
  FEngine := TMsgMergeDxEngine.Create;
end;

destructor TConsoleApplication.Destroy;
begin
  FreeAndNil (FEngine);
  inherited;
end;

procedure TConsoleApplication.Execute;
begin
  if (ParamCount = 0) or
     ((ParamCount = 1) and (UpperCase(ParamStr(1))='--HELP')) then
  begin
    WriteHelp;
  end
  else
  begin
    AnalyzeCommandline;

    FEngine.OnlyNewAndChangedTranslations := True;
    FEngine.Execute;
  end;
end;

procedure TConsoleApplication.WriteHelp;
begin
  writeln (Format(_('extractChangedTranslations %s'), (.version.)));
  writeln;
  writeln (_('Usage:'));
  writeln (_('  extractChangedTranslations -o output.po oldTranslations.po newTranslations.po'));
  writeln;
  // Do not make this more than 70 characters wide, please.
  writeln (_('Extract only new translated or changed strings from two uniform ' + sLineBreak +
             'translation files.'));
  writeln ('');
  writeln (_('Options:'));
  writeln ('  --IgnoreComments   ' + _('ignore different comments'));
end;

end.
