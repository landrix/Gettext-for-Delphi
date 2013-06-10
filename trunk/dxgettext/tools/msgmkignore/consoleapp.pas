unit consoleapp;

interface

uses
  msgmkignoreengine;
  
type
  TConsoleApp=
    class
    private
      engine:Tmsgmkignoreengine;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AnalyzeCommandline;
      procedure Execute;
      procedure WriteHelp;
    end;



implementation

uses
  SysUtils,
  gnugettext, consoleoutput,
  appconsts;

{ TConsoleApp }

procedure TConsoleApp.AnalyzeCommandline;
var
  i: integer;
  lParam, lUParam: string;
begin
  i := 1;

  while i <= ParamCount do
  begin
    lParam  := ParamStr(i);
    lUParam := UpperCase(lParam);

    if (lUParam = '-O') then
    begin
      inc (i);
      engine.outputfilename := ExpandFileName( paramstr(i));
    end
    else if (lUParam = '--BLACKLIST') then
    begin
      inc (i);
      engine.FileNameBlackList := ExpandFileName( paramstr(i));
    end
    else if (lUParam = '--WHITELIST') then
    begin
      inc (i);
      engine.FileNameWhiteList := ExpandFileName( paramstr(i));
    end
    else
    begin
      engine.inputfilename := ExpandFileName( lParam);
    end;

    inc (i);
  end;
end;

constructor TConsoleApp.Create;
begin
  engine:=Tmsgmkignoreengine.Create;
end;

destructor TConsoleApp.Destroy;
begin
  FreeAndNil (engine);
  inherited;
end;

procedure TConsoleApp.Execute;
begin
  if (paramcount=0) or ((paramcount=1) and (uppercase(paramstr(1))='--HELP')) then
    WriteHelp
  else begin
    AnalyzeCommandline;
    engine.Execute;
  end;
end;

procedure TConsoleApp.WriteHelp;
begin
  writeln (Format(_('msgmkignore %s'),(.version.)));
  writeln;
  writeln (_('Usage:'));
  writeln (_('  msgmkignore default.po -o ignore.po'));
  writeln;
  writeln (_('This will extract texts from default.po that this program'+sLineBreak+
             'thinks should not be translated by a translator'));
  writeln;
  writeln (_('Options:'));
  writeln ('  --BlackList   ' + _('.po file with Text always added to ignore'));
  writeln ('  --WhiteList   ' + _('.po file with Text never added to ignore'));

end;

end.
