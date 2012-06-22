unit msgmergedxconsole;

interface

uses
  msgmergedxengine;
  
type
  TConsoleApp=
    class
    private
      engine: Tmsgmergedxengine;
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
  i:integer;
  param, uparam: string;
begin
  i:=1;
  while i <= paramcount do
  begin
    param:=paramstr(i);
    uparam:=uppercase(param);

    if uparam='-O' then
    begin
      inc (i);
      engine.outputfilename := ExpandFileName(paramstr(i));
    end
    else if (uparam = '--PRESERVESTATEFUZZY') then
    begin
      engine.PreserveStateFuzzy := True;
    end
    else
    begin
      if engine.translationfilename = '' then
      begin
        engine.translationfilename := ExpandFileName(param);
      end
      else
      begin
        engine.templatefilename := ExpandFileName(param);
      end;
    end;
    inc (i);
  end;
end;

constructor TConsoleApp.Create;
begin
  engine:=TMsgMergeDxEngine.Create;
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
  writeln (Format(_('msgmergedx %s'),(.version.)));
  writeln;
  writeln (_('Usage:'));
  writeln (_('  msgmergedx -o output.po oldtranslations.po newtemplate.po'));
  writeln;
  // Do not make this more than 70 characters wide, please.
  writeln (_('This is a replacement for the GNU msgmerge utility. It requires' + sLineBreak +
             'all files to use utf-8 encoding and is extremely simple. The only' + sLineBreak +
             'reason to use this tool is because it allows non-ascii msgid values.' + sLineBreak +
             'Several features are not implemented, including generating fuzzy' + sLineBreak +
             'translations.'));
  writeln ('');
  writeln (_('Options:'));
  writeln ('  --preserveStateFuzzy   ' + _('preserve "fuzzy" state in the translated file'));

end;

end.
