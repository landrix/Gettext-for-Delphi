unit consoleapp;

interface

uses
  msgimportengine;
  
type
  TConsoleApp=
    class
    private
      engine:Tmsgimportengine;
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
  param,uparam:string;
begin
  i:=1;
  while i<=paramcount do begin
    param:=paramstr(i);
    uparam:=uppercase(param);
    if uparam='-O' then begin
      inc (i);
      engine.outputfilename:=ExpandFileName(paramstr(i));
    end else begin
      engine.inputfilename:=ExpandFileName(param);
    end;
    inc (i);
  end;
end;

constructor TConsoleApp.Create;
begin
  engine:=TMsgImportEngine.Create;
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
  writeln (Format(_('msgimport %s'),(.version.)));
  writeln;
  writeln (_('Usage:'));
  writeln (_('  msgimport textfile.txt -o output.po'));
  writeln;
  writeln (_('This will generate a po file from an ascii tabulated file that'+sLineBreak+
             'contains two columns. The first column becomes msgid, and the'+sLineBreak+
             'second column becomes the translation. The text file should use'+sLineBreak+
             'utf-8 as character set. It should be fairly easy to create such'+sLineBreak+
             'a file from all kinds of tabulated file formats by saving it from'+sLineBreak+
             'a spreadsheet and then convert it to utf-8 using a text editor'+sLineBreak+
             'that is capable of reading and writing several character sets.'));
end;

end.
