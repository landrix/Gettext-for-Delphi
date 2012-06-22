unit consoleapp;

interface

uses
  assembleengine;
  
type
  TConsoleApp=
    class
    private
      engine:Tassembleengine;
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
  gnugettext, appconsts, consoleoutput;

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
    if uparam='--DXGETTEXT' then begin
      engine.SetGnuGettextPatchCode;
      engine.FileMask:='*.mo';
    end else
      if engine.exefilename<>'' then
        raise Exception.Create (_('Already specified an .exe file.'))
      else
        engine.exefilename:=ExpandFileName(param);
    inc (i);
  end;
end;

constructor TConsoleApp.Create;
begin
  engine:=Tassembleengine.Create;
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
  writeln (Format(_('assemble %s'),(.version.)));
  writeln ('');
  writeln (_('assemble usage:'));
  writeln (_('  assemble application.exe --dxgettext'));
  writeln ('');
  writeln (_('This will append all files in the directory of application.exe and in '+sLineBreak+
             'all subdirectories to the file application.exe. This will enable the '+sLineBreak+
             'gnugettext.pas unit that was compiled into application.exe to get '+sLineBreak+
             'its translations from the .exe file instead of from external files.'));
end;

end.
