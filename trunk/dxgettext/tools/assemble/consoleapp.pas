unit consoleapp;

interface

uses
  sysutils,
  assembleengine;

type
  TConsoleApp = class
  private
    engine: Tassembleengine;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AnalyzeCommandline;
    procedure Execute;
    procedure WriteHelp;
  end;

implementation

uses
  appconsts,
  consoleoutput,
  gnugettext;

{ TConsoleApp }

procedure TConsoleApp.AnalyzeCommandline;

  procedure GetOption(_inparam: string; out _param, _option: string);
  var
    gl: Integer;
  begin
    gl := Pos('=', _inparam);
    if (gl = 0) or (Copy(_inparam, 1, 2) <> '--') then begin
      _option := '';
      _param := _inparam;
    end else begin
      _param := Copy(_inparam, 1, gl - 1);
      _option := Copy(_inparam, gl + 1);
      _option := AnsiDequotedStr(_option, '"');
    end;
  end;

var
  i: integer;
  param: string;
  pa, op: string;
begin
  i := 1;
  while i <= paramcount do begin
    param := paramstr(i);
//    consoleoutput.WriteLn('param: "' + Param + '"');
    GetOption(param, pa, op);
//    WriteLn('pa: "' + pa + '"' + ' op: "' + op + '"');

    if SameText('--DXGETTEXT', pa) then begin
      engine.SetGnuGettextPatchCode;
      engine.FileMask := '*.mo';
    end else if SameText('--LOCALEBASE', pa) then begin
      if engine.basedirectory <> '' then
        raise Exception.CreateFmt(_('Already specified locale base dir (new: "%s".'), [op]);
      engine.basedirectory := op;
      consoleoutput.WriteLn(Format(_('Setting basedirectory to "%s"'), [engine.basedirectory]));
    end else if Copy(pa, 1, 2) = '--' then begin
      raise Exception.CreateFmt(_('Unknown option "%s"'), [pa]);
    end else begin
      // There is only one parameter that is not an option, the exe filename
      if engine.exefilename <> '' then
        raise Exception.CreateFmt(_('Already specified an .exe file (new: "%s".'), [param]);
      engine.exefilename := ExpandFileName(AnsiDequotedStr(param, '"'));
      consoleoutput.WriteLn(Format(_('Setting exefilename to "%s"'), [engine.exefilename]));
    end;
    inc(i);
  end;
end;

constructor TConsoleApp.Create;
begin
  engine := Tassembleengine.Create;
end;

destructor TConsoleApp.Destroy;
begin
  FreeAndNil(engine);
  inherited;
end;

procedure TConsoleApp.Execute;
begin
  if (paramcount = 0) or ((paramcount = 1) and (uppercase(paramstr(1)) = '--HELP')) then
    WriteHelp
  else begin
    AnalyzeCommandline;
    engine.Execute;
  end;
end;

procedure TConsoleApp.WriteHelp;
begin
  consoleoutput.writeln(Format(_('assemble %s'), [appconsts.version]));
  consoleoutput.writeln('');
  consoleoutput.writeln(_('assemble usage:'));
  consoleoutput.writeln(_('  assemble "application.exe" --localebase="C:\program\" --dxgettext'));
  consoleoutput.writeln('');
  consoleoutput.writeln(_('This will append all .mo files in the directory of application.exe and in ' + sLineBreak +
    'all subdirectories to the file application.exe. This will enable the ' + sLineBreak +
    'gnugettext.pas unit that was compiled into application.exe to get ' + sLineBreak +
    'its translations from the .exe file instead of from external files.' + sLineBreak +
    '--localebase sets the base directory for searching for .mo files.' + sLineBreak +
    '  The default is the directory containing application.exe.'));
end;

end.

