{$APPTYPE CONSOLE}
program msgmergePOT;
// This tool takes the name of an .mo file as parameter and outputs
// the header, i.e. _(''), to stdout.

uses
  SysUtils,
  {$ifdef MSWINDOWS}
  gnugettext in '..\..\sample\gnugettext.pas',
  appconsts in '..\..\dxgettext\appconsts.pas',
  consoleoutput in '..\..\dxgettext\consoleoutput.pas',
  {$else}
  gnugettext in '../../sample/gnugettext.pas',
  appconsts in '../../dxgettext/appconsts.pas',
  consoleoutput in '../../dxgettext/consoleoutput.pas',
  {$endif}
  Classes;

{ Main routine }

var
  gg:TGnuGettextInstance;
begin
  textdomain('dxgettext');
  AddDomainForResourceString('delphi');
  AddDomainForResourceString('kylix');

  try
    if paramcount<>1 then begin
      // Messages about how to use this program
      writeln (Format('msgshowheader %s',(.version.)));
      writeln;
      writeln (_('msgshowheader usage:'));
      writeln (_('  msgshowheader translation.mo'));
      writeln;
      writeln (_('This will output the header of the translation to stdout, i.e. _('''').'));
      writeln;
      exit;
    end;
    gg:=TGnuGettextInstance.Create;
    try
      gg.bindtextdomainToFile('singlefile',paramstr(1));
      gg.textdomain('singlefile');
      write (gg.gettext(''));
    finally
      FreeAndNil (gg);
    end;
  except
    on e:Exception do begin
      writeln (_('Exception occured'));
      writeln (e.message);
      ExitCode:=1;
    end;
  end;
end.
