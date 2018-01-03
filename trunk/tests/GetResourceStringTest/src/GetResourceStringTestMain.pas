unit GetResourceStringTestMain;

interface

procedure Main;

implementation

uses
  gnugettext;

procedure Main;
resourcestring
  sMessage = 'Hello';
var
  English, German, French: TGnuGettextInstance;
begin
  AddDomainForResourceString('bla');

  German := nil;
  French := nil;
  try
    English := TGnuGettextInstance.Create();
    English.UseLanguage('EN');

    German := TGnuGettextInstance.Create();
    German.UseLanguage('DE');

    French := TGnuGettextInstance.Create();
    French.UseLanguage('FR');

    Write('English: ');
    WriteLn(English.LoadResString(@sMessage));

    Write('German: ');
    WriteLn(German.LoadResString(@sMessage));

    Write('French: ');
    WriteLn(French.LoadResString(@sMessage));

    readln;
  finally
    German.Free();
    French.Free();
  end;
end;

end.
