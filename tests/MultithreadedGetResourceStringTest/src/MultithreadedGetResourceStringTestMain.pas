unit MultithreadedGetResourceStringTestMain;

interface

procedure Main;

implementation

uses
  SysUtils,
  Classes,
  gnugettext;

type
  TTestLanguageThread = class(TThread)
  private
    FLocaleName: string;
  protected
    procedure CheckHello(const _s: UnicodeString); virtual; abstract;
    procedure Execute; override;
  public
    FCounter: Integer;
  end;

type
  TTestEnglishThread = class(TTestLanguageThread)
  protected
    procedure CheckHello(const _s: UnicodeString); override;
  public
    constructor Create;
  end;

type
  TTestGermanThread = class(TTestLanguageThread)
  protected
    procedure CheckHello(const _s: UnicodeString); override;
  public
    constructor Create;
  end;

type
  TTestFrenchThread = class(TTestLanguageThread)
  protected
    procedure CheckHello(const _s: UnicodeString); override;
  public
    constructor Create;
  end;

procedure Main;
var
  English, German, French: TTestLanguageThread;
begin
  AddDomainForResourceString('bla');

  German := nil;
  French := nil;
  English := TTestEnglishThread.Create;
  try
    German := TTestGermanThread.Create;
    French := TTestFrenchThread.Create;

    Write('Threads are running, press <enter>');
    Readln;

    French.Terminate;
    German.Terminate;
    English.Terminate;

    French.WaitFor;
    German.WaitFor;
    English.WaitFor;

    WriteLn('English: ', English.FCounter);
    WriteLn('German: ', German.FCounter);
    WriteLn('French: ', French.FCounter);
    Write('Press <enter> to exit');
    ReadLn;
  finally
    French.Free();
    German.Free();
    English.Free();
  end;
end;

resourcestring
  sHello = 'Hello';
  sWorld = 'World';

{ TTestLanguageThread }

procedure TTestLanguageThread.Execute;
var
  Lang: TGnuGettextInstance;
  s: UnicodeString;
begin
  inherited;

  FCounter := 0;

  Lang := TGnuGettextInstance.Create();
  try
    Lang.UseLanguage(FLocaleName);

    while not Terminated do begin
      s := Lang.LoadResString(@sHello);
      CheckHello(s);
      s := Lang.LoadResString(@sWorld);
      if s <> 'World' then
        raise exception.Create('Where did the translation for World come from?');
      Inc(FCounter);
    end;

  finally
    FreeAndNil(Lang);
  end;
end;

{ TTestEnglishThread }

procedure TTestEnglishThread.CheckHello(const _s: UnicodeString);
begin
  if _s <> 'Hello' then
    raise exception.Create('Hello was translated wrongly to English.');
end;

constructor TTestEnglishThread.Create;
begin
  FLocaleName := 'EN';
  inherited Create(False);
end;

{ TTestGermanThread }

procedure TTestGermanThread.CheckHello(const _s: UnicodeString);
begin
  if _s <> 'Hallo' then
    raise exception.Create('Hello was translated wrongly to German.');
end;

constructor TTestGermanThread.Create;
begin
  FLocaleName := 'DE';
  inherited Create(False);
end;

{ TTestFrenchThread }

procedure TTestFrenchThread.CheckHello(const _s: UnicodeString);
begin
  if _s <> '''allo' then
    raise exception.Create('Hello was translated wrongly to French.');
end;

constructor TTestFrenchThread.Create;
begin
  FLocaleName := 'FR';
  inherited Create(False);
end;

end.
