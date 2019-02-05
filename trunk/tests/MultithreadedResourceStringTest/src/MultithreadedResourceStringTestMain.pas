unit MultithreadedResourceStringTestMain;

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
    FHello: UnicodeString;
  protected
    procedure Execute; override;
  public
    FCounter: Integer;
    constructor Create(const _Hello: UnicodeString);
  end;

procedure RunTest(const _Locale: string; _Hello: UnicodeString);
var
  Threads: array of TTestLanguageThread;
  i: Integer;
begin
  WriteLn('Testing ', _Locale);
  UseLanguage(_Locale);

  Setlength(Threads, 5);
  for i := Low(Threads) to High(Threads) do begin
    Threads[i] := TTestLanguageThread.Create(_Hello);
  end;

  try
    Write('Threads are running, press <enter>');
    Readln;

    for i := Low(Threads) to High(Threads) do begin
      Threads[i].Terminate;
      Threads[i].WaitFor;
    end;

    for i := Low(Threads) to High(Threads) do begin
      WriteLn('Thread ', i, ': ', Threads[i].FCounter);
    end;
  finally
    for i := Low(Threads) to High(Threads) do begin
      Threads[i].free;
    end;
  end;
end;

procedure Main;
begin
  AddDomainForResourceString('bla');
  RunTest('DE', 'Hallo');
  RunTest('FR', '''allo');
  RunTest('EN', 'Hello');
  Write('Press <enter> to exit');
  Readln;
end;

resourcestring
  sHello = 'Hello';
  sWorld = 'World';

{ TTestLanguageThread }

constructor TTestLanguageThread.Create(const _Hello: UnicodeString);
begin
  FHello := _Hello;
  inherited Create(False);
end;

procedure TTestLanguageThread.Execute;
var
  s: UnicodeString;
begin
  inherited;

  FCounter := 0;

  while not Terminated do begin
    s := sHello;
    if s <> FHello then
      raise exception.CreateFmt('Hello was translated wrongly. Expected %s but got %s.', [FHello, s]);

    s := sWorld;
    if s <> 'World' then
      raise exception.Create('Where did the translation for World come from?');

    Inc(FCounter);
  end;
end;

end.
