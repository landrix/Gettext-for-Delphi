unit GoogleTranslateBackground deprecated;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl                            *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dxgettext.po.dk/ for more information            *)
(*                                                              *)
(****************************************************************)

interface

uses
  Classes;

type
  TBackgroundTranslator=
    class
    private
      engine:TThread;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Translate (ss, _FromLng, _ToLng: string); // Each call will cancel the previous call
      function TranslationFinished:boolean;
      function GetLastTranslation:string;
    end;

implementation

uses
  GoogleTranslate, SysUtils, SyncObjs;

type
  TBackgroundTranslatorThread=
    class (TThread)
      LockTranslation:TMultiReadExclusiveWriteSynchronizer;
      FromLanguage,
      ToLanguage,
      original,
      translation:string;
      Event:TEvent;
      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;
      function Request(const _Orig, _FromLng, _ToLng: string): boolean;
    end;

{ TBackgroundTranslatorThread }

constructor TBackgroundTranslatorThread.Create;
begin
  inherited Create (False);
  LockTranslation:=TMultiReadExclusiveWriteSynchronizer.Create;
  Event:=TEvent.Create;
end;

destructor TBackgroundTranslatorThread.Destroy;
begin
  FreeAndNil (Event);
  FreeAndNil (LockTranslation);
  inherited;
end;

procedure TBackgroundTranslatorThread.Execute;
var
  FromLng, ToLng: string;
  org,trans:string;
begin
  inherited;
  while not Terminated do begin
    while not Terminated do begin
      LockTranslation.BeginRead;
      try
        if (Original<>'') and ((org<>Original) or (FromLng<>FromLanguage) or (ToLng <> ToLanguage)) then begin
          org:=Original;
          FromLng := FromLanguage;
          ToLng := ToLanguage;
          break;
        end;
      finally
        LockTranslation.EndRead;
      end;
      Event.WaitFor(100);
      Event.ResetEvent;
    end;

    if Terminated then
      break;

    try
      trans:=TranslateViaGoogle(org, FromLng, ToLng);
    except
      // Default action on errors is not to translate.
      trans:='';
    end;

    if Terminated then
      break;

    LockTranslation.BeginWrite;
    try
      if org=Original then
        translation:=trans;
    finally
      LockTranslation.EndWrite;
    end;
  end;
end;

function TBackgroundTranslatorThread.Request(const _Orig, _FromLng, _ToLng: string): boolean;
begin
  LockTranslation.BeginWrite;
  try
    if (FromLanguage <> _FromLng) or (ToLanguage <> _ToLng) or (original <> _Orig) then begin
      FromLanguage := _FromLng;
      ToLanguage := _ToLng;
      original := _Orig;
      translation := '';

      UniqueString(FromLanguage);
      UniqueString(ToLanguage);
      UniqueString(original);
      UniqueString(translation);
    end;
  finally
    LockTranslation.EndWrite;
  end;
  Event.SetEvent;
end;

{ TBackgroundTranslator }

constructor TBackgroundTranslator.Create;
var
  thr:TBackgroundTranslatorThread;
begin
  thr:=TBackgroundTranslatorThread.Create;
  engine:=thr;
end;

destructor TBackgroundTranslator.Destroy;
begin
  (engine as TBackgroundTranslatorThread).Event.SetEvent;
  engine.Terminate;
  engine.WaitFor;
  FreeAndNil (engine);
end;

function TBackgroundTranslator.GetLastTranslation: string;
var
  thr:TBackgroundTranslatorThread;
begin
  thr:=Engine as TBackgroundTranslatorThread;
  thr.LockTranslation.BeginRead;
  try
    Result:=thr.translation;
  finally
    thr.LockTranslation.EndRead;
  end;
end;

procedure TBackgroundTranslator.Translate (ss, _FromLng, _ToLng: string);
var
  thr:TBackgroundTranslatorThread;
begin
  thr:=engine as TBackgroundTranslatorThread;
  thr.Request(ss, _FromLng, _ToLng);
  inherited;
end;

function TBackgroundTranslator.TranslationFinished: boolean;
begin
  raise Exception.Create('Not implemented');
end;

end.