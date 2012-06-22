unit u_TranslatorEngine;

interface

uses
  Classes,
  u_Languages;

type
  TTranslatorEngine = class
  public
  {(*}
    type
      TOnTranslated = procedure(_Sender: TObject; const _Orig, _Translation: string);
    type
      TTranslateResult = (trOK, trRetryLater, trFailed);
  {*)}
  private
    FOnTranslated: TOnTranslated;
    FLastRequest: string;
  protected
    procedure doOnTranslated(const _Orig, _Trans: string);
    function doTranslate(const _Orig: string; out _Translation: string): TTranslateResult; virtual; abstract;
  public
    ///<summary>By default, all engines allow batch translations but some might not. Those
    ///         should override this function. </summary>
    function AllowsBatchTranslation: boolean; virtual;
    function GetDescription: string; virtual; abstract;
    function TargetLanguage: TdxLanguage; virtual; abstract;
    function Translate(const _Orig: string; out _Translation: string): TTranslateResult;
    function BatchTranslate(_Orig, _Trans: TStrings): TTranslateResult; virtual;
    function GetLastTranslation(out _s: string): boolean;
    procedure AsyncTranslate(const _Orig: string); virtual;
    property OnTranslated: TOnTranslated read FOnTranslated write FOnTranslated;
  end;

implementation

{ TTranslatorEngine }

function TTranslatorEngine.AllowsBatchTranslation: boolean;
begin
  Result := True;
end;

procedure TTranslatorEngine.AsyncTranslate(const _Orig: string);
var
  Trans: string;
begin
  if Translate(_Orig, Trans) = trOK then
    doOnTranslated(_Orig, Trans);
end;

procedure TTranslatorEngine.doOnTranslated(const _Orig, _Trans: string);
begin
  if Assigned(FOnTranslated) then
    FOnTranslated(Self, _Orig, _Trans);
end;

function TTranslatorEngine.Translate(const _Orig: string;
  out _Translation: string): TTranslateResult;
begin
  FLastRequest := _Orig;
  Result := doTranslate(_Orig, _Translation);
end;

function TTranslatorEngine.GetLastTranslation(out _s: string): boolean;
begin
  Result := (FlastRequest <> '') and (Translate(FLastRequest, _s) = trOK);
end;

function TTranslatorEngine.BatchTranslate(_Orig, _Trans: TStrings): TTranslateResult;
var
  i: Integer;
  s: string;
begin
  Result := trOK;
  _Trans.Clear;
  for i := 0 to _Orig.Count - 1 do begin
    if Translate(_Orig[i], s) = trOK then begin
      _Trans.Add(s);
    end else begin
      _Trans.Add('');
      Result := trRetryLater;
    end;
  end;
end;

end.

