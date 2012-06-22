///<summary> This is an extract from u_dzMicrosoftTranslate which is part of dzlib available from berlios
///          under the MPL 1.1 </summary>
unit u_dzMicrosoftTranslate;

interface

uses
  SysUtils,
  IdHttp; // Indy

type
  ///<summary> This is a very simple implementation of the Microsoft translation V2 API.
  ///          Error handling is nearly non-existent. </summary>
  TMicrosoftTranslate = class
  private
    FReferrer: string;
    FUserAgent: string;
    FSourceLang: string;
    FTargetLang: string;
    FAppId: string;
  public
    ///<summary>
    /// @param AppId is the 16 character AppId you got from Microsoft (it's free, you just have
    ///              to create a Windows Live account and apply for it)
    /// @param SourceLang is the source language code, e.g. 'en', see the API description
    ///                   for a list of possible values.
    /// @param TargetLang is the target language code, e.g. 'de', see the API description
    ///                   for a list of possible values. </summary>
    constructor Create(const _AppId: string; const _SourceLang, _TargetLang: string);
//    ///<summary>Display this branding near the input and result of translations. </summary>
    function GetBranding: string;
    function Translate(const _Input: string): string;
    ///<summary> Referrer is the url to the website using the translation service </summary>
    property Referrer: string read FReferrer write FReferrer;
    ///<summary> UserAgent is the http user agent, e.g. your application's name, defaults
    ///                    to 'TMicrosoftTranslate'. </summary>
    property UserAgent: string read FUserAgent write FUserAgent;
  end;

implementation

uses
  Classes,
  Character,
  HttpApp,
  gnugettext,
  u_dzStringUtils;

type
  TResponse = record
  private
    FData: string;
    FDetails: string;
    FStatus: integer;
  public
    procedure Init(const _Answer: string);
  end;

{ TMicrosoftTranslate }

constructor TMicrosoftTranslate.Create(const _AppId: string; const _SourceLang, _TargetLang: string);
begin
  inherited Create;
  FReferrer := '';
  FUserAgent := 'TMicrosoftTranslate';
  FSourceLang := _SourceLang;
  FTargetLang := _TargetLang;
  FAppId := _AppId;
end;

function URLEncode(const _s: string): string;
const
  NoConversion = ['A'..'Z', 'a'..'z', '_', '-'];
begin
  Result := UrlEncodeControlChars(_s, [#0..#255] - NoConversion);
end; // URLEncode

function ConvertToCodePage(s: string; cp: Integer): string;
var
  data: TStringStream;
  Size: Integer;
  Buffer: TBytes;
  encode: TEncoding;
begin
  if s = '' then begin
    Result := '';
    Exit;
  end;

  data := TStringStream.Create();
  try
    data.WriteString(s);
    data.Seek(0, soFromBeginning);

    Size := data.Size - data.Position;
    SetLength(Buffer, Size);
    data.Read(Buffer[0], Size);

    encode := TEncoding.GetEncoding(cp);
    Size := TEncoding.GetBufferEncoding(Buffer, encode);
    Result := encode.GetString(Buffer, Size, Length(Buffer) - Size);
  finally
    FreeAndNil(data);
  end;
end;

function LookupCodepage(sc: string): Integer;
begin
  // Finds the codepage of each language, so that the data from Microsoft can be converted correctly
  sc := Uppercase(sc);

  if (sc = 'ZH-CN') then
    Result := 936
  else if (sc = 'ZH-TW') then
    Result := 950
  else if (sc = 'SV') then
    Result := 28592
  else if (sc = 'TH') then
    Result := 874
  else if (sc = 'RU') then
    Result := 20866
  else if (sc = 'JA') then
    Result := 932
  else if (sc = 'EL') then
    Result := 28597
  else if (sc = 'FA') then
    Result := 1256
  else if (sc = 'KO') then
    Result := 51949
  else if (sc = 'ID') then
    Result := 28591
  else if (sc = 'UK') then
    Result := 1251
  else
    Result := 28591;
end;

function TMicrosoftTranslate.GetBranding: string;
begin
  Result := 'powered by Microsoft Translation';
end;

function TMicrosoftTranslate.Translate(const _Input: string): string;
const
  url_template = 'http://api.microsofttranslator.com/V2/Http.svc/Translate?'
    + 'appId=%s' // this is your 16 character AppID from Microsoft
    + '&text=%s' // text to translate, url encoded
    + '&from=%s' // from language code, eg. de, en or fr
    + '&to=%s'; // to language code, e.g. de, en or fr
var
  url: string;
//  status: string;
  s: string;
  utfs: UTF8String;
  http: TidHttp;
  Response: TResponse;
  cp: Integer;
begin
  http := TidHttp.Create;
  try
    s := URLEncode(_Input);
    utfs := UTF8String(s);
    url := Format(url_template, [FAppId, string(utfs), FSourceLang, FTargetLang]);

    http.Request.Referer := FReferrer;
    http.Request.UserAgent := FUserAgent;
    s := http.Get(url);

    Response.Init(s);
    if Response.FStatus <> 0 then
      raise Exception.Create(Response.FDetails);
    cp := LookupCodepage(FTargetLang);
    Result := ConvertToCodePage(Response.FData, cp);
  finally
    http.Free;
  end;
end;

{ TResponse }

procedure TResponse.Init(const _Answer: string);
var
  s: string;
  p: Integer;
begin
  FData := '';
  FDetails := '';
  FStatus := 0;

  // An answer looks like this:
  // <string xmlns="http://schemas.microsoft.com/2003/10/Serialization/">this is the translation</string>
  p := Pos('>', _Answer);
  if p = 0 then begin
    FStatus := 1;
    FDetails := _('Cannot parse answer. No ">" found.');
    exit;
  end;
  s := Copy(_Answer, p + 1);
  p := Pos('</string>', s);
  if p = 0 then begin
    FStatus := 2;
    FDetails := _('Cannot parse answer. No "</string>" found.');
    exit;
  end;
  FData := Copy(s, 1, p - 1);
end;

end.

