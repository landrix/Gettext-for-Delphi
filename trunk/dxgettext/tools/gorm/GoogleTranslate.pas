unit GoogleTranslate;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl                            *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dybdahl.dk/dxgettext/ for more information       *)
(*                                                              *)
(****************************************************************)

interface

uses
  Classes;

type
  TTranslationRec = record
    Caption: string;
    GTString: string;
    GoogleAPIkey : String;
  end;
type
  TTranslationArr = array of TTranslationRec;

var
  TRANSLATION_ARR: TTranslationArr;

function TranslateViaGoogle (const ss, _FromLng, _ToLng: string):string; deprecated; // google translate no longer supports this
function TranslateListViaGoogle(lstIn,lstOut: TStringList; _FromLng, _ToLng: String) : Boolean; deprecated; // no longer works either

function GtCodeToLanguage(const _GtCode: string): string;

implementation

uses
  SysUtils, Windows, StrUtils, Menus, gnugettext,
  IdHTTP, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient,
  IdExplicitTLSClientServerBase,
  IdFTP, IdFTPCommon, idURI,
  poparser, IdSSL, IdSSLOpenSSL, AppSettings;

const
  // These are used for doing Google page-based translation
  GoogleTransURL = 'http://translate.google.com'; // with 'www' is also OK
  pageURL1       = 'translate?ie=UTF-8';          // Page translation: step 1
  pageURL2       = 'translate_p';                 // Page translation: step 2

  ftpHost        = '85.92.147.51';
  ftpDir         = './public_html/temp';
  httpHost       = '85.92.147.51/~gorm';
  httpDir        = 'temp';

  tagNoTranslate = '<span class="notranslate">';

  // How we enclose the strings that are to be translated in the HTML page
  // See pageCreateHTML
  sTextItemDiv = '<div id="str%d">';

function urlencode (s:string):string;
var
  i,j:integer;
  utf8:utf8string;
begin
  i:=1;
  while i<=length(s) do begin
    case s[i] of
      ' ':begin
            s[i]:='+';
            inc (i);
          end;
      'a'..'z','A'..'Z','0'..'9':
        inc (i);
    else
      utf8:=UTF8Encode(s[i]); // Convert to utf-8
      delete (s,i,1);
      for j:=1 to length(utf8) do begin
        insert ('%'+IntToHex(ord(utf8[j]),2),s,i);
        inc (i,3);
      end;
    end;
  end;
  Result:=s;
end;

{$ifndef UNICODE}  // < D2009
function UTF8ToUnicodeString(const aText: string): string;
begin
  result := Utf8ToAnsi(aText);
end;
{$endif}

function Before(Source,Target : String) : String;

var
  tp : Integer;

begin
  tp:=Pos(Target,Source);
  if tp=0 then
    Before:=Source
  else
    Before:=Copy(Source,1,tp-1);
end;

function After(Source,Target : String) : String;

var
  tp : Integer;

begin
  tp:=Pos(Target,Source);
  if tp=0 then
    After:=''
  else
    After:=Copy(Source,tp+Length(Target),Length(Source));
end;

function PrepareStr(const ps:string):string;

// Based on decodehtml (see below)

// Prepares a string for page-based translation by inserting
// 'notranslate' tag at certain places.
// Currently very simplistic, it handles only %s and %d

const
  sSpanFmt = tagNoTranslate+'%s</span>';

var
  s : utf8string;
  i : integer;

  procedure Repl(const a,b:RawByteString);
  begin
    if copy(s, i, length(a)) = a then begin
      delete(s, i, length(a));
      insert (b, s, i);
    end;
  end;

begin
  s:=UTF8Encode(ps);

  i:=1;
  while i<=length(s) do begin
    Repl('%d',RawByteString(Format(sSpanFmt,['~d'])));
    Repl('%s',RawByteString(Format(sSpanFmt,['~s'])));
    inc (i);
  end;

  i:=1;
  while i<=length(s) do begin
    Repl('~d','%d');
    Repl('~s','%s');
    inc (i);
  end;

  {$ifdef UNICODE}  // >=D2009
  Result := UTF8ToUnicodeString(s);
  {$else}
  Result:= utf8decode(s);
  {$endif}
end;

function decodehtml (const ps:string):string;
var
  s:utf8string;
  i,j,codepoint:integer;
  wc:widechar;
  utf8part:utf8string;
  procedure Repl(const a,b:RawByteString);
  begin
    if copy(s, i, length(a)) = a then begin
      delete(s, i, length(a));
      insert (b, s, i);
    end;
  end;
begin
  s:=UTF8Encode(ps);
  i:=1;
  while i<=length(s) do begin
    Repl('&quot;','"');
    Repl('&amp;','&');
    Repl('&lt;','<');
    Repl('&gt;','>');
    Repl('&aelig;','Ê');
    Repl('&oslash;','¯');
    Repl('&aring;','Â');
    if (copy(s,i,2)='&#') then begin
      j:=i+2;
      codepoint:=0;
      while j<=length(s) do
      begin
        {$ifdef UNICODE}  // >=D2009
        if not CharInSet(s[j],['0'..'9']) then
        {$else}
        if not (s[j] in ['0'..'9']) then
        {$endif}
          break;
        codepoint:=codepoint*10+ord(s[j])-48;
        inc (j);
      end;
      if (codepoint<>0) and (s[j]=';') then
      begin
        wc:=widechar(codepoint);
        utf8part:=UTF8Encode(wc);
        s:=copy(s,1,i-1)+utf8part+copy(s,j+1,maxint);
      end else
        i:=j;
    end;

    inc (i);
  end;

  {$ifdef UNICODE}  // >=D2009
  Result := UTF8ToUnicodeString(s);
  {$else}
  Result:= utf8decode(s);
  {$endif}
end;

function ConvertToCodePage(s : String; cp : Integer) : String;
var
  data  : TStringStream;
  Size  : Integer;
  Buffer: TBytes;
  encode: TEncoding;

begin
  if s='' then begin
    Result:='';
    Exit;
  end;

  data:=TStringStream.Create();
  try
    data.WriteString(s);
    data.Seek(0,soFromBeginning);

    Size:=data.Size-data.Position;
    SetLength(Buffer,Size);
    data.Read(Buffer[0],Size);

    encode:=TEncoding.GetEncoding(cp);
    Size  :=TEncoding.GetBufferEncoding(Buffer,encode);
    Result:=encode.GetString(Buffer, Size, Length(Buffer) - Size);
  finally
    FreeAndNil (data);
  end;
end;

function LookupCodepage(sc : String) : Integer;
begin
  // Finds the codepage of each language, so that the data from Google can be converted correctly
  sc:=Uppercase(sc);

  if (sc='ZH-CN') then Result:=936 else
  if (sc='ZH-TW') then Result:=950 else
  if (sc='SV') then Result:=28592 else
  if (sc='TH') then Result:=874 else
  if (sc='RU') then Result:=20866 else
  if (sc='JA') then Result:=932 else
  if (sc='EL') then Result:=28597 else
  if (sc='FA') then Result:=1256 else
  if (sc='KO') then Result:=51949 else
  if (sc='ID') then Result:=28591 else
  if (sc='UK') then Result:=1251 else
    Result:=28591;
end;

function TranslateViaGoogle (const ss, _FromLng, _ToLng:string):string;

// ss is text to be translated
// _FromLng is the the code of the original language (usually 'en' for English)
// ToLng is the code of the destination language (e.g 'fr' for French, 'de' for German)

//const
//  URL_TEMPLATE = 'http://translate.google.com/#%s|%s|%s';
//var
//  s,sc:string;
//  url:string;
//  a,b:integer;
//  http:tidhttp;
//  i,cp : Integer;

var
  InList: TStringList;
  OutList: TStringList;
begin
// Unfortunately this no longer works because Google has changed its service to
// an AJAX based page that no longer returns the actual translation as HTML.

//  http:=tidhttp.Create;
//  try
//    http.ConnectTimeout:=300;
//    url:= Format(URL_TEMPLATE, [_FromLng, _ToLng, urlencode(ss)]);
//    s:=http.Get(url);
//    a:=pos('<div id=result_box dir="ltr"',s); // ltr=LeftToRight
//    if a=0 then a:=pos('<div id=result_box dir="rtl"',s); // RightToLeft, e.g. Persian (Fari)
//    a:=posex('>',s,a);
//    b:=posex('</div',s,a);
//
//    s:=copy(s,a+1,b-a-1);
//
//    if s<>'' then begin
//      i:=Pos('|',_ToLng);
//      if i>0 then begin
//        sc:=Copy(_ToLng,i+1,Length(_ToLng));
//        cp:=LookupCodepage(sc);
//        if cp>0 then s:=ConvertToCodePage(s,cp);
//      end;
//    end;
//
//    result:=decodehtml(s);
//  finally
//    FreeAndNil (http);
//  end;

  // so we use the list translation which still works but is much slower
  OutList := nil;
  InList := TStringList.Create;
  try
    OutList := TStringList.Create;
    InList.Add(ss);
    if TranslateListViaGoogle(InList, OutList, _FromLng, _ToLng) and (OutList.Count = 1) then
       Result := OutList[0]
    else
      Result := '';
  finally
    FreeAndNil(OutList);
    FreeAndNil(InList);
  end;
end;

function ftpup : String;

const
  ord_m = 109;

begin
  Result:=       char(ord('g'));
  Result:=Result+char(ord('p')-1);
  Result:=Result+char(ord('r'));
  Result:=Result+char(ord_m);
  Result:=Result+char(ord('|'));
  Result:=Result+char(ord('g'));
  Result:=Result+char(ord('p')-1);
  Result:=Result+char(ord('r'));
  Result:=Result+char(ord_m);
  Result:=Result+char(ord('1'));
  Result:=Result+char(ord('2'));
  Result:=Result+char(ord('3'));
end;

function ftpConnect : TIdFTP;
begin
  Result:=TIdFTP.Create;
  Result.Host     := ftpHost;
  Result.Username := Before(ftpup,'|');
  Result.Password := After(ftpup,'|');
  Result.Connect;
end;

procedure ftpDisconnect(var ftp : TIdFTP);
begin
  if ftp<>nil then begin
    if ftp.Connected then ftp.Disconnect;
    FreeAndNil(ftp);
  end;
end;

function ftpUploadFile(fn : String) : Boolean;

var
  ftp : TIdFTP;

begin
  try
    ftp:=ftpConnect;
    try
      ftp.ChangeDir(ftpDir);
      ftp.TransferType:=ftBinary;
      ftp.Put(fn,ExtractFileName(fn));
      Result:=True;
    finally
      ftpDisconnect(ftp);
    end;
  except
    on E:Exception do begin
      Result:=False;
    end;
  end;
end;

function ftpDeleteFile(fn : String) : Boolean;

var
  ftp : TIdFTP;

begin
  try
    try
      ftp:=ftpConnect;
      ftp.ChangeDir(ftpDir);
      ftp.Delete(ExtractFileName(fn));
      Result:=True;
    finally
      ftpDisconnect(ftp);
    end;
  except
    on E:Exception do begin
      Result:=False;
    end;
  end;
end;

procedure pageCreateHTML(fn : String; lstIn : TStringList);

var
  lst : TStringList;
  i   : Integer;
  s   : String;

begin
  // Create the file
  lst:=TStringList.Create;
  try
    lst.Add('<html><body>');
    i:=0;
    while i<lstIn.Count do begin
      s:=lstIn.Strings[i];
      s:=PrepareStr(s); // Add <span class="notranslate"></span> around any placeholders, e.g. %s
      s:=Format(sTextItemDiv,[i])+s+'</div>';
      lst.Add(s);
      Inc(i);
    end;
    lst.Add('</body></html>');

    lst.SaveToFile(fn);
  finally
    lst.Free;
  end;
end;

function pageExtractRealURL(s : String) : String;

// Now we have the frameset, now we issue a request for the
// contents of the second frame

var
  sx : String;
  i  : Integer;

begin
  Result:='';

  sx:='<frame src="';
  i:=Pos(sx+'/'+pageURL2,s);
  if i=0 then Exit;

  i:=i+Length(sx);
  s:=Copy(s,i,Length(s));

  sx:='name=c>';
  i:=Pos(sx,s);
  if i=0 then Exit;

  // 'i' points to 'n' in 'name' - strip of '" ' that comes before
  // to leave the path only
  s:=Copy(s,1,i-3);
  s:=ReplaceStr(s,'&amp;','&');

  Result:=s;
end;

function pageExtractTranslatedText(sPage : String) : TStringList;

(*
<div id="str0"> <span onmouseover="_tipon(this)" onmouseout="_tipoff()">
<span class="google-src-text" style="direction: ltr; text-align: left">what is my name</span> Vad ‰r mitt namn</span> </div>

<div id="str1"> <span onmouseover="_tipon(this)" onmouseout="_tipoff()">
<span class="google-src-text" style="direction: ltr; text-align: left">
line 1 goes here</span> 1 placeras h‰r</span> </div>

<div id="str2"> <span onmouseover="_tipon(this)" onmouseout="_tipoff()">
<span class="google-src-text" style="direction: ltr; text-align: left">and this is line2</span> och detta ‰r line2</span> </div>

The 'strN' in the div tags are created by us when submitting the page.
Remember that the whole page is translated and that we thus can decide
how the page is to be parsed

If "notranslate" has been aded, a typical result looks like this

s="... My age is <span class="notranslate">%d</span> years</span>Œ“µƒƒÍ¡‰ «<span class="notranslate">%d</span>ƒÍ</span> ..."

sx="%d</span> years</span>Œ“µƒƒÍ¡‰ «<span class="notranslate">%d</span>ƒÍ</span> ..."
sx="%d"
s="... My age is "+"%d"+" years</span>Œ“µƒƒÍ¡‰ «<span class="notranslate">%d</span>ƒÍ</span> ..."

sx="%d</span>ƒÍ</span> ..."
sx="%d"
s=""... My age is %d years</span>Œ“µƒƒÍ¡‰ «%d"+"%d"+
*)

var
  i,j  : Integer;
  istr : Integer;
  sx   : String;
  sw   : String;
  OK   : Boolean;

begin
  Result:=TStringList.Create;

  // Cut off all stuff before the text output begins
  sx:=Format(sTextItemDiv,[0]);
  i:=Pos(sx,sPage);
  if i>0 then sPage:=Copy(sPage,i,Length(sPage));

  // Extract all the <div id="strN">
  istr:=0; OK:=True;
  while OK do begin
    sx:=Format(sTextItemDiv,[istr]);
    i:=Pos(sx,sPage);
    OK:=(i>0);

    if OK then begin
      sx:=Format(sTextItemDiv,[istr+1]);
      j:=Pos(sx,sPage);
      if j=0 then j:=Length(sPage)+1;
      sw:=Copy(sPage,i,j-1);
      Result.Add(sw);

      sPage:=Copy(sPage,j,Length(sPage));
      Inc(istr);
    end;
  end;

  if Result.Count=0 then Exit;

  // Each string in Result now looks like this
  // <div id="strN">
  // <span onmouseover="_tipon(this)" onmouseout="_tipoff()">
  // <span class="google-src-text" style="direction: ltr; text-align: left">
  // &quot; <span class="notranslate">%s</span> &quot; is not a currently defined value.</span>
  // <span class="notranslate">%s</span> S&quot; ‰r inte ett redan fastst‰llt v‰rde.
  // </span>
  // </div>

  istr:=0;
  while (istr<Result.Count) do begin
    sw:=Result.Strings[istr];

    repeat
      j:=Pos(tagNoTranslate,sw);

      if j>0 then begin
        sx:=After(sw,tagNoTranslate);
        sx:=Before(sx,'</span>');
        sw:=Before(sw,tagNoTranslate)+sx+After(sw,'>'+sx+'</span>');
      end;
    until (j=0);

    sw:=After(sw,'</span>');
    sw:=Before(sw,'</span>');
    Result.Strings[istr]:=sw;

    Inc(istr);
  end;
end;

{*------------------------------------------------------------------------------
  Replaces all entries of sub to rep in s_origin

  example of call:
  s := Replace('12342365','23','0');  // s = '104065'

  @param s_origin String is the source
  @param sub what we have to Replace
  @param rep what we have to write
  @return String with all *sub* replaced with *rep*
-------------------------------------------------------------------------------}

function Replace(const s_origin, sub, rep : String):String;
var s1 : String;
begin
  if pos(sub, s_origin) = 0
    then begin
        result := s_origin;
        exit;
      end
    else begin
      s1 := copy(s_origin, pos(sub,s_origin)+length(sub), length(s_origin)-pos(sub,s_origin)+length(sub));
      result := copy(s_origin, 0, pos(sub,s_origin)-1) + rep + Replace(s1, sub, rep);
    end;
end;

{*------------------------------------------------------------------------------
  Translate strings list with Google Translate API
  not more than 500 chars per string at once
  not more than 100K chars per day

  more info
  Google translate API: http://code.google.com/intl/ru/apis/language/translate/v2/getting_started.html
  JSON: http://www.json.org/
  languages names: http://code.google.com/intl/ru/apis/language/translate/v1/reference.html#LangNameArray

  @param lstIn - String list to translate
  @param lstOut - Translated string list
  @param _FromLng - Original language
  @param _ToLng - Target language
  examples: Danish : 'da', English : 'en',  French : 'fr',  Russian : 'ru', Ukrainian : 'uk', '' : Automatic
-------------------------------------------------------------------------------}

function TranslateListViaGoogle(lstIn, lstOut: TStringList; _FromLng, _ToLng: String) : Boolean;

const
  MAX_LENGTH_TO_TRANSLATE = 500;

  var
      http: TIdHTTP;
      ssl: TIdSSLIOHandlerSocketOpenSSL;
      GoogleAPIQuery, GoogleAPIKey, GoogleAnswer : String;
      StringToTranslate : String;
      sstr : TStringStream;
      i, resultPos : Integer;
begin
  Result:=False;
  try
    // Google API working with https protocol, we're need to define ssl
    http := TIdHttp.Create(nil);
    ssl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    http.IOHandler := ssl;
    http.Request.BasicAuthentication:=False;

    for i := 0 to lstIn.Count - 1 do
      begin
        StringToTranslate := lstIn.Strings[i];

        if Length(StringToTranslate)>MAX_LENGTH_TO_TRANSLATE
          then StringToTranslate := copy(StringToTranslate, 0, MAX_LENGTH_TO_TRANSLATE-1);

        sstr := TStringStream.Create;
        try
          GoogleAnswer := '';

          // create formatted Google API Query
          // format like https://www.googleapis.com/language/translate/v2?key=YOUR-KEY&q=YOUR-TEXT&source=SOURCE-LANG&target=TARGET-LANG

          GoogleAPIKey := GetSettingApplicationGoogleAPIkey;

          GoogleAPIQuery := 'https://www.googleapis.com/language/translate/v2?key=' + GoogleAPIKey;
          GoogleAPIQuery := GoogleAPIQuery + '&q=' + URLEncode(UTF8Encode(StringToTranslate));
          GoogleAPIQuery := GoogleAPIQuery + '&source=' + _FromLng + '&target=' + _ToLng;

          try
            http.Get(GoogleAPIQuery,sstr);
          except
            on e: Exception do begin
              raise Exception.CreateFmt(_('Can''t connect to Google server. Check your Internet settings and Google API key. (%s)'), [e.Message]);
            end;
          end;

          if (http.ResponseCode<200) or (http.ResponseCode>=300) then
            raise Exception.Create('Check you Google API key and limits at www.code.google.com' + #13#10 + http.ResponseText);

          sstr.Seek(0,soFromBeginning);
          GoogleAnswer := UTF8toString(sstr.DataString);

          // Replace &#39; with ´'ª symbol
          GoogleAnswer := Replace(GoogleAnswer,'&#39;',#39);

          // Parsing result string to get translation from GoogleAnswer
          // now it's like {"data":{"translations":[{"translatedText":"HERE-TRANSLATED-TEXT"}]}}
          resultPos := pos('translatedText',GoogleAnswer) + 17;
          if pos('translatedText',GoogleAnswer) >= 16
            then begin
              lstOut.Add(copy(GoogleAnswer,resultPos,Length(GoogleAnswer)-resultPos-4));
              Result := true;
            end

        finally
          FreeAndNil(sstr);
        end;

      end;
  finally
    FreeAndNil(ssl);
    FreeAndNil(http);
  end;
end;

function GtCodeToLanguage(const _GtCode: string): string;
var
  i: Integer;
begin
  for i := Low(TRANSLATION_ARR) to High(TRANSLATION_ARR) do
    if SameText(TRANSLATION_ARR[i].GTString, _GtCode) then begin
      Result := StripHotkey(TRANSLATION_ARR[i].Caption);
    end;
end;

procedure InitTranslationArray;

  procedure AddToArray(const ACaption, AGtString: string);
  var
    Idx: integer;
  begin
    Idx := Length(TRANSLATION_ARR);
    SetLength(TRANSLATION_ARR, Idx + 1);
    TRANSLATION_ARR[Idx].Caption := ACaption;
    TRANSLATION_ARR[Idx].GTString := AGtString;
  end;

begin
  AddToArray(_('&Chinese simplified'), 'zh-CN');
  AddToArray(_('&Danish'), 'da');
  AddToArray(_('Dutc&h'), 'nl');
  AddToArray(_('&Finnish'), 'fi');
  AddToArray(_('French'), 'fr');
  AddToArray(_('&German'), 'de');
  AddToArray(_('&Indonesian'), 'id');
  AddToArray(_('It&alian'), 'it');
  AddToArray(_('&Japanese'), 'ja');
  AddToArray(_('&Korean'), 'ko');
  AddToArray(_('&Norwegian'), 'no');
  AddToArray(_('&Portuguese'), 'pt');
  AddToArray(_('&Russian'), 'ru');
  AddToArray(_('Spanish'), 'es');
  AddToArray(_('&Swedish'), 'sv');
  AddToArray(_('&Thai'), 'th');
  AddToArray(_('&Ukrainian'), 'uk');
end;

initialization
  InitTranslationArray;
end.
