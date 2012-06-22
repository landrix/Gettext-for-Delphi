{$WARN SYMBOL_DEPRECATED OFF}
unit D5compat;
{$I JEDI.INC}

interface

{$IFNDEF COMPILER6_UP}
type
  UTF8String = AnsiString;

const
  PathDelim = '\';
  sLineBreak = #13#10;


function IncludeTrailingPathDelimiter(s: string): string;
function ExcludeTrailingPathDelimiter(s: string): string;
procedure RaiseLastOSError;
function UTF8Decode(const S: UTF8String): WideString;
function UTF8Encode(const WS: WideString): UTF8String;
function StrToFloatDef(const S:String;Default:Extended):Extended;
function DirectoryExists(const Name:string):boolean;
{$ENDIF}

implementation
{$IFNDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
uses
  FileCtrl, SysUtils;
  
function DirectoryExists(const Name:string):boolean;
begin
  Result := FileCtrl.DirectoryExists(Name);
end;

function IncludeTrailingPathDelimiter(s: string): string;
begin
  Result := IncludeTrailingBackslash(s);
end;

function ExcludeTrailingPathDelimiter(s: string): string;
begin
  Result := ExcludeTrailingBackslash(s);
end;

procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

function StrToFloatDef(const S:string;Default:Extended):Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := Default;
end;


function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then
    Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := Char(c);
        Inc(count);
      end
      else
        if c > $7FF then
        begin
          if count + 3 > MaxDestBytes then
            break;
          Dest[count] := Char($E0 or (c shr 12));
          Dest[count + 1] := Char($80 or ((c shr 6) and $3F));
          Dest[count + 2] := Char($80 or (c and $3F));
          Inc(count, 3);
        end
        else //  $7F < Source[i] <= $7FF
        begin
          if count + 2 > MaxDestBytes then
            break;
          Dest[count] := Char($C0 or (c shr 6));
          Dest[count + 1] := Char($80 or (c and $3F));
          Inc(count, 2);
        end;
    end;
    if count >= MaxDestBytes then
      count := MaxDestBytes - 1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count + 1; // convert zero based index to byte count
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then
          Exit; // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then
            Exit; // malformed trail byte or out of range char
          if i >= SourceBytes then
            Exit; // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then
          Exit; // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then
      count := MaxDestChars - 1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then
          Exit; // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then
            Exit; // malformed trail byte or out of range char
          if i >= SourceBytes then
            Exit; // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then
          Exit; // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count + 1;
end;

function UTF8Decode(const S: UTF8String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then
    Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp) + 1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8Encode(const WS: WideString): UTF8String;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then
    Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PChar(Temp), Length(Temp) + 1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;
{$ENDIF}


end.


