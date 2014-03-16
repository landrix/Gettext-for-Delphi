///<summary> This is an extract from u_dzStringUtils which is part of dzlib available from berlios
///          under the MPL 1.1 </summary>
unit u_dzStringUtils;

interface

uses
  SysUtils;

const
  STANDARD_CONTROL_CHARS = [#0..' '];

type
  TCharSet = set of ansichar;

///<summary> returns the sub string starting from position Start </summary>
function TailStr(const _s: string; _Start: Integer): string;

/// <summary>
/// extracts the substring from the start of Source up to the Delimiter
/// </summary>
function ExtractStr(var _Source: string; _Delimiter: char): string; overload;

///<summary> Replaces all control characters (ord(c) <= ord(' '), " and ') with Prefix<hexcode> </summary>
function HexEncodeControlChars(_Prefix: char; const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;

///<summary> Replaces all control characters (ord(c) <= ord(' '), " and ') with %<hexcode> </summary>
function UrlEncodeControlChars(const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;

implementation

uses
  StrUtils;

function HexEncodeControlChars(_Prefix: Char; const _s: string; _ControlChars: TCharSet): string;
var
  i: integer;
begin
  Result := '';
  Include(_ControlChars, AnsiChar(_Prefix));
  for i := 1 to Length(_s) do begin
    if CharInSet(_s[i], _ControlChars) then
      Result := Result + Format('%s%.2x', [_Prefix, Ord(_s[i])]) // do not translate
    else
      Result := Result + _s[i];
  end;
end;

function UrlEncodeControlChars(const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;
begin
  Result := HexEncodeControlChars('%', _s, _ControlChars);
end;

function TailStr(const _s: string; _Start: Integer): string;
begin
  if _Start > Length(_s) then
    Result := ''
  else
    Result := Copy(_s, _Start, Length(_s) - _Start + 1);
end;

function ExtractStr(var _Source: string; _Delimiter: char): string;
var
  p: Integer;
begin
  p := Pos(_Delimiter, _Source);
  if p = 0 then begin
    Result := _Source;
    _Source := '';
  end else begin
    Result := LeftStr(_Source, p - 1);
    _Source := TailStr(_Source, p + 1);
  end;
end;

end.

