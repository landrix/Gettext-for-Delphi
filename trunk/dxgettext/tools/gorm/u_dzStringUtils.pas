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

///<summary> Replaces all control characters (ord(c) <= ord(' '), " and ') with Prefix<hexcode> </summary>
function HexEncodeControlChars(_Prefix: char; const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;

///<summary> Replaces all control characters (ord(c) <= ord(' '), " and ') with %<hexcode> </summary>
function UrlEncodeControlChars(const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;

implementation

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

end.

