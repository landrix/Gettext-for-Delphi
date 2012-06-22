unit xgettexttools;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl and Jens Berke           *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  Classes;

{$ifdef MSWINDOWS}
var
  DefCP:integer=CP_ACP;
{$endif}

function ConvertWhitespaceToSpaces(const s:string):string;
function is_identifier(const ws:string):boolean;
function poscode (const substr,line:string):integer;
function StreamReadln (s:TStream; var line:string; utf8:boolean):boolean; // Returns false if end of line
function measureindent(const s: string): word;
function scope2comment(sl: TStrings; const name: string): string;
function RemoveFilenameSpaces (const s:string):string;
function WindowsPathDelim2LinuxPathDelim (const path:string):string;
function IsDirective(const directive, line: string): boolean;



implementation

uses
  SysUtils, gnugettext;

function measureindent(const s: string): word;
// Returns number of spaces this line used to indent this line
begin
  Result := 0;
  while (copy(s, Result + 1, 1) = ' ') do
    inc(Result);
end;

function WindowsPathDelim2LinuxPathDelim (const path:string):string;
var
  i:integer;
begin
  Result:=path;
  for i:=1 to length(Result) do
    if Result[i]='\' then
      Result[i]:='/';
end;

function ConvertWhitespaceToSpaces (const s:string):string;
var
  i:integer;
begin
  Result:=s;
  for i:=1 to length(Result) do
    if Result[i]<=#32 then
      Result[i]:=' ';
end;

function is_identifier(const ws:string):boolean;
var
  i:integer;
  s:ansistring;
begin
  if ws='' then begin
    Result:=False;
    exit;
  end;

  s:=UTF8Encode(ws);
  Result:=s[1] in ['a'..'z','A'..'Z','_'];
  if Result then begin
    for i:=2 to length(s) do begin
      Result:=Result and (s[i] in ['a'..'z','A'..'Z','_','0'..'9']);
      if not Result then
        break;
    end;
  end;
end;

function poscode (const substr,line:string):integer;
// Same as pos(), but ignores everything inside strings
var
  i:integer;
  ssl:integer;
  quotemode:boolean;
begin
  Result:=0;
  if pos(substr, line) = 0 then Exit;  //if not found at all, do not do the expensive "copy" check!

  ssl:=length(substr);
  quotemode:=False;
  for i:=1 to length(line)-ssl+1 do begin
    if line[i]='''' then
      quotemode:=not quotemode;
    if (not quotemode) and (copy(line,i,ssl)=substr) then begin
      Result:=i;
      exit;
    end;
  end;
end;

function RemoveFilenameSpaces (const s:string):string;
var
  i:integer;
begin
  Result:=s;
  for i:=1 to length(Result) do begin
    if Result[i]=' ' then Result[i]:='_';
  end;
end;

function StreamReadln (s:TStream; var line:string; utf8:boolean):boolean; // Returns false if end of line. reads single-bytes and converts these.
var
  c:ansichar;
  aline:rawbytestring;
begin
  Assert (s<>nil,_('StreamReadln requires the stream to be not nil.'));
  Assert (utf8,'This parameter must be set to true in order to use this function');
  Result:=True;
  aline:='';
  while true do begin
    if s.Read(c,1)=0 then begin
      Result:=False;
      break;
    end;
    if c=#10 then begin
      break;
    end;
    if c<>#13 then
      aline:=aline+c;
  end;
  line:=string(utf8string(aline));
end;

function scope2comment(sl: TStrings; const name: string): string;
// Converts a list of strings to a single-line comment separated by dots
var
  i: integer;
begin
  Result := '';
  for i := 0 to sl.Count - 1 do begin
    if Result <> '' then Result := Result + '.';
    Result := Result + sl.Strings[i];
  end;
  if Result <> '' then Result := Result + '.';
  Result := Result + name;
end;

function IsDirective(const directive, line: String): boolean;
begin
  Result := pos (directive, lowerCase (copy (line, 1, length(directive)))) = 1;
end;

end.
