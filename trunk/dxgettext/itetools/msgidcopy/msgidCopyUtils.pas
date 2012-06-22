{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Peter Thornqvist [peter3@peter3.com]
Portions created by XX are Copyright (C) 2003 Peter Thornqvist. All Rights Reserved.


You may retrieve the latest version of this file at dxgettext's home page,
located at http://dybdahl.dk/dxgettext/

-----------------------------------------------------------------------------}
unit msgidCopyUtils;
{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

interface

procedure Run;

implementation
uses
  SysUtils, POUtils, D5Compat;

procedure ShowHelp;
begin
  writeln('msgidcopy: Copies msgid to msgstr if msgstr is empty');
  writeln('');
  writeln('Usage:');
  writeln('msgidcopy <pofile>');
  writeln('');
  writeln('<pofile> defaults to "default.po" in the current folder');
  writeln('');
end;

procedure Run;
var
  POFile: string;
  PO:TPOFile;
  i: integer;
begin
  try
    POFile := ExpandUNCFilename(ParamStr(1));
    if POFile = '' then
      POFile := IncludeTrailingPathDelimiter(GetCurrentDir) + 'default.po';
    if FileExists(POFile) then
    begin
      PO := TPOFile.Create;
      try
        PO.LoadFromFile(POFile);
        for i := 0 to PO.Count - 1 do
          if trim(PO.Items[i].MsgStr) = '' then
            PO.Items[i].MsgStr := PO.Items[i].MsgID;
        PO.SaveToFile(POFile);
      finally
        PO.Free;
      end;
      writeln('Done');
    end
    else
    begin
      ShowHelp;
      raise Exception.Create('PO file not found!');
    end;
  except
    on E: Exception do
      writeln('ERROR: ', E.Message);
  end;
end;
end.

