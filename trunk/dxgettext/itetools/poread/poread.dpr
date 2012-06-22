{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Peter Thornqvist [peter3@peter3.com]
Portions created by Peter Thornqvist are Copyright (C) 2003 Peter Thornqvist. All Rights Reserved.

You may retrieve the latest version of this file at dxgettext's home page,
located at http://dybdahl.dk/dxgettext/

-----------------------------------------------------------------------------}

program poread;
{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  SysUtils, POUtils, D5Compat;
// test program for the TPOFile class in POUtils
// reads a PO file, writes out some stats and creates a copy of the input file

procedure ShowHelp;
begin
  writeln('poread: test program for TPOFile class');
  writeln('Copyright (c) 2003 by Peter Thornqvist: all rights reserved');
  writeln('');
  writeln('Reads a po file, writes out some stats and creates');
  writeln('a backup of the read file. Compare the original and');
  writeln('backup to find any problems with the TPOFile class');
  writeln('');
  writeln('Usage:');
  writeln('poread <pofile>');
  writeln('where <pofile> defaults to "default.po"');
end;

var
  APOFile: string;
  i: integer;
begin
  try
    ShowHelp;
    if ParamCount = 0 then
      APOFile := IncludeTrailingPathDelimiter(GetCurrentDir) + 'default.po'
    else
      APOFile := ExpandUNCFilename(ParamStr(1));
    if not FileExists(APOFile) then
      raise Exception.Create('PO file not found!');
    with TPOFile.Create do
    try
      LoadFromFile(APoFile);
      writeln('Content of ', APoFile, ', generated on ', DateTimeToStr(Now), ':');
      for i := 0 to Count - 1 do
      begin
        writeln('comments:');
        writeln(trim(Items[i].Comments.Text));
        writeln('msgid:');
        writeln(Items[i].MsgID);
        writeln('msgstr:');
        writeln(Items[i].MsgStr);
      end;
      writeln(Count, ' items found in file. Writing to ', ExtractFileName(APOFile), '.bak...');
      SaveToFile(APoFile + '.bak');
    finally
      Free;
    end;
  except
    on E: Exception do
      writeln('ERROR: ', E.Message);
  end;
end.

