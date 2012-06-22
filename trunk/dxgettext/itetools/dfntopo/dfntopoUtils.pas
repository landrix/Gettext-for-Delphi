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
unit dfntopoUtils;
{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

interface
uses
  Windows, Classes, SysUtils;
const
  DFN_ERR_SUCCESS = 0;
  DFN_ERR_NOPARAMS = 1;
  DFN_ERR_PONOTFOUND = 2;
  DFN_ERR_DFNNOTFOUND = 3;

function Run: integer;

implementation
uses
  ITEUtils, POUtils, D5Compat;

procedure ShowMsg(const S: string);
var
  ST: TStringlist;
  i: integer;
begin
  ST := TStringlist.Create;
  try
    ST.Text := S;
    for i := 0 to ST.Count - 1 do
      writeln(ST[i]);
  finally
    ST.Free;
  end;
end;

procedure ShowMsgFmt(const S:string; Args:array of const);
begin
  ShowMsg(Format(S,Args));
end;

procedure ShowHelp;
begin
  writeln('DFNToPO: extracts strings from DFN and RC files and insert any translations into a PO file');
  writeln('');
  writeln('This program is subject to the Mozilla Public License');
  writeln('Version 1.1 (the "License"); you may not use this program except');
  writeln('in compliance with the License. You may obtain a copy of the License at');
  writeln('http://www.mozilla.org/MPL/MPL-1.1.html');
  writeln('');
  writeln('Portions created by Peter Thornqvist are');
  writeln('Copyright (c) 2003 by Peter Thornqvist; all rights reserved.');
  writeln('');
  writeln('Usage:');
  writeln('DFNToPO [options]');
  writeln('where [options] can be any of the following:');
  writeln('-s :  searches in sub-folders also (defaults to FALSE)');
  writeln('-f :  force creation of DFN/RC items not found in po (defaults to FALSE)');
  writeln('-m :  merge TStrings items into single item delimited by \n (defaults to FALSE)');
  writeln('-p<POFile>  : full path and filename of the po file (defaults to "default.po" in current dir).');
  writeln('            NOTE: Filenames with spaces must be enclosed in quotes.');
  writeln('-d<DFNPath> : full path to the dfn files. Do NOT include a filemask (defaults to current dir).');
  writeln('            NOTE: Paths with spaces must be enclosed in quotes.');
  writeln('');
end;

function StripQuotes(const S: string; Ch: Char): string;
begin
  if (Length(S) > 0) and (S[1] = Ch) and (S[Length(S)] = Ch) then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

function DFNStrToPO(const S: string): string;
begin
{ TODO -op3 -cietools : Replace with better implementation when verified to work }
  Result := StringReplace(StringReplace(StringReplace(StringReplace(
    StringReplace(S, '\r', '', [rfReplaceAll]), '\t', '\x09', [rfReplaceAll]), #39'#39'#39, #39, [rfReplaceAll]),
    #39'#13#10'#39, '\n', [rfReplaceAll]), '"', '\"', [rfReplaceAll]);
  if Length(Result) > 1 then
  begin
    if (Result[1] = #39) and (Result[Length(Result)] = #39) then
      Result := Copy(Result, 2, Length(Result) - 2);
  end;
end;

function StripEnd(const S, EndString: string): string;
begin
  Result := Copy(S, Length(S) - Length(EndString) + 1, Length(EndString));
  if AnsiSameText(Result, EndString) then
    Result := Copy(S, 1, Length(S) - Length(EndString));
end;

function RCStrToPO(const S: string): string;
begin
{ TODO -op3 -cietools : Replace with better implementation when verified to work }
  Result := StringReplace(
    StringReplace(S, '\r', '', [rfReplaceAll]), '\t', '\x09\', [rfReplaceAll]);
  if Length(Result) > 1 then
  begin
    if (Result[1] = '"') and (Result[Length(Result)] = '"') then
      Result := Copy(Result, 2, Length(Result) - 2);
  end;
end;

procedure DFNToPO(DFN: TDFNFile; PO: TPOFile; ForceCreate: boolean);
var
  i, j: integer;
  tmp: string;
begin
  for i := 0 to DFN.Count - 1 do
  begin
    tmp := UTF8Encode(DFNStrToPO(DFN[i].Orig));
    if tmp = '' then Continue; // this is the "header", do not translate
    j := PO.IndexOf(tmp);
    if j < 0 then // try adding a last \n and see if it matches
      j := PO.IndexOf(tmp + '\n');
    if (j >= 0) then
    begin
      // replace current translation
      if StripQuotes(DFN[i].Curr, #39) <> '' then
        PO.Items[j].MsgStr := UTF8Encode(DFNStrToPO(DFN[i].Curr))
      else
        PO.Items[j].MsgStr := UTF8Encode(DFNStrToPO(DFN[i].PrevCurr));
    end
    // add new item (unless it's an empty one)
    else if ForceCreate and (tmp <> '') then
    begin
      with PO.Add do
      begin
        Comments.Add('. Auto-Created by DFNToPO');
        Comments.Add('. ' + DFN[i].ID);
        Comments.Add(': ' + ExtractFileName(DFN.Filename));
        MsgID := tmp;
        if StripQuotes(DFN[i].Curr, #39) <> '' then
          MsgStr := UTF8Encode(DFNStrToPO(DFN[i].Curr))
        else
          MsgStr := UTF8Encode(DFNStrToPO(DFN[i].PrevCurr));
      end;
    end
    else
      ShowMsgFmt('"%s": not found in po file!',[tmp]);
  end;
end;

procedure RCToPO(RC: TITERCFile; PO: TPOFile; ForceCreate: boolean);
var
  i, j: integer;
  tmp: string;
begin
  for i := 0 to RC.Count - 1 do
  begin
    tmp := UTF8Encode(RCStrToPO(RC[i].Orig));
    j := PO.IndexOf(tmp);
    if j < 0 then // try adding a last \n and see if it matches
      j := PO.IndexOf(tmp + '\n');
    if j >= 0 then
    begin
      if StripQuotes(RC[i].Curr, '"') <> '' then
        PO.Items[j].MsgStr := UTF8Encode(RCStrToPO(RC[i].Curr))
      else
        PO.Items[j].MsgStr := UTF8Encode(RCStrToPO(RC[i].PrevCurr));
    end
    else if ForceCreate then
    begin
      with PO.Add do
      begin
        Comments.Add('. Auto-Created by DFNToPO');
        Comments.Add('. ' + RC[i].ID);
        Comments.Add(': ' + ExtractFileName(RC.Filename));
        MsgID := tmp;
        if StripQuotes(RC[i].Curr, '"') <> '' then
          MsgStr := UTF8Encode(RCStrToPO(RC[i].Curr))
        else
          MsgStr := UTF8Encode(RCStrToPO(RC[i].PrevCurr));
      end;
    end
    else
      ShowMsgFmt('"%s": not found in po file!',[tmp]);
  end;
end;

procedure FindFiles(FileMask: string; Files: TStrings; Recurse: boolean);
var
  FindHandle: THandle;
  FD: TWin32FindData;
  APath: string;
begin
  APath := ExtractFilePath(Filemask);
  FindHandle := FindFirstFile(PChar(Filemask), FD);
  try
    if FindHandle <> INVALID_HANDLE_VALUE then
      repeat
        if (FD.cFileName[0] <> '.') and (FD.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0) then
          Files.Add(APath + FD.cFileName);
      until not FindNextFile(FindHandle, FD);
  finally
    if FindHandle <> INVALID_HANDLE_VALUE then
      Windows.FindClose(FindHandle);
  end;
  if Recurse then
  begin
    FindHandle := FindFirstFile(PChar(APath + '*.*'), FD);
    try
      if FindHandle <> INVALID_HANDLE_VALUE then
        repeat
          if (FD.cFileName[0] <> '.') and (FD.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY)
            then
            FindFiles(APath + FD.cFileName + '\' + ExtractFilename(Filemask), Files, true);
        until not FindNextFile(FindHandle, FD);
    finally
      if FindHandle <> INVALID_HANDLE_VALUE then
        Windows.FindClose(FindHandle);
    end;
  end;
end;

function Run: integer;
var
  S: TStringlist;
  Recurse, ForceCreate, MergeStrings: boolean;
  i: integer;
  POFile, DFNPath: string;
  APO: TPOFile;
  ADFN: TDFNFile;
  ARC: TITERCFile;
begin
  Result := DFN_ERR_SUCCESS;
  try
    // command line: dfntopo -s -f -p<pofile> -d<searchpath>
    // -s recurse subdirs
    // -f force creation of po item if not found
    // -m merge TStrings into single \n delimited string
    // -p<pofile> = path and filename of po file (default current dir + default.po)
    // -d<path> = path to dfn files (default current dir)

    Recurse := false;
    ForceCreate := false;
    MergeStrings := false;
    DFNPath := GetCurrentDir;
    POFile := IncludeTrailingPathDelimiter(DFNPath) + 'default.po';
    if ParamCount = 0 then
    begin
      Result := DFN_ERR_NOPARAMS;
      ShowHelp;
      Exit;
    end;
    for i := 1 to ParamCount do
      if Pos('-s', LowerCase(ParamStr(i))) = 1 then
        Recurse := true
      else if Pos('-p', LowerCase(ParamStr(i))) = 1 then
        POFile := StripQuotes(Copy(ParamStr(i), 3, MaxInt), #39)
      else if Pos('-d', LowerCase(ParamStr(i))) = 1 then
        DFNPath := StripQuotes(Copy(ParamStr(i), 3, MaxInt), #39)
      else if Pos('-f', LowerCase(ParamStr(i))) = 1 then
        ForceCreate := true
      else if Pos('-m', LowerCase(ParamStr(i))) = 1 then
        MergeStrings := true
      else if Pos('-h', LowerCase(ParamStr(i))) = 1 then
      begin
        ShowHelp;
        Exit;
      end;
    if not FileExists(POFile) then
    begin
      ShowMsgFmt('ERROR: File "%s" not found!', [POFile]);
      Result := DFN_ERR_PONOTFOUND;
      Exit;
    end;
    if not DirectoryExists(DFNPath) then
    begin
      ShowMsgFmt('ERROR: Path "%s" not found!', [DFNPath]);
      Result := DFN_ERR_DFNNOTFOUND;
      Exit;
    end;

    // start with dfn's:
      S := TStringlist.Create;
      ADFN := TDFNFile.Create('');

      APO := TPOFile.Create;
      try
        ADFN.StringsOnly := true;
        ADFN.StripUnused := true;
        FindFiles(IncludeTrailingPathDelimiter(DFNPath) + '*.dfn', S, Recurse);
        APO.LoadFromFile(POFile);
        APO.Pack;
        APO.Sort;
        for i := 0 to S.Count - 1 do
        begin
          ADFN.LoadFromFile(S[i]);
          if MergeStrings then
            ADFN.MergeStrings;
          ShowMsgFmt('Extracting from "%s"...',[ExtractFileName(S[i])]);
          DFNToPO(ADFN, APO, ForceCreate);
        end;
        ShowMsgFmt('Done: %d DFN files found and parsed without errors.',[S.Count]);
      // now do rc's:
        S.Clear;
        FindFiles(IncludeTrailingPathDelimiter(DFNPath) + '*.rc', S, Recurse);
        ARC := TITERCFile.Create('', true);
        try
          for i := 0 to S.Count - 1 do
          begin
            ARC.LoadFromFile(S[i]);
            ShowMsgFmt('Extracting from "%s"..',[ExtractFileName(S[i])]);
            RCToPO(ARC, APO, ForceCreate);
          end;
        finally
          ARC.Free;
        end;
        ShowMsgFmt('%d RC files found and parsed without errors.',[S.Count]);
        APO.SaveToFile(POFile);
      finally
        S.Free;
        ADFN.Free;
        APO.Free;
      end;
  except
    on E: Exception do
      ShowMsgFmt('ERROR: %s',[E.Message]);
  end;
end;

end.

