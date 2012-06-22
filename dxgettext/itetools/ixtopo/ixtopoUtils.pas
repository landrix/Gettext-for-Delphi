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
{$I JEDI.INC}
unit ixtopoUtils;
{$IFDEF COMPILER7_UP}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}
                                             
interface
uses
  ITEUtils;
const
  IX_ERR_SUCCESS = 0;
  IX_NOTENOUGHPARAMS = 1;
  IX_ERR_XMLNOTFOUND = 2;
  IX_ERR_PONOTFOUND = 3;
  IX_ERR_NOLOCALE = 4;
  
function Run:integer;

implementation
uses
  Windows, SysUtils, Classes, POUtils, D5Compat;

procedure ShowHelp;
begin
  writeln('IXTOPO 1.0: extracts strings from an ITE xml file and inserts any translations into a PO file.');
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
  writeln('');
  writeln('ixtopo <xmlfile> <pofile> <locale> [-f]');
  writeln('');
  writeln('where:');
  writeln('');
  writeln('<xmlfile>    is the xml file to read from (REQUIRED)');
  writeln('<pofile>     is the po file to write to (REQUIRED)');
  writeln('<locale>     is the locale to extract and insert into the po file (REQUIRED)');
  writeln('-f           forces the creation of new entries in the po file if not found (OPTIONAL, defaults to FALSE)');
  writeln('');

  writeln('NOTE:');
  writeln('Since locale names in XML files contains spaces, you must');
  writeln('put quotes around the locale, i.e use "US en" to extract');
  writeln('the American english translations');
end;


function XMLStrToPO(const S:string):string;
begin
  Result := StringReplace(StringReplace(
    StringReplace(S,'\r','',[rfReplaceAll]),'\t','\x09',[rfReplaceAll]),#39'#39'#39,#39,[rfReplaceAll]);
  if Length(Result) > 1 then
  begin
    if ((Result[1] = #39) and (Result[Length(Result)] = #39)) or ((Result[1] = '"') and (Result[Length(Result)] = '"'))then
      Result := Copy(Result,2,Length(Result)-2);
  end;
end;

procedure ParseTranslations(XMLFile: TITEXMLFile; ALocaleIndex: integer; POFile: TPOFile;ForceCreate:boolean);
var
  i, j: integer;
  S: string;
begin
  for i := 0 to XMLFile.Count - 1 do
  begin
    S := XMLStrToPO(XMLFile.Items[i].Original);
    j := POFile.IndexOf(S);
    if j > -1 then // item is in PO file
    begin
      if ALocaleIndex >= 0 then // not original
        S := XMLStrToPO(XMLFile.Items[i].Translations[ALocaleIndex]);
      POFile.Items[j].MsgStr := S;
    end
    else if ForceCreate then
    begin
      with POFile.Add do
      begin
        Comments.Add('. Auto-created by ixtopo');
        MsgID := XMLStrToPO(XMLFile.Items[i].Original);;
        if ALocaleIndex >= 0 then // not original
          MsgStr := XMLStrToPO(XMLFile.Items[i].Translations[ALocaleIndex])
      end;
    end
    else
      writeln('"',S,'": not found in po');
  end;
end;

procedure ParseLocale(const Filename, Locale: string; POFile: TPOFile;ForceCreate:boolean);
var
  XMLFile: TITEXMLFile;
  ALocaleIndex: integer;
begin
  XMLFile := TITEXMLFile.Create('');
  try
    XMLFile.LoadLocalesOnly(Filename);
    if AnsiSameText(Locale, XMLFile.BaseLocale) then
      ALocaleIndex := -2
    else
      ALocaleIndex := XMLFile.TargetLocales.IndexOf(Locale);
    if ALocaleIndex = -1 then
    begin
      writeln('');
      writeln('ERROR: unable to find locale "', Locale, '" in XML file.');
      writeln('');
      writeln('Avaiable locales:');
      writeln('=================');
      writeln('"'+XMLFile.BaseLocale,'" (BASE-LOCALE)');
      for ALocaleIndex := 0 to XMLFile.TargetLocales.Count - 1 do
        writeln('"'+XMLFile.TargetLocales[ALocaleIndex]+'"');
      Exit;
    end;
    // load entire file
    XMLFile.LoadFromFile(Filename);
    ParseTranslations(XMLFile, ALocaleIndex, POFile,ForceCreate);
  finally
    XMLFile.Free;
  end;
end;

function Run:integer;
var
  ALocale,
  APOFile,
  AXMLFile: string;
  APO: TPOFile;
  ForceCreate:boolean;
begin
  Result := IX_ERR_SUCCESS;
  // params: xmlfile pofile locale
  if ParamCount < 3 then
  begin
    writeln('ERROR: Not enough command-line parameters!');
    ShowHelp;
    Result := IX_NOTENOUGHPARAMS;
    Exit;
  end;
  AXMLFile := ExpandUNCFilename(ParamStr(1));
  if not FileExists(AXMLFile) then
  begin
    Result := IX_ERR_XMLNOTFOUND;
    writeln('ERROR:', AXMLFile, ' not found!');
    Exit;
  end;

  APOFile := ExpandUNCFilename(ParamStr(2));
  if not FileExists(APOFile) then
  begin
    Result := IX_ERR_PONOTFOUND;
    writeln('ERROR:', APOFile, ' not found!');
    Exit;
  end;
  ForceCreate := FindCmdLineSwitch('f');
  ALocale := ParamStr(3);
  if ALocale = '' then
  begin
    writeln('ERROR: Locale not specified!');
    Result := IX_ERR_NOLOCALE;
    Exit; 
  end;
  APO := TPOFile.Create;
  try
    APO.LoadFromFile(APOFile);
    APO.Pack;
    APO.Sort;
    ParseLocale(AXMLFile, ALocale, APO,ForceCreate);
    APO.SaveToFile(APOFile);
  finally
    APO.Free;
  end;
end;

end.

