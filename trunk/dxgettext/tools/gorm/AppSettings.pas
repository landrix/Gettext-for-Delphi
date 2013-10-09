unit AppSettings;

interface

uses
  Classes;

function GetIniFilename:string;

function GetSettingStartupAutoUpgrade:boolean;
function GetSettingStartupActionsEnabled:boolean;
function GetSettingStartupAutoCreateLabels:boolean;
function GetSettingStartupParallelTranslation:string;
function GetSettingStartupOpenFilename:string;
function GetSettingApplicationExeFilename:string;
function GetSettingApplicationMoFilename:string;
function GetSettingApplicationExternalEditorFilename: string;
function GetSettingApplicationExternalEditorUseLineNumbers: boolean;
function GetSettingSaveWrapAfter: integer;
function GetSettingShowStatus: boolean;
function GetSettingApplicationGoogleAPIkey:string;
function GetSettingApplicationBingAppId: string;

function GetSettingTranslationMemory:boolean;
function GetSettingMemoryFileSize: integer;
function GetSettingMemorySuggestionsAccuracy: integer;
function GetSettingMemorySuggestionsNumber: integer;

function GetSettingTranslationRepositoryDir: string;

function GetSettingSpecCharsFontSize:integer;

procedure SetSetting (Sectkey,Key:string;value:boolean); overload;
procedure SetSetting (Sectkey,Key:string;value:string); overload;

procedure SaveList(SectKey : String; AList : TStringList);
procedure ReadList(SectKey : String; AList : TStringList);

implementation

uses
  IniFiles, SysUtils;

var IniFileName:string;

function GetIniFilename:string;
begin
  Result:=IniFileName;
end;

procedure SetSetting (Sectkey,Key:string;value:boolean);
var
  ini:TMemIniFile;
begin
  ini:=TMemIniFile.Create(IniFilename);
  try
    ini.WriteBool(Sectkey,Key,value);
    ini.UpdateFile;
  finally
    FreeAndNil (ini);
  end;
end;

procedure SetSetting (Sectkey,Key:string;value:string);
var
  ini:TMemIniFile;
begin
  ini:=TMemIniFile.Create(IniFilename);
  try
    ini.WriteString(Sectkey,Key,value);
    ini.UpdateFile;
  finally
    FreeAndNil (ini);
  end;
end;

function GetSettingBool (Sectkey,Key:string; _Default: boolean = false):boolean;
var
  ini:TMemIniFile;
begin
  ini:=TMemIniFile.Create(IniFilename);
  try
    Result:=ini.ReadBool(Sectkey,Key,_Default);
  finally
    FreeAndNil (ini);
  end;
end;

function GetSettingString (Sectkey,Key:string):string;
var
  ini:TMemIniFile;
begin
  ini:=TMemIniFile.Create(IniFilename);
  try
    Result:=ini.ReadString(Sectkey,Key,'');
  finally
    FreeAndNil (ini);
  end;
end;

function GetSettingStartupAutoCreateLabels:boolean;
begin
  Result:=GetSettingBool ('Startup','AutoCreateLabels');
end;

function GetSettingStartupParallelTranslation:string;
begin
  Result:=GetSettingString ('Startup','ParallelTranslation');
end;

function GetSettingStartupOpenFilename:string;
begin
  Result:=GetSettingString ('Startup','OpenFilename');
end;

function GetSettingStartupActionsEnabled:boolean;
begin
  Result:=GetSettingBool ('Startup','Enabled');
end;

function GetSettingTranslationMemory:boolean;
begin
  Result:=GetSettingBool ('MemoryTranslation','TranslationMemory');
end;

function GetSettingMemoryFileSize: integer;
begin
  Result := StrToIntDef(GetSettingString('MemoryTranslation', 'MaxMemoryFileSize'), 10);
end;

function GetSettingMemorySuggestionsNumber: integer;
begin
  Result := StrToIntDef(GetSettingString('MemoryTranslation', 'SuggestionsNumber'), 10);
end;

function GetSettingMemorySuggestionsAccuracy: integer;
begin
  Result := StrToIntDef(GetSettingString('MemoryTranslation', 'SuggestionsAccuracy'), 3);
end;

function GetSettingTranslationRepositoryDir: string;
begin
  Result := GetSettingString('TranslationRepository', 'Directory');
  if Result = '' then
    Result := ExtractFileDir(ParamStr(0));
end;

// Returns the special chars font size
function GetSettingSpecCharsFontSize: integer;
begin
  Result := StrToIntDef(GetSettingString('SpecialChars', 'FontSize'), 12);
end;

function GetSettingStartupAutoUpgrade:boolean;
begin
  Result:=GetSettingBool ('Startup','AutoUpgrade');
end;

function GetSettingApplicationExeFilename:string;
begin
  Result:=GetSettingString ('Application','ExeFilename');
end;

function GetSettingApplicationMoFilename:string;
begin
  Result:=GetSettingString ('Application','MoFilename');
end;

function GetSettingApplicationExternalEditorFilename: string;
begin
  Result := GetSettingString('Application', 'ExternalEditorFilename');
end;

function GetSettingApplicationExternalEditorUseLineNumbers: boolean;
begin
  Result := GetSettingBool('Application', 'ExternalEditorUseLineNumbers', true);
end;

function GetSettingApplicationGoogleAPIkey:string;
begin
  Result:=GetSettingString ('Google','GoogleAPIkey');
end;

function GetSettingApplicationBingAppId: string;
begin
  Result := GetSettingString('Microsoft', 'BingAppId');
end;

function GetSettingSaveWrapAfter: integer;
begin
  Result := StrToIntDef(GetSettingString('Save', 'WrapAfter'), 70);
end;

function GetSettingShowStatus: boolean;
begin
  Result := GetSettingBool('GUI', 'ShowStatus', true);
end;

procedure SaveList(SectKey : String; AList : TStringList);
var
  ini:TMemIniFile;
  i  :Integer;
begin
  ini:=TMemIniFile.Create(IniFilename);
  try
    ini.EraseSection(SectKey);
    i:=0;
    while i<AList.Count do begin
      Ini.WriteString(SectKey,'Item_'+IntToStr(i),AList.Strings[i]);
      Inc(i);
    end;
    ini.UpdateFile;
  finally
    FreeAndNil (ini);
  end;
end;

procedure ReadList(SectKey : String; AList : TStringList);
var
  ini:TMemIniFile;
  i  :Integer;
  s  :String;
begin
  ini:=TMemIniFile.Create(IniFilename);
  try
    i:=0;
    repeat
      s:=Ini.ReadString(SectKey,'Item_'+IntToStr(i),'');
      if s<>'' then AList.Add(s);
      Inc(i);
    until (s='');
  finally
    FreeAndNil (ini);
  end;
end;

initialization
  IniFilename:=ChangeFileExt(paramstr(0),'.ini');

end.

