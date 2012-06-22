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
{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

unit ITEUtils;

interface
uses
  SysUtils, Classes, SimpleXML;

type
  { TDFNItem - one item in a DFN file }
  TDFNItem = class
  private
    FOriginalLocale,FTranslateLocale:Cardinal;

    FStripQuotes: boolean;
    FStatus: integer;
    FOrder: integer;
    FLocalize: integer;
    FClose: string;
    FID: string;
    FCurr: string;
    FObjectType: string;
    FComment: string;
    FPrevOrig: string;
    FOrig: string;
    FOpen: string;
    FPrevCurr: string;
    FModified: TDateTime;
    FCreated: TDateTime;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetCurr: string;
    function GetOrig: string;
    function GetPrevCurr: string;
    function GetPrevOrig: string;
  public
    constructor Create(const ADFNString: string; OriginalLocale,TranslateLocale:Cardinal; AStripQuotes: boolean = false);
    property AsString: string read GetAsString write SetAsString;
    property ID: string read FID write FID;
    property Orig: string read GetOrig write FOrig;
    property Status: integer read FStatus write FStatus;
    property Curr: string read GetCurr write FCurr;
    property PrevOrig: string read GetPrevOrig write FPrevOrig;
    property PrevCurr: string read GetPrevCurr write FPrevCurr;
    property Order: integer read FOrder write FOrder;
    property ObjectType: string read FObjectType write FObjectType;
    property Open: string read FOpen write FOpen;
    property Localize: integer read FLocalize write FLocalize;
    property Close: string read FClose write FClose;
    property Created: TDateTime read FCreated write FCreated;
    property Modified: TDateTime read FModified write FModified;
    property Comment: string read FComment write FComment;
  end;

  { TDFNFile - a collection of DFNItems }
  TDFNFile = class
  private
    FSrcLocale,FTransLocale:Cardinal;
    FFilename: string;
    FItems: TList;
    FStripQuotes: boolean;
    FStartStub: TStrings;
    FStringsOnly: boolean;
    FStripUnused: boolean;
    function GetCount: integer;
    function GetItems(Index: integer): TDFNItem;
    function GetStartStub: TStrings;
  public
    constructor Create(const AFilename: string; AStripQuotes: boolean = false;AStringsOnly: boolean = false;AStripUnused: boolean = true);
    destructor Destroy; override;
    property StripQuotes: boolean read FStripQuotes write FStripQuotes default false;
    property StringsOnly:boolean read FStringsOnly write FStringsOnly default false;
    property StripUnused:boolean read FStripUnused write FStripUnused default true;
    function Add(const ADFNString: string): TDFNItem;overload;
    function Add(ADFNItem:TDFNItem):integer;overload;
    function IndexOf(const ID:string):integer;
    procedure Delete(Index:integer);overload;
    procedure Delete(Item:TDFNItem);overload;
    // MergeStrings merges individual DFN Strings[i] items into one item
    procedure MergeStrings;

    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure Clear;
    property Filename: string read FFilename;
    property Items[Index: integer]: TDFNItem read GetItems; default;
    property Count: integer read GetCount;
    // contains the initial comments in a normal DFN file
    property StartStub: TStrings read GetStartStub;
  end;

  { TITEXMLItem - one translation in a Translation Repository exported XML file }
  TITEXMLItem = class
  private
    FTranslations: TStrings;
    FOriginal: string;
  public
    constructor Create(AOriginal: string);
    destructor Destroy; override;
    // the original string
    property Original: string read FOriginal;
    // the translations (in same order as the TargetLocales in TDFNFile)
    property Translations: TStrings read FTranslations;
  end;

  { TITEXMLFile - reads an XML file exported from the Translation Repository}
  TITEXMLFile = class
  private
    FItems: TList;
    FCurrentTranslation: TITEXMLItem;
    FBaseLocale: string;
    FTargetLocales: TStrings;
    FSorted: boolean;
    function GetCount: integer;
    function GetItems(Index: integer): TITEXMLItem;
    procedure ParseElem(const AElem: TJvSimpleXMLElem);
    // returns false when it's time to stop parsing
    function ParseLocales(const AElem: TJvSimpleXMLElem): boolean;
  public
    procedure Clear;
    function Add(AOriginal: string): TITEXMLItem;
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    procedure Sort;
    // the locales found in the file
    function IndexOf(const AOriginal:string):integer;
    property BaseLocale: string read FBaseLocale;
    property TargetLocales: TStrings read FTargetLocales;
    procedure LoadLocalesOnly(const Filename: string);
    procedure LoadFromFile(const Filename: string);
    property Items[Index: integer]: TITEXMLItem read GetItems;
    property Count: integer read GetCount;
    property Sorted:boolean read FSorted;
  end;

  TITERCItem = class
  private
    FStatus: integer;
    FPrevCurr: string;
    FOrig: string;
    FCurr: string;
    FID: string;
    FComment: string;
    FPrevOrig: string;
    FModified: TDateTime;
    FCreated: TDateTime;
    FStripQuotes:boolean;
    function GetAsString: string;
    function GetOrig: string;
    function GetPrevCurr: string;
    function GetPrevOrig: string;
    procedure SetAsString(const Value: string);
    function GetCurr: string;
  public
    constructor Create(const ARCString: string; AStripQuotes: boolean = false);
    property AsString: string read GetAsString write SetAsString;
    property ID: string read FID write FID;
    property Orig: string read GetOrig write FOrig;
    property Status: integer read FStatus write FStatus;
    property PrevOrig: string read GetPrevOrig write FPrevOrig;
    property PrevCurr: string read GetPrevCurr write FPrevCurr;

    property Created: TDateTime read FCreated write FCreated;
    property Modified: TDateTime read FModified write FModified;
    property Comment: string read FComment write FComment;
    property Curr:string read GetCurr write FCurr;
  end;

  TITERCFile = class
  private
    FItems:TList;
    FStripQuotes: boolean;
    FFilename: string;
    function GetCount: integer;
    function GetItems(Index: integer): TITERCItem;
  public
    constructor Create(const AFilename: string; AStripQuotes: boolean = false);
    destructor Destroy; override;
    property StripQuotes: boolean read FStripQuotes write FStripQuotes default false;
    function Add(const ARCString: string): TITERCItem;

    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure Clear;
    property Filename: string read FFilename;
    property Items[Index: integer]: TITERCItem read GetItems; default;
    property Count: integer read GetCount;
  end;

implementation
uses
  Windows, D5Compat;

function DoStripQuotes(const S:String;Ch:Char):string;
begin
  if (Length(S) > 0) and (S[1] = Ch) and (S[Length(S)] = Ch) then
    Result := Copy(S,2,Length(S) -2)
  else
    Result := S;
end;

function MyFloatToStr(Value:Extended):string;
begin
  Str(Value,Result);
end;

function MyStrToFloat(const S:string):Extended;
var C:integer;
begin
  Val(S,Result,C);
  if C <> 0 then
    Result := 0.0;
end;

function MyStrToFloatDef(const S:String;Default:Extended):Extended;
var C:integer;
begin
  Val(S,Result,C);
  if C <> 0 then
    Result := Default;
end;

procedure StrTokenize(const S: string; Delimiters: TSysCharSet;
  List: TStrings; MinLength: integer = 1; QuoteChar:char='"');
var
  i, j: integer;
  tmp: string;
  InQuote:boolean;
begin
  j := 1;
  InQuote := false;
  for i := 1 to Length(S) do
  begin
    if (S[i] in Delimiters) and not InQuote then
    begin
      tmp := Copy(S, j, i - j);
      if Length(tmp) >= MinLength then
        List.Add(tmp);
      j := i + 1;
    end
    else if S[i] = QuoteChar  then
    begin
      if not InQuote then
      begin
        InQuote := true;
        j := i + 1;
      end
      else
      begin
        InQuote := false;
        tmp := Copy(S, j, i - j);
        if Length(tmp) >= MinLength then
          List.Add(tmp);
      end;
    end;
  end;
  if j <= Length(S) then
  begin
    tmp := Copy(S, j, MaxInt);
    if Length(tmp) >= MinLength then
      List.Add(tmp);
  end;
end;

{ TDFNItem }

constructor TDFNItem.Create(const ADFNString: string; OriginalLocale,TranslateLocale:Cardinal;AStripQuotes: boolean = false);
begin
  inherited Create;
  FStripQuotes := AStripQuotes;
  FOriginalLocale := OriginalLocale;
  FTranslateLocale := TranslateLocale;
  AsString := ADFNString;
end;

function TDFNItem.GetAsString: string;
begin
  Result :=
    Format(
    '/* %s'#4'Orig:%s'#4'Status:%d'#4'Curr:%s'#4'PrevOrig:%s'#4 +
    'PrevCurr:%s'#4'Order:%d'#4'Type:%s'#4'Open:%s'#4'Localize:%d'#4 +
    'Close:%s'#4'Created:%s'#4'Modified:%s'#4'Comment:%s'#4' */',
    [ID, FOrig, FStatus, FCurr, FPrevOrig, FPrevCurr, FOrder, FObjectType, FOpen, FLocalize,
    FClose, MyFloatToStr(FCreated), MyFloatToStr(FModified), Comment]);
end;

function TDFNItem.GetCurr: string;
begin
  Result := FCurr;
  if FStripQuotes then
    Result := DoStripQuotes(Result,#39);
end;

function TDFNItem.GetOrig: string;
begin
  Result := FOrig;
  if FStripQuotes then
    Result := DoStripQuotes(Result,#39);
end;

function TDFNItem.GetPrevCurr: string;
begin
  Result := FPrevCurr;
  if FStripQuotes then
    Result := DoStripQuotes(Result,#39);
end;

function TDFNItem.GetPrevOrig: string;
begin
  Result := FPrevOrig;
  if FStripQuotes then
    Result := DoStripQuotes(Result,#39);
end;

procedure TDFNItem.SetAsString(const Value: string);
var
  S: TStringlist;

  function StripToColon(const S: string): string;
  begin
    Result := trim(Copy(S, Pos(':', S) + 1, MaxInt));
  end;
begin
  S := TStringlist.Create;
  try
    strTokenize(Value, [#4], S, 0,#0);
    if S.Count < 14 then
      raise Exception.CreateFmt('Invalid DFN string: %s', [Value]);
    // remove first "/* "
    ID := StringReplace(S[0],'/* ID:','',[]);
    Orig := StringReplace(S[1],'Orig:','',[]);
    Status := StrToIntDef(StringReplace(S[2],'Status:','',[]), 0);
    Curr := StringReplace(S[3],'Curr:','',[]);
    PrevOrig := StringReplace(S[4],'PrevOrig:','',[]);
    PrevCurr := StringReplace(S[5],'PrevCurr:','',[]);
    Order := StrToIntDef(StringReplace(S[6],'Order:','',[]), 0);
    ObjectType := StringReplace(S[7],'Type:','',[]);
    Open := StringReplace(S[8],'Open:','',[]);
    Localize := StrToIntDef(StringReplace(S[9],'Localize:','',[]), 0);
    Close := StringReplace(S[10],'Close:','',[]);
    Created := MyStrToFloatDef(StringReplace(S[11],'Created:','',[]), 0);
    Modified := MyStrToFloatDef(StringReplace(S[12],'Modified:','',[]), 0);
    Comment := StringReplace(S[13],'Comment:','',[]);
  finally
    S.Free;
  end;
end;

{ TDFNFile }

function TDFNFile.Add(const ADFNString: string): TDFNItem;
var i:integer;
begin
  Result := TDFNItem.Create(ADFNString, FSrcLocale, FTransLocale, StripQuotes);
  i := Add(Result);
  if (StringsOnly and ((Copy(Result.Orig, 1, 1) <> #39))) or
    (StripUnused and (Result.Status = 3)) then
  begin
    Delete(i);
    Result := nil;
  end;
end;

function TDFNFile.Add(ADFNItem: TDFNItem): integer;
begin
  Result := FItems.Add(ADFNItem);
end;

procedure TDFNFile.Clear;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    TDFNItem(FItems[i]).Free;
  FItems.Clear;
  FreeAndNil(FStartStub);
end;

constructor TDFNFile.Create(const AFilename: string; AStripQuotes: boolean = false;AStringsOnly: boolean = false;AStripUnused: boolean = true);
begin
  inherited Create;
  FItems := TList.Create;
  FStripQuotes := AStripQuotes;
  FStringsOnly := AStringsOnly;
  FStripUnused := AStripUnused;
  LoadFromFile(AFilename);
end;

procedure TDFNFile.Delete(Index: integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

procedure TDFNFile.Delete(Item: TDFNItem);
var i:integer;
begin
  i := FItems.IndexOf(Item);
  if i > -1 then
    Delete(i);
end;

destructor TDFNFile.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TDFNFile.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TDFNFile.GetItems(Index: integer): TDFNItem;
begin
  if (Index < 0) or (Index >= FItems.Count) then
    raise Exception.CreateFmt('Index (%d) out of bounds', [Index]);
  Result := TDFNItem(FItems[Index]);
end;

function TDFNFile.GetStartStub: TStrings;
begin
  if FStartStub = nil then
    FStartStub := TStringlist.Create;
  Result := FStartStub;
end;

function TDFNFile.IndexOf(const ID: string): integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiSameText(Items[Result].ID,ID) then
      Exit;
  Result := -1;
end;

procedure TDFNFile.LoadFromFile(const Filename: string);
var
  S,T: TStringlist;
  i: integer;
begin
  Clear;
  if not FileExists(Filename) then
    Exit;
  S := TStringlist.Create;
  T := TStringlist.Create;
  try
    S.LoadFromFile(Filename);
    for i := 0 to S.Count - 1 do
      if Pos('/*ITE:', S[i]) = 1 then
      begin
        // format of "ITE header":
        // /*ITE: <version> <date> <date> <date> <int> <int> <SrcLocale> <TransLocale> "<original dfm filename>"
        //        0         1      2      3      4     5     6           7              8
        if i = 0 then
        begin
          strTokenize(Copy(S[i],7,Length(S[i])-9),[' '],T,0,#0);
          if T.Count >= 9 then
          begin
            FSrcLocale := StrToIntDef(T[6],GetThreadLocale);
            FTransLocale := StrToIntDef(T[7],GetThreadLocale);
          end;
        end;
        // save "ITE" comments
        StartStub.Add(S[i])
      end
      else
        Add(S[i]);
  finally
    S.Free;
    T.Free;
  end;
  FFilename := Filename;
end;

procedure TDFNFile.MergeStrings;
var i,j:integer;tmp:string;S:TStringlist;D,D2:TDFNItem;
begin
  S := TStringlist.Create;
  D := nil;
  try
  for i := 0 to Count - 1 do
    if Pos(']',Items[i].ID) = Length(Items[i].ID) then
    begin
      j := Pos('[',Items[i].ID);
      tmp := Copy(Items[i].ID,1,j-1);
      if tmp <> '' then
        S.AddObject(tmp,Items[i]);
    end;
  tmp := '';
  for i := 0 to S.Count - 1 do
  begin
    D2 := TDFNItem(S.Objects[i]);
    if not AnsiSameText(tmp,S[i]) then
    begin
      if D <> nil then
        D.FOrig := #39 + D.FOrig + #39;
      D := Add(D2.AsString);
      tmp := S[i];
      D.ID := tmp;
      D.Orig := '';
    end;
    if D.Orig = '' then
    begin
      D.FOrig := DoStripQuotes(D2.FOrig,#39) + '\n';
      D.FCurr := DoStripQuotes(D2.FCurr,#39)  + '\n';
      D.FPrevCurr := DoStripQuotes(D2.FPrevCurr,#39)  + '\n';
      D.FPrevOrig := DoStripQuotes(D2.FPrevOrig,#39)  + '\n';
    end
    else
    begin
      D.FOrig := D.FOrig + DoStripQuotes(D2.FOrig,#39)  + '\n';
      D.FCurr := D.FCurr + DoStripQuotes(D2.FCurr,#39)  + '\n';
      D.FPrevCurr := D.FPrevCurr + DoStripQuotes(D2.FPrevCurr,#39)  + '\n';
      D.FPrevOrig := D.FPrevOrig + DoStripQuotes(D2.FPrevOrig,#39)  + '\n';
    end;
    Delete(D2);
  end;
  if D <> nil then
    D.FOrig := #39 + D.FOrig + #39;
  finally
    S.Free;
  end;
end;

procedure TDFNFile.SaveToFile(const Filename: string);
var
  S: TStringList;
  i: integer;
begin
  S := TStringlist.Create;
  try
    if FStartStub <> nil then
      S.AddStrings(FStartStub);
    for i := 0 to Count - 1 do
      S.Add(Items[i].AsString);
    S.SaveToFile(Filename);
    FFilename := Filename;
  finally
    S.Free;
  end;
end;

{ TITEXMLItem }

constructor TITEXMLItem.Create(AOriginal: string);
begin
  inherited Create;
  FOriginal := AOriginal;
  FTranslations := TStringList.Create;
end;

destructor TITEXMLItem.Destroy;
begin
  FTranslations.Free;
  inherited;
end;

{ TITEXMLFile }

function TITEXMLFile.Add(AOriginal: string): TITEXMLItem;
begin
  Result := TITEXMLItem.Create(AOriginal);
  FItems.Add(Result);
  FSorted := false;
end;

procedure TITEXMLFile.Clear;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
  FTargetLocales.Clear;
  FCurrentTranslation := nil;
end;

constructor TITEXMLFile.Create(const AFilename: string);
begin
  inherited Create;
  FItems := TList.Create;
  FTargetLocales := TStringlist.Create;
  if FileExists(AFilename) then
    LoadfromFile(AFilename);
end;

destructor TITEXMLFile.Destroy;
begin
  Clear;
  FTargetLocales.Free;
  FItems.Free;
  inherited;
end;

function TITEXMLFile.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TITEXMLFile.GetItems(Index: integer): TITEXMLItem;
begin
  Result := TITEXMLItem(FItems[Index]);
end;

function XMLBinarySearch(AList: TList; const AOriginal: string; var AIndex: integer): boolean;
var
  L, R, M: integer;
  CompareResult: integer;
begin
  L := 0;
  R := AList.Count - 1;
  while L <= R do
  begin
    M := (L + R) div 2;
    // NB: case-sensitive!!!
    CompareResult := AnsiCompareStr(TITEXMLItem(AList[M]).Original, AOriginal);
    if (CompareResult < 0) then
      L := M + 1
    else if (CompareResult > 0) then
      R := M - 1
    else
    begin
      AIndex := M;
      Result := true;
      Exit;
    end;
  end;
  // not found, should be located here:
  Result := false;
  AIndex := L;
end;

function TITEXMLFile.IndexOf(const AOriginal: string): integer;
begin
  if Sorted then
  begin
    if XMLBinarySearch(FItems,AOriginal,Result) then
      Exit
  end
  else
    for Result := 0 to Count - 1 do
      if AnsiSameStr(Items[Result].Original,AOriginal) then
        Exit;
  Result := -1;
end;

procedure TITEXMLFile.LoadFromFile(const Filename: string);
var
  XML: TJvSimpleXML;
begin
  Clear;
  if not FileExists(Filename) then Exit;
  XML := TJvSimpleXML.Create(nil);
  try
//    XML.EncodeDecode := false;
    XML.LoadFromFile(Filename);
    ParseElem(XML.Root);
  finally
    XML.Free;
  end;
  FSorted := false;
end;

procedure TITEXMLFile.LoadLocalesOnly(const Filename: string);
var
  XML: TJvSimpleXML;
begin
  Clear;
  if not FileExists(Filename) then Exit;
  XML := TJvSimpleXML.Create(nil);
  try
    XML.LoadFromFile(Filename);
    ParseLocales(XML.Root);
  finally
    XML.Free;
  end;
end;

procedure TITEXMLFile.ParseElem(const AElem: TJvSimpleXMLElem);
var
  i: integer;
begin
  if AElem = nil then
    Exit;
  if AnsiSameText(AElem.Name, 'BASE-LOCALE') then
    FBaseLocale := AElem.Value
  else
    if AnsiSameText(AElem.Name, 'TARGET-LOCALE') then
    FTargetLocales.Add(AElem.Value)
  else
    if AnsiSameText(AElem.Name, 'ORIGINAL') then
    FCurrentTranslation := Add(AElem.Value)
  else
    if AnsiSameText(AElem.Name, 'TRANSLATION') and (FCurrentTranslation <> nil) then
    FCurrentTranslation.Translations.Add(AElem.Value);
  for i := 0 to AElem.Items.Count - 1 do
    ParseElem(AElem.Items[i]);
end;

function TITEXMLFile.ParseLocales(const AElem: TJvSimpleXMLElem): boolean;
var
  i: integer;
begin
  Result := true;
  if AElem = nil then
    Exit;
  if AnsiSameText(AElem.Name, 'BASE-LOCALE') then
    FBaseLocale := AElem.Value
  else
    if AnsiSameText(AElem.Name, 'TARGET-LOCALE') then
      FTargetLocales.Add(AElem.Value)
  else
    if AnsiSameText(AElem.Name, 'ORIGINAL') then
  begin // all locales have definitely been read
    Result := false;
    Exit;
  end;
  for i := 0 to AElem.Items.Count - 1 do
    if not ParseLocales(AElem.Items[i]) then
      Exit;
end;

function ITEXMLCompare(Item1,Item2:Pointer):integer;
begin
  Result := AnsiCompareStr(TITEXMLItem(Item1).Original,TITEXMLItem(Item2).Original);
end;

procedure TITEXMLFile.Sort;
begin
  FItems.Sort(ITEXMLCompare);
  FSorted := true;
end;

{ TITERCItem }

constructor TITERCItem.Create(const ARCString: string;  AStripQuotes: boolean);
begin
  inherited Create;
  FStripQuotes := AStripQuotes;
  AsString := ARCString;
end;

function TITERCItem.GetAsString: string;
begin
  Result :=
    Format('/* ID:%s'#4'Orig:%s'#4'Status:%d'#4'Created:%s'#4'Modified:%s'#4'PrevOrig:%s'#4'PrevCurr:%s'#4'Comment:%s'#4' */#13#10  %s %s',
    [ID,FOrig,Status,MyFloatToStr(Created),MyFloatToStr(Modified),
      FPrevOrig,FPrevCurr,FComment, ID, FCurr]);
end;

function TITERCItem.GetCurr: string;
begin
  Result := FCurr;
  if FStripQuotes then
    Result := DoStripQuotes(Result, '"');
end;

function TITERCItem.GetOrig: string;
begin
  Result := FOrig;
  if FStripQuotes then
    Result := DoStripQuotes(Result, '"');
end;

function TITERCItem.GetPrevCurr: string;
begin
  Result := FPrevCurr;
  if FStripQuotes then
    Result := DoStripQuotes(Result, '"');
end;

function TITERCItem.GetPrevOrig: string;
begin
  Result := FPrevOrig;
  if FStripQuotes then
    Result := DoStripQuotes(Result, '"');
end;

procedure TITERCItem.SetAsString(const Value: string);
var S,T:TStringlist;i:integer;
begin
//     1           2             3               4                5                 6                 7                 8
//	/* ID:<string>Orig:<string>Status:<string>Created:<string>Modified:<string>PrevOrig:<string>PrevCurr:<string>Comment:<string> */
//  ExceptDlg_RsScreenRes "Screen: %dx%d pixels, %d bpp"

  S := TStringlist.Create;
  T := TStringlist.Create;
  try
    S.Text := Value;
    if S.Count < 2 then
      raise Exception.CreateFmt('Invalid RC entry:'#13#10'(%s)!',[Value]);
    strTokenize(S[0],[#4],T,0,#0);
//    if T.Count < 8 then
//      raise Exception.CreateFmt('Invalid RC entry:'#13#10'(%s)!',[Value]);
    if T.Count > 0 then
      FID := trim(StringReplace(T[0],'/* ID:','',[]));
    if T.Count > 1 then
      FOrig := StringReplace(T[1],'Orig:','',[]);
    if T.Count > 2 then
      FStatus := StrToIntDef(StringReplace(T[2],'Status:','',[]),0);
    if T.Count > 3 then
      FCreated := MyStrToFloat(StringReplace(T[3],'Created:','',[]));
    if T.Count > 4 then
      FModified := MyStrToFloat(StringReplace(T[4],'Modified:','',[]));
    if T.Count > 5 then
      FPrevOrig := StringReplace(T[5],'PrevOrig:','',[]);
    if T.Count > 6 then
      FPrevCurr := StringReplace(T[6],'PrevCurr:','',[]);
    if T.Count > 7 then
      FComment := StringReplace(T[7],'Comment:','',[]);

    S[1] := trim(S[1]);
    i := Pos(' ',S[1]);
    if i < 1 then
      raise Exception.CreateFmt('Invalid RC entry:'#13#10'(%s)!',[Value]);
    FCurr := Copy(S[1],i+1,MaxInt);
  finally
    T.Free;
    S.Free;
  end;
end;

{ TITERCFile }

function TITERCFile.Add(const ARCString: string): TITERCItem;
begin
  Result := TITERCItem.Create(ARCString,StripQuotes);
  FItems.Add(Result);
end;

procedure TITERCFile.Clear;
begin

end;

constructor TITERCFile.Create(const AFilename: string;
  AStripQuotes: boolean);
begin
  inherited Create;
  FItems := TList.Create;
  FStripQuotes := AStripQuotes;
  if FileExists(AFilename) then
    LoadFromFile(AFilename);
end;

destructor TITERCFile.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TITERCFile.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TITERCFile.GetItems(Index: integer): TITERCItem;
begin
  Result := TITERCItem(FItems[Index]);
end;

procedure TITERCFile.LoadFromFile(const Filename: string);
var i:integer;S:TStringlist;
begin
  Clear;
  S := TStringlist.Create;
  try
    S.LoadFromFile(Filename);
    for i := 0 to S.Count - 1 do
      if (Pos('/* ID:',S[i]) >= 1) and (i < S.Count - 1) then
        Add(S[i] + #13#10 + S[i+1]);
    FFilename := Filename;
  finally
    S.Free;
  end;
end;

procedure TITERCFile.SaveToFile(const Filename: string);
begin
//
end;

end.

