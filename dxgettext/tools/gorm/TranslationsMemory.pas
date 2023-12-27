{*------------------------------------------------------------------------------
  This unit have contain class to provide translations memory
  When user work with different languages - memory file created for each of them
  When this option is enabled - application stores all translation to memory file after saving
  When user opens item - application searches suggestions in memory and proposes to user
  Suggestions are sorted by similarity criteria - and if that is equals - by the date of store
  When memory file become larger then user-specified size (10 Mb by default) - 50% of oldest items are removed

  Typical format of item in memory file:
  # TRMEM: <fn>c:\gorm\test1.po</fn><dt>10-01-2011 15:35:09</dt><tr> (automatisk)</tr>
  # TRMEM: <fn>c:\gorm\test2.po</fn><dt>10-01-2011 16:38:12</dt><tr> (automatisk)</tr>
  # TRMEM: <fn>c:\gorm\test4.po</fn><dt>10-01-2011 15:40:58</dt><tr> (automatisk)</tr>
  msgid " (automatic)"
  msgstr ""

  <fn>...</fn>  is a name of file where translation had saved
  <dt>...</dt>  is a translation save date time
  <tr>...</tr>  is a translation string

  !Important: Every entry can store lots of memory store stamps.

-------------------------------------------------------------------------------}

unit TranslationsMemory;

interface

uses
  Classes, SysUtils, poparser, Generics.Collections, Generics.Defaults,
  StrUtils, Messages, Windows, u_Languages;

const
  // this signature help to handle translations memory list
  TRMEM_SIGNATURE = '# TRMEM: ';
  // different countries had different local datetime settings, so inner format must be frozen to avoid convert exception
  TRMEM_DATETIME_FORMAT = 'dd-mm-yyyy hh:mm:ss';

type

  // temporary structure to handle suggestions and make them easy to sort
  TSuggestion = record
    translation : String;
    filepath    : String;
    timestamp   : TDateTime;
    goal_dist   : Integer;
  end;

  TTranslationsMemory = class
    private
      // memory map of oldest translations
      FMemList: TPoEntryList;

      // current language
      FLanguageCode: string;

      // memory file name
      FFilename : string;

      // list of suggestions (builded automatically)
      FSuggList : TList<TSuggestion>;

      // the criteria to suggestions search (specified by user)
      FSuggestionAccuracy : Integer;

      // this flag show the necessity to reduce memory file size
      FReduceMemoryFileOnSave : Boolean;

      // POList usually raise an exception if it contains duplicates. This function help to avoid this.
      function TranslationExists(trans : String; poe : TPOEntry) : boolean;

      // Reduces the memory filesize by removing 50% of oldest entries
      procedure MaintainMemoryFile;
      procedure SetSuggestionAccuracy(_a : Integer);
      procedure SetLanguageCode( const _s : string);

    public

      constructor Create;
      destructor Destroy; override;


      // Returns sorted string list with possible translate's suggestions
      function FindSuggestions(const string2translate : String) : TStringList;

      // Clear all translation memory entries
      procedure ClearTranslationsMemory;

      // Add new item to translations memory (yours truly, C.O.)
      procedure AddItemToTranslationsMemory(poe : TPOEntry; filename : String; timestamp : TDateTime);

      // Load translation memory entries from currently defined file
      procedure LoadMemoryFile;

      // Save translation memory entries to currently defined file
      procedure SaveMemoryFile;

      property aSuggestionAccuracy:Integer read FSuggestionAccuracy write SetSuggestionAccuracy;
      property aLanguage:string read FLanguageCode write SetLanguageCode;
  end;


implementation

uses
  Forms, AppSettings, gnugettext;

{*------------------------------------------------------------------------------
  Takes a text between specified tags

  example of call:
  str := '# TRMEM: <fn>c:\gorm\test1.po</fn><dt>10-01-2011 15:35:09</dt><tr> (automatisk)</tr>'
  opentag = '<fn>'
  closetag = '</fn>'
  will return 'c:\gorm\test1.po'

  @param str String is the source
  @param opentag String is the open tag
  @param closetag String is the close tag
  @return String with text between opentag and closetag
-------------------------------------------------------------------------------}
function GetTextBetweenTags(str, opentag, closetag : string) : string;
begin
  result := copy(str,pos(opentag,str)+length(opentag),pos(closetag,str)-pos(opentag,str)-length(opentag));
end;

{*------------------------------------------------------------------------------
  Split memory file string to sugestion data structure

  example of call:
  s := # TRMEM: <fn>c:\gorm\test1.po</fn><dt>10-01-2011 15:35:09</dt><tr> (automatisk)</tr>
  will return sugestion (st)
  st.translation = ' (automatisk)'
  st.filepath = 'c:\gorm\test1.po'
  st.timestamp = 10-01-2011 15:35:09

  @param s_origin String is the source
  @return TSuggestion with data from string s
-------------------------------------------------------------------------------}

function GetSuggestion(s : String) : TSuggestion;
var absFormat : TFormatSettings;
begin
  absFormat.TimeSeparator := ':';
  absFormat.DateSeparator := '-';
  absFormat.ShortDateFormat := 'dd-mm-yyyy';
  absFormat.ShortTimeFormat := 'hh:mm:ss';

  Result.translation := copy(s,pos('<tr>',s)+4,pos('</tr>',s)-pos('<tr>',s)-4);
  Result.filepath := copy(s,pos('<fn>',s)+4,pos('</fn>',s)-pos('<fn>',s)-4);
  Result.timestamp := StrToDateTimeDef(copy(s,pos('<dt>',s)+4,pos('</dt>',s)-pos('<dt>',s)-4), now, absFormat);
end;


{*------------------------------------------------------------------------------
  Constructor for translation memory object
  Creates SuggList - a list of TSuggestion with /similarity -> datetime/ comparator

  @return TTranslationsMemory object
-------------------------------------------------------------------------------}

constructor TTranslationsMemory.Create;
var  Comparer: IComparer<TSuggestion>;
begin
  inherited Create;

  if not(Assigned(FMemList))
    then FMemList := TPoEntryList.Create;
  FSuggestionAccuracy := GetSettingMemorySuggestionsAccuracy;
  FReduceMemoryFileOnSave := false;

  Comparer := TDelegatedComparer<TSuggestion>.Create(
  function(const sugg1, sugg2: TSuggestion): Integer
  begin
    if sugg1.goal_dist <> sugg2.goal_dist
      then Result := sugg1.goal_dist - sugg2.goal_dist
      else Result := trunc(86400 * (sugg1.timestamp - sugg2.timestamp)); // get 1 sec accuracy
  end);
  if NOT (Assigned(FSuggList))
    then FSuggList := TList<TSuggestion>.Create(Comparer);
end;

{*------------------------------------------------------------------------------
  Desctructor for translation memory object
-------------------------------------------------------------------------------}

destructor TTranslationsMemory.Destroy;
begin
  FreeAndNil(FMemList);
  FSuggList.Clear;
  FreeAndNil(FSuggList);
  inherited;
end;

procedure TTranslationsMemory.SetSuggestionAccuracy(_a : Integer);
begin
  FSuggestionAccuracy := _a;
end;

procedure TTranslationsMemory.SetLanguageCode( const _s : string);
var
  lLanguageName: String;
begin
  lLanguageName := '';

  FLanguageCode := _s;

  if not dxlanguages.TryGetLanguageForCode( FLanguageCode, lLanguageName) then
  begin
    FLanguageCode := '';
    lLanguageName := '';
  end;

  FFilename :=  IncludeTrailingPathDelimiter( ExtractFileDir( Application.Exename)) +
                'gormmemory-' + trim( lowercase( lLanguageName)) + '.po';
end;

{*------------------------------------------------------------------------------
  @return minimum value from a b c
-------------------------------------------------------------------------------}
function min3(a, b, c: integer): integer;
begin
  Result := a;
  if b < Result then Result := b;
  if c < Result then Result := c;
end;

{*------------------------------------------------------------------------------
  Returns Levenshtein distance from one string to another.
  The Levenshtein distance is a metric for measuring the amount of difference between two sequences (i.e. an edit distance).

  For example, the Levenshtein distance between "kitten" and "sitting" is 3,
  since the following three edits change one into the other, and there is no way to do it with fewer than three edits:
    kitten > sitten (substitution of 'k' with 's')
    sitten > sittin (substitution of 'e' with 'i')
    sittin > sitting (insert 'g' at the end).
  more info: http://en.wikipedia.org/w/index.php?title=Levenshtein_distance

  @param s, t is the strings to compare
  @return integer value of dictance between s and t
-------------------------------------------------------------------------------}

function FuzzyCompare(s, t: string): integer;
const
  MAX_BUFFER_LENGTH = 255; // maxlength
var
  i, j, m, n: integer;
  cost: integer;
  flip: boolean;
  buf: array[0..((MAX_BUFFER_LENGTH * 2) - 1)] of integer; // buffer

begin
  s := copy(s, 1, MAX_BUFFER_LENGTH - 1);
  t := copy(t, 1, MAX_BUFFER_LENGTH - 1);
  m := length(s);
  n := length(t);
  if m = 0
    then Result := n
    else if n = 0
      then  Result := m
        else begin
          flip := false;
          for i := 0 to n do buf[i] := i;
          for i := 1 to m do
            begin
              if flip then buf[0] := i else buf[MAX_BUFFER_LENGTH] := i;
              for j := 1 to n do
                begin
                  if s[i] = t[j] then cost := 0 else cost := 1;
                  if flip
                    then buf[j] := min3((buf[MAX_BUFFER_LENGTH + j] + 1), (buf[j - 1] + 1), (buf[MAX_BUFFER_LENGTH + j - 1] + cost))
                    else  buf[MAX_BUFFER_LENGTH + j] := min3((buf[j] + 1), (buf[MAX_BUFFER_LENGTH + j - 1] + 1), (buf[j - 1] + cost));
                end;
              flip := not flip;
            end;
          if flip then Result := buf[MAX_BUFFER_LENGTH + n] else Result := buf[n];
        end;
end;


{*------------------------------------------------------------------------------
  Removes some special chars from translation memory items
  and reduces the memory filesize by removing 50% of oldest entries if this is neccessary
-------------------------------------------------------------------------------}

procedure TTranslationsMemory.MaintainMemoryFile;
var poe : TPOEntry;
    MedianTime : TDateTime;
    i : Integer;
    TimeStampList : TList<TDateTime>;
    Comparer : IComparer<TDateTime>;
begin

  // first necessarily need to remove #13 #10 entries. Otherwise the memory translation string would be broken
  // And also deletes the unnecessary comments
  poe := FMemList.FindFirst;
  while (poe<>nil) do begin
    if poe.UserCommentList.Count>0 then begin
      i := 0;
      while (i<poe.UserCommentList.Count) do begin

        poe.UserCommentList[i] := StringReplace(poe.UserCommentList[i],#13,'',[rfReplaceAll]);
        poe.UserCommentList[i] := StringReplace(poe.UserCommentList[i],#10,'',[rfReplaceAll]);

        if pos(TRMEM_SIGNATURE, poe.UserCommentList[i])=0
          Then poe.UserCommentList.Delete(i);
        inc(i);
      end;
    end;
    poe.AutoCommentList.Clear;
    poe := FMemList.FindNext(poe);
  end;

  // Optional - if memory file become too big
  if FReduceMemoryFileOnSave Then begin

    // We need to find the median timestamp for every translation.
    // Let's create a TDateTime List with ascending sorting then sort it -
    // and the middle index will give us the median timestamp value
    Comparer := TDelegatedComparer<TDateTime>.Create(
    function(const date1, date2: TDateTime): Integer
    begin
      Result := trunc(86400 * (date1 - date2)); // get 1 sec accuracy
    end);

    if NOT (Assigned(TimeStampList))
      then TimeStampList := TList<TDateTime>.Create(Comparer);

    // fill the TDateTime List with data fetched from memory items list
    try
      poe := FMemList.FindFirst;
      while (poe<>nil) do begin
        if poe.UserCommentList.Count>0 then
          for i := 0 to poe.UserCommentList.Count - 1 do
            if pos(TRMEM_SIGNATURE,poe.UserCommentList[i])<>0
              Then TimeStampList.Add(GetSuggestion(poe.UserCommentList.Strings[i]).timestamp);
        poe := FMemList.FindNext(poe);
      end;

      // Sort the list
      TimeStampList.Sort;

      // Finding 50% median value
      if odd(TimeStampList.Count)
        then MedianTime := TimeStampList.Items[trunc(TimeStampList.Count / 2)]
        else MedianTime := (TimeStampList.Items[trunc(TimeStampList.Count / 2) -1] + TimeStampList.Items[trunc(TimeStampList.Count / 2)]) / 2;

      // Removes all translation entries that are older of median timestamp
      poe := FMemList.FindFirst;
      while (poe<>nil) do begin
        if poe.UserCommentList.Count>0 then begin
          i := 0;
          while (i<poe.UserCommentList.Count) do begin
            if pos(TRMEM_SIGNATURE,poe.UserCommentList[i])<>0
              Then if GetSuggestion(poe.UserCommentList.Strings[i]).timestamp < MedianTime
                Then poe.UserCommentList.Delete(i);
            inc(i);
          end;
        end;
        poe := FMemList.FindNext(poe);
      end;

    finally
      FreeAndNil(poe);
      FreeAndNil(TimeStampList);
      FReduceMemoryFileOnSave := false;
    end;

  end;
end;

{*------------------------------------------------------------------------------
  Loads all translation entries from specified file and set the Reduce-size flag if neccessary
-------------------------------------------------------------------------------}

procedure TTranslationsMemory.LoadMemoryFile;
var f : Text;
    CurrentFileSize : Integer;
begin
  try
    if FileExists(FFilename) then begin

      // Define memory file size (in Megabytes)
      FMemList.LoadFromFile(FFilename);
      AssignFile(f,FFilename); Reset(f);
      CurrentFileSize := trunc (filesize(f) * 128/1048576); // size in MBytes
      CloseFile(f);

      // If the file is too big - clean it when next save
      if CurrentFileSize > GetSettingMemoryFileSize
        Then FReduceMemoryFileOnSave := true;
    end;
  except
    raise Exception.Create(_('Translations memory file is corrupted'));
    raise;
  end;
end;

{*------------------------------------------------------------------------------
  Saves all translation entries to specified file and set the haeder if the file don't exists
-------------------------------------------------------------------------------}
procedure TTranslationsMemory.SaveMemoryFile;
var poe : TPOEntry;
begin
  if (NOT FileExists(FFilename))
    then begin
      poe := TPOEntry.Create;
      try
        poe.UserCommentList.Clear;
        poe.AutoCommentList.Clear;
        poe.AutoCommentList.Add('# GORM translation memory file');
        poe.MsgStr := 'MIME-Version: 1.0' + #13#10 + 'Content-Type: text/plain; charset=UTF-8' + #13#10 + 'Content-Transfer-Encoding: 8bit' + #13#10;
        FMemList.Add(poe);
      finally
        FreeAndNil (poe);
      end;
    end;
  MaintainMemoryFile;
  try
    FMemList.SaveToFile( FFilename,
                         False,
                         1024);
  except
    raise Exception.Create(_('Error during memory file saving.'));
  end;
end;

{*------------------------------------------------------------------------------
  POList usually raise an exception if it contains duplicates. This function help to avoid this.
  It checks possible translation through all memory item sugestions

  @param trans String with possible translation
  @param poe TPOEntry that we need to check
  @return True if even one of @poe translations memory strings are contains @trans
-------------------------------------------------------------------------------}
function TTranslationsMemory.TranslationExists(trans : String; poe : TPOEntry) : boolean;
  var i : Integer;
begin
  result := false;
  if poe.UserCommentList.Count > 0 then
    For i := 0 to poe.UserCommentList.Count - 1 do
      if pos(TRMEM_SIGNATURE,poe.UserCommentList[i])<>0
        then if GetSuggestion(poe.UserCommentList.Strings[i]).translation = trans
          Then Result := true;
end;


{*------------------------------------------------------------------------------
  Adds new formatted string to the translations memory list.

  @param poe - Incoming TPOEntry
  @param filename - the name of file in memory stamp
  @param timestamp - the datetime memory stamp
-------------------------------------------------------------------------------}
procedure TTranslationsMemory.AddItemToTranslationsMemory(poe : TPOEntry; filename : String; timestamp : TDateTime);
var filetime, CommentString : string;
    po  : TPoEntry;
    i : integer;
begin
  if (trim(poe.MsgId)='') OR (trim(poe.MsgStr)='') then Exit; // don't store empty values in translations memory
  DateTimeToString(filetime, TRMEM_DATETIME_FORMAT, timestamp);
  po := FMemList.Find(poe.MsgId);
  CommentString := format(TRMEM_SIGNATURE + '<fn>%s</fn><dt>%s</dt><tr>%s</tr>',[filename, filetime, poe.MsgStr]);

  if po=nil
    Then begin
      // if this entry is new - add full new entry
      FMemList.Add(poe);
      po := FMemList.Find(poe.MsgId);
      po.UserCommentList.Add(CommentString);
    end
    else
      if not TranslationExists(poe.MsgStr, po)
        then po.UserCommentList.Add(CommentString) // if this translation not exists yet - adds it
        else // if the same translation exists - just updates its timestamp.
          for i := 0 to po.UserCommentList.Count - 1 do
            if (GetTextBetweenTags(po.UserCommentList.Strings[i],'<tr>','</tr>') = poe.MsgStr)
              then po.UserCommentList.Strings[i] := CommentString;
end;

{*------------------------------------------------------------------------------
  Clears all memory entries
-------------------------------------------------------------------------------}
procedure TTranslationsMemory.ClearTranslationsMemory;
begin
  FMemList.Clear;
end;

{*------------------------------------------------------------------------------
  Finds a list of possible suggestions for string.
  Sorts result list at the matches suitability. For equal values sorted by datetime stamps

  @param string2translate is a string to find a suggestions
  @return sorted StringList with more suitable suggestions
-------------------------------------------------------------------------------}
function TTranslationsMemory.FindSuggestions(const string2translate : String) : TStringList;
var poe : TPOEntry;
    dist, i : Integer;
    Sugg : TSuggestion;
begin
  result := TStringList.Create;
  poe := TPOEntry.Create;
  // Define search accuracy
  // (i.e. 'samle' with  FSuggestionAccuracy=2 will find a 'sample', 'Sample.', 'Sempla' entries
  FSuggestionAccuracy := GetSettingMemorySuggestionsAccuracy;
  try
    poe := FMemList.FindFirst;
    FSuggList.Clear;
    while (poe<>nil) do begin
       dist := FuzzyCompare(poe.MsgId, string2translate);
       if (dist <= FSuggestionAccuracy)
          Then begin
            Sugg.translation := poe.MsgStr;
            Sugg.goal_dist := dist;
            if poe.UserCommentList.Count > 0 Then
              for i := 0 to poe.UserCommentList.Count - 1 do
                if pos(TRMEM_SIGNATURE,poe.UserCommentList[i])<>0
                  Then begin
                    // fetching all data from translation memory entry
                    Sugg := GetSuggestion(poe.UserCommentList.Strings[i]);
                    Sugg.goal_dist := dist;
                    FSuggList.Add(Sugg);
                  end;
          end;
       poe := FMemList.FindNext(poe);
    end;

    // Sorts result list at the matches suitability. For equal values sorted by datetime stamps
    FSuggList.Sort;

    // Makes a result list
    for i := 0 to FSuggList.Count-1 do
      result.Add(FSuggList[i].translation);
  finally
    FreeAndNil(poe);
  end;
end;

end.