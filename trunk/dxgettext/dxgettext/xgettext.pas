unit xgettext;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl, Jens Berke and            *)
(*        Jacques Garcia Vazquez                                *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dxgettext.po.dk/ for more information            *)
(*                                                              *)
(****************************************************************)

{$ifndef UNICODE}
{$message error 'compiling this unit requires a unicode string aware compiler'}
{$endif}

interface

uses
  Classes, poparser;

type
  {TXExcludeFormClassProperties: represents 1..n properties of a certain class
   that shall be excluded from string extraction in form files. }
  TXExcludeFormClassProperties = class(TCollectionItem)
  private
    FProperties: TStringList;
    FNameOfClass: string;
    procedure SetNameOfClass(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function ExcludeFormClassProperty(aPropertyname: string): boolean;
    procedure AddFormClassProperty(aPropertyname: string);
    property NameOfClass: string read FNameOfClass write SetNameOfClass; // "Classname" already used by TObject => needed other name
  end;

  {TXExcludeFormClassPropertyList: represents a collection of
   TXExcludeFormClassProperties}
  TXExcludeFormClassPropertyList = class(TCollection)
  private
    function GetItems(Index: integer): TXExcludeFormClassProperties;
    procedure SetItem(Index: integer;
      const Value: TXExcludeFormClassProperties);
    function Add: TXExcludeFormClassProperties;
    function AddFormClass(aClassname: string): TXExcludeFormClassProperties;
  public
    function FindItem(aClassname: string): TXExcludeFormClassProperties;
    function ExcludeFormClassProperty(aClassname, aPropertyname: string): Boolean;
    function AddFormClassProperty(aClassPropertyname: string): TXExcludeFormClassProperties;
    property Items[Index: integer]: TXExcludeFormClassProperties read GetItems write SetItem; default;
  end;

  {TXExcludes: holds all information about what shall be excluded from string
   extraction, specified in a "ggexclude.cfg" file }
  TXExcludes = class(TObject)
  private
    FFormClasses: TStringList;
    FFormInstances: TStringList;
    FDirectories: TStringList;
    FFiles: TStringList;
    FBaseDirectory: string;
    FExcludeFormClassPropertyList: TXExcludeFormClassPropertyList;
    FLastErrorMsg: string;
    function GetFullInternalPath(s:string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddDirectory(aDirectory: string): boolean;
    function AddFormFile(aFilename: string): boolean;
    function AddFormClass(aClassname: string): boolean;
    function AddFormClassProperty(aPropertyname: string): boolean;
    function AddFormInstance(aInstanceName: string): boolean;
    function ExcludeDirectory(aDirectory: string): Boolean;
    function ExcludeFormFile(aFilename: string): Boolean;
    function ExcludeFormClass(aClassname: string): Boolean;
    function ExcludeFormClassProperty(aClassname, aPropertyname: string): Boolean; overload;
    function ExcludeFormClassProperty(aClassname: string): Boolean; overload;
    function ExcludeFormInstance(aFilename, aInstanceName: string): boolean;
    function FormClassHasWildcard(aClassname: string): Boolean;
    property BaseDirectory: string read FBaseDirectory write FBaseDirectory;
    property LastErrorMsg: string read FLastErrorMsg;
  end;

  TOnOverwrite = Procedure (sender: TObject; const aFileName: string; var Overwrite: boolean) of object;
  TWarningType=
    (wtGenericWarning, wtUnexpectedException, wtConstantReplaced,
     wtSyntaxError, wtParameterError, wtExtendedDirectiveError, wtNonAscii,
     wtNotImplemented,
     wtExcludeFile);
  TXGTDomain=
    class
    public
      msgid:TStringList;  // Sorted for binary lookups, objects are TItem
      order:TStringList;  // same as msgid, but sorted after occurence. Points to the same objects as msgid, so don't free them!
      constructor Create;
      destructor Destroy; override;
    end;
  TOnProgress=
    procedure (const CurrentTask, CurrentFileName: string;
               const LineNumber: Integer) of object;
  TOnWarning=
    procedure (WarningType: TWarningType; const Msg, Line: string;
               const Filename: string; LineNumber:Integer) of object;
  TXGetText =
    class
    private
      ignorelist: TPoEntryList;
      domainlist: TStringList; // Strings are domain name, values are TXGTDomain
      constlist:TStringList;   // List of consts. Strings are names, Objects are TConst
      definedDomain: string;
      definedContext: string;
      procedure doHandleExtendedDirective (var line: string);
      procedure ClearConstList;
      function GetDomain(const domain: string): TXGTDomain;
      procedure AddTranslation(const domain:string; const aMsgid: string;
        const Comments: string; Location: string; LineNo: integer);
      procedure WriteAll(const Destinationpath:string; const domain: string);
      function MakePathLinuxRelative(const path: string): string;
    private
      resourcestringmode: Integer;  // 0=None, 1=Const, 2=Resourcestring
      CurrentFilename:string;
      LastLineRead:string;
      FLineNr: Integer;
      FLines: TStringList;
      commentmode:string; // Empty means that dxreadln is not inside a comment
      lastcomment:string;
      BaseDirectoryList:TStringList; // Always ends in a pathdelimiter
      BaseDirectory:string;
      Excludes: TXExcludes;
      procedure WritePoFiles (const DestinationPath: string);
      procedure Warning (WarningType:TWarningType;const msg:string); overload;
      procedure Warning (WarningType:TWarningType;const Msg,Line:string;const Filename:string;LineNumber:Integer); overload;
      procedure dxreadln (var line:string; var firstline:boolean; var isutf8:boolean); // same as system.readln, but takes care of comments
      procedure extractstring(var source:string;var res: string);
      function readstring(var line: string; var firstline:boolean; var isutf8:boolean): string; // Reads a pascal ansistring constant
      function isStringObjectPascalFormat(const xMsgId: String): TObjectPascalFormat;
      procedure ExtractFromPascal(const sourcefilename: string);
      procedure ExtractFromDFM(const sourcefilename: string);
      procedure ExtractFromRC(const sourcefilename: string);
      {$ifdef mswindows}
      procedure ExtractFromEXE(const sourcefilename: string);
      {$endif}
      procedure ExtractFromFile(const sourcefilename: string);
      procedure ExtractFromFileMasks(const mask: string);
      procedure ParseExcludeFile;
    public
      // When set, only default domain is written to a file, and this file has it's filename from this variable
      SingleOutputFilename:string;
      
      OnProgress:TOnProgress;
      OnWarning:TOnWarning;
      Recurse:boolean;
      UpdateIgnore:boolean;  // Will create ignore.po if not exists, and put obvious untranslatable items into it
      UseIgnoreFile:boolean; // Will make sure that no item from ignore.po is stored in other files
      AllowNonAscii:boolean;
      OrderbyMsgid:boolean;
      PreserveUserComments: Boolean;
      MaxWidth: integer;
      NoWildcards:boolean;
      LineNumbers: Boolean; // if true (default), line numbers are written as comments
      filemasks:TStringList;
      DestinationPath:string;
      CFiles:TStringList;   // This will contain filenames of C/C++ source files to be scanned elsewhere
      OnOverwrite: TOnOverwrite;
      constructor Create;
      destructor Destroy; override;
      procedure AddBaseDirectory (const path:string);
      procedure AddDelphiFilemasks;
      procedure AddKylixFilemasks;
      procedure HandleIgnores;
      procedure HandleComments;
      procedure Execute;
    end;




implementation

uses
{$ifdef MSWINDOWS}
  Windows, ExeImage, rxtypes,
{$endif}
  SysUtils, Math, appconsts, gnugettext, xgettexttools, Masks,
  ignoredetector, StrUtils;

type
  TConst=
    class
      name:string;
      value:string;
    end;
  EGetText=
    class (Exception)
    end;

const
  cDefineDirective   = '{gnugettext:'; // Start of an option inside the source code
  cScanOption        = 'scan-all';     // Means that all strings in the source code should be extracted
  cDomainDefinition  = 'text-domain';  // Changes default text domain for strings
  cContextDefinition = 'context';      // Sets the context for all subsequent strings
  cScanResetOption   = 'reset';        // Changes back to default behaviour (default domain, no context)

  { consts for exclusion of files, directories, properties and classes from extraction: }
  cExcludeFormInstance = 'exclude-form-instance';
  cExcludeFormClassProperty = 'exclude-form-class-property';
  cExcludeFormClass = 'exclude-form-class';
  cExcludeFile = 'exclude-file';
  cExcludeDir = 'exclude-dir';

function RemoveNuls (const s:string):string;
// Since PluralSplitter is used to separate msgid_plural values inside msgid strings
// in this software, PluralSplitter cannot be present in msgid values. In order to
// prevent this, this function replaces PluralSplitter with '#0'.
var
  p:integer;
begin
  Result:=s;
  while true do begin
    p:=pos(PluralSplitter,Result);
    if p=0 then break;
    Result:=MidStr(Result,1,p-1)+'#0'+MidStr(Result,p+1,maxint);
  end;
end;

procedure TXGetText.extractstring(var source: string; var res: string);
const whitespace=[#0..#32];
// Extracts the Pascal coded string at the beginning of source.
// Returns the result in res.
// Removes the extracted data from source.
var
  charset: set of ansichar;
  s: string;
  constname,uconstname:string;
  idx:integer;
begin
  res := '';
  while source <> '' do begin
    case source[1] of
      '#':
        begin
          if copy(source, 2, 1) = '$' then begin
            s := '$';
            delete(source, 1, 2);
            charset := ['0'..'9', 'a'..'f', 'A'..'F'];
          end else begin
            delete(source, 1, 1);
            s := '';
            charset := ['0'..'9'];
          end;
          while (source <> '') and (ord(source[1])<=255) and (ansichar(ord(source[1])) in charset) do begin
            s := s + source[1];
            delete(source, 1, 1);
          end;
          res := res + widechar(StrToInt(s));
          while (source<>'') and (ord(source[1])<=255) and (ansichar(ord(source[1])) in whitespace) do delete (source,1,1);
          if (length(trim(source))>=2) and (copy(source,1,1)='+') then delete (source,1,1);
        end;
      '''':
        begin
          delete(source, 1, 1);
          while true do begin
            if source = '' then begin
              Warning (wtSyntaxError, _('Single quote detected - string starts but does not end'));
              exit;
            end;
            if copy(source, 1, 1) = '''' then begin
              if copy(source, 2, 1) = '''' then begin
                // Double quote detected
                res := res + '''';
                delete(source, 1, 2);
              end else begin
                // End of text part detected
                delete(source, 1, 1);
                break;
              end
            end else begin
              res := res + copy(source, 1, 1);
              delete(source, 1, 1);
            end;
          end;
        end;
      'a'..'z','A'..'Z','_':
        begin
          constname:='';
          while (source<>'') and (ord(source[1])<=255) and (ansichar(ord(source[1])) in ['a'..'z','A'..'Z','_','0'..'9']) do begin
            constname:=constname+source[1];
            delete (source,1,1);
          end;
          uconstname:=uppercase(constname);
          if constlist.Find(uconstname,idx) then begin
            res:=res+(constlist.Objects[idx] as TConst).value;
          end else
          if uconstname='CRLF' then begin
            res:=res+#10;
            if (resourcestringmode<>1) then
              Warning (wtConstantReplaced,Format(_('CRLF substituted with #10 for %s. Consider to use sLineBreak instead.'),[constname]));
          end else
          if uconstname='SLINEBREAK' then begin
            // Don't make a warning on this one because it is so common
            res:=res+#10;
          end else
          if uconstname='EOF' then begin
            // Don't make a warning on this one because it is so common
            res:=res+#26;
          end else
          if uconstname='EOL' then begin
            // Don't make a warning on this one because it is so common
            res:=res+#10;
          end else
          if (uconstname='DEPRECATED') or (uconstname='PLATFORM') or (uconstname='LIBRARY') then begin
            // The hinting directive was detected and ignored.
          end else
          begin
            if resourcestringmode=1 then // Don't handle consts that don't work
              break;
            Warning (wtGenericWarning,Format(_('Constant %s is not known.'),[constname]));
          end;
        end;
    else
      break;
    end;
    while (source<>'') and (ord(source[1])<=255) and (ansichar(ord(source[1])) in whitespace) do
      delete (source,1,1);
    if (length(trim(source))>=2) and (copy(source,1,1)='+') then
      delete (source,1,1);
    while (source<>'') and (ord(source[1])<=255) and (ansichar(ord(source[1])) in whitespace) do
      delete (source,1,1);
  end;
end;

function TXGetText.readstring(var line: string; var firstline:boolean; var isutf8:boolean): string;
var
  s: string;
  pluscoming:boolean;
begin
  Result := '';
  while true do begin
    if line='' then
      dxreadln(line, firstline, isutf8);
    extractstring(line, s);
    Result := Result + s;
    line := trim(line);
    pluscoming:=(line='');
    if (line='+') or pluscoming then begin
      // This is a multi-line string
      dxreadln(line, firstline, isutf8);
      line := trim(line);
      if pluscoming then begin
        if copy(line,1,1)='+' then begin
          delete (line,1,1);
          line:=trim(line);
        end else begin
          if resourcestringmode<>1 then
            Warning (wtSyntaxError,_('This line is not connected with the previous line using a plus (+).'));
          break;
        end;
      end;
    end else
      break;
  end;
end;

function TXGetText.MakePathLinuxRelative (const path:string):string;
var
  baselen:integer;
begin
  baselen:=length(BaseDirectory);
  {$ifdef MSWINDOWS}
  if uppercase(copy(path,1,baselen))=uppercase(BaseDirectory) then begin
    Result:=copy(path,baselen+1,maxint);
  end else begin
    Result:=copy(path,3,maxint);
  end;
  Result:=WindowsPathDelim2LinuxPathDelim(Result);
  {$endif}
  {$ifdef LINUX}
  if copy(path,1,baselen)=BaseDirectory then begin
    Result:=copy(path,baselen+1,maxint);
  end else begin
    Result:=path;
  end;
  {$endif}
end;

procedure TXGetText.ExtractFromPascal(const sourcefilename: string);
  procedure ApplyContext(var msgid: string);
  var
    contextPos: Integer;
    contextStr: string;
    contextValue: string;
  begin
    contextPos := Pos(cContextDefinition+'=', lastcomment);
    if contextPos > 0 then begin
      contextStr := Copy(lastcomment, contextPos + length (cContextDefinition) + 1 );
      extractstring(contextstr, contextValue);
      if length(contextValue) > 0 then begin
        delete (lastcomment, contextPos, length (cContextDefinition) + 1 + length(contextValue) + 2);
        lastcomment := trim(lastcomment);
      end;
    end;

    if (length(definedContext) > 0) and (length(contextValue) = 0) then
      contextValue := definedContext;

    if (length(contextValue) > 0) and (Pos(GETTEXT_CONTEXT_GLUE, msgid) <= 0) then
      msgid := contextValue + GETTEXT_CONTEXT_GLUE + msgid;
  end;

  procedure PrepareLastCommentForConst(const constident: string; var msgid: string);
  begin
    ApplyContext(msgid);

    if lastcomment<>'' then
      lastcomment:=lastcomment+sLinebreak;
    lastcomment:=lastcomment+'Programmer''s name for it: '+constident;
  end;
// I didn't have a Pascal parser available when this code was written.
var
  line, uline:string;
  s:string;
  msgid: string;
  p, p2, idx:Integer;
  domain: string;
  co:TConst;
  constident:string;
  idlength,idoffset:integer;
  idplural:boolean;
  idcontext:boolean;
  firstline:boolean;
  isutf8:boolean;
begin
  if lowercase(extractfilename(sourcefilename)) = 'gnugettext.pas' then exit;
  if lowercase(extractfilename(sourcefilename)) = 'gnugettextd5.pas' then exit;
  ClearConstList;

  FLines := TStringList.Create;
  try
    FLines.LoadFromFile(sourcefilename);
    firstline:=True;
    definedDomain := '';
    lastcomment := '';
    resourcestringmode := 0;
    FLineNr := 0;
    while FLineNr < FLines.Count do begin
      dxreadln(line, firstline, isutf8);
      line := trim(line);

      // don't parse lines behind "end."
      if LowerCase(line) = 'end.' then
        Break;

      s := ConvertWhitespaceToSpaces (uppercase(line)) + ' ';

      // This should catch resourcestring start
      if (copy(s, 1, 15) = 'RESOURCESTRING ') then begin
        resourcestringmode := 2;
        delete (line,1,15);
      end;
      if (copy(s, 1, 6) = 'CONST ') then begin
        resourcestringmode := 1;
        delete (line,1,6);
      end;
      // This should catch all ends of resourcestring areas
      if (copy(s, 1, 9) = 'FUNCTION ') or (copy(s, 1, 10) = 'PROCEDURE ') or
        (copy(s, 1, 6) = 'BEGIN ') or (copy(s, 1, 4) = 'VAR ') or
        (copy(s, 1, 5) = 'TYPE ') or
        (copy(s, 1, 12) = 'CONSTRUCTOR ') or (copy(s, 1, 11) = 'DESTRUCTOR ') then
        resourcestringmode := 0;

      if resourcestringmode<>0 then begin
        while true do begin
          line:=trim(line);
          p := pos('''', line);
          if p = 0 then
            break;

          s := trim(copy(line, 1, p - 1));
          // sometimes the strings start with something like #34
          while Copy(s, Length(s) - 2, 1) = '#' do begin
            s := Trim(Copy(s, 1, Length(s) - 3));
          end;
          if copy(s, length(s), 1) = '=' then
          begin
            // Identifier probably precedes the string
            s := trim(copy(s, 1, length(s) - 1));
            if is_identifier(s) then
            begin
              constident := s;
            end
            else
            begin
              constident := '';
            end;
          end
          else
          begin
            //*** when no '=' in line the ident is unknown (maybe a line before...)
            constident := '';
          end;

          delete(line, 1, p - 1);
          // Extract the string
          msgid:=RemoveNuls(readstring(line, firstline, isutf8));
          if resourcestringmode=2 then begin
            if constident<>'' then begin
              PrepareLastCommentForConst(constident, msgid);
            end;
            AddTranslation('default', msgid, lastcomment, sourcefilename, FLineNr);
            lastcomment := '';
          end;
          if constident<>'' then begin
            if constlist.Find(uppercase(constident),idx) then begin
              co:=constlist.Objects[idx] as TConst;
            end else begin
              co:=TConst.Create;
              co.Name:=constident;
              constlist.AddObject(uppercase(co.name),co);
            end;
            co.Value:=msgid;

            // If source-code comments for gnugettext enable it,
            // extract the constant even though it is not a resourcestring.
            if Length (definedDomain) > 0 then begin
              PrepareLastCommentForConst(constident, msgid);
              AddTranslation (definedDomain, msgid, lastcomment, sourcefilename, FLineNr);
              lastcomment := '';
            end;
          end;

          // Check what comes next in the line
          if copy(line, 1, 1) <> ';' then begin
            // First parameter is line number, second is the contents of the line
            if resourcestringmode=2 then
              Warning (wtSyntaxError,_('resourcestring does not end in semicolon.'));
            line:='';
            break;
          end else begin
            // If it ended with a semicolon, analyze the rest of the line as if it was a new line
            delete(line, 1, 1);
          end;
        end;
      end else begin
        // Check for occurence of gettext()
        idcontext:=False;
        while true do begin
          uline:=uppercase(line);
          p:=poscode('_',uline);
          p2:=poscode('GETTEXT', uline);
          if p=0 then begin
            p:=p2;
          end else
            if p2<>0 then
              p:=min(p,p2);
          if p=0 then
            break;
          if (poscode('FUNCTION',uline)<>0) or
             (poscode('PROCEDURE',uline)<>0) then
            break;

          domain := 'default';
          idoffset:=0;
          if copy(uline,p,1)='_' then begin
            idlength:=1;
            idplural:=False;
          end else begin
            idlength:=7;
            if uppercase(copy(line, p - 1, 1)) = 'D' then begin
              domain := '';
              idlength:=8;
              idoffset:=-1;
            end;
            if uppercase(copy(line, p - 2, 2)) = 'DC' then begin
              domain := '';
              idlength:=9;
              idoffset:=-2;
            end;
            idplural:=False;
            if uppercase(copy(line, p - 2, 2)) = 'DN' then begin
              domain := '';
              idlength:=9;
              idoffset:=-2;
              idplural:=True;
            end else
            if uppercase(copy(line, p - 1, 1)) = 'N' then begin
              idlength:=8;
              idoffset:=-1;
              idplural:=True;
            end
            else if (uppercase(copy(line, p + idoffset + idlength, 5)) = '_NOOP') then
            begin
              //*** Implement the extraction for getText_noop (no operation,
              //    only extraction) function
              //    see gettext manual
              //      4.7 - Special Cases of Translatable Strings
              //      http://www.gnu.org/software/hello/manual/gettext/Special-cases.html#Special-cases
              idlength := idlength + 5;
            end;
            idcontext:=False;
            if uppercase(copy(line, p + idoffset - 1, 1)) = 'P' then begin
              Inc(idlength);
              Dec(idoffset);
              idcontext:=True;
            end
          end;
          if ((p + idoffset = 1) or
              (not ((ord(uline[p + idoffset - 1]) <= 255) and
                    (ansichar(ord(uline[p + idoffset - 1])) in ['a'..'z','A'..'Z','_','0'..'9'])))) and
             (length(line) >= p + idlength + idoffset) and
             (not ((ord(uline[p + idoffset + idlength]) <= 255) and
                   (ansichar(ord(uline[p + idoffset + idlength])) in ['a'..'z','A'..'Z','_','0'..'9']))) then
          begin
            line := trim(copy(line, p + idlength+idoffset, maxint));
            if copy(line, 1, 1) = '(' then
            begin
              line := trim(copy(line, 2, maxint));
              if domain = '' then begin
                // get first parameter
                extractstring(line, domain);
                line := trim(line);
                if copy(line, 1, 1) = ',' then begin
                  delete(line, 1, 1);
                  line:=trim(line);
                end else begin
                  // First parameter is line number, second is line contents
                  Warning (wtSyntaxError,_('Missing comma after first parameter'));
                end;
              end;

              // get context from first parameter
              if idcontext then
              begin
                msgid := RemoveNuls(readstring(line, firstline, isutf8))+GETTEXT_CONTEXT_GLUE;
                if copy(line, 1, 1) = ',' then begin
                  delete(line, 1, 1);
                  line:=trim(line);
                end else begin
                  Warning (wtSyntaxError,_('Missing comma after first parameter'));
                end;
              end
              else
              begin
                msgid := '';
              end;

              // Get parameter that contains the msgid
              msgid := msgid+RemoveNuls(readstring(line, firstline, isutf8));
              if idplural then begin
                line := trim(line);
                if copy(line, 1, 1) = ',' then begin
                  delete(line, 1, 1);
                  line:=trim(line);
                end else begin
                  Warning (wtSyntaxError,_('Missing comma after parameter'));
                end;
                if line='' then
                  dxreadln(line, firstline, isutf8);
                msgid := msgid+PluralSplitter+RemoveNuls(readstring(line, firstline, isutf8));
              end;
              ApplyContext(msgid);
              AddTranslation(domain, msgid, lastcomment, sourcefilename, FLineNr);
              lastcomment := '';
            end { if a parenthesis is found };
          end else begin
            line := trim(copy(line, p + idlength+idoffset, maxint));
          end { if it looks like a function call identifier };
        end { loop that finds several occurences in the same line };
      end { if resourcestringmode };
    end;
  finally
    FreeAndNil(FLines);
  end;

  If length (definedDomain) > 0 then begin
    Warning (wtExtendedDirectiveError, _('$gnugettext: end directive is missing !'));
  end;
end;

constructor TXGetText.Create;
begin
  inherited Create;
  ignorelist:=TPoEntryList.Create;
  CFiles:=TStringList.Create;
  CFiles.Duplicates:=dupError;
  CFiles.CaseSensitive:=True;
  CFiles.Sorted:=True;
  BaseDirectoryList:=TStringList.create;
  filemasks:=TStringList.Create;
  filemasks.Sorted:=True;
  filemasks.Duplicates:=dupIgnore;
  filemasks.CaseSensitive:=True;
  domainlist := TStringList.Create;
  domainlist.Sorted := True;
  domainlist.Duplicates:=dupError;
  domainlist.CaseSensitive:=True;
  constlist:=TStringList.Create;
  constlist.Sorted:=True;
  constlist.Duplicates:=dupError;
  constlist.CaseSensitive:=True;
  Excludes := TXExcludes.Create;
  MaxWidth := 70;
  LineNumbers := True;
end;

destructor TXGetText.Destroy;
begin
  ClearConstList;
  FreeAndNil (constlist);
  while domainlist.Count <> 0 do begin
    domainlist.Objects[0].Free;
    domainlist.Delete(0);
  end;
  FreeAndNil(domainlist);
  FreeAndNil (BaseDirectoryList);
  FreeAndNil (filemasks);
  FreeAndNil (CFiles);
  FreeAndNil (ignorelist);
  FreeAndNil(Excludes);
  inherited;
end;

procedure TXGetText.ExtractFromDFM(const sourcefilename: string);
var
  fs,
  src: TStream;
  mem: TMemoryStream;
  line, lastline:string;
  s: string;
  i:integer;
  indent: integer;
  comment: string;
  p: integer;
  scope: TStringList;
  propertyname: string;
  multilinevalue: boolean;
  mvalue: string;
  p1, p2, p3: integer;
  pClassname: integer;
  c:char;
  classnamepart: string;
  linechar:char;
  currentclassname: string;
  classnames: TStringList;
  instancenames: TStringList;
  excludeclass:boolean;
  excludeinstance:boolean;
  collectionlevel:integer; // will be increased which each occurence of a collection, in order to recognize nested collections
  collectionpropertyname:string; // will be the propertyname of the highest-level collection property

  procedure AddEntry(const aValue:string);
  var
    propname:string;
  begin
    if collectionlevel > 0 then
      propname := collectionpropertyname
    else
      propname := propertyname;
    if not excludeclass and not excludeinstance and not Excludes.ExcludeFormClassProperty(classnames[indent], propname) then begin
      comment := scope2comment(scope, propertyname);
      AddTranslation('default', RemoveNuls(aValue), comment, sourcefilename, FLineNr);
    end;
  end;

begin
  src := TMemoryStream.Create;
  fs  := TFileStream.Create(sourcefilename,fmOpenRead);
  try
    fs.Position := 0;
    src.CopyFrom(fs, fs.Size);  //copy to far more efficient memorystream (no more expensive filereads!)
    src.Position := 0;
  finally
    fs.Free;
  end;
  try
    // Check for empty file
    if src.Read(c,1)=0 then
      exit;

    // Check for binary dfm file
    src.Seek(0, soFromBeginning);
    if c=#$FF then begin
      // Convert the file into text form in a memory stream
      mem:=TMemoryStream.Create;
      ObjectResourceToText(src,mem);
      FreeAndNil (src);
      src:=mem;
    end;
    src.Seek(0,soFrombeginning);

    scope := TStringList.Create;
    classnames := TStringlist.Create;
    instancenames := TStringlist.Create;
    try
      classnames.Add(''); // we need that one because "indent" might start with 0
      instancenames.Add('');
      FLineNr := 0;
      line := '';
      propertyname := '';
      collectionpropertyname := '';
      multilinevalue := false;
      collectionlevel := 0;
      while true do begin
        // Get next line and check it out
        lastline := line;
        if not StreamReadln (src, line, true) then break;
        inc(FLineNr);
        indent := measureindent(line);
        line := trim(line);
        if line='' then continue;  // *** ABORT IF LINE IS EMPTY ***

        // Check if a collection starts or ends in this line.
        // If we have nested collections, the nesting-level
        // will be remembered                                                    
        if RightStr(line, 3) = '= <' then
          inc(collectionlevel);
        if RightStr(lowercase(line), 4) = 'end>' then begin
          dec(collectionlevel);
          if collectionlevel = 0 then
            collectionpropertyname := '';
        end;

        // Always adjust the count of "classnames" to the current "indent"
        // and make sure, the a bigger indent gets the same classname as the
        // smaller indent before. This will be overwritten as soon as we reach
        // an line containing "object", "inherited" or "inline", like this:
        //
        // object Form1: TForm      indent = 0, classname[0] = 'TForm'
        //  Caption = 'Form1'       indent = 1, classname[1] = 'TForm'
        //  object Edit1: TEdit     indent = 1, classname[1] = 'TEdit'
        //   Left = 1;              indent = 2, classname[2] = 'TEdit'
        while indent < classnames.Count-1 do begin
          classnames.Delete(classnames.Count-1);
          instancenames.Delete(instancenames.Count-1);
        end;
        while indent > classnames.Count-1 do begin
          classnames.Add(classnames[classnames.Count-1]);
          instancenames.Add(instancenames[instancenames.Count-1]);
        end;

        // check for occurence of a classname and remember it at the current indention.
        // Take into account that some properties might contain identifiers as part
        // of their name, e.g. "InlineSkaterCount" or "InheritedFromGrandPa"
        if (Pos(':', line) > 0) and ((Pos('object ', lowercase(line)) > 0) or (Pos('inherited ', lowercase(line)) > 0) or (Pos('inline ', lowercase(line)) > 0)) then begin
          pClassname := Pos(':', line);
          if pClassname > 0 then begin
            currentclassname := '';
            classnamepart := Trim(Copy(line, pClassname+1, Length(line)-pClassname+1));
            for i := 1 to Length(classnamepart) do
            begin
              // be aware of constructs like "TScrollbox [0]" or other unlikely things, simply just get only the chars that are valid for classnames
              linechar := classnamepart[i];
              {$ifdef UNICODE} // >=D2009
              if not CharInSet(linechar,['a'..'z','A'..'Z','_','0'..'9']) then
              {$else}
              if not (linechar in ['a'..'z','A'..'Z','_','0'..'9']) then
              {$endif}
                break
              else
                currentclassname := currentclassname + linechar;
            end;
            classnames[indent] := currentclassname;
            // remember the name of instance of that class as well in the same way
            p := Pos(' ', line);
            instancenames[indent] := Copy(line, p +1, pClassname -p -1);
          end;
        end;

        // check if the whole class shall be excluded
        excludeclass := Excludes.ExcludeFormClass(classnames[indent]);
        excludeinstance := false;
        if not excludeclass then begin
          for i := indent downto 0 do // check parent classes if they contain a wildcard
            if Excludes.FormClassHasWildcard(classnames[i]) then begin
              excludeclass := true;
              break;
            end;
          if not excludeclass then begin
            excludeinstance := Excludes.ExcludeFormInstance(sourcefilename, instancenames[indent]);
            if not excludeinstance then begin
              for i := indent downto 0  do
                if Excludes.ExcludeFormInstance(sourcefilename, instancenames[i]) then begin
                  excludeinstance := true;
                  break;
                end;
            end;
          end;
        end;

        // Check for changes in scope
        if (indent < scope.Count) and multilinevalue then begin
          multilinevalue := false;
          AddEntry(mvalue);
          scope.Delete(scope.count - 1);
        end;
        while indent < scope.Count do begin
          scope.Delete(scope.count - 1);
        end;

        if indent > scope.Count then begin
          p := pos(' ', lastline);
          if p = 0 then s := lastline else s := copy(lastline, p + 1, maxint);
          p := pos(':', s);
          multilinevalue := true;
          mvalue := '';
          if p = 0 then s := '' else s := copy(s, 1, p - 1);
        end;
        while indent > scope.Count do begin
          scope.Add(s);
          s := '';
        end;

        // Analyze the line
        p := pos(' =', line);
        p1 := pos('''', line);
        p2 := pos('#', line);
        if p1 = 0 then p1 := maxint;
        if p2 = 0 then p2 := maxint;
        p3 := min(p1, p2);

        // Extract property name if the line contains such one
        if (p <> 0) and (p < p3) then begin
          propertyname := trim(copy(line, 1, p - 1));
          // is we're in a collection (and it's the highest level if there are nested collections), remember the property name of that collection
          if (collectionlevel = 1) and (collectionpropertyname = '') then
            collectionpropertyname := propertyname;
          multilinevalue := false;
        end;

        // Extract string, if the line contains such one
        if p3 <> maxint then begin
          delete(line, 1, p3 - 1);
          extractstring(line, s);
          if multilinevalue then begin
            mvalue := mvalue + s;
            if trim(line) <> '+' then begin
              AddEntry(mvalue);
              mvalue:='';
            end;
          end else begin
            AddEntry(s);
          end;
        end;
      end;
    finally
      FreeAndNil(scope);
      FreeAndNil(classnames);
    end;
  finally
    FreeAndNil (src);
  end;
end;

procedure TXGetText.AddTranslation(const domain:string; const aMsgid: string;
  const Comments: string; Location: string; LineNo: integer);
// Adds something to translate to the list
var
  it: TPoEntry;
  i, e: integer;
  sl: TStringList;
  dom:TXGTDomain;
  smsgid,
  lookupvalue:string;
begin
  Location := MakePathLinuxRelative(Location);
  if LineNumbers then
    Location := Location + ':' + IntToStr(LineNo);

  smsgid := aMsgid;

  // Check, that all parts of msgid are nonempty, if there are multiple parts
  if smsgid<>'' then begin
    for i:=1 to length(smsgid)+1 do begin
      if copy(PluralSplitter+smsgid+PluralSplitter,i,2)=PluralSplitter+PluralSplitter then
        raise Exception.Create('Illegal msgid_plural value: It contained empty strings.');
    end;
  end;

  // Check for non-ascii characters
  if not AllowNonAscii then
  begin
    for i:=1 to length(smsgid) do
    begin
      if ord(smsgid[i])>=128 then
      begin
        Warning (wtNonAscii,format(_('msgid contains non-ascii characters: "%s"'),[smsgid]));
        // Don't add an invalid msgid
        exit;
      end;
    end;
  end;

  // Remove any Carriage Returns
  while true do begin
    i:=pos(#13,smsgid);
    if i=0 then break;
    delete (smsgid,i,1);
  end;

  // Don't add empty strings
  if smsgid = '' then exit;

  // Don't add numbers
  val(smsgid, i, e);
  if (e = 0) and (smsgid = IntToStr(i)) then exit;

  dom:=GetDomain(domain);
  sl:=TStringList.Create;
  try
    sl.Text := smsgid;
    if sl.Count=0 then
      lookupvalue:='Weird, but happens if the string contains weird ascii chars'
    else
      lookupvalue:=sl.Strings[0];
  finally
    FreeAndNil(sl);
  end;
  it:=nil;
  if dom.msgid.Find(lookupvalue,i) then
  begin
    // Scroll back to the first in the list that has the same
    // first line in msgid
    while (i > 0) and
          (dom.msgid.Strings[i - 1] = lookupvalue) do
      dec(i);

    // Now loop through all those in the list it may be
    while true do
    begin
      it := dom.msgid.Objects[i] as TPoEntry;

      // Check if we found the correct one
      if it.msgid = smsgid then
        break;
      // Check if we have scrolled past the last one

      if (i = dom.msgid.Count - 1) or
         (dom.msgid.Strings[i+1] <> lookupvalue) then
      begin
        it := nil;
        break;
      end;

      inc(i);
    end;
  end;

  if it = nil then
  begin
    it := TPoEntry.Create;

    dom.msgid.AddObject(lookupvalue, it);
    it.msgid := smsgid;
    it.IsObjectPascalFormat := IsStringObjectPascalFormat(smsgid);
    dom.order.AddObject(lookupvalue, it);
  end;

  if comments<>'' then
  begin
    sl:=TStringList.Create;
    try
      sl.Text:=comments;
      for i:=0 to sl.Count-1 do begin
        it.AutoCommentList.Add('#. '+sl.Strings[i]);
      end;
    finally
      FreeAndNil (sl);
    end;
  end;

  it.AutoCommentList.Add('#: ' + RemoveFilenameSpaces(Location));
end;

procedure TXGetText.WriteAll(const Destinationpath, domain: string);
// Outputs a .po file
var
  destination: TFileStream;
  i: integer;
  item: TPoEntry;
  dom:TXGTDomain;
  filename: string;
  orderlist:TStrings;
  overwrite: boolean;
begin
  dom:=GetDomain(domain);
  if SingleOutputFilename<>'' then begin
    if domain='default' then begin
      filename:=SingleOutputFilename;
    end else begin
      exit;
    end;
  end else begin
    filename := destinationpath + domain + '.po';
  end;

  // Check for overwriting. Call custom handler if present, and abort if overwriting is not permitted.
  if FileExists (fileName) then begin
    overwrite := True;
    if assigned (OnOverwrite) then OnOverwrite (self, fileName, overwrite);
    if not overwrite then begin
      OnProgress (format (_('Overwrite %s aborted.'), [fileName]), filename, 0);
      Exit;
    end;
  end;

  // %s will be replaced by the filename
  if Assigned(OnProgress) then
    OnProgress (Format(_('Writing %s'),[filename]),filename,0);
  destination:=TFileSTream.Create (filename, fmCreate);
  try
    // Write a dummy header that the user can modify
    StreamWriteDefaultPoTemplateHeader(destination,Format(_('dxgettext %s'),[version]));

    // Write out all msgids
    if OrderbyMsgid then orderlist:=dom.msgid
                    else orderlist:=dom.order;
    for i := 0 to orderlist.Count - 1 do begin
      item := orderlist.Objects[i] as TPoEntry;
      item.WriteToStream(destination, MaxWidth);
    end;
  finally
    FreeAndNil (destination);
  end;
end;

procedure TXGetText.ExtractFromFile(const sourcefilename: string);
var
  ext:string;
begin
  CurrentFilename:=sourcefilename;
  FLineNr:=0;
  if ExpandFileName(CurrentFilename)<>CurrentFilename then
    CurrentFilename:=BaseDirectory+CurrentFilename;
  if Excludes.ExcludeDirectory(ExtractFilePath(CurrentFilename)) or Excludes.ExcludeFormFile(CurrentFilename) then
    Exit;
  try
    ext:=uppercase(ExtractFileExt(CurrentFilename));
    if (ext='.C') or (ext='.CPP') then
      CFiles.Add(CurrentFilename)
    else begin
      if Assigned(OnProgress) then
        OnProgress (Format(_('Reading %s'),[CurrentFilename]),CurrentFilename,0);
      if (ext='.DFM') or (ext='.XFM') or (ext = '.FMX') then
        ExtractFromDFM(CurrentFilename)
      else
      if ext='.RC' then
        ExtractFromRC(CurrentFilename)
      else
{$ifdef mswindows}
      if (ext='.DLL') or (ext='.EXE') or (ext='.BPL') then
        ExtractFromEXE(CurrentFilename)
      else
{$endif}
      if (ext='.PAS') or (ext='.DPR') or (ext='.INC') then
        ExtractFromPascal(CurrentFilename)
      else begin
        Warning (wtParameterError,Format(_('WARNING: Unknown file extension %s. Reading file as being pascal source.'),[ext]));
        ExtractFromPascal(CurrentFilename)
      end;
    end;
  except
    on e:EControlC do begin
      raise;
    end;
    on e:Exception do begin
      Warning (wtUnexpectedException,'Exception '+e.ClassName+sLineBreak+e.Message);
    end;
  end;
  CurrentFilename:='';
end;

procedure TXGetText.ExtractFromFileMasks(const mask:string);
var
  sr: TSearchRec;
  more: boolean;
  sMask,
  curdir:string;
  dirlist:TStringList;
  sl:TStringList;
  i, idx:integer;
  maskcheck:TMask; // This is only necessary because of a bug in the Windows API FindFirst()
begin
  sMask:=ExpandFileName(BaseDirectory+mask);
  dirlist:=TStringList.Create;
  try
    dirlist.Add(ExtractFilePath(sMask));
    sMask := ExtractFileName(sMask);

    if recurse then begin
      idx:=0;
      while idx<dirlist.count do begin
        curdir:=dirlist.Strings[idx];

        // Find all subdirectories
        more := FindFirst(curdir+'*', faAnyFile, sr) = 0;
        while more do begin
          if (sr.Attr and faDirectory<>0) and (sr.Name<>'.') and (sr.Name<>'..') then
            dirlist.Add(curdir+sr.Name+PathDelim);
          more := FindNext(sr) = 0;
        end;
        SysUtils.FindClose (sr);
        inc (idx);
      end;
    end;

    dirlist.Sort;

    for idx:=0 to dirlist.Count-1 do begin
      curdir:=dirlist.Strings[idx];

      maskcheck:=TMask.Create (sMask);
      sl:=TStringList.Create;
      try
        // Extract from all files in current directory
        more := FindFirst(curdir+sMask, faAnyFile-faDirectory, sr) = 0;
        while more do begin
          // The following if is only necessary, because several Windows versions
          // have a bug in FindFirst, that makes "test.cpp,v" match on the
          // file mask "*.cpp"
          if maskcheck.Matches(sr.Name) then
            sl.Add (curdir + sr.Name);
          more := FindNext(sr) = 0;
        end;
        SysUtils.FindClose(sr);
        sl.Sort;
        for i:=0 to sl.count-1 do
          ExtractFromFile(sl.Strings[i]);
      finally
        FreeAndNil (sl);
        FreeAndNil (maskcheck);
      end;
    end;
  finally
    FreeAndNil (dirlist);
  end;
end;

function TXGetText.GetDomain(const domain: string): TXGTDomain;
var
  i: integer;
begin
  if domainlist.Find(domain, i) then begin
    Result := domainlist.Objects[i] as TXGTDomain;
  end else begin
    Result := TXGTDomain.Create;
    domainlist.AddObject(domain, Result);
  end;
end;

procedure TXGetText.dxreadln (var line:string; var firstline:boolean; var isutf8:boolean);
var
  i:integer;

  procedure GetNextLine(var _Line: string);
  begin
    if FLineNr < FLines.Count  then begin
      _Line :=  FLines[FLineNr];
      Inc(FLineNr);
    end else
      _Line := '';
  end;

  procedure cutuntil (const endtag:string);
  var p:integer;
  begin
    p:=i+length(endtag)-1;
    while p<=length(line) do begin
      if copy(line,p,length(endtag))=endtag then begin
        delete (line,i,p+length(endtag)-i);
        exit;
      end;
      inc (p);
    end;
    // At this place, the end tag was not found in the line
    line:=copy(line,1,i-1);
    commentmode:=endtag;
  end;

  procedure GetIt;
  //var
  //  aline:RawByteString;
  begin
   GetNextLine(line);
    //readln(src, aline);
    {
    if firstline then begin
      if copy(line,1,3)=#$EF#$BB#$BF then begin
        delete (line,1,3);
        isutf8:=True;
      end;
      firstline:=False;
    end;
    if isutf8 then begin
      line:= UTF8ToUnicodeString(aline);
    end else
      line:= string(ansistring(aline));
    }
  end;

begin
  line:='';
  //while (not eof(src)) and (line='') do begin
  while (FLineNr < FLines.Count) and
        (line='') do
  begin
    if commentmode<>'' then begin
      while true do begin
        if FLineNr >= FLines.Count then begin
          line:='';
          exit;
        end;
        GetIt;
        line:=trim(line);
        LastLineRead:=line;
        i:=pos(commentmode,line);
        if i<>0 then begin
          delete (line,1,i+length(commentmode)-1);
          commentmode:='';
          break;
        end;
      end;
    end else begin
      GetIt;
      line:=trim(line);
      LastLineRead:=line;
      if line='' then
        lastcomment:='';
    end;
    i:=1;
    while i<=length(line) do begin
      //if copy(line,i,1)='''' then begin
      if (line[i] = '''') then begin
        // A string was detected - find the end of it.
        inc (i);
        while true do begin
          //if copy(line,i,1)='''' then begin
          if (line[i] = '''') then begin
            inc (i);
            break;
          end;
          // If the string doesn't end until the line is over, finish the procedure
          if i>=length(line) then
            exit;
          inc (i);
        end;
      end else
      if (line[i] = '/') and (copy(line,i,2)='//') then
      begin
        // The rest of the line is a comment
        if lastcomment<>'' then
          lastcomment:=lastcomment+sLineBreak;
        lastcomment:=trim(copy(line,i+2,maxint));
        line:=copy(line,1,i-1);
        exit;
      end else
      //if copy(line,i,1)='{' then begin
      if line[i] = '{' then
      begin
        if pos (cDefineDirective, lowercase(copy(line,1,length(cDefineDirective)))) = 1 then
          doHandleExtendedDirective (line);

        // Bracket comment
        cutuntil ('}');
      end else
      if (line[i] = '(') and (copy(line,i,2)='(*') then begin
        // Bracket comment, Danish style
        cutuntil ('*)');
      end else
        inc (i);
    end;
    line := trim(line);
  end;
end;

{ TXGTDomain }

constructor TXGTDomain.Create;
begin
  msgid:=TStringList.Create;
  order:=TStringList.Create;
  msgid.Sorted:=True;
  msgid.Duplicates:=dupAccept;
  msgid.CaseSensitive:=True;
end;

destructor TXGTDomain.Destroy;
begin
  while msgid.count<>0 do begin
    msgid.Objects[0].Free;
    msgid.Delete (0);
  end;
  FreeAndNil (msgid);
  FreeAndNil (order);
  inherited;
end;

procedure TXGetText.WritePoFiles (const DestinationPath:string);
var
  i:integer;
begin
  for i:=0 to domainlist.Count-1 do begin
    // Write all domain.po files
    WriteAll(DestinationPath,domainlist.Strings[i]);
  end;
end;

procedure TXGetText.ClearConstList;
begin
  while constlist.Count<>0 do begin
    constlist.Objects[0].Free;
    constlist.Delete (0);
  end;
end;

procedure TXGetText.Warning(WarningType:TWarningType;const msg: string);
begin
  if Assigned(OnWarning) then
    OnWarning (WarningType,msg,LastLineRead,CurrentFilename,FLineNr);
end;

procedure TXGetText.Warning(WarningType: TWarningType; const Msg,
  Line: string; const Filename:string;LineNumber: Integer);
begin
  if Assigned(OnWarning) then
    OnWarning (WarningType,msg,Line,Filename,linenumber);
end;

procedure TXGetText.ExtractFromRC(const sourcefilename: string);
var
  tf:TextFile;
  aline:ansistring;
  line:string;
  p, i:integer;
  ident:string;
begin
  // Currently, this scanner is based on the RC file that was included
  // with DBISAM version 3. It may not work with other RC files, but
  // if you find an RC file that it does not work with, please send that
  // RC file to Lars@dybdahl.dk
  FileMode:=fmOpenRead;
  AssignFile (tf,sourcefilename);
  Reset (tf);
  try
    FLineNr:=0;
    while not eof(tf) do begin
      // Get next line
      readln (tf,aline);
      line:=string(aline);
      inc (FLineNr);
      line:=trim(line);
      LastLineRead:=line;

      if copy(line,1,1)<>'#' then begin
        p:=pos('"',line);
        if p<>0 then begin
          // Find identifier in the beginning of the line
          ident:=trim(copy(line,1,p-1));
          if copy(ident,length(ident),1)=',' then
            delete (ident,length(ident),1);
          if ident<>'' then
            ident:='Programmer''s name: '+ident;

          // Find the msgid
          delete (line,1,p);
          i:=1;
          while i<=length(line) do begin
            if copy(line,i,2)='\n' then begin
              delete (line,i,1);
              line[i]:=#10;
            end else
            if copy(line,i,2)='\r' then begin
              delete (line,i,1);
              line[i]:=#13;
            end else
            if copy(line,i,2)='\t' then begin
              delete (line,i,1);
              line[i]:=#9;
            end else
            if copy(line,i,2)='\f' then begin
              delete (line,i,1);
              line[i]:=#26;
            end else
            if line[i]='\' then begin
              case line[i+1] of
                'n': line[i+1] := #10;
                'r': line[i+1] := #13;
                't': line[i+1] := #9;
              end;
              delete (line,i,1);
            end else
            if line[i]='"' then begin
              delete (line,i,maxint);
            end;
              inc (i);
          end;
          AddTranslation('default',RemoveNuls(line),ident,sourcefilename, FLineNr);
        end;
      end;
    end;
  finally
    CloseFile (tf);
  end;
end;

procedure TXGetText.AddBaseDirectory(const path: string);
begin
  if path<>'' then
    BaseDirectoryList.Add(IncludeTrailingPathDelimiter(path))
  else
    BaseDirectoryList.Add('');
end;

procedure TXGetText.Execute;
var
  i,j:integer;
begin

  // If no base directories, make one
  if BaseDirectoryList.Count=0 then
    AddBaseDirectory(IncludeTrailingPathDelimiter(ExpandFileName('.')));

  // Find destination path
  if DestinationPath='' then
    DestinationPath:=IncludeTrailingPathDelimiter(ExpandFileName('.'));

  // Read current ignore.po file
  if FileExists(DestinationPath+'ignore.po') then
    ignorelist.LoadFromFile(DestinationPath+'ignore.po');

  // Iterate base directories
  for j:=0 to BaseDirectoryList.Count-1 do begin
    BaseDirectory:=BaseDirectoryList.Strings[j];
    ParseExcludeFile;
    for i:=0 to filemasks.count-1 do begin
      if NoWildcards then begin
        ExtractFromFile(filemasks.Strings[i]);
      end else begin
        ExtractFromFileMasks(filemasks.Strings[i]);
      end;
    end;
  end;

  // Handle ignores
  HandleIgnores;

  //*** Handle comments
  HandleComments;

  // Write files
  if UpdateIgnore then
    ignorelist.SaveToFile(DestinationPath+'ignore.po');
  WritePoFiles (DestinationPath);
end;

procedure TXGetText.AddDelphiFilemasks;
begin
  filemasks.add ('*.pas');
  filemasks.add ('*.inc');
  filemasks.Add ('*.rc');
  filemasks.add ('*.dpr');
  filemasks.add ('*.xfm');
  filemasks.add ('*.dfm');
end;

procedure TXGetText.AddKylixFilemasks;
begin
  filemasks.add ('*.pas');
  filemasks.add ('*.inc');
  filemasks.Add ('*.rc');
  filemasks.add ('*.dpr');
  filemasks.add ('*.xfm');
end;


procedure TXGetText.doHandleExtendedDirective(var line: string);
  function GetString: string;
  begin
    Result := '';
    if (length (line) > 0) and (line[1] = '=') then begin
      delete (line, 1, 1);
      line := trim (line);
      extractstring(line,Result);
    end;
  end;

Const
  cErrOptionUnknown = '{gnugettext: Unknonw option.';
  cErrMissingStart = '{gnugettext: reset found without scan-all.';
  cErrDomainSyntax = '{gnugettext: error in the domain name definition.';
  cErrContextSyntax = '{gnugettext: error in the context name definition.';
begin
  delete (line, 1, length(cDefineDirective));
  line := trim (line);
  if IsDirective(cScanOption, line) then begin
    delete (line, 1, length (cScanOption));
    line := trim (line);
    if pos (cDomainDefinition, lowerCase (copy (line, 1, length(cDomainDefinition)))) = 1 then begin
      delete (line, 1, Length (cDomainDefinition));
      line := trim (line);

      definedDomain := GetString;
      if length (definedDomain) = 0 then begin
        Warning (wtExtendedDirectiveError, _(cErrDomainSyntax));
      end;
    end
    else definedDomain := 'default';
  end
  else if IsDirective(cContextDefinition, line) then begin
    delete (line, 1, length (cContextDefinition));
    line := trim (line);

    definedContext := GetString;
    if length (definedContext) = 0 then begin
      Warning (wtExtendedDirectiveError, _(cErrContextSyntax));
    end;
  end
  else if IsDirective(cScanResetOption, line) then begin
    if (length (definedDomain) = 0) and (length(definedContext) = 0) then begin
      Warning(wtExtendedDirectiveError, _(cErrMissingStart))
    end
    else begin
      definedDomain := '';
      definedContext := '';
    end;
  end
  else begin
    Warning (wtExtendedDirectiveError, _(cErrOptionUnknown))
  end;
end;

{$ifdef mswindows}
procedure TXGetText.ExtractFromEXE(const sourcefilename: string);
  procedure recurse (rl:TResourceList);
  var
    r:TResourceItem;
    i,j:integer;
    ws:string;
    itemno:integer;
  begin
    for i:=0 to rl.Count-1 do begin
      r:=rl.Items[i];
      if r.IsList then begin
        recurse (r.List)
      end else begin
        case r.ResType of
          rtString:
            begin
              itemno:=0;
              ws:=PWideChar(r.RawData);
              while ws<>'' do begin
                inc (itemno);
                j:=ord(ws[1]);
                AddTranslation('default',RemoveNuls(copy(ws,2,j)),'Resource '+r.Name+', item no. '+IntToStr(itemno),sourcefilename, FLineNr);
                delete (ws,1,j+1);
              end;
            end;
        end;
      end;
    end;
  end;
var
  exe:TExeImage;
begin
  exe := TExeImage.CreateImage(nil, sourceFileName);
  try
    recurse (exe.Resources);
  finally
    FreeAndNil (exe);
  end;
end;
{$endif}

procedure TXGetText.HandleIgnores;
var
  j:integer;
  dom:TXGTDomain;
  item:TPoEntry;
  newitem:TPoEntry;
  ignoreitem:TPoEntry;
begin
  // Only default domain is affected by ignore.po
  dom:=GetDomain('default');

  // Add new ignores to new ignore list and update autocomments
  if UpdateIgnore then begin
    for j := 0 to dom.order.Count - 1 do
    begin
      if Assigned(OnProgress) then
      begin
        OnProgress (_('Update ignore list'), '', 0);
      end;

      item := dom.order.Objects[j] as TPoEntry;
      ignoreitem:=ignorelist.Find(item.MsgId);
      if ignoreitem=nil then begin
        newitem:=TPoEntry.Create;
        newitem.Assign(item);
        if not IsProbablyTranslatable( newitem,
                                       nil,
                                       nil) then
          ignorelist.Add(newitem)
        else
          FreeAndNil (newitem);
      end else begin
        ignoreitem.AutoCommentList.Text:=item.AutoCommentList.Text;
      end;
    end;
  end;

  // Remove ignores from default list
  if UseIgnoreFile then
  begin
    for j := dom.order.Count - 1 downto 0 do
    begin
      if Assigned(OnProgress) then
      begin
        OnProgress (_('Remove ignored strings from template'), '', 0);
      end;

      item := dom.order.Objects[j] as TPoEntry;
      if ignorelist.Find(item.MsgId) <> nil then
      begin
        // Only delete from order list
        dom.order.Delete (j);
      end;
    end;
  end;
end;

procedure TXGetText.HandleComments;
var
  i, j: integer;
  lDomain: TXGTDomain;
  lItem, lTemplatItem: TPoEntry;
  lFileName: TFileName;
  lTemplateList: TPoEntryList;
begin
  //*** preserve comments from a existing template-file
  if PreserveUserComments then
  begin
    //DestinationPath+'ignore.po'
    for i := 0 to domainlist.Count - 1 do
    begin
      lFileName := IncludeTrailingPathDelimiter(DestinationPath) +
                   domainlist.Strings[i] + '.po';

      if Assigned(OnProgress) then
      begin
        OnProgress (_('copy user comments from existing template'), lFileName, 0);
      end;

      if FileExists (lFileName) then
      begin
        lDomain := GetDomain(domainlist.Strings[i]);

        //** Read existing Template file
        lTemplateList := TPoEntryList.Create;
        try
          lTemplateList.LoadFromFile(lFileName);

          for j := lDomain.Order.Count - 1 downto 0 do
          begin
            lItem        := lDomain.order.Objects[j] as TPoEntry;
            lTemplatItem := lTemplateList.Find(lItem.MsgId);

            if (lTemplatItem <> nil) then
            begin
              //*** copy the existing comment to the new template (new template
              //    can not have user comments)
              lItem.UserCommentList.Append(lTemplatItem.UserCommentList.Text);
            end;
          end;
        finally
          lTemplateList.Free;
        end;
      end;
    end;
  end;
end;

function TXGetText.isStringObjectPascalFormat(const xMsgId: String): TObjectPascalFormat;
begin
  Result := opfUndefined;

  if (pos('%', xMsgId) > 0) then
  begin
    result := opfFalse;
  end;

  if (pos('%s', xMsgId) > 0) or
     (pos('%d', xMsgId) > 0) or
     (pos('%f', xMsgId) > 0) or
     (pos('%%', xMsgId) > 0) then
  begin
    Result := opfTrue;
  end;
end;

procedure TXGetText.ParseExcludeFile;
const
 cExcludeFilename = 'ggexclude.cfg';
var
  excludefile: string;
  F: TextFile;
  section,
  line: string;
  lnr: integer;
  added:boolean;
begin
  lnr := 0;
  Excludes.Clear;
  Excludes.Basedirectory := BaseDirectory;
  excludefile :=BaseDirectory;
  if RightStr(excludefile, 1) <> PathDelim then
    excludefile := excludefile + PathDelim;
  excludefile := ExpandFilename(excludefile + cExcludeFilename);
  if not FileExists(excludefile) then
    Exit;
  section := '';
  FileMode:=fmOpenRead;
  AssignFile(F, excludefile);
  Reset(F);
  try
    if Assigned(OnProgress) then
      OnProgress (Format(_('Reading %s'),[excludefile]),excludefile,0);
    while not EOF(F) do begin
      Readln(F, line);
      line := Trim(line);
      inc(lnr);
      if line <> '' then begin // skip empty lines
        if line[1] = '#' then // skip remarks
          Continue;
        if line[1] = '[' then begin // entering new section
          if RightStr(line, 1) = ']' then begin
            section := LowerCase(Copy(line, 2, Length(line) - 2));
            if (section <> cExcludeDir)
              and (section <> cExcludeFile)
              and (section <> cExcludeFormClass)
              and (section <> cExcludeFormClassProperty)
              and (section <> cExcludeFormInstance) then
                Warning(wtExcludeFile, Format(_('Line %d: Unknown section'), [lnr]), section, excludefile, lnr);
             continue;
          end else
            Warning(wtExcludeFile, Format(_('Line %d: Looks like a section but has no closing square brackets'), [lnr]), line, excludefile, lnr);
        end;
        added := true;
        if section = cExcludeDir then
          added := Excludes.AddDirectory(line)
        else if section = cExcludeFile then
          added := Excludes.AddFormFile(line)
        else if section = cExcludeFormClass then
          added := Excludes.AddFormClass(line)
        else if section = cExcludeFormClassProperty then
          added := Excludes.AddFormClassProperty(line)
        else if section = cExcludeFormInstance then
          added := Excludes.AddFormInstance(line);
        if not added then
          Warning(wtExcludeFile, Format(_('Line %d: %s'), [lnr, Excludes.LastErrorMsg]), line, excludefile, lnr);
      end;
    end;
  finally
    CloseFile(F);
  end;
end;

{ TXExcludes }

function TXExcludes.AddDirectory(aDirectory: string): boolean;
begin
  Result := True;
  aDirectory := Trim(aDirectory);
  if aDirectory = '' then
    Exit;
  if BaseDirectory <> '' then begin
    aDirectory := GetFullInternalPath(aDirectory);
    if RightStr(aDirectory, 1) = PathDelim then
      aDirectory := Copy(aDirectory, 1, Length(aDirectory) -1);
    if DirectoryExists(aDirectory) then begin
      {$ifdef mswindows}
      FDirectories.Add(AnsiLowerCase(aDirectory));
      {$else}
      FFiles.Add(aDirectory);
      {$endif}
    end else begin
      Result := False;
      FLastErrorMsg := Format(_('Directory %s doesn''t exist'), [aDirectory]);
    end;
  end;
end;


function TXExcludes.AddFormClass(aClassname: string): boolean;
begin
  Result := True;
  if aClassname = '' then
    Exit;
  if Pos('.', aClassname) > 0 then begin
    Result := False;
    FLastErrorMsg := Format(_('Wrong section: %s is a property name and not a class'), [aClassname]);
  end else begin
    aClassname := Trim(Lowercase(aClassname));
    FFormClasses.Add(aClassname);
  end;
end;

function TXExcludes.AddFormClassProperty(aPropertyname: string): boolean;
var
  p:integer;
begin
  Result := True;
  p := Pos('.', aPropertyname);
  if p = 0 then begin
    Result := False;
    FLastErrorMsg := Format(_('Wrong section: %s seems to be a class and not a property name'), [aPropertyname]);
  end else 
    FExcludeFormClassPropertyList.AddFormClassProperty(aPropertyname);
end;

function TXExcludes.AddFormFile(aFilename: string): boolean;
var
  wildcardfilecount: integer;
begin
  Result := True;
  aFilename := Trim(aFilename);
  if aFilename = '' then
    Exit;
  if BaseDirectory <> '' then begin
    wildcardfilecount := 0;
    // if a wildcard is used, add all possible Delphi- or Kylix-files to the list
    if RightStr(aFilename, 2) = '.*' then begin
      aFilename := Copy(aFilename, 1, Length(aFilename) -2);
      if AddFormFile(aFilename + '.dpr') then
        inc(wildcardfilecount);
      if AddFormFile(aFilename + '.pas') then
        inc(wildcardfilecount);
      if AddFormFile(aFilename + '.dfm') then
        inc(wildcardfilecount);
      if AddFormFile(aFilename + '.xfm') then
        inc(wildcardfilecount);
      if AddFormFile(aFilename + '.inc') then
        inc(wildcardfilecount);
      if AddFormFile(aFilename + '.rc') then
        inc(wildcardfilecount);
      if wildcardfilecount = 0 then begin
        Result := False;
        FLastErrorMsg := Format(_('No file found for "%s.*"'), [aFilename]);
      end;
      Exit;
    end;

    aFilename := GetFullInternalPath(aFilename);
    if FileExists(aFilename) then begin
      {$ifdef mswindows}
      FFiles.Add(AnsiLowerCase(aFilename));
      {$else}
      FFiles.Add(aFilename);
      {$endif}
    end else begin
      Result := False;
      FLastErrorMsg := Format(_('File %s doesn''t exist'), [aFilename]);
    end;
  end;
end;

function TXExcludes.AddFormInstance(aInstanceName: string): boolean;
var
  filenamepart,
  instancenamepart: string;
  i: integer;
  p: integer;
begin
  Result := True;
  aInstanceName := Trim(aInstanceName);
  if aInstanceName = '' then
    Exit;

  // Search from the end of the line
  // Take into account that filenames might be absolute, containing
  // ':' on Windows; and that a file-ext might be there.
  p := 0;
  for i := Length(aInstanceName) downto 1 do begin
    if aInstanceName[i] = ':' then begin
      p := i;
      break;
    end;
  end;

  if p = 0 then begin
    Result := False;
    FLastErrorMsg := Format(_('Wrong syntax: No ":" found in %s'), [aInstanceName]);
    exit;
  end;

  if p = Length(aInstanceName) then begin
    Result := False;
    FLastErrorMsg := Format(_('Wrong syntax: ":" is at the end of the line of %s'), [aInstanceName]);
    exit;
  end;

  filenamepart := GetFullInternalPath(LeftStr(aInstanceName, p-1));
  if not FileExists(filenamepart) then begin
    Result := False;
    FLastErrorMsg := Format(_('File "%s" doesn''t exist'), [filenamepart]);
    exit;
  end;
  {$ifdef mswindows}
  filenamepart := AnsiLowerCase(filenamepart);
  {$endif}
  instancenamepart := RightStr(aInstancename, Length(aInstancename)-p);
  FFormInstances.Append(Format('%s:%s', [filenamepart,instancenamepart]));
end;

procedure TXExcludes.Clear;
begin
  FFiles.Clear;
  FDirectories.Clear;
  FFormClasses.Clear;
  FFormInstances.Clear;
  FExcludeFormClassPropertyList.Clear;
  FLastErrorMsg := '';
  FBaseDirectory := '';
end;

constructor TXExcludes.Create;
begin
  FExcludeFormClassPropertyList := TXExcludeFormClassPropertyList.Create(TXExcludeFormClassProperties);
  FLastErrorMsg := '';

  FDirectories := TStringList.Create;
  FDirectories.Duplicates := dupIgnore;
  FDirectories.Sorted := True;
  FDirectories.CaseSensitive := True;

  FFiles := TStringList.Create;
  FFiles.Duplicates := dupIgnore;
  FFiles.Sorted := True;
  FFiles.CaseSensitive := True;

  FFormClasses := TStringList.Create;
  FFormClasses.Sorted := True;
  FFormClasses.Duplicates := dupIgnore;
  FFormClasses.CaseSensitive := False;

  FFormInstances := TStringList.Create;
end;

destructor TXExcludes.Destroy;
begin
  FreeAndNil(FExcludeFormClassPropertyList);
  FreeAndNil(FFormInstances);
  FreeAndNil(FFormClasses);
  FreeAndNil(FFiles);
  FreeAndNil(FDirectories);
  inherited;
end;

function TXExcludes.ExcludeDirectory(aDirectory: string): Boolean;
var
 i: Integer;
begin
  Result := False;
  if (Trim(aDirectory) = '') or (FDirectories.Count = 0) then
    Exit;
  if RightStr(aDirectory, 1) = PathDelim then
    aDirectory := Copy(aDirectory, 1, Length(aDirectory) -1);
  aDirectory := WindowsPathDelim2LinuxPathDelim(aDirectory);
  {$ifdef mswindows}
  aDirectory := AnsiLowerCase(aDirectory);
  {$endif}
  for i := 0 to FDirectories.Count-1 do begin
    // this checks for subfolders in FDirectories[i] as well:
    if Pos(FDirectories[i], aDirectory) = 1 then begin
      Result := True;
      Exit;
    end;
  end;
end;

function TXExcludes.ExcludeFormClass(aClassname: string): Boolean;
var
  i:integer;
  s:string;
begin
  Result := False;
  if (aClassname = '') or (FFormClasses.Count = 0) then
    Exit;
  aClassname := Trim(LowerCase(aClassname));
  for i := 0 to FFormClasses.Count-1 do begin
    s := FFormClasses[i];
    if RightStr(s, 1) = '*' then
      s := LeftStr(s, Length(s)-1);
    if s = aClassname then begin
      Result := true;
      exit;
    end;
  end;
end;

function TXExcludes.ExcludeFormClassProperty(aClassname,
  aPropertyname: string): Boolean;
begin
  Result := FExcludeFormClassPropertyList.ExcludeFormClassProperty(aClassname, aPropertyname)
end;

function TXExcludes.ExcludeFormClassProperty(aClassname: string): Boolean;
begin
  Result := Assigned(FExcludeFormClassPropertyList.FindItem(aClassname));
end;

function TXExcludes.ExcludeFormFile(aFilename: string): Boolean;
begin
  Result := False;
  if (aFilename = '') or (FFiles.Count = 0) then
    Exit;
  aFilename := WindowsPathDelim2LinuxPathDelim(aFilename);
  {$ifdef mswindows}
  aFilename := AnsiLowerCase(aFilename);
  {$endif}
  Result := FFiles.IndexOf(aFilename) > -1;
end;

function TXExcludes.ExcludeFormInstance(aFilename, aInstanceName: string): boolean;
var
  i,p: integer;
  filenamepart,
  instancenamepart: string;
begin
  Result := False;
  aFilename := WindowsPathDelim2LinuxPathDelim(aFilename);
  aInstanceName := Trim(aInstanceName);
  if (aInstanceName = '') or (aFilename = '') or (FFormInstances.Count = 0) then
    Exit;
  aInstanceName := LowerCase(aInstanceName);
  p := 0;
  for i := 0 to FFormInstances.Count-1 do begin
    if lowercase(RightStr(FFormInstances[i], Length(aInstancename))) = aInstancename then
      p := Length(aInstancename) -1
    else
      continue;
    if p > 0 then begin
      filenamepart := LeftStr(FFormInstances[i], Length(FFormInstances[i])-p-2);
      instancenamepart := lowercase(RightStr(FFormInstances[i], p+1));
      {$ifdef mswindows}
      if (AnsiLowercase(filenamepart) = AnsiLowercase(aFilename))
      {$else}
      if (filenamepart = aFilename)
      {$endif}
      and (instancenamepart = aInstanceName) then begin
        Result := true;
        exit;
      end;
    end;
  end;
end;

function TXExcludes.FormClassHasWildcard(aClassname: string): Boolean;
var
  i:integer;
begin
  Result := False;
  aClassname := Trim(LowerCase(aClassname));
  for i := 0 to FFormClasses.Count-1 do begin
    if RightStr(FFormClasses[i], 1) = '*' then begin
      if LeftStr(FFormClasses[i], Length(FFormClasses[i])-1) = aClassname then begin
        Result := true;
        exit;
      end;
    end;
  end;
end;

function TXExcludes.GetFullInternalPath(s:string): string;
begin
  Result := Trim(WindowsPathDelim2LinuxPathDelim(ExpandFilename(Basedirectory+s)));
end;

{ TXExcludeFormClassProperties }

procedure TXExcludeFormClassProperties.AddFormClassProperty(aPropertyname: string);
begin
  FProperties.Add(Trim(Lowercase(StringReplace(aPropertyname,'/','.',[rfReplaceAll]))));
end;

constructor TXExcludeFormClassProperties.Create(Collection: TCollection);
begin
  inherited;
  FProperties := TStringList.Create;
  FProperties.Duplicates := dupIgnore;
  FProperties.Sorted := True;
  FProperties.CaseSensitive := False;
end;

destructor TXExcludeFormClassProperties.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

function TXExcludeFormClassProperties.ExcludeFormClassProperty(
  aPropertyname: string): boolean;
begin
  Result := FProperties.IndexOf(Trim(LowerCase(aPropertyname))) > -1;
end;

procedure TXExcludeFormClassProperties.SetNameOfClass(const Value: string);
begin
  FNameOfClass := Trim(Lowercase(Value));
end;

{ TXExcludeFormClassPropertyList }

function TXExcludeFormClassPropertyList.Add: TXExcludeFormClassProperties;
begin
  Result := TXExcludeFormClassProperties(inherited Add);
end;

function TXExcludeFormClassPropertyList.AddFormClass(aClassname: string): TXExcludeFormClassProperties;
begin
  Result := FindItem(aClassname);
  if not assigned(Result) then begin
    Result := Add;
    Result.NameOfClass := aClassname;
  end;
end;

function TXExcludeFormClassPropertyList.AddFormClassProperty(
  aClassPropertyname: string): TXExcludeFormClassProperties;
var
  p: integer;
  theclassname,
  thepropertyname: string;
  item: TXExcludeFormClassProperties;
begin
  Result := nil;
  if aClassPropertyname = '' then
    Exit;
  p := Pos('.', aClassPropertyname);
  aClassPropertyname := Trim(aClassPropertyname);
  theclassname := Trim(LeftStr(aClassPropertyname, p-1));
  thepropertyname := Trim(RightStr(aClassPropertyname, Length(aClassPropertyname) - p));
  item := AddFormClass(theclassname);
  assert(assigned(item), 'This should''t happen: item of a class was neither created nor found');
  item.AddFormClassProperty(thepropertyname);
end;

function TXExcludeFormClassPropertyList.ExcludeFormClassProperty(
  aClassname, aPropertyname: string): Boolean;
var
  item: TXExcludeFormClassProperties;
  ItemWildcardPos: integer;
  ItemClassname: string;
  ItemClassnameLen: integer;
  ClassnameLen: integer;
  i: integer;
begin
  Result := False;
  if Count > 0 then begin
    aClassname := Trim(Lowercase(aClassname));
    ClassnameLen := Length(aClassname);
    for i := 0 to Count-1 do begin
      item := Items[i];
      ItemClassname := item.NameOfClass;
      ItemWildcardPos := pos('*', ItemClassname);
      ItemClassnameLen:= Length(ItemClassname);
      {(*}
      if
         //exact class name
         (ItemClassname = aClassname) or
         // just * wildcard
         ((ItemWildcardPos = 1) and (ItemClassnameLen = 1)) or
         // postfix wildcard Class*
         ((ItemWildcardPos = ItemClassnameLen) and
          (Copy(ItemClassname, 1, ItemWildcardPos - 1)
           = Copy(aClassname, 1, ItemWildcardPos - 1))) or
         // prefix wildcard *Class
         ((ItemWildcardPos = 1) and (ClassnameLen >= ItemClassnameLen - 1) and
          (Copy(ItemClassname, 2, ItemClassnameLen - 1)
           = Copy(aClassname, ClassnameLen - ItemClassnameLen + 2, ItemClassnameLen - 1)))
      {*)}
      then begin
        Result := Items[i].ExcludeFormClassProperty(aPropertyname);
        if Result then
          exit;
      end;
    end;
  end;
end;

function TXExcludeFormClassPropertyList.FindItem(
  aClassname: string): TXExcludeFormClassProperties;
var
  i:integer;
begin
  Result := nil;
  if Count > 0 then begin
    aClassname := Trim(Lowercase(aClassname));
    for i := 0 to Count-1 do
      if Items[i].NameOfClass = aClassname then begin
        Result := Items[i];
        exit;
      end;
  end;
end;

function TXExcludeFormClassPropertyList.GetItems(
  Index: integer): TXExcludeFormClassProperties;
begin
  Result := TXExcludeFormClassProperties(inherited Items[Index]);
end;

procedure TXExcludeFormClassPropertyList.SetItem(Index: integer;
  const Value: TXExcludeFormClassProperties);
begin
  inherited SetItem(Index, Value);
end;


end.