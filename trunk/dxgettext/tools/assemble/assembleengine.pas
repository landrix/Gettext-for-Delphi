unit assembleengine;
// This is the unit that actually embeds translations into the exe file.

interface

uses
  Classes;

type
  Tassembleengine=
    class
    public
      exefilename:string;
      basedirectory: string;
      detectioncode:RawByteString;
      startheader:RawByteString;
      endheader:RawByteString;
      filemask:string;
      filelist:TStringList;  // Objects are TFileInfo
      constructor Create;
      destructor Destroy; override;
      procedure PrepareFileList(const dir: string); // Always run that before Execute;
      procedure SkipFile (filename:string); // removes item from list
      procedure Execute;
      procedure SetGnuGettextPatchCode;
    private
      procedure RecurseDirs (list:TStringList; dir:string);
      function FindSignature(const signature: RawByteString; str: TFileStream): Boolean;
      function TryOpenExeStream: TFileStream;
    end;

implementation

uses
  SysUtils,
  StrUtils,
  gnugettext;

type
  TFileInfo=
    class
      filename:string; // Relative, including the first directory name
      offset:int64;    // Position in .exe file where this file starts
      size:int64;
    end;

procedure StreamWriteRaw (s:TStream;line:RawByteString);
var
  nextpos:integer;
begin
  nextpos:=length(line);
  if nextpos>0 then
    if s.Write(line[1],nextpos)<>nextpos then
      raise Exception.Create (_('Error when writing to stream.'));
end;

procedure StreamWriteInt64 (s:TStream; i:int64);
begin
  Assert (sizeof(i)=8);
  s.WriteBuffer(i,8);
end;

{ Tassembleengine }

constructor Tassembleengine.Create;
begin
  filemask:='*';
  filelist:=TStringList.Create;
end;

destructor Tassembleengine.Destroy;
begin
  while filelist.Count<>0 do begin
    filelist.Objects[0].Free;
    filelist.Delete (0);
  end;
  FreeAndNil (filelist);
  inherited;
end;


function Tassembleengine.TryOpenExeStream: TFileStream;
const
  MaxTries = 10;
var
  Tries: Integer;
begin
  Tries := 1;
  while True do begin
    try
      Result := TFileStream.Create(exefilename, fmOpenReadWrite);
      // if we get here, it worked, so we simply exit
      WriteLn('Opening executable worked on ', Tries, ' try.');
      Exit; //==>
    except
      on EFOpenError do begin
        // If opening the file failed, it's probably the virus scanner which is currently
        // accessing it. So we wait a second and try again.
        if Tries >= MaxTries then begin
          // we already failed too many times, give up
          raise;
        end;
        Inc(Tries);
        WriteLn('Opening executable failed, will try again in 1 second.');
        // sleep 1 second and try again
        Sleep(1000);
      end;
    end;
  end;
end;

procedure Tassembleengine.Execute;
var
  str,infile:TFileStream;
  i:integer;
  nextpos:int64;
  fi:TFileInfo;
  relativeoffsethelper,
  tableoffset:int64;
  s: string;
begin
  if exefilename = '' then
    raise Exception.Create(_('No .exe filename specified.'));
  if basedirectory = '' then
    basedirectory := ExtractFilePath(exefilename);
  basedirectory := IncludeTrailingPathDelimiter(basedirectory);
  basedirectory := IncludeTrailingPathDelimiter(basedirectory + 'locale\');

  // Find all files to include
  if filelist.count = 0 then
    PrepareFileList(basedirectory);

  if filelist.Count = 0 then begin
    WriteLn(Format(_('No files matching "%s" found in "%s", leaving the executable unchanged.'), [filemask, basedirectory]));
    exit;
  end;

  filelist.Sort;

  str:=TryOpenExeStream;
  try
    if not FindSignature(detectioncode, str) then
      raise Exception.Create (Format(_('Signature "%s" was not found in .exe file. Please make sure the .exe file has been compiled with the correct libraries.'), [detectioncode]));

    if FindSignature(startheader, str) or FindSignature(endheader, str) then
      raise Exception.Create(_('This file has already been modified. Please recompile this .exe file.'));

    // add new begin header to the end of the exe file
    str.Seek(0, soFromEnd);
    StreamWriteRaw(str, startheader);
    relativeoffsethelper := str.Position;

    // Add files to the end of the exe file
    str.Seek(0, soFromEnd);
    for i:=0 to filelist.count-1 do begin
      fi:=filelist.objects[i] as TFileInfo;
      writeln(Format(_('Adding file: %s'), [fi.filename]));
      infile := TFileStream.Create(fi.filename, fmOpenRead);
      try
        fi.offset := str.Position;
        fi.size := infile.Size;
        str.CopyFrom(infile, 0);
      finally
        FreeAndNil(infile);
      end;
    end;

    // Write List of files
    while str.position and $ff<>0 do
      StreamWriteRaw (str,#0);
    tableoffset:=str.Position;
    nextpos:=tableoffset;
    for i:=0 to filelist.Count-1 do begin
      while str.position<>nextpos do
        StreamWriteRaw (str,' ');
      fi:=filelist.Objects[i] as TFileInfo;
      nextpos := ((str.Position + sizeof(nextpos) + sizeof(fi.offset)
        + sizeof(fi.size) + length(fi.filename)) + 256) and (not $ff);
      StreamWriteInt64(str, nextpos-relativeoffsethelper);
      StreamWriteInt64(str, fi.offset-relativeoffsethelper);
      StreamWriteInt64(str, fi.size);
      s := 'locale\' + Copy(fi.filename, Length(basedirectory) + 1);
      Writeln(Format(_('Adding Header: %s'), [s]));
      StreamWriteRaw(str, utf8encode(s));
    end;
    while str.position<>nextpos do
      StreamWriteRaw (str,' ');
    StreamWriteInt64(str,0);

    // finalize by writing the relative table offset (8 byte) and end header
    str.Seek(0, soFromEnd);
    StreamWriteInt64(str, tableoffset-relativeoffsethelper);
    StreamWriteRaw(str, endheader);
  finally
    FreeAndNil (str);
  end;
  WriteLn(Format(_('Successfully added %d files to %s'), [Filelist.Count, exefilename]));
end;

function Tassembleengine.FindSignature(const signature: RawByteString; str: TFileStream): Boolean;
// Finds the position of patchcode in the file.
const
  bufsize=100000;
var
  a:RawByteString;
  b:RawByteString;
  rd,p:Integer;
begin
  Result := False;
  if signature='' then
    raise Exception.Create (_('No patch code has been specified.'));

  str.Seek(0, soFromBeginning);
  
  SetLength (a, bufsize);
  SetLength (b, bufsize);
  str.Read(a[1],bufsize);
  while true do begin
    rd:=str.Read(b[1],bufsize);
    p:=pos(signature,a+b);
    if p<>0 then begin
      Result:=True;
      exit;
    end;
    if rd<>bufsize then begin
      // Prematurely ended without finding anything
      exit;
    end;
    a:=b;
  end;
end;

procedure Tassembleengine.PrepareFileList(const dir: string);
begin
  if filelist.Count = 0 then
    RecurseDirs(filelist, dir);
end;

procedure Tassembleengine.RecurseDirs(list: TStringList; dir: string);
var
  sr: TSearchRec;
  dirlist: TStringList;
  more: boolean;
  fi: TFileInfo;
begin
  dirlist := TStringList.Create;
  try
    dirlist.Add(dir);

    while dirlist.Count <> 0 do begin
      dir := dirlist.Strings[0];
      dirlist.Delete(0);

      // Scan this directory for subdirectories and append them to dirlist
      more := (FindFirst(dir + '*', faAnyFile, sr) = 0);
      while more do begin
        if (sr.Name <> '.') and (sr.Name <> '..') then begin
          if (sr.Attr and faDirectory) <> 0 then begin
            dirlist.Add(dir + sr.Name + PathDelim);
          end;
        end;
        more := (Findnext(sr) = 0);
      end;
      FindClose(sr);

      // Scan this directory for files and append them to list
      more := (FindFirst(dir + filemask, faAnyFile, sr) = 0);
      while more do begin
        if (sr.Name <> '.') and (sr.Name <> '..') then begin
          if (sr.Attr and faDirectory) = 0 then begin
            if not SameText(dir + sr.Name, exefilename) then begin
              fi := TFileInfo.Create;
              fi.filename := dir + sr.Name;
              list.AddObject(fi.filename, fi);
            end;
          end;
        end;
        more := (Findnext(sr) = 0);
      end;
      FindClose(sr);
    end;
  finally
    FreeAndNil(dirlist);
  end;
end;

procedure Tassembleengine.SetGnuGettextPatchCode;
const
  AssemblePrefix: AnsiString = 'DXG'; // gnugettext checks for the prefix + signature
begin
  detectioncode := '2E23E563-31FA-4C24-B7B3-90BE720C6B1A';
  startheader := AssemblePrefix + 'BD7F1BE4-9FCF-4E3A-ABA7-3443D11AB362';
  endheader :=AssemblePrefix + '1C58841C-D8A0-4457-BF54-D8315D4CF49D';
end;

procedure Tassembleengine.SkipFile(filename: string);
var
  idx:integer;
begin
  idx:=filelist.IndexOf(filename);
  if idx = -1 then
    raise Exception.Create('Internal error. Filename not found in list: ' + filename);
  filelist.Objects[idx].Free;
  filelist.Delete(idx);
end;

end.
