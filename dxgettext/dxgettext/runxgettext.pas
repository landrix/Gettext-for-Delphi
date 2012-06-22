unit runxgettext;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl                          *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)

// This unit handles running the xgettext.exe program to extract C/C++
// texts from C++ Builder projects

interface

uses
  Classes;

type
  TRunXGettext=
    class
      FileList:TStringList;
      OutputDir:string;
      constructor create;
      destructor Destroy; override;
      procedure Execute;
    end;

implementation

uses
  Dialogs, SysUtils, ConsoleAppHandler, Windows;

{ TRunXGettext }

constructor TRunXGettext.create;
begin
  FileList:=TStringList.Create;
end;

destructor TRunXGettext.Destroy;
begin
  FreeAndNil (FileList);
  inherited;
end;

function shellescape (s:string):string;
var
  i:integer;
begin
  Result:='';
  for i:=1 to length(s) do begin
    if s[i]='"' then
      Result:=Result+'\"'
    else
    if s[i]='\' then
      Result:=Result+'\\'
    else
      Result:=Result+s[i];
  end;
end;

procedure TRunXGettext.Execute;
var
  AppOutput:TStringList;
  cmdline:string;
  appout:string;
  filelistname:string;
  tf:TextFile;
  i:integer;
  temppath:string;
begin
  SetLength (temppath, 1000);
  i:=GetTempPath(1000,PChar(temppath));
  if i=0 then begin
    filelistname:='c:\filelist.txt'; 
    // This is extremely rare, and absolutely an Operating system error, and therefore doesn't need translation.
    ShowMessage (Format('Please check your temp path settings - they are incorrect. Using "%s" instead.',[filelistname]));
  end else begin
    SetLength (temppath,i);
    SetLength (filelistname, MAX_PATH);
    i:=GetTempFileName(PChar(temppath),'ggd',0,PChar(filelistname));
    if i=0 then begin
      filelistname:='c:\filelist.txt';
      // This is extremely rare, and absolutely an Operating system error, and therefore doesn't need translation.
      ShowMessage (Format('Please check your temp path settings - they are incorrect. Using "%s" instead.',[filelistname]));
    end else
      SetLength (filelistname, strlen(PChar(filelistname)));
  end;
                           
  AssignFile (tf,filelistname);
  Rewrite (tf);
  for i:=0 to FileList.Count-1 do begin
    Writeln (tf,FileList.Strings[i]);
  end;
  CloseFile (tf);
  AppOutput:=TStringList.Create;
  try
    cmdline:='xgettext --keyword=_ -n --files-from="'+shellescape(filelistname)+'" -j -p "'+shellescape(OutputDir)+'" -d default 2>&1';
    // Use this command line in later versions, when xgettext.exe has been replaced with a version that understands --from-code
    // cmdline:='xgettext --keyword=_ --from-code=UTF-8 -n --files-from="'+shellescape(filelistname)+'" -j -p "'+shellescape(OutputDir)+'" -d default 2>&1';
    cmdline:='-c "'+shellescape(cmdline)+'"';
    ExecConsoleApp ('bash.exe',cmdline,AppOutput,nil);
    appout:=trim(AppOutput.text);
    if appout<>'' then
      MessageDlg (appout,mtInformation,[mbOK],0);
  finally
    FreeAndNil (AppOutput);
  end;
  SysUtils.DeleteFile (filelistname);
end;

end.
