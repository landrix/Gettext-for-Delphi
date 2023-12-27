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
  Classes, System.UITypes;

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
  Dialogs, SysUtils, StrUtils, ConsoleAppHandler, Windows;

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

procedure TRunXGettext.Execute;
var
  lAppOutput:TStringList;
  lCmdLine:string;
  lAppOut:string;
  lFileListName:string;
  lTF:TextFile;
  i:integer;
  lTempPath:string;
begin
  SetLength( lTempPath, 1000);

  i := GetTempPath( 1000, PChar( lTempPath));

  if i = 0 then
  begin
    lFileListName := 'c:\filelist.txt';
    // This is extremely rare, and absolutely an Operating system error, and therefore doesn't need translation.
    ShowMessage( Format( 'Please check your temp path settings - they are incorrect. Using "%s" instead.',
                         [ lFileListName]));
  end
  else
  begin
    SetLength( lTempPath, i);
    SetLength( lFileListName, MAX_PATH);
    i := GetTempFileName( PChar( lTempPath),
                          'ggd',
                          0,
                          PChar( lFileListName));
    if i = 0 then
    begin
      lFileListName := 'c:\filelist.txt';
      // This is extremely rare, and absolutely an Operating system error, and therefore doesn't need translation.
      ShowMessage( Format( 'Please check your temp path settings - they are incorrect. Using "%s" instead.',
                           [ lFileListName]));
    end
    else
    begin
      SetLength( lFileListName, StrLen( PChar( lFileListName)));
    end;
  end;
                           
  AssignFile( lTF, lFileListName);
  Rewrite( lTF);

  for i := 0 to FileList.Count - 1 do
  begin
    Writeln( lTF,
             FileList.Strings[i]);
  end;
  CloseFile (lTF);

  lAppOutput := TStringList.Create;
  try
    lCmdLine := '--keyword=_ -n --files-from="' + lFileListName + '"';

    if FileExists( OutputDir + 'default.po') then
    begin
      lCmdLine := lCmdLine + ' -j';
    end;
    lCmdLine := lCmdLine + ' --output-dir=' + OutputDir + ' -d default';
    //lCmdLine := lCmdLine + ' 2>&1';

    // Use this command line in later versions, when xgettext.exe has been
    // replaced with a version that understands --from-code
    // cmdline:='xgettext --keyword=_ --from-code=UTF-8 -n --files-from="'+shellescape(filelistname)+'" -j -p "'+shellescape(OutputDir)+'" -d default 2>&1';

    ExecConsoleApp ( 'xgettext.exe',
                     lCmdLine,
                     lAppOutput,
                     nil);

    lAppOut := trim( lAppOutput.text);

    if ( ( lAppOut <> '') and
         not AnsiStartsText( 'Execution succes', lAppOut)) then
    begin
      MessageDlg ( lAppOut,
                   mtInformation,
                   [ mbOK],
                   0);
    end;
  finally
    FreeAndNil( lAppOutput);
  end;

  SysUtils.DeleteFile( lFileListName);
end;

end.
