unit ToolsAutoUpgrader;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl                            *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dybdahl.dk/dxgettext/ for more information       *)
(*                                                              *)
(****************************************************************)

interface

procedure AutomaticDownloadAndReplace (DownloadUrl:string);


implementation

uses Windows, Classes, IniFiles, SysUtils, IdHttp, gnugettext;

type
  TUpgraderThread=
    class (TThread)
      url:string;
      procedure Execute; override;
    end;

var
  t:TUpgraderThread;

{-------------------------------------------------------------------------------
  Call this as the first thing you do in your app.
  It will download the specified URL and replace the current exe with this
  new one. It saves its settings in an ini file.
-------------------------------------------------------------------------------}
procedure AutomaticDownloadAndReplace (DownloadUrl:string);
begin
  t:=TUpgraderThread.Create(true);
  t.url:=DownloadUrl;
  t.FreeOnTerminate:=True;
  t.Resume;
end;

function GetTempPathPas:widestring;
var
  temppath:widestring;
  len:integer;
begin
  SetLength (temppath,MAX_PATH);
  len:=Windows.GetTempPathW(MAX_PATH,PWideChar(temppath));
  if len=0 then raise Exception.Create (_('Could not get filesystem path for temporary files from Windows API. Please inspect your computer settings.'));
  SetLength (temppath,len);
  Result:=temppath;
end;

function GetTempFilename:string;
var
  temppath,tempfilename:string;
  len:integer;
begin
  temppath:=GetTempPathPas;
  SetLength (tempfilename,MAX_PATH);
  len:=Windows.GetTempFileName(PChar(temppath),'CIS',0,PChar(tempfilename));
  if len=0 then
    tempfilename:='c:\tempfile.$$$'
  else
    SetLength(tempfilename,StrLen(PChar(tempfilename)));
  Result:=tempfilename;
end;

{ TUpgraderThread }

procedure TUpgraderThread.Execute;
var
  http:TIdHttp;
  fs:TFileStream;
  DownloadFilename,AppFilename,TempFilename,BackupFilename:string;
  ini:TIniFile;
  DoUpgrade:boolean;
begin
  try
    AppFilename:=paramstr(0);
    TempFilename:=AppFilename+'.new';
    BackupFilename:=AppFilename+'.old';
    if FileExists(BackupFilename) then DeleteFile (BackupFilename);
    if FileExists(TempFilename) then DeleteFile (TempFilename);

    ini:=TIniFile.Create(changefileext(paramstr(0),'.autoupdate.ini'));
    try
      if abs(ini.ReadDateTime('Autoupgrade','Lastcheck',0)-Now)<1 then
        exit;
      ini.WriteDateTime('Autoupgrade','Lastcheck',Now);
      ini.UpdateFile;

      DownloadFilename:=GetTempFilename;
      http:=TIdHttp.Create(nil);
      try
        http.RedirectMaximum:=5;
        http.Request.URL:=url;
        http.Head(url);
        DoUpgrade:=
          (http.ResponseCode>=200) and
          (http.ResponseCode<300) and
          (  (abs(http.Response.LastModified-ini.ReadDateTime('Autoupgrade','LastModified',0))>1/1440)
          or (http.Response.ContentVersion<>ini.ReadString('Autoupgrade','ContentVersion','')));

        if DoUpgrade then begin
          fs:=TFileStream.Create(DownloadFilename,fmCreate);
          try
            http.Get(url,fs);
          finally
            FreeAndNil (fs);
          end;

          if CopyFile (PChar(DownloadFilename),PChar(TempFilename),false) then
          if RenameFile(AppFilename,BackupFilename) then begin
            if not RenameFile(TempFilename,AppFilename) then
              MessageBox (0,'Warning!','During an automatic upgrade, your application exe file was renamed, but upgrade was not completed. Please inspect your application files.',MB_OK)
            else begin
              ini.WriteDateTime('Autoupgrade','LastModified',http.Response.LastModified);
              ini.WriteString('Autoupgrade','ContentVersion',http.Response.ContentVersion);
              ini.UpdateFile;
            end;
          end;
          DeleteFile (DownloadFilename);
        end;
      finally
        FreeAndNil (http);
      end;
    finally
      FreeAndNil (ini);
    end;
  except
    // Catch all errors and do nothing. Autoupgrade is not that important.
  end;
end;

end.
