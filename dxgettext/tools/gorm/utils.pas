unit utils;

interface

uses
  Windows;

function MapiInstalled: Boolean;
function SendMail(AHandle : HWnd;
                  const Subject, Body, FileName,
                  SenderName, SenderEMail,
                  RecipientName, RecipientEMail: ansistring): Integer;

implementation

uses
  Registry,
  MAPI;

function MapiInstalled: Boolean;

var
  Reg: TRegistry;

begin
  Result:=True;
  try
    Reg:=TRegistry.Create;
    try
      Reg.RootKey:=HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows Messaging Subsystem') then
        Result:=(Reg.ReadString('MAPI')='1');
    finally
      Reg.Free;
    end;
  except
  end;
end;

function SendMail(AHandle : HWnd;
                  const Subject, Body, FileName,
                  SenderName, SenderEMail,
                  RecipientName, RecipientEMail: ansistring): Integer;

// From http://www.scalabium.com/faq/dct0115.htm

var
  Message: TMapiMessage;
  lpSender, lpRecipient: TMapiRecipDesc;
  FileAttach: TMapiFileDesc;
  SM: TFNMapiSendMail;
  MAPIModule: HModule;

begin
  FillChar(Message, SizeOf(Message), 0);
  with Message do begin
    if (Subject <> '') then
      lpszSubject := PAnsiChar(Subject);

    if (Body <> '') then
      lpszNoteText := PAnsiChar(Body);

    if (SenderEmail <> '') then begin
      lpSender.ulRecipClass := MAPI_ORIG;
      if (SenderName = '') then
        lpSender.lpszName := PAnsiChar(SenderEMail)
      else
        lpSender.lpszName := PAnsiChar(SenderName);
      lpSender.lpszAddress := PAnsiChar(SenderEmail);
      lpSender.ulReserved := 0;
      lpSender.ulEIDSize := 0;
      lpSender.lpEntryID := nil;
      lpOriginator := @lpSender;
    end;

    if (RecipientEmail <> '') then begin
      lpRecipient.ulRecipClass := MAPI_TO;
      if (RecipientName = '') then
        lpRecipient.lpszName := PAnsiChar(RecipientEMail)
      else
        lpRecipient.lpszName := PAnsiChar(RecipientName);
      lpRecipient.lpszAddress := PAnsiChar(RecipientEmail);
      lpRecipient.ulReserved := 0;
      lpRecipient.ulEIDSize := 0;
      lpRecipient.lpEntryID := nil;
      nRecipCount := 1;
      lpRecips := @lpRecipient;
    end
    else
      lpRecips := nil;

    if (FileName = '') then begin
      nFileCount := 0;
      lpFiles := nil;
    end
    else begin
      FillChar(FileAttach, SizeOf(FileAttach), 0);
      FileAttach.nPosition    := $FFFFFFFF; // No position
      FileAttach.lpszPathName := PAnsiChar(FileName);
      FileAttach.lpszFileName := nil;
      FileAttach.lpFileType   := nil;

      nFileCount := 1;
      lpFiles := @FileAttach;
    end;
  end;

  MAPIModule := LoadLibrary(PChar(MAPIDLL));
  if MAPIModule = 0 then
    Result := -1
  else begin
    try
      @SM := GetProcAddress(MAPIModule, 'MAPISendMail');
      if @SM <> nil then begin
        Result := SM(0, AHandle, Message, MAPI_DIALOG or MAPI_LOGON_UI, 0);
      end
      else
        Result := 1;
    finally
      FreeLibrary(MAPIModule);
    end;
  end;
end;

end.
