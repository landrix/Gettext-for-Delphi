unit uRun;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XPMan, ExtCtrls, StrUtils;

type
  TFormRun = class(TForm)
    ButtonGo: TButton;
    EditTranslation: TLabeledEdit;
    EditTemplate: TLabeledEdit;
    ButtonChooseTemplate: TButton;
    XPManifest: TXPManifest;
    ButtonChooseTranslation: TButton;
    CheckBoxCreateBackup: TCheckBox;
    CheckBoxSaveSettings: TCheckBox;
    CheckBoxNonAscii: TCheckBox;
    cb_CreateRemovedAndNewFile: TCheckBox;
    cb_PreserveStateFuzzy: TCheckBox;
    procedure ButtonGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonChooseTranslationClick(Sender: TObject);
    procedure ButtonChooseTemplateClick(Sender: TObject);
  private
    function ExecuteConsoleApplication(const xWorkingDirectory,
                                             xApplicationName,
                                             xParameters: String): Boolean;
    function GetRemovedAndNewFileName(const xFileNameTimeStamp: TDateTime;
                                      const xTranslationFile,
                                            xSuffix: String): TFileName;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRun: TFormRun;

implementation

uses
  ConsoleAppHandler, gnugettext, shellapi, IniFiles, appconsts,
  msgmergedxengine;

{$R *.dfm}

function ShellEscape(s:String): String;
var
  i: Integer;
begin
  Result:='';

  for i := 1 to length(s) do
  begin
    if s[i] = '"' then
      Result := Result + '\"'
    else
    if s[i] = '\' then
      Result := Result + '\\'
    else
      Result := Result + s[i];
  end;
end;

function TFormRun.ExecuteConsoleApplication(const xWorkingDirectory,
                                                  xApplicationName,
                                                  xParameters: String): Boolean;
var
  lCurrentDirectory: TFileName;
  lOutput, lCommand: String;
  lAppOutput: TStringList;
  lRes: DWORD;
begin
  Result := False;

  GetDir(0, lCurrentDirectory);
  try
    ChDir(xWorkingDirectory);

    lCommand := xApplicationName;
    if not (xApplicationName[Length(xApplicationName)] = ' ') or
           (xParameters[1] = ' ') then
    begin
      lCommand := lCommand + ' ';
    end;
    lCommand := lCommand + shellescape(xParameters);

    lAppOutput := TStringList.Create;
    try
      lRes := ExecConsoleApp( 'bash.exe', '-c "' + lCommand + '"',
                              lAppOutput, nil);

      lOutput := Trim(lAppOutput.Text);

      if ((lOutput <> '') and
          not AnsiStartsText('Execution succes', lOutput)) then
      begin
        ShowMessage (lOutput);
        ShowMessage (Format(_('Because there was unexpected output from %s, ' +
                              'no merging has been done.'), [xApplicationName]));
        exit;
      end;

      if (lRes <> 0) then
      begin
        raise Exception.Create(Format(_('%s failed with exit code %s.'),
                                      [xApplicationName, IntToStr(lRes)]));
      end;
    finally
      FreeAndNil(lAppOutput);
    end;

    Result := True;
  finally
    ChDir(lCurrentDirectory);
  end;
end;

function TFormRun.GetRemovedAndNewFileName(const xFileNameTimeStamp: TDateTime;
                                           const xTranslationFile, xSuffix: String): TFileName;
var
  i: Integer;
  lTempFileName, lFileNumber: TFileName;
const
  cMaxCnt = 9999;
  cFileExtension = '.po';
begin
  Result := '';

  lTempFileName := ExtractFilePath(xTranslationFile)+
                   FormatDateTime('yyyy-mm-dd hhnn ', xFileNameTimeStamp) +
                   Trim(ChangeFileExt(ExtractFileName(xTranslationFile), '')) + ' ' +
                   Trim(xSuffix);

  //*** if no file with this name exists return the name, else search for a
  //    file with a file number offset
  if not FileExists(lTempFileName +'.po') then
  begin
    Result := lTempFileName + cFileExtension;
  end
  else
  begin
    // Nach alten Backup-Dateien suchen:
    for i := 1 to cMaxCNT do
    begin
      lFileNumber := Format('%4.4d', [i]);

      Result := lTempFileName + ' ' + lFileNumber + cFileExtension;
      if not FileExists(Result) then
      begin
        Break;
      end;
    end;
  end;
end;

procedure TFormRun.ButtonGoClick(Sender: TObject);
var
  lTranslation, lTranslationBackup, lTemplate, lTempFileName,
  lRemovedAndNewFileName: String;
  lFileNameTimeStamp: TDateTime;
  ini: TIniFile;
  lMsgMergeDxEngine: TMsgMergeDxEngine;
begin
  screen.cursor := crHourglass;
  try
    lTranslation := ExpandFileName(EditTranslation.Text);
    if not fileexists(lTranslation) then
      raise Exception.Create (_('The specified translation file does not exist.'));

    lTemplate := ExpandFileName(EditTemplate.Text);
    if not fileexists(lTemplate) then
      raise Exception.Create (_('The specified template file does not exist.'));

    lTempFileName := ChangeFileExt(lTranslation, '.pox');

    if CheckBoxNonAscii.Checked then
    begin
      // Non-ASCII support required. Use internal function.
      lMsgMergeDxEngine := TMsgMergeDxEngine.Create;
      try
        lMsgMergeDxEngine.translationfilename  := lTranslation;
        lMsgMergeDxEngine.templatefilename     := lTemplate;
        lMsgMergeDxEngine.outputfilename       := lTempFileName;
        lMsgMergeDxEngine.PreserveStateFuzzy   := cb_PreserveStateFuzzy.Checked;
        lMsgMergeDxEngine.Execute;
      finally
        FreeAndNil (lMsgMergeDxEngine);
      end;
    end
    else
    begin
      // ASCII only. Use external msgmerge.exe
      if not ExecuteConsoleApplication( ExtractFilePath(ParamStr(0)),
                                        'msgmerge.exe',
                                        '--no-fuzzy-matching ' +
                                        '-q "' +
                                        lTranslation + '" "' +
                                        lTemplate + '" ' +
                                        '-o "' + lTempFileName + '" 2>&1') then
      begin
        Exit;
      end;
    end;

    lTranslationBackup := changefileext(lTranslation, '.~po');

    if Fileexists (lTranslationBackup) then
    begin
      Deletefile (lTranslationBackup);
    end;

    if cb_CreateRemovedAndNewFile.Checked then
    begin
      lFileNameTimeStamp := Now;

      //*** Create a file with the removed Strings
      lRemovedAndNewFileName := GetRemovedAndNewFileName(lFileNameTimeStamp,
                                                         lTranslation, 'removed');
      if not ExecuteConsoleApplication( ExtractFilePath(ParamStr(0)),
                                        'msgremove.exe',
                                        ' "' + lTranslation + '" ' +
                                        '-i "' + lTempFileName + '" ' +
                                        '-o "' + lRemovedAndNewFileName + '"') then
      begin
        Exit;
      end;

      //*** Create a file with the new Strings
      lRemovedAndNewFileName := GetRemovedAndNewFileName(lFileNameTimeStamp,
                                                         lTranslation, 'new');
      if not ExecuteConsoleApplication( ExtractFilePath(ParamStr(0)),
                                        'msgremove.exe',
                                        ' "' + lTempFileName + '" ' +
                                        '-i "' + lTranslation + '" ' +
                                        '-o "' + lRemovedAndNewFileName + '"') then
      begin
        Exit;
      end;
    end;

    if not RenameFile(lTranslation, lTranslationBackup) then
    begin
      raise Exception.Create (Format(_('Cannot rename %s to %s'),
                                     [lTranslation, lTranslationBackup]));
    end;

    if not RenameFile(lTempFileName, lTranslation) then
    begin
      raise Exception.Create (Format(_('Cannot rename %s to %s'),
                                     [lTempFileName, lTranslation]));
    end;

    if fileexists(lTempFileName) then
    begin
      deletefile (lTempFileName);
    end;

    if not CheckBoxCreateBackup.Checked then
    begin
      deletefile (lTranslationBackup);
    end;

    if MessageDlg(_('The template was merged into the translation file.' + sLineBreak +
                    'Do you want to open the translation file now?' + sLineBreak +
                    '(This requires you to have a .po file editor installed)'),
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      ShellExecute (Application.Handle, 'open', PChar(lTranslation),
                    nil, nil, SW_RESTORE);
    end;

    if CheckBoxSaveSettings.Checked then
    begin
      ini := TIniFile.Create(ChangeFileExt(lTranslation, '.ini'));
      try
        ini.WriteString('ggmerge', 'template'            , lTemplate);
        ini.WriteBool  ('ggmerge', 'createbackup'        , CheckBoxCreateBackup      .Checked);
        ini.WriteBool  ('ggmerge', 'supportnonascii'     , CheckBoxNonAscii          .Checked);
        ini.WriteBool  ('ggmerge', 'preserveStateFuzzy'  , cb_PreserveStateFuzzy     .Checked);
        ini.WriteBool  ('ggmerge', 'createfilenewremoved', cb_CreateRemovedAndNewFile.Checked);
      finally
        FreeAndNil (ini);
      end;
    end;
    Close;
  finally
    screen.cursor:=crDefault;
  end;
end;

procedure TFormRun.FormCreate(Sender: TObject);
var
  lIni: TIniFile;
  lIniFileName: String;
begin
  if paramcount=1 then
  begin
    EditTranslation.Text := ExpandFileName(paramstr(1));
  end;

  TranslateComponent (self);
  FormResize (self);

  lIniFileName := ChangeFileExt(EditTranslation.Text, '.ini');

  CheckBoxSaveSettings.Checked := FileExists(lIniFileName);

  if CheckBoxSaveSettings.Checked then
  begin
    lIni := TIniFile.Create(lIniFileName);
    try
      EditTemplate              .Text    := lIni.ReadString('ggmerge', 'template'            , '');
      CheckBoxCreateBackup      .Checked := lIni.ReadBool  ('ggmerge', 'createbackup'        , CheckBoxCreateBackup      .Checked);
      CheckBoxNonAscii          .Checked := lIni.ReadBool  ('ggmerge', 'supportnonascii'     , CheckBoxNonAscii          .Checked);
      cb_PreserveStateFuzzy     .Checked := lIni.ReadBool  ('ggmerge', 'preserveStateFuzzy'  , cb_PreserveStateFuzzy     .Checked);
      cb_CreateRemovedAndNewFile.Checked := lIni.ReadBool  ('ggmerge', 'createfilenewremoved', cb_CreateRemovedAndNewFile.Checked);
    finally
      FreeAndNil(lIni);
    end;
  end;
  Caption := Caption + ' (ggmerge ' + Version + ')';
end;

procedure TFormRun.FormResize(Sender: TObject);
begin
  ButtonGo.Left:=(Width-ButtonGo.Width) div 2;
end;

procedure TFormRun.ButtonChooseTranslationClick(Sender: TObject);
var
  lod: TOpenDialog;
begin
  lod := TOpenDialog.Create(self);
  try
    lod.FileName := EditTranslation.Text;
    lod.DefaultExt := 'po';
    lod.Filter := _('Translation files (*.po)|*.po|All files (*.*)|*.*');
    lod.Options := [ofHideReadOnly, ofNoChangeDir, ofPathMustExist,
                    ofFileMustExist, ofNoReadOnlyReturn, ofEnableSizing];
    if lod.Execute then
      EditTranslation.Text := lod.FileName;
  finally
    FreeAndNil (lod);
  end;
end;

procedure TFormRun.ButtonChooseTemplateClick(Sender: TObject);
var
  lod: TOpenDialog;
begin
  lod := TOpenDialog.Create(self);
  try
    lod.FileName := EditTemplate.Text;
    lod.DefaultExt := 'po';
    lod.Filter := _('Template files (*.po;*.pot)|*.po;*.pot|All files (*.*)|*.*');
    lod.Options := [ofHideReadOnly, ofNoChangeDir, ofPathMustExist,
                    ofFileMustExist, ofNoReadOnlyReturn, ofEnableSizing];
    if lod.Execute then
    begin
      EditTemplate.Text := lod.FileName;
    end;
  finally
    FreeAndNil (lod);
  end;
end;

end.

