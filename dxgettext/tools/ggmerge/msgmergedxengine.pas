unit msgmergedxengine;

interface

uses
  sysutils, classes, poparser, consoleoutput, gnugettext, xgettexttools,
  ConsoleAppHandler;

type
  TMsgMergeDxEngine = class
  private
    FIgnoreComments: Boolean;
    FPreserveStateFuzzy: Boolean;
    FOnlyNewAndChangedTranslations: Boolean;
    FUseGetTextDefaultFormatting: Boolean;
    FCreateRemovedAndNewFile: Boolean;
    procedure MergeTranslationFileWithTemplate( var xTemplateFile: TextFile;
                                                xOutputFileStream: TFileStream);
    procedure ExtractOnlyNewAndChangedTranslations( var xTemplateFile: TextFile;
                                                    xOutputFileStream: TFileStream);
    function MergeCommentText( const xCommentTemplate,
                                     xCommentTranslation: String): String;
    function GetRemovedAndNewFileName( const xFileNameTimeStamp: TDateTime;
                                       const xTranslationFile,
                                             xSuffix: String): TFileName;
    procedure WriteRemovedAndNewFile( xFile: TFileName);
  public
    TranslationFileName,
    TemplateFileName: string;
    OutputFileName: string;
    procedure Execute;
    property IgnoreComments: Boolean read FIgnoreComments write FIgnoreComments;
    property PreserveStateFuzzy: Boolean read FPreserveStateFuzzy write FPreserveStateFuzzy;
    property OnlyNewAndChangedTranslations: Boolean read FOnlyNewAndChangedTranslations write FOnlyNewAndChangedTranslations;
    property UseGetTextDefaultFormatting: Boolean read FUseGetTextDefaultFormatting write FUseGetTextDefaultFormatting;
    property CreateRemovedAndNewFile: Boolean  read FCreateRemovedAndNewFile write FCreateRemovedAndNewFile;
  end;

implementation

{ TMsgImportEngine }

function TMsgMergeDxEngine.MergeCommentText( const xCommentTemplate,
                                                   xCommentTranslation: String): String;
var
  lCommentTemplate, lCommentTranslation: String;
begin
  Result := '';

  lCommentTemplate    := Trim(xCommentTemplate);
  lCommentTranslation := Trim(xCommentTranslation);

  if (lCommentTemplate <> '') or (lCommentTranslation <> '') then
  begin
    if (lCommentTemplate = '') then
    begin
      Result := lCommentTranslation;
    end
    else if (lCommentTranslation = '') then
    begin
      Result := lCommentTemplate;
    end
    else
    begin
      if (Pos(lCommentTemplate, lCommentTranslation) > 0) then
      begin
        Result := lCommentTranslation;
      end
      else if (Pos(lCommentTranslation, lCommentTemplate) > 0) then
      begin
        Result := lCommentTemplate;
      end
      else
      begin
        Result := lCommentTemplate + sLineBreak + lCommentTranslation;
      end;
    end;
  end;
end;

procedure TMsgMergeDxEngine.MergeTranslationFileWithTemplate( var xTemplateFile: TextFile;
                                                              xOutputFileStream: TFileStream);
var
  lTransList: TPoEntryList;
  lPe, lPeTr: TPoEntry;
  lParser: TPoParser;
  lNewUserComment: String;
begin
  if Assigned(xOutputFileStream) then
  begin
    lTransList := TPoEntryList.Create;
    try
      lTransList.LoadFromFile(translationfilename);
      lParser := TPoParser.Create;
      try
        while true do
        begin
          lPe := lParser.ReadNextEntry(xTemplateFile);
          if lPe = nil then
          begin
            // exit function after last entry
            break;
          end;

          lPeTr := lTransList.Find(lPe.MsgId);
          if lPeTr <> nil then
          begin
            lPe.MsgStr := lPeTr.MsgStr;

            lNewUserComment := MergeCommentText( lPe.UserCommentList  .Text,
                                                 lPeTr.UserCommentList.Text);
            if (lNewUserComment <> '') then
            begin
              lPe.UserCommentList.Text := lNewUserComment;
            end;

            if FPreserveStateFuzzy then
            begin
              lPe.Fuzzy              := lPeTr.Fuzzy;
            end;
          end;

          lPe.WriteToStream(xOutputFileStream);
        end;
      finally
        FreeAndNil (lParser);
      end;
    finally
      FreeAndNil (lTransList);
    end;
  end;

  if UseGetTextDefaultFormatting then
  begin
    FormatOutputWithMsgCat( xOutputFileStream);
  end;
end;

procedure TMsgMergeDxEngine.ExtractOnlyNewAndChangedTranslations( var xTemplateFile: TextFile;
                                                                  xOutputFileStream: TFileStream);
var
  lTransList: TPoEntryList;
  lPe, lPeTr: TPoEntry;
  lParser: TPoParser;
begin
  if Assigned(xOutputFileStream) then
  begin
    lTransList := TPoEntryList.Create;
    try
      lTransList.LoadFromFile(translationfilename);
      lParser := TPoParser.Create;
      try
        while true do
        begin
          lPe := lParser.ReadNextEntry(xTemplateFile);
          if lPe = nil then
          begin
            // exit function after last entry
            break;
          end;

          lPeTr := lTransList.Find(lPe.MsgId);

          if lPeTr <> nil then
          begin
            if (lPe.MsgStr               <> lPeTr.MsgStr) or
               (not FIgnoreComments and
                (lPe.UserCommentList.Text <> lPeTr.UserCommentList.Text)) or
               (lPe.Fuzzy                <> lPeTr.Fuzzy) or
               (lPe.IsObjectPascalFormat <> lPeTr.IsObjectPascalFormat) then
            begin
              lPe.WriteToStream(xOutputFileStream);
            end;
          end;
        end;
      finally
        FreeAndNil (lParser);
      end;
    finally
      FreeAndNil (lTransList);
    end;
  end;
end;

procedure TMsgMergeDxEngine.Execute;
var
  lTemplateFile: TextFile;
  lFs: TFileStream;
begin
  FileMode:=fmOpenRead;
  AssignFile (lTemplateFile, templatefilename);
  Reset (lTemplateFile);
  try
    lFs := TFileStream.Create(OutputFileName, fmCreate);
    try
      if FOnlyNewAndChangedTranslations then
      begin
        ExtractOnlyNewAndChangedTranslations(lTemplateFile, lFs);
      end
      else
      begin
        MergeTranslationFileWithTemplate(lTemplateFile, lFs);
      end;
    finally
      FreeAndNil (lFs);
    end;
  finally
    CloseFile (lTemplateFile);
  end;

  if FCreateRemovedAndNewFile then
  begin
    WriteRemovedAndNewFile( OutputFileName);
  end;
end;

function TMsgMergeDxEngine.GetRemovedAndNewFileName( const xFileNameTimeStamp: TDateTime;
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

procedure TMsgMergeDxEngine.WriteRemovedAndNewFile( xFile: TFileName);
var
  lCurrentDirectory: TFileName;
  lFileNameTimeStamp: TDateTime;
  lRemovedAndNewFileName: String;
  lAppOutput: TStringList;
  lRes: Integer;
const
  cmsgRemoveExe = 'msgremove.exe';
begin
  lFileNameTimeStamp := Now;

  GetDir( 0, lCurrentDirectory);
  try
    ChDir( ExtractFilePath( ParamStr( 0)));

    lAppOutput := nil;
    try
      lAppOutput := TStringList.Create;


      //*** Create a file with the removed Strings
      lRemovedAndNewFileName := GetRemovedAndNewFileName( lFileNameTimeStamp,
                                                          translationfilename,
                                                          'removed');
      lRes := ExecConsoleApp( cmsgRemoveExe,
                              ' "' + translationfilename + '" ' +
                              '-i "' + OutputFileName + '" ' +
                              '-o "' + lRemovedAndNewFileName + '"',
                              lAppOutput,
                              nil);
      if ( lRes <> 0) then
      begin
        raise Exception.Create( Format( _('%s failed with exit code %s.'),
                                        [ cmsgRemoveExe,
                                          IntToStr( lRes)]));
      end;


      //*** Create a file with the new Strings
      lRemovedAndNewFileName := GetRemovedAndNewFileName( lFileNameTimeStamp,
                                                          translationfilename,
                                                          'new');
      lRes := ExecConsoleApp( cmsgRemoveExe,
                              ' "' + OutputFileName + '" ' +
                              '-i "' + translationfilename + '" ' +
                              '-o "' + lRemovedAndNewFileName + '"',
                              lAppOutput,
                              nil);
      if ( lRes <> 0) then
      begin
        raise Exception.Create( Format( _('%s failed with exit code %s.'),
                                        [ cmsgRemoveExe,
                                          IntToStr( lRes)]));
      end;
    finally
      FreeAndNil( lAppOutput);
    end;
  finally
    ChDir(lCurrentDirectory);
  end;
end;

end.

