unit msgmergedxengine;

interface

uses
  sysutils, classes, poparser, consoleoutput, gnugettext;

type
  TMsgMergeDxEngine = class
  private
    FIgnoreComments: Boolean;
    FPreserveStateFuzzy: Boolean;
    FOnlyNewAndChangedTranslations: Boolean;
    procedure MergeTranslationFileWithTemplate( var xTemplateFile: TextFile;
                                                xOutputFileStream: TFileStream);
    procedure ExtractOnlyNewAndChangedTranslations( var xTemplateFile: TextFile;
                                                    xOutputFileStream: TFileStream);
    function MergeCommentText( const xCommentTemplate,
                                     xCommentTranslation: String): String;
  public
    TranslationFileName,
    TemplateFileName: string;
    OutputFileName: string;
    procedure Execute;
    property IgnoreComments: Boolean read FIgnoreComments write FIgnoreComments;
    property PreserveStateFuzzy: Boolean read FPreserveStateFuzzy write FPreserveStateFuzzy;
    property OnlyNewAndChangedTranslations: Boolean read FOnlyNewAndChangedTranslations write FOnlyNewAndChangedTranslations;
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
end;



end.

