unit EditorUi;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl                            *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dxgettext.po.dk/ for more information            *)
(*                                                              *)
(****************************************************************)

(*
A note from Lars (24-9-2009) concerning labels (what is and what isn't):

"Fuzzy" and "Untranslated" are not labels - they are settings for each
message, according to the PO file standard and the gnugettext standard,
and they must be shown in the first column. A message is untranslated
if msgstr="". If msgstr<>"", there can be a fuzzy setting. All items
with msgstr="" and all items with fuzzy are disregarded when running
msgfmt.exe which generates mo files.

"Ignore", "Ignorer", "Disregard" etc. are Gorm-editor-specific labels
picked more or less randomly by the enduser, which should not be shown
in the grid, because they have no relevance outside Gorm. The label
system was introduced in Gorm in order to keep track of items, to select
them in different ways. It's basically an item selection system that
no other applications can read.
*)

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Menus,
  StdCtrls,
  Grids,
  CheckLst,
  TypInfo,
  Buttons,
  ActnList,
  poparser,
  utils,
  u_TranslatorEngine,
  ImgList,
  w_TextFilter,
  u_TranslationRepository,
  TranslationsMemory,
  ComCtrls,
  ToolWin;

const
  strIgnore = 'ignore';
  strFuzzy = 'fuzzy';

type
  TFormEditor = class(TForm)
    PanelList: TPanel;
    PanelDetails: TPanel;
    MenuMain: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    PanelFilter: TPanel;
    Splitter: TSplitter;
    MenuItemAbout: TMenuItem;
    EditFilterText: TLabeledEdit;
    MenuLabels: TMenuItem;
    MenuItemAddLabel: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    Grid: TStringGrid;
    MenuItemRunApplication: TMenuItem;
    LabelRowCount: TLabel;
    MenuItemUseMoFile: TMenuItem;
    MenuTools: TMenuItem;
    MenuItemApplyLabelsBasedOnRules: TMenuItem;
    ComboBoxLabel: TComboBox;
    MenuItemApplyWarningLabel: TMenuItem;
    MenuItemPreferences: TMenuItem;
    LabelLabelFilter: TLabel;
    MenuItemEnableGoogleTranslate: TMenuItem;
    MenuItemAutomatictranslation: TMenuItem;
    MenuTranslations: TMenuItem;
    MenuItemClearalltranslations: TMenuItem;
    MenuItemGeneraterandomtranslations: TMenuItem;
    MenuItemAutofilltranslations: TMenuItem;
    MenuItemHashInsteadOfLetters: TMenuItem;
    MenuItemCopyoriginal: TMenuItem;
    MenuAutoTranslate: TMenuItem;
    MenuItemCopyallitemstoclipboard: TMenuItem;
    N5: TMenuItem;
    TimerTranslate: TTimer;
    StaticTextFilterExplanation: TStaticText;
    MenuItemCopyMsgIds: TMenuItem;
    MenuItemCopyForProgrammers: TMenuItem;
    mi_LabelsAddstandardlabels: TMenuItem;
    N6: TMenuItem;
    N3: TMenuItem;
    mi_HelpHelp: TMenuItem;
    MenuItemCompiletoMOfile: TMenuItem;
    MenuItemGormhomepage: TMenuItem;
    MenuItemGNUgettextforDelphi: TMenuItem;
    MenuItemAppendtoexistingpofile: TMenuItem;
    chkInverseFilter: TCheckBox;
    PanelBottomLeft: TPanel;
    PanelComments: TPanel;
    PanelSourceLoc: TPanel;
    PanelMessages: TPanel;
    LabelProgrammerComments: TLabel;
    ListBoxProgrammerComments: TListBox;
    LabelSourceLocation: TLabel;
    ListBoxSource: TListBox;
    LabelMessages: TLabel;
    MemoMessages: TMemo;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    PanelBottomCenter: TPanel;
    PanelTranslation: TPanel;
    MemoMsgStr: TMemo;
    Panel9: TPanel;
    LabelMsgStr: TLabel;
    PanelAutoTranslation: TPanel;
    PanelOriText: TPanel;
    LabelMsgId: TLabel;
    MemoMsgId: TMemo;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Splitter6: TSplitter;
    Splitter7: TSplitter;
    PanelBottomRight: TPanel;
    PanelLabels: TPanel;
    LabelLabels: TLabel;
    CheckListBoxLabels: TCheckListBox;
    PanelEditableComments: TPanel;
    LabelEditableComments: TLabel;
    MemoEditableComments: TMemo;
    Splitter8: TSplitter;
    mi_AutoDisable: TMenuItem;
    mi_AutoMicrosoft: TMenuItem;
    ButtonMoveDown: TSpeedButton;
    ButtonMoveUp: TSpeedButton;
    chkQuickNavigation: TCheckBox;
    btnRefreshFilter: TBitBtn;
    ButtonShowAll: TBitBtn;
    N4: TMenuItem;
    ButtonCopyMsgId2MsgStr: TSpeedButton;
    ButtonParallel: TSpeedButton;
    PanelButtons: TPanel;
    SpeedButtonQuickFuzzy: TSpeedButton;
    SpeedButtonQuickIgnore: TSpeedButton;
    MenuItemSynch: TMenuItem;
    MenuItemReopen: TMenuItem;
    MenuItemSendbyemail: TMenuItem;
    MenuItemCreateHTMLpage: TMenuItem;
    N7: TMenuItem;
    MenuItemFindfirstwarning1: TMenuItem;
    MenuItemFindnextwarning1: TMenuItem;
    N8: TMenuItem;
    act_LabelsUnfuzzySelection: TAction;
    mi_LabelsUnfuzzySelection: TMenuItem;
    act_LabelsFuzzySelection: TAction;
    mi_LabelsFuzzySelection: TMenuItem;
    MenuItemLabelRemoveLabelFromSelection: TMenuItem;
    N9: TMenuItem;
    MenuItemLabelApplyLabelToSelection: TMenuItem;
    TheActionList: TActionList;
    act_FileOpen: TAction;
    act_FileSyncWithOther: TAction;
    act_FileReadFrom: TAction;
    act_FileSave: TAction;
    act_FileAppendTo: TAction;
    act_FileSaveAs: TAction;
    act_FileReload: TAction;
    act_LabelsSaveIgnore: TAction;
    act_LabelsLoadIgnore: TAction;
    act_FileHeader: TAction;
    act_FileExit: TAction;
    act_AutoDisable: TAction;
    act_AutoGoogle: TAction;
    act_AutoMicrosoft: TAction;
    act_AutoMo: TAction;
    act_AutoPo: TAction;
    act_AutoRepository: TAction;
    act_LabelsToggleIgnore: TAction;
    act_LabelsToggleFuzzy: TAction;
    act_LabelsAddLabel: TAction;
    act_LabelsAddStandardLabels: TAction;
    act_LabelsApplyByRule: TAction;
    act_LabelsApplyWarning: TAction;
    act_LabelsApplyToSelection: TAction;
    act_LabelsRemoveFromSelection: TAction;
    act_TransAutomaticTranslation: TAction;
    act_TransCopyMsgid: TAction;
    act_TransCopyWithComments: TAction;
    act_TransSendEmail: TAction;
    act_TransCreateHtml: TAction;
    act_TransAutofillGorm: TAction;
    act_TransAutofillHashes: TAction;
    act_TransAutofillCopy: TAction;
    act_TransClear: TAction;
    act_ToolsPreferences: TAction;
    act_ToolsFindFirstWarning: TAction;
    act_ToolsFindNextWarning: TAction;
    act_ToolsCompileToMo: TAction;
    act_ToolsRun: TAction;
    act_ToolsRepositoryShow: TAction;
    act_ToolsRepositoryLearn: TAction;
    TheImageList: TImageList;
    N10: TMenuItem;
    mi_LabelsToggleIgnore: TMenuItem;
    mi_LabelsToggleFuzzy: TMenuItem;
    mi_AutoUsePOfile: TMenuItem;
    mi_AutoUseRepository: TMenuItem;
    mi_ToolsRepository: TMenuItem;
    mi_ToolsRepositoryShow: TMenuItem;
    mi_ToolsRepositoryLearn: TMenuItem;
    b_FilterSelect: TButton;
    mi_FileEditHeader: TMenuItem;
    TheStatusBar: TStatusBar;
    N1: TMenuItem;
    mi_FileAddTo: TMenuItem;
    mi_FileSaveAs: TMenuItem;
    mi_FileReload: TMenuItem;
    mi_LabelsLoadIgnore: TMenuItem;
    mi_LabelsSaveIgnore: TMenuItem;
    N11: TMenuItem;
    listAutoTranslation: TListBox;
    lblParallelLanguageTranslation: TLabel;
    btSpecChars: TBitBtn;
    mi_LabelsImportIgnore: TMenuItem;
    act_LabelsImportIgnore: TAction;
    N12: TMenuItem;
    act_TransCreateMsgstr: TAction;
    Createtranslationtrackingmsgstr1: TMenuItem;
    mi_GormDiffHelp: TMenuItem;
    procedure MenuItemAboutClick(Sender: TObject);
    procedure act_FileOpenExecute(Sender: TObject);
    procedure act_FileReloadExecute(Sender: TObject);
    procedure act_FileSaveExecute(Sender: TObject);
    procedure act_FileSaveAsExecute(Sender: TObject);
    procedure act_FileSyncWithOtherExecute(Sender: TObject);
    procedure act_FileAppendToExecute(Sender: TObject);
    procedure act_FileHeaderExecute(Sender: TObject);
    procedure act_FileReadFromExecute(Sender: TObject);
    procedure act_FileExitExecute(Sender: TObject);
    procedure act_AutoDisableExecute(Sender: TObject);
    procedure act_AutoGoogleExecute(Sender: TObject);
    procedure act_AutoMicrosoftExecute(Sender: TObject);
    procedure act_AutoMoExecute(Sender: TObject);
    procedure act_AutoPoExecute(Sender: TObject);
    procedure act_AutoRepositoryExecute(Sender: TObject);
    procedure MenuItemOpenMRUClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditFilterTextChange(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure act_ToolsRunExecute(Sender: TObject);
    procedure MenuItemAddLabelClick(Sender: TObject);
    procedure act_LabelsApplyByRuleExecute(Sender: TObject);
    procedure ListBoxProgrammerCommentsClick(Sender: TObject);
    procedure ListBoxSourceClick(Sender: TObject);
    procedure ComboBoxLabelChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ButtonCopyMsgId2MsgStrClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure act_LabelsApplyWarningExecute(Sender: TObject);
    procedure act_ToolsPreferencesExecute(Sender: TObject);
    procedure MemoMsgStrChange(Sender: TObject);
    procedure ButtonShowAllClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure act_TransAutomaticTranslationExecute(Sender: TObject);
    procedure act_TransClearExecute(Sender: TObject);
    procedure act_TransAutofillGormExecute(Sender: TObject);
    procedure act_TransAutofillHashesExecute(Sender: TObject);
    procedure act_TransAutofillCopyExecute(Sender: TObject);
    procedure ButtonParallelClick(Sender: TObject);
    procedure TimerTranslateTimer(Sender: TObject);
    procedure act_TransCopyMsgIdExecute(Sender: TObject);
    procedure act_TransCopyWithCommentsExecute(Sender: TObject);
    procedure act_LabelsAddStandardLabelsExecute(Sender: TObject);
    procedure mi_HelpHelpClick(Sender: TObject);
    procedure act_ToolsCompileToMoExecute(Sender: TObject);
    procedure MenuItemGormhomepageClick(Sender: TObject);
    procedure MenuItemGNUgettextforDelphiClick(Sender: TObject);
    procedure ButtonMoveDownClick(Sender: TObject);
    procedure ButtonMoveUpClick(Sender: TObject);
    procedure chkInverseFilterClick(Sender: TObject);
    procedure btnRefreshFilterClick(Sender: TObject);
    procedure act_LabelsToggleFuzzyExecute(Sender: TObject);
    procedure MemoMsgStrKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure act_LabelsToggleIgnoreExecute(Sender: TObject);
    procedure CheckListBoxLabelsClick(Sender: TObject);
    procedure CheckListBoxLabelsClickCheck(Sender: TObject);
    procedure act_TransSendEmailExecute(Sender: TObject);
    procedure act_TransCreateHtmlExecute(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure act_ToolsFindFirstWarningExecute(Sender: TObject);
    procedure act_ToolsFindNextWarningExecute(Sender: TObject);
    procedure act_LabelsUnfuzzySelectionExecute(Sender: TObject);
    procedure act_LabelsFuzzySelectionExecute(Sender: TObject);
    procedure act_LabelsApplyToSelectionExecute(Sender: TObject);
    procedure act_LabelsRemoveFromSelectionExecute(Sender: TObject);
    procedure b_FilterSelectClick(Sender: TObject);
    procedure act_ToolsRepositoryShowExecute(Sender: TObject);
    procedure act_ToolsRepositoryLearnExecute(Sender: TObject);
    procedure ActivateTranslationsMemory(Sender: TObject);
    procedure listAutoTranslationDblClick(Sender: TObject);
    procedure btSpecCharsClick(Sender: TObject);
    procedure act_LabelsLoadIgnoreExecute(Sender: TObject);
    procedure act_LabelsImportIgnoreExecute(Sender: TObject);
    procedure act_LabelsSaveIgnoreExecute(Sender: TObject);
    procedure act_TransCreateMsgstrExecute(Sender: TObject);
    procedure mi_GormDiffHelpClick(Sender: TObject);
  private
    CurrentFilename: string; // File currently opened (if any)
    Items: TPoEntryList; // Contains all item read from CurrentFilename
    Labels: TStringList; // User defined labels (see AddStandardLabels)
    Rows: TStringList; // Contains (sorted) list of selected TPoEntry objects (affected by Filter)
    CurrentItem: TPoEntry;
    CurrentItemRow: integer; // The row index of Grid where CurrentItem belongs to.
                             // Must be set every time CurrentItem is set. First index is 0.
    Translator: TTranslatorEngine;
    FOpenMRU: TStringList; // Open Most Recently Used
    FTextSearchWhere: TSearchWhereSet;
    FTextSearchInv: boolean; // if true, the Text Search is inverted
    FTranslationRepository: TTranslationRepository;
    TrMem : TTranslationsMemory;  // Contains older translations
    FSpecChars : TStringList;     // Contains the list of special chars grouped by similarity. Readed from .ini file.

    FIngoreImportFilename: string; // stores the filename to import ignore labels from
    FDropfilesActivator: TObject;

    procedure ExecuteFilter;
    procedure CloseGuiItem;
    procedure OpenItem;
    procedure ApplyLabelList(_LabelToSelect: string = strIgnore);
    procedure ScanItemsForLabels;
    procedure FileSave;
    procedure GridSelectFirst;
    procedure UpdateWarnings;
    procedure UpdateGridRow(Row: Integer; item: TPoEntry);
    procedure MoveToNextRow;
    procedure MoveToPreviousRow;
    procedure LoadFileByName(const _FileName: string);
    procedure LoadPostProcess;
    procedure AddStandardLabels;
    procedure EnableGoogleTranslate(const GtCode: string);
    procedure EnableMicrosoftTranslate(const _AppId, _LngCode: string);
    procedure CompileToMoFile;
    procedure AddOpenMRU(AIndex: Integer);
    procedure CreateHTML;
    procedure FindWarning(iFirst: Integer);
    procedure SetTranslator(_Translator: TTranslatorEngine);
    procedure ShowHintOnStatusBar(_Sender: TObject);
    procedure InitStatusBar;
    function SelectFileToSave(const _FileType, _FileMask: string; var _Filename: string): boolean;
    function SelectPoFileToSave(var _Filename: string): boolean;
    function SelectFileToOpen(const _FileType, _FileMask: string; var _Filename: string): boolean;
    function SelectPoFileToOpen(var _Filename: string): boolean;

    // this procedures handles
    procedure SpecCharsKeyDown(Sender: TObject; var Key: Char);
    procedure SpecCharsClick( Sender: TObject; Button: TMouseButton;
                              Shift: TShiftState; X, Y: Integer);
    procedure RemoveFuzzyFromEmtpyTranslations;
    procedure AutoTranslate;
    function EditHeader(_ForceLanguageInput: boolean): boolean;
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
    procedure LoadFileInteractive(const _Filename: string);
  public
  end;

var
  FormEditor: TFormEditor;

implementation

uses
  gnugettext,
  Math,
  StrUtils,
  EditLabel,
  LabelApplicationByRulesUi,
  Clipbrd,
  Character,
  PoTools,
  ShellApi,
  HelpUi,
  PreferencesUi,
  AppSettings,
  ConsoleAppHandler,
  ToolsAutoUpgrader,
  AutoTranslateOptions,
  GoogleTranslateSettings,
  w_MicrosoftTranslateSettings,
  u_TranslatorEngineGoogle,
  u_TranslatorEngineMicrosoft,
  u_TranslatorEngineGetText,
  u_TranslatorEnginePoFile,
  u_TranslatorEngineRepository,
  w_TranslationDb,
  u_TranslationDBAccess,
  w_EditHeader,
  u_Languages,
  w_dzProgress,
  u_dzOsUtils,
  u_dzVclUtils,
  w_IgnoreLoad,
  w_IgnoreImport,
  w_IgnoreSave,
  w_TranslationDbLearnOptions,
  PoDiffHtml
  ;

{$R *.dfm}

type
  TVersionInfo = record
    MajorVersion, MinorVersion, Release, Build: Word;
  end;

function GetVersionInfo(const AFileName: string): TVersionInfo;

var
  sFileName: string;
  iBufferSize: DWORD;
  iDummy: DWORD;
  pBuffer: Pointer;
  pFileInfo: Pointer;

begin
  with Result do begin
    Result.MajorVersion := 0;
    Result.MinorVersion := 0;
    Result.Release := 0;
    Result.Build := 0;
  end;

  // get filename of exe/dll if no filename is specified
  sFileName := AFileName;
  if (sFileName = '') then begin
    // prepare buffer for path and terminating #0
    SetLength(sFileName, MAX_PATH + 1);
    SetLength(sFileName, GetModuleFileName(hInstance, PChar(sFileName), MAX_PATH + 1));
  end;

  // get size of version info (0 if no version info exists)
  iBufferSize := GetFileVersionInfoSize(PChar(sFileName), iDummy);
  if (iBufferSize > 0) then begin
    GetMem(pBuffer, iBufferSize);
    try
      // get fixed file info (language independent)
      GetFileVersionInfo(PChar(sFileName), 0, iBufferSize, pBuffer);
      VerQueryValue(pBuffer, '\', pFileInfo, iDummy);

      // read version blocks
      Result.MajorVersion := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
      Result.MinorVersion := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
      Result.Release := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
      Result.Build := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    finally
      FreeMem(pBuffer);
    end;
  end;
end;

{----------------------------------------------------------------------}

procedure TFormEditor.EditFilterTextChange(Sender: TObject);
begin
  CloseGuiItem;
  ExecuteFilter;
  GridSelectFirst;
end;

procedure TFormEditor.ExecuteFilter;

  function MatchesProgrammerComments(_Item: TPoEntry; const _SearchTerm: string): boolean;
  var
    sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      ExtractProgrammerComments(_Item, sl);
      Result := (Pos(_SearchTerm, AnsiLowerCase(sl.Text)) > 0);
    finally
      FreeAndNil(sl);
    end;
  end;

  function MatchesSourcecode(_Item: TPoEntry; const _SearchTerm: string): boolean;
  var
    sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      ExtractSourcecodeLocations(_Item, sl);
      Result := (Pos(_SearchTerm, AnsiLowerCase(sl.Text)) > 0);
    finally
      FreeAndNil(sl);
    end;
  end;

  function MatchesSearchTerm(_Item: TPoEntry; const _SearchTerm: string): boolean;
  begin
    if _SearchTerm = '' then
      Result := not FTextSearchInv
    else begin
      Result := False;
      if not Result and (swMsgId in FTextSearchWhere) then begin
        if Pos(_SearchTerm, AnsiLowerCase(_Item.MsgId)) >= 1 then
          Result := True;
      end;
      if not Result and (swMsgStr in FTextSearchWhere) then begin
        if Pos(_SearchTerm, AnsiLowerCase(_Item.MsgStr)) >= 1 then
          Result := True;
      end;
      if not Result and (swComments in FTextSearchWhere) then begin
        if MatchesProgrammerComments(_Item, _SearchTerm) then
          Result := True;
      end;
      if not Result and (swSource in FTextSearchWhere) then begin
        if MatchesSourcecode(_Item, _SearchTerm) then
          Result := True;
      end;
    end;
  end;

var
  item: TPoEntry;
  i: integer;
  SearchTerm, sortkey: string;
  LabelName: string;
  LabelInverted: boolean;
  FuzzyFilterOn: boolean;
  icountfiltered, countfuzzy, countuntranslated: integer;
begin
  CloseGuiItem;
  SearchTerm := AnsiLowerCase(EditFilterText.Text);
  i := ComboBoxLabel.ItemIndex;
  if i <> -1 then
    LabelName := ComboBoxLabel.Items[i]
  else
    LabelName := '';
  LabelInverted := chkInverseFilter.Checked;
  if LabelName = strFuzzy then begin
    FuzzyFilterOn := true;
    LabelName := '';
  end else
    FuzzyFilterOn := false;

  Rows.Clear;
  item := items.FindFirst;
  countfuzzy := 0;
  countuntranslated := 0;
  icountfiltered := 0;
  while item <> nil do begin
    if (item.MsgId <> '') then begin
      if (MatchesSearchTerm(item, SearchTerm) xor FTextSearchInv)
        and ((LabelName = '') or
        (not LabelInverted and HasLabel(item, LabelName)) or
        (LabelInverted and not HasLabel(item, LabelName))
        )
        and ((not FuzzyFilterOn) or
        (not LabelInverted and item.Fuzzy) or
        (LabelInverted and not item.Fuzzy)
        ) then begin
        if item.MsgStr = '' then begin
          sortkey := '0';
          inc(countuntranslated);
        end else if item.Fuzzy then begin
          sortkey := '1';
          inc(countfuzzy);
        end else
          sortkey := '2';

        sortkey := sortkey + trim(item.MsgId);
        Rows.AddObject(sortkey, item);
      end else
        inc(icountfiltered);
    end;
    item := items.FindNext(item);
  end;
  LabelRowCount.Caption := Format(_('%d total, %d untranslated, %d fuzzy, %d filtered'), [Rows.Count, countuntranslated, countfuzzy, icountfiltered]);
  Grid.RowCount := Rows.Count + 1;
  Grid.Cells[0, 0] := 'Status';
  Grid.Cells[1, 0] := 'MsgId';
  Grid.Cells[2, 0] := 'MsgStr';
  Grid.FixedRows := min(Rows.Count, 1);
  for i := 0 to Rows.Count - 1 do begin
    item := Rows.Objects[i] as TPoEntry;
    UpdateGridRow(i + 1, item);
  end;
end;

procedure TFormEditor.act_TransCopyMsgIdExecute(Sender: TObject);
var
  i: integer;
  item: TPoEntry;
  s: string;
begin
  CloseGuiItem;
  s := '';
  for i := 0 to Rows.Count - 1 do begin
    item := Rows.Objects[i] as TPoEntry;
    s := s + 'MSGID:' + sLineBreak + item.MsgId + sLineBreak + sLinebreak;
  end;
  Clipboard.AsText := s;
end;

procedure TFormEditor.FileSave;
var poe : TPOEntry;
    timestamp : TDateTime;
begin
  Screen.Cursor := crHourGlass;
  try
    TheStatusBar.SimpleText := _('Saving file...');
    if CurrentFilename = '' then
      raise Exception.Create(_('No file is currently open.'));
    if Items.Language = '' then
      raise Exception.Create(_('The language of this po file is unknown, please edit the header first to provide it.'));
    items.SaveToFile(CurrentFilename, GetSettingSaveWrapAfter);
    TheStatusBar.SimpleText := '';

    // by VGL: Write all translations to translations memory file

    if GetSettingTranslationMemory and Assigned(TrMem) Then begin
      timestamp := now;

      poe := items.FindFirst;
      while (poe<>nil) do begin
        TrMem.AddItemToTranslationsMemory(poe, CurrentFileName, timestamp);
        poe := items.FindNext(poe);
      end;

      TheStatusBar.SimpleText := _('Saving translation memory file...');
      TrMem.SaveMemoryFile;
      TheStatusBar.SimpleText := '';
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormEditor.GridSelectFirst;
begin
  if Grid.RowCount >= 2 then begin
    Grid.Row := 1;
    OpenItem;
  end;
end;

procedure TFormEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CurrentFilename = '' then begin
    CanClose := True;
  end else begin
    CanClose := False;
    CloseGuiItem;
    case MessageDlg(_('Do you want to save?'), mtConfirmation, [mbYes, mbNo, mbCancel], 0, mbCancel) of
      mrCancel: ;
      mrYes: begin
          FileSave;
          CanClose := True;
        end;
      mrNo:
        CanClose := True;
    end;
  end;
end;

procedure TFormEditor.AddOpenMRU(AIndex: Integer);

var
  mi: TMenuItem;

begin
  mi := TMenuItem.Create(MenuItemReopen);
  mi.Caption := FOpenMRU.Strings[AIndex];
  mi.Tag := AIndex;
  mi.OnClick := MenuItemOpenMRUClick;
  mi.Name := 'miOpenMRU_' + IntToStr(AIndex);
  MenuItemReopen.Add(mi);
end;


procedure TFormEditor.ShowHintOnStatusBar(_Sender: TObject);
begin
  TStatusBar_SetLongSimpleText(TheStatusBar, Application.Hint);
end;

procedure TFormEditor.InitStatusBar;
begin
  if GetSettingShowStatus then begin
    TheStatusBar.Visible := true;
    Application.OnHint := ShowHintOnStatusBar;
  end else begin
    TheStatusBar.Visible := false;
  end;
end;

procedure TFormEditor.ActivateTranslationsMemory(Sender: TObject);
begin
  if GetSettingTranslationMemory
    then begin
      if NOT Assigned(TrMem) then TrMem := TTranslationsMemory.Create;
      TrMem.ClearTranslationsMemory;
      if assigned(items) then TrMem.aLanguage := items.Language;
      TrMem.LoadMemoryFile;
    end;
end;

procedure TFormEditor.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  if GetSettingStartupAutoUpgrade then
    AutomaticDownloadAndReplace('http://dybdahl.dk/dxgettext/gorm.exe');

  TranslateComponent(self);

  TForm_ReadPlacement(self, fpePosAndSize);

  TStatusBar_EnableLongSimpleText(TheStatusBar);
  InitStatusBar;

  FDropfilesActivator := TForm_ActivateDropFiles(self, HandleFilesDropped);

  FTextSearchWhere := [swMsgId, swMsgStr];

  items := TPoEntryList.Create;

  Rows := TStringList.Create;
  Rows.Sorted := True;
  Rows.Duplicates := dupAccept;
  Rows.CaseSensitive := False;

  Labels := TStringList.Create;
  Labels.Sorted := True;
  Labels.Duplicates := dupIgnore;
  Labels.CaseSensitive := True;

  FOpenMRU := TStringList.Create;
  ReadList('OpenMRU', FOpenMRU);
  FOpenMRU.Sorted := true;
  i := 0;
  while i < FOpenMRU.Count do begin
    if FileExists(FOpenMRU.Strings[i]) then begin
      AddOpenMRU(i);
      Inc(i);
    end else
      FOpenMRU.Delete(i);
  end;
  MenuItemReopen.Visible := (FOpenMRU.Count > 0);

  ExecuteFilter;
  CloseGuiItem;

  FTranslationRepository := TTranslationRepository.Create(GetSettingTranslationRepositoryDir);
end;

procedure TFormEditor.FormDestroy(Sender: TObject);
begin
  TForm_StorePlacement(self, fpePosAndSize);

  FreeAndNil(FDropfilesActivator);
  FreeAndNil(TrMem);
  FreeAndNil(FTranslationRepository);
  FreeAndNil(Translator);
  FreeAndNil(Labels);
  FreeAndNil(Rows);
  FreeAndNil(Items);
  FreeAndNil(FOpenMRU);
end;

procedure TFormEditor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ActiveControl <> Grid then begin
    case Key of
      VK_NEXT: MoveToNextRow;
      VK_PRIOR: MoveToPreviousRow;
    end;
  end;
  if ssCtrl in Shift then begin
    case Key of
      VK_DOWN: MoveToNextRow;
      VK_UP: MoveToPreviousRow;
    end;
  end;
  if (ssAlt in Shift) and (Key = Ord('C')) then begin
    ButtonCopyMsgId2MsgStr.Click;
  end;
end;

procedure TFormEditor.FormResize(Sender: TObject);
var
  width: integer;
begin
  width := ((Grid.Width - Grid.ColWidths[0]) - 20) div 2;
  Grid.ColWidths[1] := width;
  Grid.ColWidths[2] := width;
end;

procedure TFormEditor.FormShow(Sender: TObject);
var
  filename: string;
  s: string;
begin
  if (ParamCount>2)and(ParamStr(1)='-diff') then
    begin
    if (paramcount>3)and(paramstr(4)<>'') then
      begin
      PoDiffHtml.RunDiff(ParamStr(2),ParamStr(3),ParamStr(4));
      end
    else
      begin
      PoDiffHtml.RunDiff(ParamStr(2),ParamStr(3),ParamStr(2));
      end;
    Close;
    end
  else if paramcount = 1 then
    LoadFileByName(Paramstr(1));
  // Automatic startup actions. These may trigger exceptions!
  if (CurrentFilename = '') and GetSettingStartupActionsEnabled then begin
    if GetSettingStartupAutoCreateLabels then
      AddStandardLabels;
    s := GetSettingStartupParallelTranslation;
    if s <> '' then
      EnableGoogleTranslate(s);
    filename := GetSettingStartupOpenFilename;
    if filename <> '' then begin
      if FileExists(filename) then
        LoadFileByName(filename)
      else
        ShowMessage(format(_('Cannot find the file "%s" that contains translations. Are you sure that you have arranged the files correctly on your harddisk?'), [filename]));
    end;
  end;
end;

procedure TFormEditor.act_ToolsCompileToMoExecute(Sender: TObject);
begin
  screen.Cursor := crHourglass;
  try
    CloseGuiItem;
    CompileToMoFile;
  finally
    screen.Cursor := crDefault;
  end;
  ShowMessage(_('Finished compiling.'));
end;

procedure TFormEditor.act_TransCopyWithCommentsExecute(Sender: TObject);
var
  i, j: integer;
  item: TPoEntry;
  s, comments: string;
  sl: TStringList;
begin
  CloseGuiItem;
  s := '';
  sl := TStringList.Create;
  try
    for i := 0 to Rows.Count - 1 do begin
      item := Rows.Objects[i] as TPoEntry;
      ExtractSourcecodeLocations(item, sl);
      comments := '';
      for j := 0 to sl.Count - 1 do
        comments := comments + sl.Strings[j] + sLineBreak;
      sl.Text := item.MsgId;
      for j := 0 to sl.Count - 1 do
        comments := comments + '"' + sl.Strings[j] + '"' + sLineBreak;

      ExtractUserComments(item, sl);
      for j := 0 to sl.Count - 1 do
        comments := comments + '' + sl.Strings[j] + sLineBreak;
      s := s + comments + sLineBreak;
    end;
  finally
    FreeAndNil(sl);
  end;
  Clipboard.AsText := s;
end;

procedure TFormEditor.act_TransAutofillGormExecute(Sender: TObject);
var
  i: integer;
  item: TPoEntry;
begin
  if mrYes <> MessageDlg(_('For all selected entries copy the MsgId to the MsgStr, with "Gorm" as prefix.' + sLineBreak
    + 'This only makes sense for test purposes.' + sLineBreak
    + 'Are you really sure that you want to do that?'), mtConfirmation, [mbYes, mbCancel], 0, mbCancel) then
    exit;

  Screen.Cursor := crHourGlass;
  try
    CloseGuiItem;
    for i := 0 to Rows.Count - 1 do begin
      item := Rows.Objects[i] as TPoEntry;
      if (trim(item.MsgId) <> '') and (not item.IsPluralForm) then
        item.MsgStr := 'Gorm' + item.MsgId;
    end;
    ExecuteFilter;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormEditor.MenuItemGormhomepageClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://gorm.po.dk/', nil, nil, sw_normal);
end;

procedure TFormEditor.act_TransAutofillHashesExecute(Sender: TObject);
var
  i, j: integer;
  item: TPoEntry;
  s: string;
begin
  if mrYes <> MessageDlg(_('This will make all selected translations look very unreadable.' + sLineBreak
    + 'This only makes sense for test purposes.' + sLineBreak
    + 'Are you really sure that you want to do that?'), mtConfirmation, [mbYes, mbCancel], 0, mbCancel) then
    exit;

  Screen.Cursor := crHourGlass;
  try
    CloseGuiItem;
    for i := 0 to Rows.Count - 1 do begin
      item := Rows.Objects[i] as TPoEntry;
      if (trim(item.MsgId) <> '') and (not item.IsPluralForm) then begin
        s := item.MsgId;
        for j := 1 to length(s) do begin
          if TCharacter.IsLetter(s[j]) or (s[j] = '%') then
            s[j] := '#';
        end;
        item.MsgStr := s;
      end;
    end;
    ExecuteFilter;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormEditor.act_LabelsApplyToSelectionExecute(Sender: TObject);
var
  i: Integer;
  item: TPoEntry;
  WhichLabel: string;
begin
  CloseGuiItem;
  if not TFormLabel.Execute(self, WhichLabel) then
    exit;
  Labels.Add(WhichLabel);
  ApplyLabelList;
  for i := 0 to Rows.Count - 1 do begin
    item := Rows.Objects[i] as TPoEntry;
    AddLabel(Item, WhichLabel);
  end;
end;

procedure TFormEditor.act_LabelsRemoveFromSelectionExecute(Sender: TObject);
var
  i: Integer;
  item: TPoEntry;
  WhichLabel: string;
begin
  CloseGuiItem;
  if not TFormLabel.Execute(self, WhichLabel) then
    exit;
  Labels.Add(WhichLabel);
  ApplyLabelList;
  for i := 0 to Rows.Count - 1 do begin
    item := Rows.Objects[i] as TPoEntry;
    RemoveLabel(Item, WhichLabel);
  end;
end;

function TFormEditor.SelectFileToOpen(const _FileType, _FileMask: string; var _Filename: string): boolean;
var
  od: TOpenDialog;
begin
  od := TOpenDialog.Create(self);
  try
    od.Filter := _FileType + ' (' + _FileMask + ')|' + _FileMask
      + '|' + _('All files') + ' (*.*)|*.*';
    if _Filename <> '' then begin
      od.InitialDir := ExtractFileDir(_Filename);
      od.FileName := _Filename;
    end;
    Result := od.Execute;
    if Result then
      _Filename := od.Filename;
  finally
    FreeAndNil(od);
  end;
end;

function TFormEditor.SelectPoFileToOpen(var _Filename: string): boolean;
begin
  Result := SelectFileToOpen(_('PO Files'), '*.po', _Filename);
end;

function TFormEditor.SelectFileToSave(const _FileType, _FileMask: string; var _Filename: string): boolean;
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(self);
  try
    sd.Filter := _FileType + ' (' + _FileMask + ')|' + _FileMask
      + '|' + _('All files') + ' (*.*)|*.*';
    Result := sd.Execute;
    if Result then
      _Filename := sd.FileName;
  finally
    FreeAndNil(sd);
  end;
end;

function TFormEditor.SelectPoFileToSave(var _Filename: string): boolean;
begin
  Result := SelectFileToSave(_('PO files'), '*.po', _Filename);
end;

procedure TFormEditor.act_FileSyncWithOtherExecute(Sender: TObject);
// Synchronizes with a template file the user selects.
// All items found in the template file but not in the current file
// (i.e. MsgId not found) are added to the current file with an
// *empty* MsgStr (even if there is a translation in the template file).
// In addition, all items in the current file that are not in the
// master file are *deleted* from the current file.
// This means that afterwards the current file contains only MsgIds
// that are in the master file. The files differ only in the
// MsgStr.
// Basically that's the same thing that the msgmerge tool does.
var
  cnt_add: Integer;
  cnt_del: Integer;
  xitems: TPoEntryList;
  item: TPoEntry;
  xitem: TPoEntry;
  fn, s: string;
  n, nx: Integer;
begin
  CloseGuiItem;

  if not SelectPoFileToOpen(fn) then
    exit;

  // This prevents the custom drawing to fail because an item is deleted in the following
  // process.
  Grid.RowCount := 1;

  xitems := TPoEntryList.Create;
  try
    Screen.Cursor := crHourglass;
    xitems.Clear;
    xitems.LoadFromFile(fn);

    n := Items.Count;
    nx := xitems.Count;

    // Add all items from master file that are not found in the current file
    cnt_add := 0;
    xitem := xitems.FindFirst;
    while xitem <> nil do begin
      item := Items.Find(xitem.MsgId);
      if item = nil then begin
        xitem.MsgStr := '';
        items.Add(xitem);
        Inc(cnt_add);
      end;
      xitem := xitems.FindNext(xitem);
    end;

    // Delete all items from the current file that do not exist in the master file
    cnt_del := 0;
    item := Items.FindFirst;
    while item <> nil do begin
      xitem := xitems.Find(item.MsgId);
      s := item.MsgId;
      item := items.FindNext(item);

      if xitem = nil then begin
        Items.Delete(s);
        Inc(cnt_del);
      end;
    end;

    s := Format(_('Items in current file: %d') + #13 +
      _('Items in synch file: %d'), [n, nx]);

    if (cnt_add > 0) or (cnt_del > 0) then begin
      LoadPostProcess;
      ShowMessageFmt(_('New items added: %d') + #13 +
        _('Obsolete items removed: %d') + #13 +
        ' '#13 + s,
        [cnt_add, cnt_del]);
    end else
      ShowMessage(_('No differences found') + #13 + ' '#13 + s);
  finally
    FreeAndNil(xitems);
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormEditor.MenuItemGNUgettextforDelphiClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://dxgettext.po.dk/', nil, nil, sw_normal);
end;

procedure TFormEditor.GridClick(Sender: TObject);
begin
  screen.Cursor := crHourglass;
  try
    CloseGuiItem;
    if (Grid.Row > 0) and (Grid.Row <= Rows.Count) then OpenItem;

    if Assigned(Translator) then begin
      OpenItem;
    end;
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TFormEditor.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  item: TPoEntry;
  s: string;
begin
  if (ARow = 0) or (ARow > Rows.Count) then
    Exit;

  item := Rows.Objects[ARow - 1] as TPoEntry;
  if (item <> nil) and HasLabel(item, strIgnore) then begin
    if (gdSelected in State) then
      Grid.Canvas.Brush.Color := $1133DD // Normal is $3399FF
    else
      Grid.Canvas.Brush.Color := $D4D4D4; // A light gray
    Grid.Canvas.FillRect(Rect);
    s := Grid.Cells[ACol, ARow];
    Grid.Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, s);
  end;
end;

procedure TFormEditor.ListBoxProgrammerCommentsClick(Sender: TObject);
var
  s: string;
  Idx: Integer;
begin
  Idx := ListBoxProgrammerComments.ItemIndex;
  if Idx >= 0 then begin
    s := ListBoxProgrammerComments.Items[Idx];
    ShowMessage(s);
    Clipboard.AsText := s;
  end;
end;

procedure TFormEditor.ListBoxSourceClick(Sender: TObject);
var
  s, lBasePath, lCompletePath, lEditorFilename, lParameterStr, lMessage: string;
  Idx: Integer;
  lPoHeader: TPoEntry;
  lSourceCodeLocation: TSourceCodeLocation;
  lShowPathAsShowMessage: Boolean;
begin
  lShowPathAsShowMessage := true;
  lMessage := '';

  Idx := ListBoxSource.ItemIndex;
  if Idx >= 0 then
  begin
    s := ListBoxSource.Items[Idx];

    lEditorFilename := GetSettingApplicationExternalEditorFilename;
    if ( lEditorFilename <> '') and
       FileExists( lEditorFilename) then
    begin
      lPoHeader := Items.Find('');
      if Assigned( lPoHeader) then
      begin
        lBasePath := GetPoHeaderEntry( lPoHeader.MsgStr, PO_HEADER_Poedit_BasePath);
        if lBasePath <> '' then begin
          lBasePath := IncludeTrailingPathDelimiter(lBasePath);

          if not DirectoryExists( lBasePath) then
          begin
            lMessage := Format( _('Invalid base path: "%s"'),
                                [ lBasePath]);
          end
          else
          begin
            lSourceCodeLocation := ExtractLocationPathLineNumber( s);

            lCompletePath := lBasePath + lSourceCodeLocation.Path;

            if not FileExists( lCompletePath) then
            begin
              lMessage := Format( _('File not found: "%s"'),
                                  [ lCompletePath]);
            end
            else
            begin
              if (lSourceCodeLocation.Path <> '') then
              begin
                lParameterStr := AnsiQuotedStr( lCompletePath, '"');

                if GetSettingApplicationExternalEditorUseLineNumbers and
                   ( lSourceCodeLocation.LineNumber <> 0) then
                begin
                  lParameterStr := lParameterStr + ' ' +
                                   Format( '-n%d',
                                           [ lSourceCodeLocation.LineNumber]);
                end;

                if not ShellExecEx(lEditorFilename, lParameterStr,
                                '', SW_SHOWNORMAL) then
                  RaiseLastOSError;
                lShowPathAsShowMessage := false
              end;
            end;
          end;
        end;
      end;
    end;

    if lShowPathAsShowMessage then
    begin
      if (lMessage <> '') then
      begin
        lMessage := lMessage + sLineBreak;
      end;
      lMessage := lMessage + s;

      ShowMessage( lMessage);

      Clipboard.AsText := s;
    end;
  end;
end;

procedure TFormEditor.AutoTranslate;
var
  Idx: integer;
begin
  Idx := listAutoTranslation.ItemIndex;
  if (Idx < 0) and (listAutoTranslation.Count = 1) then
    Idx := 0;
  if Idx >= 0 then begin
    MemoMsgStr.Lines.Text := trim(listAutoTranslation.Items[Idx]);
    if MemoMsgStr.CanFocus then begin
      MemoMsgStr.SetFocus;
      MemoMsgStr.SelectAll;
    end;
  end;
end;

procedure TFormEditor.listAutoTranslationDblClick(Sender: TObject);
begin
  AutoTranslate;
end;

procedure TFormEditor.OpenItem;
var
  Trans: string;
  SuggCount : Integer;
  suggList : TStringList;

begin
  CurrentItemRow := Grid.Row - 1;
  CurrentItem := Rows.Objects[CurrentItemRow] as TPoEntry;
  MemoMsgId.Lines.Text := FormatMsgIdForDisplay(CurrentItem.MsgId);
  MemoMsgStr.Lines.Text := CurrentItem.MsgStr;
  listAutoTranslation.Items.Clear;
  if Assigned(Translator) then begin
    case Translator.Translate(CurrentItem.MsgId, Trans) of
      trOK: begin
          listAutoTranslation.Items.Add(Trans);
        end;
      trRetryLater: begin
          listAutoTranslation.Items.Add(_('... fetching translation ...'));
          TimerTranslate.Enabled := true;
        end;
    end;
  end;

  ExtractProgrammerComments(CurrentItem, ListBoxProgrammerComments.Items);
  ExtractSourcecodeLocations(CurrentItem, ListBoxSource.Items);
  ExtractUserComments(CurrentItem, MemoEditableComments.Lines);
  ExtractLabels(CurrentItem, CheckListBoxLabels);

  SpeedButtonQuickFuzzy.Enabled := (CurrentItem.MsgStr <> '');

  PanelDetails.Enabled := True;
  MemoMsgStr.Color := clWindow;

  if CurrentItem.IsPluralForm then begin
    MemoMsgId.Lines.Text := _('This is a plural form');
    MemoMsgStr.Color := clBtnFace;
    CurrentItem := nil;
    PanelDetails.Enabled := False;
  end;

  UpdateWarnings;

  // by VGL: Searches for suitable translation suggestions in memory
  if GetSettingTranslationMemory AND (NOT Assigned(Translator))
    then begin
      listAutoTranslation.Items.Clear;
      if Assigned(CurrentItem) then begin
        suggList := trMem.FindSuggestions(CurrentItem.MsgId);
        listAutoTranslation.Visible := false;
        lblParallelLanguageTranslation.Visible := false;
        try
          if suggList.Count > GetSettingMemorySuggestionsNumber
            then SuggCount := GetSettingMemorySuggestionsNumber
            else SuggCount := suggList.Count;
          if SuggCount > 0
            then begin
              listAutoTranslation.Items.Assign(suggList);
              listAutoTranslation.Visible := true;
              lblParallelLanguageTranslation.Visible := true;
              lblParallelLanguageTranslation.Caption := Format(_('%d suggestions found in memory:'), [SuggCount]);
            end;
        finally
          If Assigned(suggList)
            then FreeAndNil(suggList);
        end;
      end;
    end;

end;

procedure TFormEditor.ScanItemsForLabels;
var
  i: integer;
  item: TPoEntry;
  s: string;
begin
  item := Items.FindFirst;
  while item <> nil do begin
    for i := item.UserCommentList.Count - 1 downto 0 do begin
      s := item.UserCommentList.Strings[i];
      if LeftStr(s, 9) = '# Label: ' then begin
        delete(s, 1, 9);
        Labels.Add(s);
      end;
    end;
    item := Items.FindNext(item);
  end;
end;

procedure TFormEditor.act_LabelsToggleIgnoreExecute(Sender: TObject);
var
  i: integer;
begin
  // If the list doesn't contain 'ignore', add it
  i := CheckListBoxLabels.Items.IndexOf(strIgnore);
  if i = -1 then begin
    i := CheckListBoxLabels.Items.Add(strIgnore);
    CheckListBoxLabels.Checked[i] := True;
  end else
    CheckListBoxLabels.Checked[i] := not CheckListBoxLabels.Checked[i];

  //update status
  //UpdateGridRow( Grid.Row, Rows.Objects[Grid.Row-1] as TPoEntry);
  CloseGuiItem;
  OpenItem;
end;

procedure TFormEditor.act_LabelsToggleFuzzyExecute(Sender: TObject);
begin
  if CurrentItem <> nil then begin
    CurrentItem.Fuzzy := not CurrentItem.Fuzzy;
    UpdateGridRow(CurrentItemRow + 1, CurrentItem);
  end;
end;

procedure TFormEditor.TimerTranslateTimer(Sender: TObject);
var
  s: string;
begin
  if Assigned(Translator) and Translator.GetLastTranslation(s) then begin
    listAutoTranslation.Items[0] := s;
    TimerTranslate.Enabled := False;
  end;
end;

procedure TFormEditor.UpdateWarnings;
var
  s: string;
begin
  s := '';
  if CurrentItem <> nil then
    s := trim(FindWarnings(CurrentItem));
  if s <> '' then begin
    MemoMessages.Lines.Text := s;
    MemoMessages.Visible := True;
    LabelMessages.Visible := True;
    PanelMessages.Visible := True;
  end else begin
    MemoMessages.Visible := False;
    LabelMessages.Visible := False;
    MemoMessages.Clear;
    PanelMessages.Visible := False;
  end;
end;

procedure TFormEditor.act_TransAutomaticTranslationExecute(Sender: TObject);
var
  Include: TAutoTranslateInclude;

  function IsEntryIncluded(_po: TPoEntry): boolean;
  begin
    if _po.IsPluralForm then begin
      Result := false;
      exit;
    end;
    if _po.MsgStr = '' then begin
      Result := true;
      exit;
    end;
    if _po.Fuzzy and (Include = atiFuzzy) then begin
      Result := true;
      exit;
    end;
    Result := (Include = atiTranslated);
  end;

var
  i: integer;
  item: TPoEntry;
  ReqCount: integer;
  OK: Boolean;
  lstIn: TStringList;
  lstOut: TStringList;
  j: integer;
  MarkFuzzy: Boolean;
  UnFuzzy: Boolean;
  LabelAs: string;
  s: string;
  LabelOnly: Boolean;
begin
  if not Assigned(Translator) then begin
    ShowMessage(_('In order to use this feature, you must first enable auto translate.'));
    exit;
  end;

  if not Translator.AllowsBatchTranslation then begin
    ShowMessage(Format(_('The currently selected translator engine'#13#10
      + '%s'#13#10
      + 'does not allow batch translation.'),
      [Translator.GetDescription]));
    exit;
  end;

  if Rows.Count = 0 then begin
    ShowMessage(_('The current selection is empty.'));
    exit;
  end;

  Include := atiFuzzy;
  MarkFuzzy := false;
  UnFuzzy := false;
  LabelAs := 'auto';
  LabelOnly := false;
  if not TFormAutoTranslateOptions.Execute(self, Translator.GetDescription, Include, MarkFuzzy,
      UnFuzzy, LabelAs, LabelOnly) then
    exit;

  CloseGuiItem;

  if LabelAs <> '' then begin
    Labels.Add(LabelAs);
    ApplyLabelList;
  end;

  ReqCount := 0;

  lstIn := nil;
  lstOut := nil;

  // Do the actual translation
  Screen.Cursor := crHourglass;
  try
    lstIn := TStringList.Create;
    lstIn.CaseSensitive := True;
    for i := 0 to Rows.Count - 1 do begin
      item := Rows.Objects[i] as TPoEntry;
      if IsEntryIncluded(Item) then
        lstIn.Add(item.MsgId);
    end;

    //lstIn.SaveToFile(ExtractFilePath(Application.Exename)+'all_strings.txt');

    // Lists will be 1-to-1
    lstOut := TStringList.Create;
    lstOut.CaseSensitive := True;
    OK := (Translator.BatchTranslate(lstIn, lstOut) in [trOK, trRetryLater]);

    if OK then begin
      for i := 0 to Rows.Count - 1 do begin
        item := Rows.Objects[i] as TPoEntry;
        if IsEntryIncluded(Item) then begin
          j := lstIn.IndexOf(item.MsgId);
          if (j >= 0) and (j < lstOut.Count) then begin
            s := lstOut[j];
            if (s <> '') and (s <> item.Msgstr) then begin
              if not LabelOnly then begin
                item.MsgStr := s;
                if MarkFuzzy then
                  item.Fuzzy := True;
                if UnFuzzy then
                  item.Fuzzy := False;
              end;
              if LabelAs <> '' then
                AddLabel(item, LabelAs);
              Inc(ReqCount);
            end;
          end
        end;
      end;
    end else
      ShowMessage(_('Translation request failed.'));
  finally
    Screen.Cursor := crDefault;
    if lstIn <> nil then
      lstIn.Free;
    if lstOut <> nil then
      lstOut.Free;
  end;

  if OK then
    ShowMessage(_('Translation completed.') + #13 + Format(_('Strings translated: %d'), [ReqCount]));

  ExecuteFilter;
end;

procedure TFormEditor.mi_GormDiffHelpClick(Sender: TObject);
begin
ShowMessage(_('To configure diff viewer for SVN you should have'+slinebreak+
  'Tortoise SVN client 1.7+'+slinebreak+
  'Open context menu on your repository, go to Settings,'+slinebreak+
  'External Programs, Diff Viewer, click Advanced... button,'+slinebreak+
  'press Add... button. Fill ".po" to expension field and'+slinebreak+
  '..your path to gorm.exe file.. -diff %base %mine'+slinebreak+
  'to external program field.'+slinebreak+slinebreak+
  'Important. Gorm also can be used as 3-way Diff tool for po files.'+slinebreak+
  ' You need to pass 4 parameters: -diff'+slinebreak+
  ' base-file, mine-file, theirs-file in console'));
end;

procedure TFormEditor.mi_HelpHelpClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://gorm.po.dk/help', nil, nil, sw_normal);
end;

function GetStatusStr(item: TPoEntry): string;
// See comment from Lars (above)
begin
  (*
  if item.UserCommentList.IndexOf('# Label: '+strIgnore) >= 0 then
    Result := _('Ignored')
  else*)if item.Fuzzy then
    Result := _('Fuzzy')
  else if item.MsgStr = '' then
    Result := _('Untranslated')
  else
    Result := '';
end;

procedure TFormEditor.UpdateGridRow(Row: Integer; item: TPoEntry);
begin
  Grid.Cells[0, Row] := GetStatusStr(item);
  Grid.Cells[1, Row] := item.MsgId;
  Grid.Cells[2, Row] := item.MsgStr;
end;

procedure TFormEditor.act_LabelsApplyWarningExecute(Sender: TObject);
var
  item: TPoEntry;
  i: integer;
begin
  screen.Cursor := crHourGlass;
  try
    CloseGuiItem;
    for i := 0 to Rows.Count - 1 do begin
      item := Rows.Objects[i] as TPoEntry;
      if FindWarnings(item) <> '' then
        AddLabel(item, 'warning');
    end;
    Labels.Add('warning');
    ApplyLabelList;
  finally
    screen.Cursor := crDefault;
  end;
  ShowMessage(format(_('Filter by the label "%s" in order to see the texts that may have problems.'), ['warning']));
end;

procedure TFormEditor.act_TransClearExecute(Sender: TObject);
var
  i: integer;
  item: TPoEntry;
  cnt: integer;
begin
  cnt := Rows.Count;
  if mrYes <> MessageDlg(Format(_('This will remove the translations from the current selection (%d entries).' + sLineBreak
    + 'Are you really sure that you want to do that?'), [cnt]), mtConfirmation, [mbYes, mbCancel], 0, mbCancel) then
    exit;

  Screen.Cursor := crHourGlass;
  try
    CloseGuiItem;
    cnt := 0;
    for i := 0 to Rows.Count - 1 do begin
      item := Rows.Objects[i] as TPoEntry;
      if item.MsgStr <> '' then begin
        item.MsgStr := '';
        Inc(cnt);
      end;
    end;
    ExecuteFilter;
  finally
    Screen.Cursor := crDefault;
  end;

  MessageDlg(Format(_('%d translations have been cleared.'), [cnt]), mtInformation, [mbOK], 0);
end;

procedure TFormEditor.act_TransAutofillCopyExecute(Sender: TObject);
var
  cnt: integer;
  i: integer;
  item: TPoEntry;
begin
  cnt := Rows.Count;
  if mrYes <> MessageDlg(Format(_('This will overwrite all translations from the current selection (%d entries) with the original.' + sLineBreak
    + 'Are you really sure that you want to do that?'), [cnt]), mtConfirmation, [mbYes, mbCancel], 0, mbCancel) then
    exit;

  Screen.Cursor := crHourGlass;
  try
    CloseGuiItem;
    for i := 0 to Rows.Count - 1 do begin
      item := Rows.Objects[i] as TPoEntry;
      if (item.MsgId <> '') and not item.IsPluralForm then
        item.MsgStr := item.MsgId;
    end;
    ExecuteFilter;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormEditor.act_TransCreateHtmlExecute(Sender: TObject);
begin
  if CurrentFilename = '' then begin
    ShowMessage(_('No file is currently open.'));
    Exit;
  end;

  CloseGuiItem;
  CreateHTML;
end;

procedure TFormEditor.act_TransCreateMsgstrExecute(Sender: TObject);
var
  i: integer;
  idlabel:string;
  frm: TFormLabel;
begin
  if messagedlg('This will remove ALL existing translations from this file. Do you want to continue?',mtconfirmation,[mbyes,mbno],0)=mryes then
  begin
    frm:=TFormLabel.Create(self);
    try
      frm.editlabelname.editlabel.Caption:='Please specify an id that will'#13#10+'be included in the translations.';
      frm.EditLabelName.Text := emptystr;
      if (frm.ShowModal=mrOK) then
        idlabel := frm.EditLabelName.Text
      else
        exit;
    finally
      FreeAndNil(frm);
    end;
    for i := 0 to Rows.Count - 1 do begin
      (Rows.Objects[i] as TPoEntry).MsgStr:=inttostr(i)+idlabel+':'+(Rows.Objects[i] as TPoEntry).MsgId;
    end;
    ExecuteFilter;
  end;
end;

procedure TFormEditor.act_LabelsAddStandardLabelsExecute(Sender: TObject);
begin
  AddStandardLabels;
end;

procedure TFormEditor.ApplyLabelList(_LabelToSelect: string = strIgnore);
begin
  if Labels.IndexOf(strIgnore) = -1 then
    Labels.Add(strIgnore);
  if Labels.IndexOf(_LabelToSelect) = -1 then
    Labels.Add(_LabelToSelect);

  CheckListBoxLabels.Items.Text := Labels.Text;
  ComboBoxLabel.Items.Clear;
  ComboBoxLabel.Items.Add('');
  ComboBoxLabel.Items.Add(strFuzzy);
  ComboBoxLabel.Items.AddStrings(Labels);

  ComboBoxLabel.ItemIndex := ComboBoxLabel.Items.IndexOf(_LabelToSelect);
end;

{*------------------------------------------------------------------------------
  Provides a feature of inserting special characters.
  List of chars are taken from .ini file and can be changed manually on the Preferences window.
  It possible to edit this list with user-preferred set of chars and they order

  When user press the button to insert a new spec. char - the new form is created.
  This form contains a StringGrid in wich every cell represents one special char.
-------------------------------------------------------------------------------}

procedure TFormEditor.SpecCharsKeyDown(Sender: TObject; var Key: Char);
  var gr : TStringGrid;
begin
  gr := Sender as TstringGrid; // just to readability
  case Key of
    #27 : (gr.Parent as TForm).Close; // EscPressed - user reconsidereds to insert a char
    #13 : begin // Enter pressed - inserts a selected char to the current cursor position in translation memo
            FormEditor.MemoMsgStr.SetSelText(gr.Cells[gr.Selection.Left, gr.Selection.Top]);
            (gr.Parent as TForm).Close;
          end;
  end;
end;

procedure TFormEditor.SpecCharsClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var gr : TStringGrid;
begin
  gr := Sender as TstringGrid;
  // inserts a selected char to the current cursor position in translation memo
  FormEditor.MemoMsgStr.SetSelText(gr.Cells[gr.Selection.Left, gr.Selection.Top]);
  (gr.Parent as TForm).Close;
end;

procedure TFormEditor.RemoveFuzzyFromEmtpyTranslations;
var
  Item: TPoEntry;
begin
  Item := Items.FindFirst;
  while Assigned(Item) do
  begin
    if (Item.MsgStr = '') and Item.Fuzzy then
      Item.Fuzzy := False;
    Item := Items.FindNext(Item);
  end;
end;

procedure TFormEditor.btSpecCharsClick(Sender: TObject);
var aChars : TStringGrid;
    fChars : TForm;
    maxstring, i, j : integer;
    btSpecCharsPos : TPoint;
begin
  FSpecChars := TStringList.Create;
  fChars := TForm.Create(nil);
  aChars  := TStringGrid.Create(fChars);
  try
    // current list of special chars
    ReadList('SpecialChars', FSpecChars);

    // every line in the special chars list will be a row in StringGrid
    aChars.RowCount := FSpecChars.Count;

    // define the lenfth of the largest string
    maxstring := 0;
    for i:= 0 to FSpecChars.Count - 1 do
      if length(FSpecChars.Strings[i]) > maxstring
        then maxstring := length(FSpecChars.Strings[i]);
    aChars.ColCount := maxstring;

    // fill the StringGrid by placing every special char to single cell
    for i:= 0 to FSpecChars.Count - 1 do
      for j:= 0 to length(FSpecChars.Strings[i])-1 do aChars.Cells[j,i] := FSpecChars.Strings[i][j+1];

    // set the visual style of form and StringGrid
    aChars.BorderStyle := bsNone;
    aChars.FixedCols := 0;
    aChars.FixedRows := 0;
    aChars.Font.Size := GetSettingSpecCharsFontSize;
    aChars.Options := [];
    aChars.ScrollBars := ssNone;
    aChars.DefaultColWidth := trunc(aChars.Font.Size / 0.5);
    aChars.DefaultRowHeight := trunc(aChars.Font.Size * 2);
    aChars.Font.Name := 'Arial Unicode MS';
    aChars.Font.Charset := DEFAULT_CHARSET;

    // the form will be shows right above the button
    fChars.Height := aChars.DefaultRowHeight * aChars.RowCount;
    fChars.Width := aChars.DefaultColWidth * aChars.ColCount;
    fChars.BorderStyle := bsNone;
    fChars.Position := poDesigned;
    btSpecCharsPos := btSpecChars.ClientToScreen(Point(btSpecChars.Left, btSpecChars.Top));
    fChars.Top := btSpecCharsPos.y - fChars.Height;
    fChars.Left := btSpecCharsPos.x - btSpecChars.Left;

    // placing a StringList to the form
    aChars.Parent := fChars;
    aChars.Align := alClient;

    // definitions of events
    aChars.OnKeyPress := SpecCharsKeyDown;
    aChars.OnMouseUp := SpecCharsClick;
    try
      // this is it - form is modal so user must select the char - or close this form with Esc button
      fChars.ShowModal;
    finally
      FreeAndNil(FSpecChars);
    end;
  finally
    FreeAndNil(fChars);
  end;
end;

procedure TFormEditor.act_AutoDisableExecute(Sender: TObject);
begin
  CloseGuiItem;
  act_AutoDisable.Checked := true;
  FreeAndNil(Translator);
  lblParallelLanguageTranslation.Visible := False;
  listAutoTranslation.Visible := False;
  ButtonParallel.Visible := False;
end;

procedure TFormEditor.btnRefreshFilterClick(Sender: TObject);
begin
  CloseGuiItem;
  ExecuteFilter;
end;

procedure TFormEditor.ButtonCopyMsgId2MsgStrClick(Sender: TObject);
begin
  if CurrentItem <> nil then begin
    MemoMsgStr.Lines.Text := CurrentItem.MsgId;
  end;
end;

procedure TFormEditor.ButtonMoveDownClick(Sender: TObject);
begin
  MoveToNextRow;
  if MemoMsgStr.CanFocus then begin
    MemoMsgStr.SetFocus;
    MemoMsgStr.SelectAll;
  end;
end;

procedure TFormEditor.ButtonMoveUpClick(Sender: TObject);
begin
  MoveToPreviousRow;
  if MemoMsgStr.CanFocus then begin
    MemoMsgStr.SetFocus;
    MemoMsgStr.SelectAll;
  end;
end;

procedure TFormEditor.ButtonParallelClick(Sender: TObject);
begin
  AutoTranslate;
end;

procedure TFormEditor.ButtonShowAllClick(Sender: TObject);
begin
  CloseGuiItem;
  EditFilterText.Clear;
  ComboBoxLabel.ItemIndex := 0;
  ExecuteFilter;
  GridSelectFirst;
  Grid.SetFocus;
end;

procedure TFormEditor.b_FilterSelectClick(Sender: TObject);
var
  sw: TSearchWhereEnum;
  s: string;
begin
  if Tf_Textfilter.Execute(Self, FTextSearchWhere, FTextSearchInv) then begin
    s := '';
    for sw := Low(TSearchWhereEnum) to High(TSearchWhereEnum) do begin
      if sw in FTextSearchWhere then
        s := s + SEARCH_WHERE[sw] + ', ';
    end;
    s := Copy(s, 1, Length(s) - 2);
    if FTextSearchInv then
      EditFilterText.EditLabel.Caption := _('Inv. Filter:') + ' ' + s
    else
      EditFilterText.EditLabel.Caption := _('Filter:') + ' ' + s;

    CloseGuiItem;
    ExecuteFilter;
    GridSelectFirst;
    Grid.SetFocus
  end;
end;

procedure TFormEditor.CheckListBoxLabelsClick(Sender: TObject);

var
  i: Integer;

begin
  i := CheckListBoxLabels.ItemIndex;
  if i >= 0 then begin
    CheckListBoxLabels.Checked[i] := not CheckListBoxLabels.Checked[i];
    CloseGuiItem; // Update the item's 'list of labels'
    OpenItem;
  end;
end;

procedure TFormEditor.CheckListBoxLabelsClickCheck(Sender: TObject);
begin
  CloseGuiItem; // Update the item's 'list of labels'
  OpenItem;
end;

procedure TFormEditor.chkInverseFilterClick(Sender: TObject);
begin
  CloseGuiItem;
  ExecuteFilter;
end;

procedure TFormEditor.CloseGuiItem;
var
  i: integer;
begin
  if (CurrentItem <> nil) and (not CurrentItem.IsPluralForm) then begin
    CurrentItem.MsgStr := ConvertMsgStrToStorageFormat(MemoMsgStr.Lines.Text);
    CurrentItem.UserCommentList.Clear;
    for i := 0 to CheckListBoxLabels.Count - 1 do begin
      if CheckListBoxLabels.Checked[i] then begin
        AddLabel(CurrentItem, CheckListBoxLabels.Items.Strings[i]);
        CheckListBoxLabels.Checked[i] := False;
      end;
    end;
    for i := 0 to MemoEditableComments.Lines.Count - 1 do
      CurrentItem.UserCommentList.Add('# ' + MemoEditableComments.Lines.Strings[i]);
    //CurrentItem.Fuzzy:=CheckBoxFuzzy.Checked;
    UpdateGridRow(CurrentItemRow + 1, CurrentItem);
    CurrentItem := nil;

    ListBoxSource.Clear;
    ListBoxProgrammerComments.Clear;
    MemoMsgId.Clear;
    listAutoTranslation.Items.Clear;

    MemoMsgStr.Clear;
    MemoEditableComments.Clear;
    UpdateWarnings;
  end;
  PanelDetails.Enabled := False;
end;

procedure TFormEditor.ComboBoxLabelChange(Sender: TObject);
begin
  CloseGuiItem;
  ExecuteFilter;
  GridSelectFirst;
end;

procedure TFormEditor.MemoMsgStrChange(Sender: TObject);
var
  s: string;
begin
  if CurrentItem <> nil then begin
    s := ConvertMsgStrToStorageFormat(MemoMsgStr.Lines.Text);
    if CurrentItem.MsgStr <> s then begin
      CurrentItem.MsgStr := s;
      CurrentItem.Fuzzy := False;
    end;
    UpdateGridRow(CurrentItemRow + 1, CurrentItem);
    SpeedButtonQuickFuzzy.Enabled := (CurrentItem.MsgStr <> '');
  end;
  UpdateWarnings;
end;

procedure TFormEditor.MemoMsgStrKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if chkQuickNavigation.Checked and
    (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT]) then
    Grid.Perform(WM_KEYDOWN, Key, 0);
end;

procedure TFormEditor.MenuItemAboutClick(Sender: TObject);

var
  vi: TVersionInfo;

begin
  vi := GetVersionInfo('');

  ShowMessage(_('Gorm editor for po files.') + #13 +
    ' '#13 +
    _('Version') + Format(': %.2d.%.2d.%.2d (build %d)', [vi.MajorVersion, vi.MinorVersion, vi.Release, vi.Build]));
end;

procedure TFormEditor.MenuItemAddLabelClick(Sender: TObject);
var
  f: TFormLabel;
begin
  CloseGuiItem;
  f := TFormLabel.Create(nil);
  try
    if f.ShowModal = mrOK then begin
      Labels.Add(f.EditLabelName.Text);
      ApplyLabelList;
    end;
  finally
    FreeAndNil(f);
  end;
end;

procedure TFormEditor.act_FileAppendToExecute(Sender: TObject);
var
  outfile: TFileStream;
  item: TPoEntry;
  i: integer;
  filename: string;
  ansistr: ansistring;
begin
  if not SelectPoFileToSave(filename) then
    exit;

  screen.Cursor := crHourGlass;
  try
    CloseGuiItem;
    outfile := TFileStream.Create(filename, fmOpenReadWrite);
    try
      outfile.Seek(0, soFromEnd);

      // Append a few linebreaks
      ansistr := sLineBreak + sLineBreak + sLineBreak;
      outfile.Write(ansistr[1], length(ansistr));

      // Write the rest
      for i := 0 to Rows.Count - 1 do begin
        item := Rows.Objects[i] as TPoEntry;
        item.WriteToStream(outfile, GetSettingSaveWrapAfter);
      end;
    finally
      FreeAndNil(outfile);
    end;
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TFormEditor.act_LabelsApplyByRuleExecute(Sender: TObject);
type
  TRule =
    record
    labelname: string;
    ruletype: integer; // 0=invalid, 1=filename, 2=msgid
    pattern: string;
  end;
var
  f: TFormLabelApplicationByRules;
  filename: string;
  rules: TStringList;
  rulesarr: array of TRule;
  rule: string;
  iApplied,
    i, j, k: integer;
  s: string;
  item: TPoEntry;
begin
  CloseGuiItem;

  // Get filename
  filename := '';
  f := TFormLabelApplicationByRules.Create(self);
  try
    if f.ShowModal = mrOK then begin
      filename := f.EditFilename.Text;
    end;
  finally
    FreeAndNil(f);
  end;
  if filename = '' then
    exit;

  screen.Cursor := crHourglass;
  try
    // Read rules from file and store in array
    rules := TStringList.Create;
    try
{$IFDEF UNICODE} // >=D2009
      rules.LoadFromFile(filename, TEncoding.UTF8);
{$ELSE}
      rules.LoadFromFile(filename);
{$ENDIF}
      SetLength(rulesarr, rules.Count);
      for i := 0 to Rules.Count - 1 do begin
        Rulesarr[i].ruletype := 0;
        Rule := Rules.Strings[i];
        j := pos(' ', rule);
        if j <> 0 then begin
          Rulesarr[i].labelname := LeftStr(Rule, j - 1);
          Delete(Rule, 1, j);
          Rule := Trim(Rule);
          j := pos(' ', rule);
          if j <> 0 then begin
            if LeftStr(Rule, j - 1) = 'file' then
              Rulesarr[i].ruletype := 1
            else if LeftStr(Rule, j - 1) = 'msgid' then
              Rulesarr[i].ruletype := 2;
            if Rulesarr[i].ruletype <> 0 then begin
              delete(Rule, 1, j);
              Rulesarr[i].pattern := trim(Rule);
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(rules);
    end;

    iApplied := 0;
    // Apply rules
    for i := 0 to Rows.Count - 1 do begin
      item := Rows.Objects[i] as TPoEntry;
      for j := low(rulesarr) to high(rulesarr) do begin
        case rulesarr[j].ruletype of
          1: begin
              // Apply label if filename matches
              for k := 0 to item.AutoCommentList.Count - 1 do begin
                s := item.AutoCommentList.Strings[k];
                if (LeftStr(s, 2) = '#:') and patternmatchspacedlist(rulesarr[j].pattern, copy(s, 3, maxint)) then begin
                  AddLabel(item, rulesarr[j].labelname);
                  inc(iApplied);
                  break;
                end;
              end;
            end;
          2: begin
              // Apply label if msgid matches
              if patternmatch(rulesarr[j].pattern, item.MsgId) then begin
                AddLabel(item, rulesarr[j].labelname);
                inc(iApplied);
              end;
            end;
        end;
      end;
    end;

    // Update GUI etc.
    ScanItemsForLabels;
    ApplyLabelList;
  finally
    screen.Cursor := crDefault;
  end;
  ShowMessage(Format(_('%d labels have been applied.'), [iApplied]));

  btnRefreshFilter.Click;
end;

procedure TFormEditor.act_FileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormEditor.act_FileOpenExecute(Sender: TObject);
var
  fn: string;
begin
  CloseGuiItem;
  if not SelectPoFileToOpen(fn) then
    exit;

  LoadFileInteractive(fn);
end;

procedure TFormEditor.LoadFileInteractive(const _Filename: string);
var
  i: Integer;
begin
  Screen.Cursor := crHourglass;
  try
    i := FOpenMRU.IndexOf(_Filename);
    if i < 0 then begin
      FOpenMRU.Add(_Filename);
      SaveList('OpenMRU', FOpenMRU);
      AddOpenMRU(FOpenMRU.Count - 1);
      MenuItemReopen.Visible := (FOpenMRU.Count > 0);
    end;

    LoadFileByName(_Filename);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormEditor.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
begin
  if _Files.Count = 0 then
    exit;
  LoadFileInteractive(_Files[0]);
end;

procedure TFormEditor.act_FileReadFromExecute(Sender: TObject);
var
  xitems: TPoEntryList;
  fn: string;
  xItem: TPoEntry;
begin
  CloseGuiItem;
  if not SelectPoFileToOpen(fn) then
    exit;

  Grid.RowCount := 0;
  xitems := TPoEntryList.Create;
  try
    xitems.LoadFromFile(fn);
    xItem := xitems.FindFirst;
    while Assigned(xItem) do begin
      if Items.Find(xitem.MsgId) = nil then begin
        Items.Add(xitem);
      end;
      xItem := xItems.FindNext(xItem);
    end;
    LoadPostProcess;
  finally
    FreeAndNil(xitems);
  end;
end;

procedure TFormEditor.act_FileReloadExecute(Sender: TObject);
var
  Msg: string;
begin
  if CurrentFilename = '' then
    exit;

  Msg := Format(_('Are you sure you want to reload file'#13#10
    + '%s'
    + #13#10'from disk?'#13#10
    + 'All changes will be lost!'), [CurrentFilename]);
  if mrYes <> MessageDlg(Msg, mtConfirmation, [mbYes, mbCancel], 0) then
    exit;

  LoadFileByName(CurrentFilename);
end;

procedure TFormEditor.MenuItemOpenMRUClick(Sender: TObject);

var
  i: Integer;
  fn: string;

begin
  CloseGuiItem;

  i := (Sender as TMenuItem).Tag;
  fn := FOpenMRU.Strings[i];

  if FileExists(fn) then begin
    screen.Cursor := crHourglass;
    try
      LoadFileByName(fn);
    finally
      screen.Cursor := crDefault;
    end;
  end else
    ShowMessage(format(_('Cannot find the file "%s" that contains translations. Are you sure that you have arranged the files correctly on your harddisk?'), [fn]));
end;

procedure TFormEditor.act_FileSaveExecute(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    CloseGuiItem;
    FileSave;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormEditor.act_FileSaveAsExecute(Sender: TObject);
var
  fn: string;
begin
  CloseGuiItem;
  if not SelectPoFileToSave(fn) then
    exit;
  Screen.Cursor := crHourGlass;
  try
    items.SaveToFile(fn, GetSettingSaveWrapAfter);
    CurrentFilename := fn;
    Self.Caption := 'Gorm - [' + CurrentFilename + ']';
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormEditor.FindWarning(iFirst: Integer);
var
  item: TPoEntry;
  i: Integer;
  OK: Boolean;
  s: string;
begin
  OK := False;
  i := 0;

  if iFirst < Rows.Count then begin
    i := iFirst;
    OK := False;
    while (i < Rows.Count) and (not OK) do begin
      item := Rows.Objects[i] as TPoEntry;
      s := trim(FindWarnings(item));
      OK := (s <> '');
      if not OK then
        Inc(i);
    end;
  end;

  if OK then
    Grid.Row := i + 1
  else
    ShowMessage(_('Warning not found.'));
end;

procedure TFormEditor.act_ToolsFindFirstWarningExecute(Sender: TObject);
begin
  if CurrentFilename <> '' then begin
    CloseGuiItem;
    FindWarning(0);
  end;
end;

procedure TFormEditor.act_ToolsFindNextWarningExecute(Sender: TObject);
begin
  if CurrentFilename <> '' then begin
    CloseGuiItem;
    FindWarning(Grid.Row);
  end;
end;

procedure TFormEditor.act_ToolsPreferencesExecute(Sender: TObject);
var
  f: TFormPreferences;
begin
  CloseGuiItem;
  f := TFormPreferences.Create(self);
  try
    f.ShowModal;
  finally
    FreeAndNil(f);
    ActivateTranslationsMemory(Sender);
  end;
  InitStatusBar;
end;

procedure TFormEditor.act_LabelsFuzzySelectionExecute(Sender: TObject);
var
  i: Integer;
  item: TPoEntry;
begin
  if MessageDlg(_('Do you really want to mark all selected items ''fuzzy''?'), mtConfirmation, [mbYes, mbCancel], 0) <> mrYes then
    exit;

  CloseGuiItem;
  i := 0;
  while (i < Rows.Count) do begin
    item := Rows.Objects[i] as TPoEntry;
    item.Fuzzy := true;
    UpdateGridRow(i + 1, Item);
    Inc(i);
  end;
end;

procedure TFormEditor.act_LabelsUnfuzzySelectionExecute(Sender: TObject);
var
  i: Integer;
  item: TPoEntry;
begin
  if MessageDlg(_('Do you really want to remove the ''fuzzy'' mark from all selected items?'), mtConfirmation, [mbYes, mbCancel], 0) <> mrYes then
    exit;

  CloseGuiItem;
  i := 0;
  while (i < Rows.Count) do begin
    item := Rows.Objects[i] as TPoEntry;
    item.Fuzzy := False;
    UpdateGridRow(i + 1, Item);
    Inc(i);
  end;
end;

procedure TFormEditor.SetTranslator(_Translator: TTranslatorEngine);
var
  Lng: string;
begin
  // Use translator engine to show an alternative translation
  // every time an item is selected.
  Translator := _Translator;
  lblParallelLanguageTranslation.Visible := True;
  listAutoTranslation.Visible := True;
  ButtonParallel.Visible := True;
  if Translator.TargetLanguage.IsValid then
    Lng := Translator.TargetLanguage.EnglishName
  else
    Lng := _('unknown language');
  lblParallelLanguageTranslation.Caption := Format(_('%s from %s'), [Lng , Translator.GetDescription]);
  act_TransAutomaticTranslation.Enabled := true;
end;

procedure TFormEditor.EnableGoogleTranslate(const GtCode: string);
begin
  SetTranslator(TTranslatorEngineGoogle.Create('en', GtCode));
  act_AutoGoogle.Checked := true;
  act_AutoGoogle.Caption := Format(_('Use &Google Translate (%s) ...'), [GtCode]);
end;

procedure TFormEditor.EnableMicrosoftTranslate(const _AppId, _LngCode: string);
begin
  SetTranslator(TTranslatorEngineMicrosoft.Create(_AppId, 'en', _LngCode));
  act_AutoMicrosoft.Checked := true;
  act_AutoMicrosoft.Caption := Format(_('Use Microsoft Translator &V2 (%s) ...'), [_LngCode]);
end;

procedure TFormEditor.act_AutoMoExecute(Sender: TObject);
var
  fn: string;
begin
  CloseGuiItem;
  if not SelectFileToOpen(_('MO files'), '*.mo', fn) then
    exit;
  SetTranslator(TTranslatorEngineGetText.Create(fn, 'parallel'));
  act_AutoMo.Checked := true;
end;

procedure TFormEditor.act_AutoPoExecute(Sender: TObject);
var
  fn: string;
begin
  CloseGuiItem;
  if not SelectPoFileToOpen(fn) then
    exit;
  SetTranslator(TTranslatorEnginePoFile.Create(fn));
  act_AutoPo.Checked := true;
end;

procedure TFormEditor.act_AutoRepositoryExecute(Sender: TObject);
var
  Lng: string;
  Code: string;
begin
  lng := Items.Language;
  while lng = '' do begin
    if idYes <>  MessageDlg(_('The language of this po file is unknown.'#13#10
      + 'Do you want to set it now?'),
      mtError, [mbYes, mbCancel], 0) then
        exit;
    if not EditHeader(true) then
      exit;
    lng := Items.Language;
  end;
  if not dxlanguages.TryGetCodeForLanguage(lng, Code) then
    raise Exception.CreateFmt(_('Language "%s" is not known to Gorm.'), [lng]);
  SetTranslator(TTranslatorEngineRepository.Create(FTranslationRepository, Code));
  act_AutoRepository.Checked := true;
end;

function TFormEditor.EditHeader(_ForceLanguageInput: boolean): boolean;
var
  Item: TPoEntry;
begin
  Item := Items.Find('');
  if not Assigned(Item) then begin
    Item := TPoEntry.Create;
    try
      Item.UserCommentList.Add('# SOME DESCRIPTIVE TITLE.');
      Item.UserCommentList.Add('# Copyright (C) YEAR THE PACKAGE''S COPYRIGHT HOLDER');
      Item.UserCommentList.Add('# This file is distributed under the same license as the PACKAGE package.');
      Item.UserCommentList.Add('# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.');
      Item.Fuzzy := true;
      Item.MsgStr := 'Project-Id-Version: PACKAGE VERSION'#13#10
        + 'POT-Creation-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + #13#10
        + 'PO-Revision-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + #13#10
        + 'Last-Translator: Somebody <your.email@address.com>'#13#10
        + 'MIME-Version: 1.0'#13#10
        + 'Content-Type: text/plain; charset=UTF-8'#13#10
        + 'Content-Transfer-Encoding: 8bit'#13#10
        + 'X-Generator: ' + 'Gorm.exe';
      Items.Add(Item);
    finally
      FreeAndNil(Item);
    end;
    // .Add does not add Item itsself but a copy, so in order to edit
    // the actual header, we need to search it again.
    Item := Items.Find('');
  end;
  Result := Tf_EditHeader.Execute(Self, Item, _ForceLanguageInput);
  if Result then begin
    Item.Fuzzy := false;
    items.language;
    ActivateTranslationsMemory(Self);
  end;
end;

procedure TFormEditor.act_FileHeaderExecute(Sender: TObject);
begin
  EditHeader(false);
end;

procedure TFormEditor.act_LabelsLoadIgnoreExecute(Sender: TObject);
var
  fn: string;
  ClearFirst: boolean;
  LabelAs: string;
  AutoFilter: boolean;
  xitems: TPoEntryList;
  Item: TPoEntry;
  xItem: TPoEntry;
  AutoLabel: boolean;
  cnt: integer;
begin
  CloseGuiItem;
  fn:='ignore.po';
  ClearFirst:= False;
  LabelAs:='newignore';
  AutoFilter:= True;
  if not Tf_IgnoreLoad.Execute(Self, fn, ClearFirst, LabelAs, AutoFilter) then
    exit;

  AutoLabel := (LabelAs <> '');
  AutoFilter := AutoFilter and AutoLabel;

  xitems := TPoEntryList.Create;
  try
    xitems.LoadFromFile(fn);
    // Only after we could actually load the file, clear the existing ignore labels
    // and optionally set the LabelAs label for all those we changed

    cnt := 0;
    if ClearFirst then begin
      Item := Items.FindFirst;
      while Assigned(Item) do begin
        if HasLabel(Item, strIgnore) then begin
          RemoveLabel(Item, strIgnore);
          Inc(cnt);
          if AutoLabel then
            AddLabel(Item, LabelAs);
        end;
        Item := Items.FindNext(Item);
      end;
    end;
    xItem := xitems.FindFirst;
    while Assigned(xItem) do begin
      Item := Items.Find(xitem.MsgId);
      if Assigned(Item) then begin
        if not HasLabel(Item, strIgnore) then begin
          AddLabel(Item, strIgnore);
          Inc(cnt);
          if AutoLabel then
            AddLabel(Item, LabelAs);
        end;
      end;
      xItem := xItems.FindNext(xItem);
    end;
  finally
    FreeAndNil(xitems);
  end;

  if AutoFilter then begin
    chkInverseFilter.Checked := False;
    ApplyLabelList(LabelAs);
  end;

  ExecuteFilter;
  GridSelectFirst;
  Grid.SetFocus;

  MessageDlg(Format(_('%d entries have been changed.'#13#10
    + 'Remember to clear the translations from all newly ignored entries, otherwise they will still be translated.'),
    [cnt]), mtInformation, [mbOK], 0, mbOK);
end;

procedure TFormEditor.act_LabelsImportIgnoreExecute(Sender: TObject);
var
  ClearFirst: boolean;
  LabelAs: string;
  AutoFilter: boolean;
  xitems: TPoEntryList;
  Item: TPoEntry;
  xItem: TPoEntry;
  AutoLabel: boolean;
  cnt: integer;
  ClearTranslation: Boolean;
begin
  CloseGuiItem;
  ClearFirst := False;
  LabelAs := 'newignore';
  AutoFilter := True;
  ClearTranslation := False;
  if not Tf_IgnoreImport.Execute(Self, FIngoreImportFilename, ClearFirst, LabelAs, AutoFilter, ClearTranslation) then
    exit;

  AutoLabel := (LabelAs <> '');
  AutoFilter := AutoFilter and AutoLabel;

  xitems := TPoEntryList.Create;
  try
    xitems.LoadFromFile(FIngoreImportFilename);
    // Only after we could actually load the file, clear the existing ignore labels
    // and optionally set the LabelAs label for all those we changed
    cnt := 0;
    if ClearFirst then begin
      Item := Items.FindFirst;
      while Assigned(Item) do begin
        if HasLabel(Item, strIgnore) then begin
          RemoveLabel(Item, strIgnore);
          Inc(cnt);
          if AutoLabel then
            AddLabel(Item, LabelAs);
        end;
        Item := Items.FindNext(Item);
      end;
    end;
    xItem := xitems.FindFirst;
    while Assigned(xItem) do begin
      if HasLabel(xItem, strIgnore) then begin
        Item := Items.Find(xitem.MsgId);
        if Assigned(Item) then begin
          if not HasLabel(Item, strIgnore) then begin
            AddLabel(Item, strIgnore);
            Inc(cnt);
            if AutoLabel then
              AddLabel(Item, LabelAs);
            if ClearTranslation then
              Item.MsgStr := '';
          end;
        end;
      end;
      xItem := xItems.FindNext(xItem);
    end;
  finally
    FreeAndNil(xitems);
  end;

  if AutoFilter then begin
    chkInverseFilter.Checked := False;
    ApplyLabelList(LabelAs);
  end;

  ExecuteFilter;
  GridSelectFirst;
  Grid.SetFocus;

  MessageDlg(Format(_('%d entries have been changed.'#13#10
    + 'Remember to clear the translations from all newly ignored entries, otherwise they will still be translated.'),
    [cnt]), mtInformation, [mbOK], 0, mbOK);
end;

procedure TFormEditor.act_LabelsSaveIgnoreExecute(Sender: TObject);
var
  fn: string;
  xitems: TPoEntryList;
  Item: TPoEntry;
  xItem: TPoEntry;
  cnt: integer;
begin
  if not Tf_IgnoreSave.Execute(Self, fn) then
    exit;

  CloseGuiItem;
  xitems := TPoEntryList.Create;
  try
    Item := Items.FindFirst;
    while Assigned(Item) do begin
      if HasLabel(Item, strIgnore) then begin
        xitem := TPoEntry.Create;
        xItem.Assign(Item);
        xItem.MsgStr := '';
        xitems.Add(xItem);
      end;
      Item := Items.FindNext(Item);
    end;
    xitems.SaveToFile(fn, GetSettingSaveWrapAfter);
    cnt := xitems.Count;
  finally
    FreeAndNil(xitems);
  end;

  MessageDlg(Format(_('%d entries have been saved.'), [cnt]), mtInformation, [mbOK], 0, mbOK);
end;

procedure TFormEditor.MoveToNextRow;
begin
  if Grid.RowCount > 1 then begin
    if Grid.Row < 1 then begin
      Grid.Row := 1;
      OpenItem;
    end else if Grid.Row + 1 < Grid.RowCount then begin
      Grid.Row := Grid.Row + 1;
      OpenItem;
    end;
  end;
  if MemoMsgStr.CanFocus then begin
    MemoMsgStr.SetFocus;
    MemoMsgStr.SelectAll;
  end;
end;

procedure TFormEditor.MoveToPreviousRow;
begin
  if Grid.Row > 1 then begin
    Grid.Row := Grid.Row - 1;
    OpenItem;
  end;
  if MemoMsgStr.CanFocus then begin
    MemoMsgStr.SetFocus;
    MemoMsgStr.SelectAll;
  end;
end;

procedure TFormEditor.LoadFileByName(const _FileName: string);
begin
  CloseGuiItem;
  Grid.RowCount := 1;

  CurrentFilename := _FileName;
  Self.Caption := 'Gorm - [' + _FileName + ']';
  act_TransSendEmail.Enabled := utils.MapiInstalled;
  act_TransCreateHtml.Enabled := True;

  Items.Clear;
  Items.LoadFromFile(CurrentFilename);

  ActivateTranslationsMemory(Self);
  LoadPostProcess;
end;

procedure TFormEditor.LoadPostProcess;
var
  Item: TPoEntry;
begin
  Item := Items.Find('');
  if not Assigned(Item) then
    EditHeader(true);

  RemoveFuzzyFromEmtpyTranslations;
  ScanItemsForLabels;
  ApplyLabelList;
  ExecuteFilter;
  GridSelectFirst;
  Grid.SetFocus;
end;

procedure TFormEditor.AddStandardLabels;
begin
  // These labels must be language-independent, in order to enable
  // scripting, and in order to make sure that everybody applies the same labels
  Labels.Add(strIgnore);
  Labels.Add('starred');
  Labels.Add('reviewprogrammers');
  Labels.Add('presentation');
  Labels.Add('needsimprovement');
  ApplyLabelList;
end;

procedure TFormEditor.act_AutoGoogleExecute(Sender: TObject);
var
  Lng: string;
  LngCode: string;
begin
  CloseGuiItem;

  Lng := Items.Language;
  if Lng <> '' then
    if not dxlanguages.TryGetCodeForLanguage(Lng, LngCode) then
      LngCode := '';
  if TGoogleTranslationSettings.Execute(Self, LngCode) then begin
    EnableGoogleTranslate(LngCode);
  end;
end;

procedure TFormEditor.act_AutoMicrosoftExecute(Sender: TObject);
var
  Lng: string;
  LngCode: string;
  AppId: string;
begin
  CloseGuiItem;
  AppId := GetSettingApplicationBingAppId;

  Lng := Items.Language;
  if Lng <> '' then
    if not dxlanguages.TryGetCodeForLanguage(Lng, LngCode) then
      LngCode := '';

  if Tf_MicrosoftTranslationSettings.Execute(Self, Appid, LngCode) then begin
    EnableMicrosoftTranslate(AppId, LngCode);
  end;
end;

procedure TFormEditor.CompileToMoFile;
var
  s: string;
  msgfmt: string;
  appoutput: TStringList;
begin
  msgfmt := GetSettingApplicationMsgFmtExe;
  if msgfmt = '' then
    msgfmt := 'msgfmt.exe';
  s := GetSettingApplicationMoFilename;
  if s = '' then
    s := ChangeFileExt(CurrentFilename, '.mo');

  if s <> '' then begin
    appoutput := TStringList.Create;
    try
      ExecConsoleApp(msgfmt, '-o "' + s + '" ' + '"' + CurrentFilename + '"', appoutput, nil);
      s := appoutput.Text;
      if trim(s) <> '' then begin
        ShowMessage(_('Output generated by PO->MO compiler:') + sLineBreak + s);
      end;
    finally
      FreeAndNil(appoutput);
    end;
  end else begin
    MessageDlg(_('No MO filename is set!'), mtError, [mbOK], 0);
    Abort;
  end;
end;

procedure TFormEditor.act_ToolsRepositoryLearnExecute(Sender: TObject);
var
  i: integer;
  item: TPoEntry;
  lng: string;
  Code: string;
  dm: TTranslationDbAccess;
  prog: Tf_dzProgress;
  Aborted: boolean;
  AddedCnt: integer;
  Source: string;
  Preview: boolean;
  RepoTag: string;
  PoFileTag: string;
begin
  CloseGuiItem;
  lng := Items.Language;
  if lng = '' then
    raise Exception.Create(_('The language of this po file is unknown, please edit the header first to provide it.'));
  if not dxlanguages.TryGetCodeForLanguage(lng, Code) then
    raise Exception.CreateFmt(_('Language "%s" is not known to Gorm.'), [lng]);

  dm := nil;
  if not FTranslationRepository.TryGetRepository(Code, dm) then
    raise Exception.Create(_('There is no repository for this language yet. Please create one first.'));
  Preview := true;
  RepoTag := 'auto';
  PoFileTag := 'added';
  if not Tf_TranslationDbLearnOptions.Execute(Self, dm, Preview, RepoTag, PoFileTag) then
    exit;

  prog := nil;
  try
    Source := Items.ProjectAndVersion;
    prog := Tf_dzProgress.Create(self);
    prog.ProgressMax := Rows.Count;
    prog.IsCancelVisible := true;
    prog.IsActionVisible := true;
    prog.Show;
    AddedCnt := 0;
    for i := 0 to Rows.Count - 1 do begin
      item := Rows.Objects[i] as TPoEntry;
      if not item.Fuzzy and (Item.MsgStr <> '') and not HasLabel(item, strIgnore) then begin
        if dm.TryAddTranslation(Item.MsgId, item.MsgStr, Source, Preview) then begin
          if PoFileTag <> '' then
            AddLabel(item, PoFileTag);
          Inc(AddedCnt);
          if Preview then
            prog.Progress(i, Format(_('labelled %s -> %s'), [Item.MsgId, Item.MsgStr]), Aborted)
          else
            prog.Progress(i, Format(_('added %s -> %s'), [Item.MsgId, Item.MsgStr]), Aborted);
        end else
          prog.Progress(i, Aborted);
      end else
        prog.Progress(i, Aborted);
      if Aborted then
        break;
    end;
    if Preview then
      MessageDlg(Format(_('%d entries have been labelled.'), [AddedCnt]), mtInformation, [mbOK], 0)
    else
      MessageDlg(Format(_('%d entries have been added to the repository.'), [AddedCnt]), mtInformation, [mbOK], 0);
    if AddedCnt > 0 then begin
      Labels.Add(PoFileTag);
      ApplyLabelList;
    end;
  finally
    FreeAndNil(prog);
    FreeAndNil(dm);
  end;
end;

procedure TFormEditor.act_ToolsRepositoryShowExecute(Sender: TObject);
begin
  Tf_TranslationDb.Execute(Self, FTranslationRepository);
end;

procedure TFormEditor.act_ToolsRunExecute(Sender: TObject);
var
  s: string;
begin
  screen.Cursor := crHourglass;
  try
    CloseGuiItem;
    CompileToMoFile;
  finally
    screen.Cursor := crDefault;
  end;

  // Start Application
  s := GetSettingApplicationExeFilename;
  if s = '' then
    ShowMessage(_('You must specify an application in the preferences before you can run it.'))
  else begin
    if not fileexists(s) then
      ShowMessage(format(_('Application file "%s" does not exist. Please check your settings.'), [s]))
    else if WinExec(PAnsiChar(ansistring(s)), SW_NORMAL) < 32 then
      raise Exception.Create(format(_('Could not run program file %s.'), [s]));
  end;
end;

procedure TFormEditor.act_TransSendEmailExecute(Sender: TObject);

var
  ires: Integer;
  svi: AnsiString;
  sdt: AnsiString;
  s: AnsiString;
  vi: TVersionInfo;

begin
  if CurrentFilename = '' then begin
    ShowMessage(_('No file is currently open.'));
    Exit;
  end;

  CloseGuiItem;

  vi := GetVersionInfo('');
  svi := AnsiString(Format('Gorm (v%.2d.%.2d.%.2d/B%d)', [vi.MajorVersion, vi.MinorVersion, vi.Release, vi.Build]));
  sdt := AnsiString(DateTimeToStr(Now));

  // Keep text in English. Two reasons:
  // 1) Most receivers can read the message
  // 2) TMapiMessage is (still) defined using AnsiChar
  s := AnsiString(CurrentFilename);
  ires := SendMail(Application.Handle,
    'Translation po-file', // Subject
    'Please find attached translation file: ' + s + #13#10 + // Body
    'Sent ' + sdt + ' by ' + svi,
    s, // Attachment
    '', '', // SenderName, SenderEMail,
    '', ''); // RecipientName, RecipientEMail

  if ires <> 0 then
    MessageDlg(_('Error sending mail') + ' (' + IntToStr(ires) + ').', mtError, [mbOK], 0);
end;

function CheckEmpty(s: string): string;
begin
  if s = '' then
    Result := '&nbsp;'
  else
    Result := s;
end;

procedure TFormEditor.CreateHTML;

const
  sColFmt = '<td style="background-color=%s">%s</td>';

var
  lst: TStringList;
  item: TPoEntry;
  i: Integer;
  fn, s: string;
  sx: string;
  scol: string;

begin
  lst := TStringList.Create;
  try
    lst.Add('<html>');
    lst.Add('<head>');
    // Output from <'Gorm'>
    lst.Add('<title>' + Format(_('Output from %s'),['Gorm']) + '</title>');
    lst.Add('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>');
    lst.Add('</head>');

    s := #39 + 'Arial Unicode MS' + #39;
    s := s + ',Tahoma';
    s := Format('<body style="font-family:%s">', [s]);
    lst.Add(s);

    s := Format(_('Translation file: "%s"'), [CurrentFilename]);
    lst.Add(s);
    lst.Add('<br>');
    lst.Add('<br>');

    lst.Add('<table width="90%" align="center" border="1">');
    lst.Add('<tr>');
    lst.Add('<td><strong>#</strong></td>');
    lst.Add('<td><strong>' + _('Status') + '</strong></td>');
    lst.Add('<td><strong>' + _('Original') + '</strong></td>');
    lst.Add('<td><strong>' + _('Translation') + '</strong></td>');
    lst.Add('</tr>');
    for i := 0 to Rows.Count - 1 do begin
      if Odd(i) then
        scol := '#FFFFFF'
      else
        scol := '#C6D4E2';

      s := '<tr>';
      item := Rows.Objects[i] as TPoEntry;

      sx := IntToStr(i + 1);
      sx := Format(sColFmt, [scol, sx]);
      s := s + sx;

      sx := CheckEmpty(GetStatusStr(item));
      sx := Format(sColFmt, [scol, sx]);
      s := s + sx;

      sx := CheckEmpty(item.MsgId);
      sx := Format(sColFmt, [scol, sx]);
      s := s + sx;

      sx := CheckEmpty(item.MsgStr);
      sx := Format(sColFmt, [scol, sx]);
      s := s + sx;

      s := s + '</tr>';
      lst.Add(s);
    end;
    lst.Add('</table>');
    lst.Add('<center><hr width="60%"></center>');
    // Output created <date + time>
    lst.Add(Format(_('Output created: %s'), [DateTimeToStr(Now)]));
    lst.Add('</body></html>');

    fn := ChangeFileExt(CurrentFileName, '.html');
    lst.SaveToFile(fn, TEncoding.UTF8);

    // OK if ires>32
    {ires:=}ShellExecute(0, 'open', PChar(fn), nil, nil, SW_SHOWNORMAL);
  finally
    lst.Free;
  end;
end;

end.