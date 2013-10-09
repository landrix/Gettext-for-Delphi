unit PreferencesUi;
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

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls, ImgList,
  u_dzVclUtils;

type
  TButtonedEdit = class(TdzButtonedEdit)
  end;

type
  TFormPreferences = class(TForm)
    LabelPath: TStaticText;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    GroupBoxStartupSettings: TGroupBox;
    ComboBoxParallelTranslation: TComboBox;
    LabelParallelTranslation: TLabel;
    l_OpenFile: TLabel;
    CheckBoxStartupAddStdLabels: TCheckBox;
    GroupBoxTranslationTest: TGroupBox;
    l_ApplicationExeFilename: TLabel;
    ed_ApplicationExeFilename: TButtonedEdit;
    l_MoFilename: TLabel;
    ed_MoFilename: TButtonedEdit;
    CheckBoxApplySettingsAtStartup: TCheckBox;
    CheckBoxAutoUpgrade: TCheckBox;
    grp_SaveSettings: TGroupBox;
    chk_WrapAtNCharacters: TCheckBox;
    ed_WrapLinesAfter: TEdit;
    chk_ShowStatus: TCheckBox;
    groupTranslationMemory: TGroupBox;
    CheckBoxTranslationMemory: TCheckBox;
    edMaxMemoryFileSize: TLabeledEdit;
    edCompareAccuracy: TLabeledEdit;
    edSuggestionsNumber: TLabeledEdit;
    udWrap: TUpDown;
    udMaxMemoryFSize: TUpDown;
    unMemCopmAccur: TUpDown;
    udMemSuggNum: TUpDown;
    EditFilenameToOpen: TButtonedEdit;
    ilOpenFile: TImageList;
    gbSymbols: TGroupBox;
    memSpecialChars: TMemo;
    Label2: TLabel;
    edSymbolsFontSize: TLabeledEdit;
    udSymbolsFontSize: TUpDown;
    grp_TranslationRepositiory: TGroupBox;
    l_TransRepDir: TLabel;
    ed_TransRepDir: TEdit;
    gb_ExternalEditor: TGroupBox;
    l_1: TLabel;
    cb_ExternalEditorUseLineNumbers: TCheckBox;
    eb_ExternalEditor: TButtonedEdit;
    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonBrowseExenameClick(Sender: TObject);
    procedure ed_MoFilenameRightButtonClick(Sender: TObject);
    procedure chk_WrapAtNCharactersClick(Sender: TObject);
    procedure CheckBoxTranslationMemoryClick(Sender: TObject);
    procedure EditFilenameToOpenRightButtonClick(Sender: TObject);
    procedure eb_ExternalEditorRightButtonClick(Sender: TObject);
  public
  end;

implementation

uses
  gnugettext, AppSettings, GoogleTranslate, Menus;

{$R *.dfm}

procedure TFormPreferences.ed_MoFilenameRightButtonClick(Sender: TObject);
var
  od:TOpenDialog;
begin
  od:=TOpenDialog.Create(self);
  try
    od.FileName := ed_MoFilename.Text;
    od.Filter:=_('MO files (*.mo)')+'|*.mo';
    if od.Execute then begin
      ed_MoFilename.Text := od.FileName;
    end;
  finally
    FreeAndNil (od);
  end;
end;

procedure TFormPreferences.eb_ExternalEditorRightButtonClick(Sender: TObject);
var
  lOpenDialog: TOpenDialog;
begin
  lOpenDialog := TOpenDialog.Create(self);
  try
    lOpenDialog.FileName := eb_ExternalEditor.Text;
    lOpenDialog.Filter   := _('Executables (*.exe)')+'|*.exe';
    if lOpenDialog.Execute then
    begin
      eb_ExternalEditor.Text := lOpenDialog.FileName;
    end;
  finally
    FreeAndNil (lOpenDialog);
  end;
end;

procedure TFormPreferences.ButtonBrowseExenameClick(Sender: TObject);
var
  od:TOpenDialog;
begin
  od:=TOpenDialog.Create(self);
  try
    od.FileName := ed_ApplicationExeFilename.Text;
    od.Filter:=_('Executables (*.exe)')+'|*.exe';
    if od.Execute then
      ed_ApplicationExeFilename.Text := od.FileName;
  finally
    FreeAndNil (od);
  end;
end;

procedure TFormPreferences.ButtonOKClick(Sender: TObject);

  function GetSelectedTranslation: string;
  var
    Idx: Integer;
    i: Integer;
    s: string;
  begin
    Result := '';
    Idx := ComboBoxParallelTranslation.ItemIndex;
    if Idx > 0 then begin
      s := ComboBoxParallelTranslation.Items[Idx];
      for i := Low(TRANSLATION_ARR) to High(TRANSLATION_ARR) do begin
        if SameText(s, StripHotkey(TRANSLATION_ARR[i].Caption)) then begin
          Result := TRANSLATION_ARR[i].GTString;
          exit;
        end;
      end;
    end;
  end;

  var SpecCharsList : TStringList;

begin
  SetSetting ('Startup','Enabled',CheckBoxApplySettingsAtStartup.Checked);
  SetSetting ('Startup','AutoCreateLabels',CheckBoxStartupAddStdLabels.Checked);
  SetSetting ('Startup','OpenFilename',trim(EditFilenameToOpen.Text));
  SetSetting ('Startup','ParallelTranslation', GetSelectedTranslation());
  SetSetting ('Startup','AutoUpgrade',CheckBoxAutoUpgrade.Checked);
  SetSetting ('GUI', 'ShowStatus', chk_ShowStatus.Checked);
  SetSetting ('Application','ExeFilename', ed_ApplicationExeFilename.Text);
  SetSetting ('Application','MoFilename', ed_MoFilename.Text);
  SetSetting ('Application','ExternalEditorFilename', eb_ExternalEditor.Text);
  SetSetting ('Application','ExternalEditorUseLineNumbers', cb_ExternalEditorUseLineNumbers.Checked);

  if chk_WrapAtNCharacters.Checked then
    SetSetting ('Save', 'WrapAfter', ed_WrapLinesAfter.Text)
  else
    SetSetting ('Save', 'WrapAfter', '0');

  // Set all translation memory settings
  SetSetting ('MemoryTranslation','TranslationMemory',CheckBoxTranslationMemory.Checked);
  if CheckBoxTranslationMemory.Checked then begin
    SetSetting ('MemoryTranslation','MaxMemoryFileSize',edMaxMemoryFileSize.Text);
    SetSetting ('MemoryTranslation','SuggestionsAccuracy',edCompareAccuracy.Text);
    SetSetting ('MemoryTranslation','SuggestionsNumber',edSuggestionsNumber.Text);
  end;

  // Special chars settings
  SpecCharsList := TStringList.Create;
  try
    SpecCharsList.Assign(memSpecialChars.Lines);
    SaveList('SpecialChars', SpecCharsList);
  finally
    FreeAndNil(SpecCharsList);
  end;
  SetSetting ('SpecialChars','FontSize',IntToStr(udSymbolsFontSize.Position));

  SetSetting('TranslationRepository', 'Directory', ed_TransRepDir.Text);

  Close;
end;

procedure TFormPreferences.CheckBoxTranslationMemoryClick(Sender: TObject);
var i : Integer;
begin
   For i:=0 To groupTranslationMemory.ControlCount-1 Do
    groupTranslationMemory.Controls[i].Enabled := CheckBoxTranslationMemory.Checked;
  CheckBoxTranslationMemory.Enabled := true;
end;

procedure TFormPreferences.chk_WrapAtNCharactersClick(Sender: TObject);
begin
  ed_WrapLinesAfter.Enabled := chk_WrapAtNCharacters.Checked;
end;

procedure TFormPreferences.EditFilenameToOpenRightButtonClick(
  Sender: TObject);
var
  od:TOpenDialog;
begin
  od:=TOpenDialog.Create(self);
  try
    od.FileName:=EditFilenameToOpen.Text;
    od.Filter:=_('PO files (*.po)')+'|*.po';
    if od.Execute then EditFilenameToOpen.Text := od.FileName;
  finally
    FreeAndNil (od);
  end;
end;

procedure TFormPreferences.FormCreate(Sender: TObject);

  function SelectConfiguredTranslation(pt: string): integer;
  var
    i: Integer;
    s: string;
  begin
    Result := 0;

    for i := Low(TRANSLATION_ARR) to High(TRANSLATION_ARR) do begin
      if SameText(TRANSLATION_ARR[i].GTString, pt) then begin
        s := StripHotkey(TRANSLATION_ARR[i].Caption);
        break;
      end;
    end;

    for i := 0 to ComboBoxParallelTranslation.Items.Count - 1 do begin
      if ComboBoxParallelTranslation.Items[i] = s then begin
        Result := i;
        break;
      end;
    end;
  end;

var
  i:integer;
  pt:string;
  SpecChars : TStringList;
begin
  TranslateComponent (self);
  LabelPath.Caption:=format(_('Settings file: %s'),[GetIniFilename]);

  memSpecialChars.Lines.Clear;
  memSpecialChars.Lines.Add(#167#169#174#183'$'#8364#8470); // do not translate
  memSpecialChars.Lines.Add(#186#185#178#179#170#176); // do not translate
  memSpecialChars.Lines.Add(#188#189#8531#8532#190#8539#8540#8541#8542#8453); // do not translate
  memSpecialChars.Lines.Add(#215#247#708#709#8240); // do not translate
  memSpecialChars.Lines.Add(#8804#8805#8801#8800#8776#8734#177); // do not translate
  memSpecialChars.Lines.Add(#8226#171#8212#187#8211); // do not translate
  memSpecialChars.Lines.Add(#8592#8593#8594#8595#8596#8597); // do not translate
  memSpecialChars.Lines.Add(#8710#8706#181#8721#8467); // do not translate
  memSpecialChars.Lines.Add(#9792#9794#9786#9787); // do not translate


  for i:= Low(TRANSLATION_ARR) to High(TRANSLATION_ARR) do
    ComboBoxParallelTranslation.Items.Add(StripHotkey(TRANSLATION_ARR[i].Caption));

  CheckBoxApplySettingsAtStartup.Checked:=GetSettingStartupAutoCreateLabels;
  CheckBoxStartupAddStdLabels.Checked:=GetSettingStartupActionsEnabled;
  CheckBoxAutoUpgrade.Checked:=GetSettingStartupAutoUpgrade;
  CheckBoxTranslationMemory.Checked := GetSettingTranslationMemory;
  chk_ShowStatus.Checked := GetSettingShowStatus;
  pt := GetSettingStartupParallelTranslation;
  EditFilenameToOpen.Text:=GetSettingStartupOpenFilename;
  ed_ApplicationExeFilename.Text := GetSettingApplicationExeFilename;
  ed_MoFilename.Text := GetSettingApplicationMoFilename;
  eb_ExternalEditor.Text := GetSettingApplicationExternalEditorFilename;
  cb_ExternalEditorUseLineNumbers.Checked := GetSettingApplicationExternalEditorUseLineNumbers;

  i := GetSettingSaveWrapAfter;
  chk_WrapAtNCharacters.Checked := (i <> 0);
  if i =0 then
    i := 70;
  // ed_WrapLinesAfter.Text := IntToStr(i);
  udWrap.Position := i;

  ComboBoxParallelTranslation.ItemIndex := SelectConfiguredTranslation(pt);

  // translations memory settings
  CheckBoxTranslationMemory.Checked := GetSettingTranslationMemory;
  udMaxMemoryFSize.Position := GetSettingMemoryFileSize;
  unMemCopmAccur.Position := GetSettingMemorySuggestionsAccuracy;
  udMemSuggNum.Position := GetSettingMemorySuggestionsNumber;

  // special chars settings
  udSymbolsFontSize.Position := GetSettingSpecCharsFontSize;
  SpecChars := TStringList.Create;
  try
    memSpecialChars.Lines.Clear;
    ReadList('SpecialChars', SpecChars);
    memSpecialChars.Lines.Assign(SpecChars);
  finally
    FreeAndNil(SpecChars);
  end;

  CheckBoxTranslationMemoryClick(Sender);

  ed_TransRepDir.Text := GetSettingTranslationRepositoryDir;
end;

end.
