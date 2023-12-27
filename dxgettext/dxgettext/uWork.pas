unit uWork;
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

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XPMan, StdCtrls, xgettext, ExtCtrls, System.UITypes;

type
  TFormWork = class(TForm)
    XPManifest: TXPManifest;
    MemoProgress: TMemo;
    ButtonOK: TButton;
    LabelProgress: TLabel;
    TimerActivate: TTimer;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerActivateTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure Progress(const CurrentTask, CurrentFileName: string;
                       const LineNumber: Integer);
    procedure Warning (WarningType: TWarningType;
                       const Msg, Line, Filename: string;
                       LineNumber: Integer);
    procedure OverwriteQuestion (sender: TObject; const aFileName: String; var Overwrite: boolean);
  public
    { Public declarations }
  end;

var
  FormWork: TFormWork;

implementation

uses uconfig, IniFiles, gnugettext, runxgettext;

{$R *.dfm}

procedure Explode (line:string;sl:TStrings);
var
  i,last:integer;
  item:string;
begin
  last:=1;
  line:=line+' ';
  for i:=1 to length(line) do begin
    if line[i]<=#32 then begin
      item:=trim(copy(line,last,i-last));
      if item<>'' then
        sl.Add (item);
      last:=i;
    end;
  end;
end;

procedure TFormWork.ButtonOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFormWork.FormResize(Sender: TObject);
begin
  ButtonOK.Left:=(Width-ButtonOK.Width) div 2;
end;

procedure TFormWork.Progress(const CurrentTask, CurrentFileName: string;
                             const LineNumber: Integer);
begin
  LabelProgress.Caption:=CurrentTask;
  LabelProgress.Update;
  if LineNumber<=1 then begin
    MemoProgress.Lines.Add(CurrentTask);
    Application.ProcessMessages;
  end;
end;

procedure TFormWork.Warning(WarningType: TWarningType;
                            const Msg, Line, Filename: string;
                            LineNumber: Integer);
begin
  MemoProgress.Lines.Add(Msg);
  MemoProgress.Lines.Add(Format(_('Line: %s'),[Line]));
  MemoProgress.Lines.Add('');
end;

procedure TFormWork.TimerActivateTimer(Sender: TObject);
var
  f: TFormConfig;
  xgt: TXGetText;
  ini: TIniFile;
  rxgt: TRunXGettext;
  filename: string;
begin
  filename := ExpandFileName(paramstr(1));
  TimerActivate.Enabled := False;
  xgt := TXGetText.Create;
  try
    f := TFormConfig.Create (self);
    try
      if fileexists(filename) then
      begin
        f.EditBasePath.Text := extractfilepath( filename);
        f.EditMask    .Text := extractfilename( filename);
        f.CheckBoxSaveSettings.Checked := false;
        f.CheckBoxRecurse     .Checked := false;
      end
      else
      begin
        f.EditBasepath.Text := IncludeTrailingPathDelimiter(filename);
        f.CheckBoxSaveSettings.Checked := fileexists( f.EditBasepath.Text + 'dxgettext.ini');
        if f.CheckBoxSaveSettings.Checked then
        begin
          ini := TIniFile.Create (f.EditBasepath.Text + 'dxgettext.ini');
          try
            f.CheckBoxRecurse                    .Checked := ini.ReadBool  ('ggdxgettext', 'recurse'                    , f.CheckBoxRecurse.Checked);
            f.EditMask                           .Text    := ini.ReadString('dxgettext'  , 'mask'                       , f.EditMask.Text);
            f.CBCreateIgnore                     .Checked := ini.ReadBool  ('ggdxgettext', 'updateignore'               , False);
            f.CBRemoveIgnore                     .Checked := ini.ReadBool  ('ggdxgettext', 'useignore'                  , False);
            f.CheckBoxAllowNonAscii              .Checked := ini.ReadBool  ('ggdxgettext', 'allownonascii'              , False);
            f.CheckBoxPreserveUserComments       .Checked := ini.ReadBool  ('ggdxgettext', 'preserveusercomments'       , False);
            f.CheckBoxUseGetTextDefaultFormatting.Checked := ini.ReadBool  ('ggdxgettext', 'usegettextdefaultformatting', False);
          finally
            FreeAndNil (ini);
          end;
        end;
      end;
      if f.ShowModal<>mrOK then
      begin
        Close;
        exit;
      end;
      xgt.Recurse                     := f.CheckBoxRecurse                    .Checked;
      xgt.UpdateIgnore                := f.CBCreateIgnore                     .Checked;
      xgt.UseIgnoreFile               := f.CBRemoveIgnore                     .Checked;
      xgt.AddBaseDirectory(IncludeTrailingPathDelimiter(f.EditBasepath.Text));
      xgt.DestinationPath             := IncludeTrailingPathDelimiter( f.EditBasepath.Text);
      xgt.AllowNonAscii               := f.CheckBoxAllowNonAscii              .Checked;
      xgt.PreserveUserComments        := f.CheckBoxPreserveUserComments       .Checked;
      xgt.UseGetTextDefaultFormatting := f.CheckBoxUseGetTextDefaultFormatting.Checked;

      Explode(f.EditMask.Text,xgt.filemasks);
    finally
      FreeAndNil (f);
    end;
    xgt.OnProgress:=Progress;
    xgt.OnWarning:=Warning;
    xgt.OnOverwrite:=OverwriteQuestion;
    Update;

    xgt.Execute;

    if xgt.CFiles.Count <> 0 then
    begin
      // %s will be replaced with the number of C/C++ files
      if Assigned(xgt.OnProgress) then
      begin
        xgt.OnProgress( Format( _('Scanning %s C/C++ files'),
                                [ IntToStr(xgt.CFiles.Count)]),
                        'C/C++ files',
                        0);
      end;

      rxgt := TRunXGettext.Create;
      try
        rxgt.FileList.Assign( xgt.CFiles);
        rxgt.OutputDir := xgt.DestinationPath;
        rxgt.Execute;
      finally
        FreeAndNil (rxgt);
      end;
    end;
  finally
    FreeAndNil (xgt);
  end;
  if MemoProgress.Lines.Count=0 then
    MemoProgress.Lines.Add(_('No warnings or errors.'));
  LabelProgress.Caption:=_('Finished.');
  ButtonOK.Enabled:=True;
end;

procedure TFormWork.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
  TimerActivate.Enabled:=True;
end;

procedure TFormWork.OverwriteQuestion(sender: TObject;
  const aFileName: String; var Overwrite: boolean);
begin
  Overwrite:=MessageDlg(Format(_('Do you want to overwrite the file named %s?'),[aFilename]),mtConfirmation,[mbYes,mbNo],0)=mrYes;
end;

end.
