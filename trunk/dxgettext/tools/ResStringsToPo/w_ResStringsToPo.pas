// This is a rather hacky way to convert .pas files that contain resource strings
// in the source (English) and destination (e.g. German) language to .po files.
// It does no parsing at all but assumes that all strings have got the form
// <name> = '<content>';
// excactly (including spaces before and after the '=')
// so it fails to handle multiline strings and probably quite a lot of others.

unit w_ResStringsToPo;

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
  StdCtrls;

type
  Tf_ResStringsToPo = class(TForm)
    l_SourceLng: TLabel;
    ed_SourceLanguage: TEdit;
    b_SourceLanguage: TButton;
    l_DestLanguage: TLabel;
    ed_DestLanguage: TEdit;
    b_DestLanguage: TButton;
    l_PoFile: TLabel;
    ed_PoFile: TEdit;
    b_PoFile: TButton;
    b_Execute: TButton;
    b_Close: TButton;
    od_Pas: TOpenDialog;
    sd_Po: TSaveDialog;
    procedure b_CloseClick(Sender: TObject);
    procedure b_SourceLanguageClick(Sender: TObject);
    procedure b_DestLanguageClick(Sender: TObject);
    procedure b_PoFileClick(Sender: TObject);
    procedure b_ExecuteClick(Sender: TObject);
  private
    procedure RemoveStuff(_sl: TStringList);
  public
  end;

var
  f_ResStringsToPo: Tf_ResStringsToPo;

implementation

{$R *.dfm}

uses
  poparser,
  StrUtils;

procedure Tf_ResStringsToPo.b_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_ResStringsToPo.b_DestLanguageClick(Sender: TObject);
begin
  od_Pas.FileName := ed_DestLanguage.Text;
  if od_Pas.Execute then
    ed_DestLanguage.Text := od_Pas.FileName;
end;

procedure Tf_ResStringsToPo.b_PoFileClick(Sender: TObject);
begin
  sd_Po.FileName := ed_PoFile.Text;
  if sd_Po.Execute then
    ed_PoFile.Text := sd_Po.FileName;
end;

procedure Tf_ResStringsToPo.b_SourceLanguageClick(Sender: TObject);
begin
  od_Pas.FileName := ed_SourceLanguage.Text;
  if od_Pas.Execute then
    ed_SourceLanguage.Text := od_Pas.FileName;
end;

procedure Tf_ResStringsToPo.b_ExecuteClick(Sender: TObject);
var
  Source: TStringList;
  Dest: TStringList;
  i: Integer;
  SrcFile: string;
  PoFile: string;
  Name: string;
  Org: string;
  Trans: string;
  pol: TPoEntryList;
  po: TPoEntry;
begin
  SrcFile := ed_SourceLanguage.Text;
  PoFile := ed_PoFile.Text;

  pol := nil;
  Dest := nil;
  Source := TStringList.Create;
  try
    Source.LoadFromFile(SrcFile);
    RemoveStuff(Source);

    Dest := TStringList.Create;
    Dest.LoadFromFile(ed_DestLanguage.Text);
    RemoveStuff(Dest);

    pol := TPoEntryList.Create;
    if FileExists(PoFile) then
      pol.LoadFromFile(PoFile);

    for i := 0 to Source.Count - 1 do begin
      Name := Source.Names[i];
      if Name <> '' then begin
        Org := AnsiDequotedStr(Trim(Source.Values[Name]), '''');
        Trans := AnsiDequotedStr(Trim(Dest.Values[Name]), '''');
        if (Org <> '') and (Trans <> '') then begin
          po := pol.Find(Org);
          if Assigned(po) then begin
            po.AutoCommentList.Add(Format('#. Programmer''s name for it: %s', [Name]));
            po.AutoCommentList.Add(Format('#: %s', [SrcFile]));
          end else begin
            po := TPoEntry.Create;
            po.AutoCommentList.Add(Format('#. Programmer''s name for it: %s', [Name]));
            po.AutoCommentList.Add(Format('#: %s', [SrcFile]));
            po.MsgId := Org;
            po.MsgStr := Trans;
            pol.Add(po);
          end;
        end;
      end;
    end;
    pol.SaveToFile(PoFile, 0);
  finally
    FreeAndNil(pol);
    FreeAndNil(Dest);
    FreeAndNil(Source);
  end;
end;

procedure Tf_ResStringsToPo.RemoveStuff(_sl: TStringList);
var
  i: Integer;
  s: string;
begin
  for i := _sl.Count - 1 downto 0 do begin
    if Pos(' = ', _sl[i]) = 0 then
      _sl.Delete(i);
  end;
  for i := 0 to _sl.Count - 1 do begin
    s := _sl[i];
    s := Trim(s);
    if RightStr(s, 1) = ';' then
      s := LeftStr(s, Length(s) - 1);
    _sl[i] := s;
  end;
end;

end.

