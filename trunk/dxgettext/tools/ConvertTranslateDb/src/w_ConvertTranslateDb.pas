unit w_ConvertTranslateDb;

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
  StdCtrls,
  ADODB;

type
  Tf_ConvertTranslateDb = class(TForm)
    ed_InputFile: TEdit;
    b_InputFile: TButton;
    ed_OutputFile: TEdit;
    b_OutputFile: TButton;
    od_MdbFile: TOpenDialog;
    b_Convert: TButton;
    b_Exit: TButton;
    procedure b_InputFileClick(Sender: TObject);
    procedure b_OutputFileClick(Sender: TObject);
    procedure b_ConvertClick(Sender: TObject);
    procedure b_ExitClick(Sender: TObject);
  private
    FSrcConn: TAdoConnection;
    function CreateAdoQuery(const _SQL: string): TADOQuery;
  public
  end;

var
  f_ConvertTranslateDb: Tf_ConvertTranslateDb;

implementation

uses
  u_TranslationDBAccess;

{$R *.dfm}

procedure Tf_ConvertTranslateDb.b_ConvertClick(Sender: TObject);
const
  CONNECTION_STRING = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%S;';
var
  dba: TTranslationDbAccess;
  SrcEng: TADOQuery;
  SrcTrans: TADOQuery;
  English: string;
  EnglishId: integer;
  Translation: string;
  Source: string;
begin
  SrcTrans := nil;
  SrcEng := nil;
  FSrcConn := nil;
  dba := TTranslationDbAccess.Create(ed_OutputFile.Text);
  try
    FSrcConn := TADOConnection.Create(Self);
    FSrcConn.ConnectionString := Format(CONNECTION_STRING, [ed_InputFile.Text]);
    FSrcConn.LoginPrompt := False;
    FSrcConn.Connected := true;
    SrcEng := CreateAdoQuery('Select * from English order by id');
    SrcEng.Open;
    SrcTrans := CreateAdoQuery('Select * from TargetLang order by English_Id');
    SrcTrans.Open;
    while not SrcEng.Eof do begin
      English := VarToStrDef(SrcEng['Content'], '');
      if English <> '' then begin
        EnglishId := SrcEng['ID'];
        if SrcTrans.Locate('English_Id', EnglishId, []) then begin
          while not SrcTrans.Eof and (SrcTrans['English_Id'] = EnglishId) do begin
            Translation := VarToStrDef(SrcTrans['Content'], '');
            Source := VarToStrDef(SrcTrans['Source'], '');
            if Translation <> '' then
              dba.TryAddTranslation(English, Translation, Source, false);
            SrcTrans.Next;
          end;
        end;
      end;
      SrcEng.Next;
    end;
  finally
    FreeandNil(SrcTrans);
    FreeAndNil(SrcEng);
    FreeAndNil(FSrcConn);
    FreeAndNil(dba);
  end;
end;

function Tf_ConvertTranslateDb.CreateAdoQuery(const _SQL: string): TADOQuery;
begin
  Result := TADOQuery.Create(nil);
  Result.Connection := FSrcConn;
  Result.DisableControls;
  Result.CursorLocation := clUseServer;
  Result.CursorType := ctKeyset;
  Result.SQL.Text := _SQL;
  Result.Prepared := true;
end;

procedure Tf_ConvertTranslateDb.b_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_ConvertTranslateDb.b_InputFileClick(Sender: TObject);
begin
  od_MdbFile.FileName := ed_InputFile.Text;
  if od_MdbFile.Execute then
    ed_InputFile.Text := od_MdbFile.FileName;
end;

procedure Tf_ConvertTranslateDb.b_OutputFileClick(Sender: TObject);
begin
  od_MdbFile.FileName := ed_OutputFile.Text;
  if od_MdbFile.Execute then
    ed_OutputFile.Text := od_MdbFile.FileName;
end;

end.

