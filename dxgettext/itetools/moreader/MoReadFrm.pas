{$I JEDI.INC}
unit MoReadFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    OpenMO: TOpenDialog;
    PageControl1: TPageControl;
    tabMO: TTabSheet;
    Label1: TLabel;
    edMOFile: TEdit;
    reMO: TRichEdit;
    btnMOOpen: TButton;
    tabPO: TTabSheet;
    btnRead: TButton;
    Label2: TLabel;
    edPOFile: TEdit;
    btnPOOpen: TButton;
    rePO: TRichEdit;
    btnCompilePO: TButton;
    OpenPO: TOpenDialog;
    tabMerge: TTabSheet;
    Label3: TLabel;
    edPO1: TEdit;
    Button1: TButton;
    Label4: TLabel;
    edPO2: TEdit;
    Button2: TButton;
    tabParse: TTabSheet;
    Label5: TLabel;
    btnParseAdd: TButton;
    btnParse: TButton;
    reParsed: TRichEdit;
    reFilesToParse: TRichEdit;
    reMerge: TRichEdit;
    btnMerge: TButton;
    btnCancelParse: TButton;
    procedure btnReadClick(Sender: TObject);
    procedure btnMOOpenClick(Sender: TObject);
    procedure btnCompilePOClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnPOOpenClick(Sender: TObject);
    procedure btnParseAddClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
    procedure btnMergeClick(Sender: TObject);
    procedure btnCancelParseClick(Sender: TObject);
  private
    { Private declarations }
    FParseCancelled: boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

resourcestring
  STestString1 = 'Test string1';
  // Comment for STestString2
  STestString2 = 'Test string2';
  SPasParseFilter = 'Pascal files (*.pas;*.dfm)|*.pas;*.dfm';
  SPoFileFilter = 'PO Files (*.po)|*.po|All files|*.*';
  SMOFileFilter = 'MO Files (*.mo)|*.mo|All files|*.*';

implementation
uses
  POUtils, StrParser;

{$R *.DFM}

// Since Delphi5 doesn't have UTF8Encode/UTF8Decode, we include these dummy versions here
// If you have your own implementations, you can remove these

{$IFNDEF COMPILER6_UP}

function UTF8Encode(const S: WideString): string;
begin
  Result := S;
end;

function UTF8Decode(const S: string): WideString;
begin
  Result := S;
end;
{$ENDIF}


procedure TForm1.btnReadClick(Sender: TObject);
var
  i: integer;
begin
  reMO.Lines.Clear;
  with TMOFile.Create do
  try
    LoadFromFile(edMOFile.Text);
    for i := 0 to Count - 1 do
      reMO.Lines.Add(Format('"%s"="%s"', [UTF8Decode(UnescapeString(Items[i].MsgID)), UTF8Decode(UnescapeString(Items[i].MsgStr))]));
  finally
    Free;
  end;
end;

procedure TForm1.btnMOOpenClick(Sender: TObject);
begin
  OpenMO.Filename := edMOFile.Text;
  if OpenMO.Execute then
    edMOFile.Text := OpenMO.Filename;
end;

procedure TForm1.btnCompilePOClick(Sender: TObject);
var
  i: integer;
  MO: TMOFile;
  S: TStringlist;
begin
  rePO.Lines.Clear;
  MO := TMOFile.Create;
  S := TStringlist.Create;
  with MO do
  try
    CompileFromPO(edPOFile.Text);
    for i := 0 to Count - 1 do
    begin
      rePO.Lines.Add(Format('"%s"="%s"', [UTF8Decode(UnescapeString(MO[i].MsgID)),UTF8Decode(UnescapeString(MO[i].MsgStr))]));
      rePO.Lines.Add('');
    end;
  finally
    Free;
    S.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edMOFile.Text := ExtractFilePath(Application.ExeName) + 'default.mo';
  edPOFile.Text := ChangeFileExt(edMOFile.Text, '.po');
end;

procedure TForm1.btnPOOpenClick(Sender: TObject);
begin
  OpenPO.Filename := edPOFile.Text;
  if OpenPO.Execute then
    edPOFile.Text := OpenPO.Filename;
end;

procedure TForm1.btnParseAddClick(Sender: TObject);
begin
  OpenPO.Options := OpenPO.Options + [ofAllowMultiSelect];
  OpenPO.Filter := SPasParseFilter;
  if OpenPO.Execute then
    reFilesToParse.Lines.AddStrings(OpenPO.Files);
  OpenPO.Options := OpenPO.Options - [ofAllowMultiSelect];
  OpenPO.Filter := SPoFileFilter;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  OpenPO.Filename := edPO1.Text;
  if OpenPO.Execute then
    edPO1.Text := OpenPO.Filename;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  OpenPO.Filename := edPO2.Text;
  if OpenPO.Execute then
    edPO2.Text := OpenPO.Filename;
end;

procedure TForm1.btnParseClick(Sender: TObject);
var
  i: integer;
  FLastTick: DWORD;
  FLastSymbol, FLastComment: string;
  FLastCommentLine: integer;
  InResourceString: boolean;
  M: TMemoryStream;
  SP: TStringParser;
  function CombineString(SP: TStringParser): string;
  begin
    Result := SP.TokenString;
//    if SP.IsDFM then
    while SP.NextToken = '+' do
    begin
      SP.NextToken;
      if SP.Token in [toString, toWString] then
        Result := Result + SP.TokenString;
    end;
  end;
  function trimComment(const S: string): string;
  begin
    if Copy(S, 1, 2) = '//' then
      Result := Copy(S, 3, MaxInt)
    else if Copy(S, 1, 1) = '{' then
      Result := Copy(S, 2, Length(S) - 2)
    else if Copy(S, 1, 2) = '(*' then
      Result := Copy(S, 3, Length(S) - 4);
    Result := trim(Result);
  end;
begin
  // This is just a simple example of how the TStringParser class might be used.
  FParseCancelled := false;
  reParsed.Lines.Clear;
  M := TMemoryStream.Create;
  try
    FLastTick := GetTickCount;
    for i := 0 to reFilesToParse.Lines.Count - 1 do
      if FileExists(reFilesToParse.Lines[i]) then
      begin
        M.LoadFromFile(reFilesToParse.Lines[i]);
        M.Seek(0, soFromBeginning);
        SP := TStringParser.Create(M, reFilesToParse.Lines[i]);
        try
          InResourceString := false;
          FLastCommentLine := 0;
          FLastComment := '';
          FLastSymbol := '';
          while SP.Token <> toEOF do
          begin
            case SP.Token of
              toSymbol:
                begin
                  if SP.TokenIn(['resourcestring', 'const', 'object', 'inherited']) then
                    InResourceString := true
                  else if SP.TokenIn(['function', 'procedure', 'const', 'var', 'implementation']) then
                    InResourceString := false
                  else if InResourceString then
                    FLastSymbol := SP.TokenString;
                end;
              toComment:
                begin
                  FLastComment := trimComment(SP.TokenString);
                  FLastCommentLine := SP.SourceLine;
                end;
              toString, toWString:
                begin
                  if (FLastCommentLine >= SP.SourceLine - 2) and (FLastComment <> '') then
                    reParsed.Lines.Add(Format('#. %s', [UnescapeString(FLastComment)]));
                  // richedits doesn't like #0's in strings, so we have to trim them away:
                  if FLastSymbol <> '' then
                    reParsed.Lines.Add(Format('#. %s', [trimAllBelow(FLastSymbol, #1)]));
                  reParsed.Lines.Add(Format('#: %s:%d', [ExtractFileName(reFilesToParse.Lines[i]), SP.SourceLine]));
                  reParsed.Lines.Add(Format('msgid "%s"', [trimAllBelow(SP.TokenString, #1)]));
                  reParsed.Lines.Add('msgstr ""');
                  reParsed.Lines.Add('');
                  FLastCommentLine := 0;
                  FLastComment := '';
                  FLastSymbol := '';
                  // Continue;
                end;
            end;
            SP.NextToken;
            if GetTickCount - FLastTick > 100 then
            begin
              Application.ProcessMessages;
              if Application.Terminated or FParseCancelled then Exit;
              FLastTick := GetTickCount;
            end;
          end;
        finally
          SP.Free;
        end;
      end;
  finally
    M.Free;
  end;
end;

procedure TForm1.btnMergeClick(Sender: TObject);
var PO1, PO2: TPOFile; i, j: integer; T: TStringlist;
begin
  PO1 := TPoFile.Create;
  PO2 := TPoFile.Create;
  T := TStringlist.Create;
  try
    PO1.LoadFromFile(edPO1.Text);
    PO2.LoadFromFile(edPO2.Text);
    // be clever: merge into largest (less to copy)
    if PO1.Count > PO2.Count then
      PO1.Merge(PO2)
    else
    begin
      PO2.Merge(PO1);
      PO1.Free;
      PO1 := PO2;
      PO2 := nil;
    end;
//    PO1.Sort; - not needed, called in Pack
    PO1.Pack;
    if PO1.Count <= 0 then Exit;
    ShowMessage('Files merged, now writing to RichEdit (this might take a long time)...');
    reMerge.Lines.Clear;
    reMerge.MaxLength := MaxInt;
    reMerge.Lines.BeginUpdate;
    try
      for i := 0 to PO1.Count - 1 do
      begin
        for j := 0 to PO1[i].Comments.Count - 1 do
          reMerge.Lines.Add(Format('#%s', [PO1[i].Comments[j]]));
        T.Text := UTF8Decode(UnescapeString(PO1[i].MsgId));
        for j := 0 to T.Count - 1 do
        begin
          if j = 0 then
            reMerge.Lines.Add(Format('msgid "%s"', [T[j]]))
          else
            reMerge.Lines.Add(Format('"%s"', [T[j]]));
        end;
        T.Text := UTF8Decode(UnescapeString(PO1[i].MsgStr));
        for j := 0 to T.Count - 1 do
        begin
          if j = 0 then
            reMerge.Lines.Add(Format('msgstr "%s"', [T[j]]))
          else
            reMerge.Lines.Add(Format('"%s"', [T[j]]));
        end;
        reMerge.Lines.Add('');
      end;
    finally
      reMerge.Lines.EndUpdate;
    end;
  finally
    PO2.Free;
    PO1.Free;
    T.Free;
  end;
end;

procedure TForm1.btnCancelParseClick(Sender: TObject);
begin
  FParseCancelled := true;
end;

end.

