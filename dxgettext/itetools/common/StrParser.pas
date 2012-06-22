unit StrParser;

interface
uses
  Windows, SysUtils, Classes;

type
  TStringParser = class(TObject)
  private
    FStream: TStream;
    FOrigin: Longint;
    FBuffer: PChar;
    FBufPtr: PChar;
    FBufEnd: PChar;
    FSourcePtr: PChar;
    FSourceEnd: PChar;
    FTokenPtr: PChar;
    FStringPtr: PChar;
    FSourceLine: Integer;
    FToken: Char;
    FSaveChar: Char;
    FFloatType: Char;
    FWideStr: WideString;
    FFilename:string;
    FIsDFM: boolean;
    procedure ReadBuffer;
    procedure SkipBlanks;
    procedure Error(const Ident: string);
  public
    constructor Create(Stream: TStream;const Filename:string);
    destructor Destroy; override;
    function NextToken: Char;
    function SourcePos: Longint;
    function TokenIn(const Identifiers:array of string):boolean;overload;
    function TokenIn(Identifiers:TStrings):boolean;overload;

    function TokenString: string;
    function TokenWideString: WideString;
    property SourceLine: Integer read FSourceLine;
    property Token: Char read FToken;
    property IsDFM:boolean read FIsDFM;
  end;

  

const
  toComment = Char(6);
  cParseBufSize = 4096;

implementation

constructor TStringParser.Create(Stream: TStream;const Filename:string);
begin
  FStream := Stream;
  FFilename := Filename;
  GetMem(FBuffer, cParseBufSize);
  FBuffer[0] := #0;
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + cParseBufSize;
  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;
  FTokenPtr := FBuffer;
  FSourceLine := 1;
  NextToken;
  FIsDFM := TokenIn(['object','inherited']);
end;

destructor TStringParser.Destroy;
begin
  if FBuffer <> nil then
  begin
    FStream.Seek(Longint(FTokenPtr) - Longint(FBufPtr), 1);
    FreeMem(FBuffer, cParseBufSize);
  end;
end;

procedure TStringParser.Error(const Ident: string);
begin
  raise Exception.CreateFmt('%s on line %d in "%s"', [Ident, SourceLine,FFilename]);
end;

function TStringParser.NextToken: Char;
var
  I, J: Integer;
  IsWideStr: Boolean;
  P, S: PChar;
begin
  SkipBlanks;
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_':
      begin
        Inc(P);
        while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do
          Inc(P);
        Result := toSymbol;
      end;
    '#', '''':
      begin
        IsWideStr := False;
        J := 0;
        S := P;
        while True do
          case P^ of
            '#':
              begin
                Inc(P);
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  Inc(P);
                end;
                if (I > 127) then
                  IsWideStr := True;
                Inc(J);
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case P^ of
                    #0:
                      Error('Invalid string');
                    '''':
                      begin
                        Inc(P);
                        if P^ <> '''' then
                          Break;
                      end;
                  end;
                  Inc(J);
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        P := S;
        if IsWideStr then
          SetLength(FWideStr, J);
        J := 1;
        while True do
          case P^ of
            '#':
              begin
                Inc(P);
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  Inc(P);
                end;
                if IsWideStr then
                begin
                  FWideStr[J] := WideChar(SmallInt(I));
                  Inc(J);
                end
                else
                begin
                  S^ := Chr(I);
                  Inc(S);
                end;
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case P^ of
                    #0, #10, #13:
                      Error('Invalid string');
                    '''':
                      begin
                        Inc(P);
                        if P^ <> '''' then
                          Break;
                      end;
                  end;
                  if IsWideStr then
                  begin
                    FWideStr[J] := WideChar(P^);
                    Inc(J);
                  end
                  else
                  begin
                    S^ := P^;
                    Inc(S);
                  end;
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        FStringPtr := S;
        if IsWideStr then
          Result := toWString
        else
          Result := toString;
      end;
    '{':
      begin
        while not (P^ in [#0, '}']) do
        begin
          if P^ = #10 then
            Inc(FSourceLine);
          Inc(P);
          if P^ = #0 then
          begin
            FSourcePtr := P;
            ReadBuffer;
            if FSourcePtr^ = #0 then
              Error('Invalid comment');
            P := FSourcePtr;
          end;
        end;
        if P^ <> '}' then
          Error('Invalid comment')
        else
          Inc(P);
        Result := toComment;
      end;
    '(':
      begin
        Inc(P);
        if P^ = '*' then
        begin
          while true do
          begin
            repeat
              if P^ = #10 then
                Inc(FSourceLine);
              Inc(P);
              if P^ = #0 then
              begin
                FSourcePtr := P;
                ReadBuffer;
                if FSourcePtr^ = #0 then
                  Error('Invalid comment');
                P := FSourcePtr;
              end;
            until P^ in [#0, '*'];
            if P^ = '*' then
              Inc(P);
            if P^ = ')' then Break;
          end;
          if P^ <> ')' then
            Error('Invalid comment')
          else
            Inc(P);
          Result := toComment;
        end
        else
          Result := toSymbol;
      end;
    '/':
      begin
        Inc(P);
        if P^ = '/' then
          while not (P^ in [#0, #10]) do
          begin
            Inc(P);
            if P^ = #0 then
            begin
              FSourcePtr := P;
              ReadBuffer;
              if FSourcePtr^ = #0 then
                Break;
              P := FSourcePtr;
            end;
          end;
        Result := toComment;
      end;
    '$':
      begin
        Inc(P);
        while P^ in ['0'..'9', 'A'..'F', 'a'..'f'] do
          Inc(P);
        Result := toInteger;
      end;
    '-', '0'..'9':
      begin
        Inc(P);
        while P^ in ['0'..'9'] do
          Inc(P);
        Result := toInteger;
        while P^ in ['0'..'9', '.', 'e', 'E', '+', '-'] do
        begin
          Inc(P);
          Result := toFloat;
        end;
        if (P^ in ['c', 'C', 'd', 'D', 's', 'S']) then
        begin
          Result := toFloat;
          FFloatType := P^;
          Inc(P);
        end
        else
          FFloatType := #0;
      end;
  else
    Result := P^;
    if Result <> toEOF then
      Inc(P);
  end;
  FSourcePtr := P;
  FToken := Result;
end;

procedure TStringParser.ReadBuffer;
var
  Count: Integer;
begin
  Inc(FOrigin, FSourcePtr - FBuffer);
  FSourceEnd[0] := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if Count <> 0 then
    Move(FSourcePtr[0], FBuffer[0], Count);
  FBufPtr := FBuffer + Count;
  Inc(FBufPtr, FStream.Read(FBufPtr[0], FBufEnd - FBufPtr));
  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr;
  if FSourceEnd = FBufEnd then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then
      Error('Line too long');
  end;
  FSaveChar := FSourceEnd[0];
  FSourceEnd[0] := #0;
end;

procedure TStringParser.SkipBlanks;
begin
  while True do
  begin
    case FSourcePtr^ of
      #0:
        begin
          ReadBuffer;
          if FSourcePtr^ = #0 then
            Exit;
          Continue;
        end;
      #10:
        Inc(FSourceLine);
      #33..#255:
        Exit;
    end;
    Inc(FSourcePtr);
  end;
end;

function TStringParser.SourcePos: Longint;
begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;

function TStringParser.TokenIn(const Identifiers: array of string): boolean;
var i:integer;
begin
  for i := Low(Identifiers) to High(Identifiers) do
    if AnsiSameText(TokenString,Identifiers[i]) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function TStringParser.TokenIn(Identifiers: TStrings): boolean;
var i:integer;
begin
  for i := 0 to Identifiers.Count - 1 do
    if AnsiSameText(TokenString,Identifiers[i]) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function TStringParser.TokenString: string;
var
  L: Integer;
begin
  if (FToken = toString) then
    L := FStringPtr - FTokenPtr
  else
    L := FSourcePtr - FTokenPtr;
  SetString(Result, FTokenPtr, L);
end;

function TStringParser.TokenWideString: WideString;
begin
  if (FToken in [toString,toComment]) then
    Result := TokenString
  else
    Result := FWideStr;
end;


end.
