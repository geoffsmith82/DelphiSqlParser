unit Data.DB.Lexer;

interface

uses
  System.SysUtils, System.Classes;

type
  TTokenType = (ttUnknown, ttKeyword, ttIdentifier, ttOperator, ttNumber, ttString, ttComment, ttWhitespace, ttEOF);

  TCharType = (ctOther, ctLetterStart, ctLetterNumber, ctNumber, ctHash, ctQuote, ctDollar, ctDash, ctComment);

  TToken = record
    TokenType: TTokenType;
    Value: string;
    Position: Integer;
    Line: Integer;
  end;

  TSQLLexer = class
  private
    FInput: string;
    FPosition: Integer;
    FLine: Integer;
    FCurrentToken: TToken;
    function GetNextChar: Char;
    function PeekNextChar: Char;
    function IsEOF: Boolean;
    function ReadWhile(const Predicate: TFunc<Char, Boolean>): string;
    function GetSourcePos: Integer;
    function GetTokenString: string;
    function GetLinePos: Integer;
    function GetToken: TCharType;
    function ReadRest: string;
    function RemainingString: string;
  public
    constructor Create(const AInput: string); overload;
    constructor Create(AStream: TStream); overload;
    function GetNextToken: TToken;
    property SourcePos: Integer read GetSourcePos;
    property TokenString: string read GetTokenString;
    property LinePos: Integer read GetLinePos;
    property Token: TCharType read GetToken;
  end;

implementation

constructor TSQLLexer.Create(const AInput: string);
begin
  FInput := AInput;
  FPosition := 1;
  FLine := 1;
end;

constructor TSQLLexer.Create(AStream: TStream);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create;
  try
    StringStream.LoadFromStream(AStream);
    FInput := StringStream.DataString;
    FPosition := 1;
    FLine := 1;
  finally
    StringStream.Free;
  end;
end;

function TSQLLexer.GetNextChar: Char;
begin
  if FPosition > Length(FInput) then
    Result := #0
  else
    Result := FInput[FPosition];
  Inc(FPosition);
  if Result = #10 then
    Inc(FLine);
end;

function TSQLLexer.RemainingString: string;
begin
  Result := FInput.Substring(FPosition);
end;

function TSQLLexer.PeekNextChar: Char;
begin
  if (FPosition) > Length(FInput) then
    Result := #0
  else
    Result := FInput[FPosition];
end;

function TSQLLexer.IsEOF: Boolean;
begin
  Result := FPosition > Length(FInput);
end;

function TSQLLexer.ReadWhile(const Predicate: TFunc<Char, Boolean>): string;
var
  Ch: Char;
begin
  Result := '';
  while not IsEOF do
  begin
    Ch := GetNextChar;
    if not Predicate(Ch) then
    begin
      Dec(FPosition);
      if Ch = #10 then
        Dec(FLine);
      Break;
    end;
    Result := Result + Ch;
  end;
end;

function TSQLLexer.ReadRest: string;
var
  Ch: Char;
begin
  Result := '';
  while not IsEOF do
  begin
    Ch := GetNextChar;
 //   if not Predicate(Ch) then
//    begin
//      Dec(FPosition);
//      if Ch = #10 then
//        Dec(FLine);
//      Break;
//    end;
    Result := Result + Ch;
  end;
end;

function TSQLLexer.GetNextToken: TToken;
var
  Ch: Char;
begin
  while not IsEOF do
  begin
    Ch := GetNextChar;

    case Ch of
      ' ', #9, #10, #13:
        begin
          // Skip whitespace characters
          Continue;
        end;
      'A'..'Z', 'a'..'z', '_':
        begin
          FCurrentToken.TokenType := ttIdentifier;
          FCurrentToken.Value := Ch + ReadWhile(function(C: Char): Boolean
            begin
              Result := CharInSet(C, ['A'..'Z', 'a'..'z', '0'..'9', '_']);
            end);
        end;
      '0'..'9':
        begin
          FCurrentToken.TokenType := ttNumber;
          FCurrentToken.Value := Ch + ReadWhile(function(C: Char): Boolean
            begin
              Result := CharInSet(C, ['0'..'9', '.', 'e', 'E', '+', '-']);
            end);
        end;
      '.', ',',';':
        begin
          FCurrentToken.TokenType := ttOperator;
          FCurrentToken.Value := Ch;
        end;
      '''', '"':
        begin
          FCurrentToken.TokenType := ttString;
          FCurrentToken.Value := Ch + ReadWhile(function(C: Char): Boolean
            begin
              Result := C <> Ch;
            end) + GetNextChar;
        end;
      '-':
        begin
          if PeekNextChar = '-' then
          begin
           // GetNextChar; // Consume the second '-'
            RemainingString;
            FCurrentToken.TokenType := ttComment;
            FCurrentToken.Value := Ch + ReadRest;
          end
          else
          begin
            FCurrentToken.TokenType := ttOperator;
            FCurrentToken.Value := Ch;
          end;
        end;
      '/':
        if PeekNextChar = '*' then
        begin
          GetNextChar; // Consume the '*'
          FCurrentToken.TokenType := ttComment;
          FCurrentToken.Value := Ch + '*' + ReadWhile(function(C: Char): Boolean
            begin
              Result := not ((C = '*') and (PeekNextChar = '/'));
            end) + '*/';
          GetNextChar; // Consume the '/'
        end
        else
        begin
          FCurrentToken.TokenType := ttOperator;
          FCurrentToken.Value := Ch;
        end;
      '#':
        begin
          FCurrentToken.TokenType := ttOperator;
          FCurrentToken.Value := Ch;
        end;
      '$':
        begin
          FCurrentToken.TokenType := ttOperator;
          FCurrentToken.Value := Ch;
        end;
      else
        begin
          FCurrentToken.TokenType := ttOperator;
          FCurrentToken.Value := Ch;
        end;
    end;

    FCurrentToken.Position := FPosition - Length(FCurrentToken.Value);
    FCurrentToken.Line := FLine;
    Result := FCurrentToken;
    Exit;
  end;

  FCurrentToken.TokenType := ttEOF;
  FCurrentToken.Value := '';
  FCurrentToken.Position := FPosition;
  FCurrentToken.Line := FLine;
  Result := FCurrentToken;
end;

function TSQLLexer.GetSourcePos: Integer;
begin
  Result := FCurrentToken.Position;
end;

function TSQLLexer.GetTokenString: string;
begin
  Result := FCurrentToken.Value;
end;

function TSQLLexer.GetLinePos: Integer;
begin
  Result := FCurrentToken.Line;
end;

function TSQLLexer.GetToken: TCharType;
var
  Ch: Char;
begin
  if IsEOF then
    Exit(ctOther);

  Ch := FCurrentToken.Value[1];

  case Ch of
    'A'..'Z', 'a'..'z', '_':
      Result := ctLetterStart;
    '0'..'9':
      Result := ctNumber;
    '#':
      Result := ctHash;
    '''', '"':
      Result := ctQuote;
    '$':
      Result := ctDollar;
    '-':
      if (Length(FCurrentToken.Value) > 1) and (FCurrentToken.Value[2] = '-') then
        Result := ctComment
      else
        Result := ctDash;
    '/':
      if (Length(FCurrentToken.Value) > 1) and (FCurrentToken.Value[2] = '*') then
        Result := ctComment
      else
        Result := ctOther;
    '.', ',':
      Result := ctOther;
    else
      Result := ctOther;
  end;
end;

end.

