unit Data.DB.Parser;

interface

uses
    System.Generics.Collections
  ;

type
  TTokenInfo = class
  strict private
    FTokenSQL : Integer;
  private
    function GetTokenSQL: Integer;
    procedure SetTokenSQL(const Value: Integer);
  public
    TokenType: Char;
    Token: string;
    property TokenSQL: Integer read GetTokenSQL write SetTokenSQL;
  end;

  TTokenTypes = record
  const
    tkSELECT = 0;
    tkFROM = 1;
    tkWHERE = 2;
    tkJoin = 8;
    tkInto = 9;
    tkOrder = 10;
    tkAsterisk = 14;
    tkEquals = 15;
    tkEndStatement = 16;
    tkLeftBracket = 17;
    tkRightBracket = 18;
    tkDotSeperator = 19;
    tkAS = 20;
    tkOn = 21;
    tkTableName = 22;
    tkFieldName = 24;
    tkDatabaseName = 66;
    tkViewName = 67;
    tkUnknownToken = -199;
  end;

  TTokenBucket = class(TObjectList<TTokenInfo>)
  public
    procedure Join(istart: Integer);
  end;

  TSQLParser = class
  public
    FTokens: TTokenBucket;
    procedure ProcessSQL(SQL: string);
    function DoesStatementModifyDB: Boolean;
    function IsDDL: Boolean;
    function DoesDoubleConstantExpressionExist: Boolean;
    function StatementCount: Integer;
    constructor Create;
    destructor Destroy; override;
  end;



function TokenStringToTokenSQL(info: TTokenInfo): Integer;
function TokenIdToTokenString(inTokenId:Integer): string;


implementation

uses
    System.SysUtils
  , System.Classes
  , System.StrUtils
  ;

function TokenIdToTokenString(inTokenId:Integer): string;
begin
  case inTokenId of
    0 : Result := 'tkSELECT';
    1 : Result := 'tkFROM';
    2 : Result := 'tkWHERE';
    8 : Result := 'tkJoin';
    9 : Result := 'tkInto';
   10 : Result := 'tkOrder';
   14 : Result := 'tkAsterisk';
   15 : Result := 'tkEquals';
   16 : Result := 'tkEndStatement';
   17 : Result := 'tkLeftBracket';
   18 : Result := 'tkRightBracket';
   19 : Result := 'tkDotSeperator';
   20 : Result := 'tkAS';
   21 : Result := 'tkOn';
   22 : Result := 'tkTableName';
   24 : Result := 'tkFieldName';
   25 : Result := 'tkFieldRefName';
   31 : Result := 'tkUpdate';
   32 : Result := 'tkSet';
   36 : Result := 'tkBetween';
   37 : Result := 'tkCase';
   38 : Result := 'tkDelete';
   39 : Result := 'tkHaving';
   40 : Result := 'tkLIKE';
   41 : Result := 'tkLimit';
   42 : Result := 'tkDISTINCT';
   44 : Result := 'tkTop';
   48 : Result := 'tkAdd';
   52 : Result := 'tkValues';
   53 : Result := 'tkAsc';
   54 : Result := 'tkDesc';
   56 : Result := 'tkEnd';
   57 : Result := 'tkGrant';
   58 : Result := 'tkTo';
   66 : Result := 'tkDatabaseName';
   67 : Result := 'tkViewName';
   70 : Result := 'tkUsername';
   71 : Result := 'tkConstantNumber';
   72 : Result := 'tkConstantString';
   73 : Result := 'tkUse';
   76 : Result := 'tkGroupBy';
   77 : Result := 'tkSchemaName';
   80 : Result := 'tkInnerJoin';
   81 : Result := 'tkLeftJoin';
   82 : Result := 'tkRightJoin';
   84 : Result := 'tkOrderBy';
   85 : Result := 'tkCreateDatabase';
   86 : Result := 'tkSum';
   87 : Result := 'tkCount';
   88 : Result := 'tkAvg';
   89 : Result := 'tkExists';
   90 : Result := 'tkOr';
   91 : Result := 'tkAnd';
   93 : Result := 'tkComma';
   94 : Result := 'tkLessThan';
   95 : Result := 'tkGreaterThan';
   96 : Result := 'tkDivide';
   98 : Result := 'tkUNION';
   99 : Result := 'tkNULL';
   106: Result := 'tkVarchar';
   107: Result := 'tkInt';
   108: Result := 'tkModify';
   109: Result := 'tkCreateView';
   112: Result := 'tkDropDatabase';
   113: Result := 'tkDropView';
   114: Result := 'tkDropTable';
   116: Result := 'tkTruncateTable';
   119: Result := 'tkAlterTable';
   121: Result := 'tkCreateIndex';
   123: Result := 'tkDeleteFrom';
   124: Result := 'tkNotEqual';
   125: Result := 'tkBackupDatabase';
   126: Result := 'tkToDisk';
   127: Result := 'tkInsertInto';
   128: Result := 'tkDropUser';
   129: Result := 'tkDropConstraint';
   130: Result := 'tkIndexName';
   131: Result := 'tkConstraintName';
   132: Result := 'tkLockTables';
   133: Result := 'tkUnlockTables';
   137: Result := 'tkDouble';
   138: Result := 'tkDefault';
   140: Result := 'tkMax';
   141: Result := 'tkNotNull';
   142: Result := 'tkZeroFill';
   143: Result := 'tkUnsigned';
   144: Result := 'tkMin';
   149: Result := 'tkPlus';
   152: Result := 'tkLessThanOrEqual';
   153: Result := 'tkGreaterThanOrEqual';
   155: Result := 'tkCast';
   156: Result := 'tkFloat';
   160: Result := 'tkComment';
   161: Result := 'tkConcat';
   162: Result := 'tkSubstr';
  end;

end;


function TokenStringToTokenSQL(info: TTokenInfo): Integer;
var
  token : string;
  intValue : Int64;
  floatValue : double;
begin
  token := info.Token.ToUpper;
  if Token = 'SELECT' then
    Result := TTokenTypes.tkSELECT
  else if (TryStrToInt64(token, intValue) = True) and not (Token = '.') then
    Result := 71 // number value
  else if (TryStrToFloat(token, floatValue) = True) and not (Token = '.') then
    Result := 71 // number value
  else if info.TokenType = System.Classes.toString then
    Result := 72 // string value
  else if Token = 'FROM' then
    Result := TTokenTypes.tkFROM
  else if Token = 'WHERE' then
    Result := 2
  else if Token = 'LEFT' then
    Result := 3
  else if Token = 'RIGHT' then
    Result := 4
  else if Token = 'INNER' then
    Result := 5
  else if Token = 'FULL' then
    Result := 6
  else if Token = 'OUTER' then
    Result := 7
  else if Token = 'JOIN' then
    Result := 8
  else if Token = 'INTO' then
    Result := 9
  else if Token = 'ORDER' then
    Result := 10
  else if Token = 'GROUP' then
    Result := 11
  else if Token = 'BY' then
    Result := 12
  else if Token = 'IN' then
    Result := 13
  else if Token = '*' then
    Result := 14
  else if Token = '=' then
    Result := TTokenTypes.tkEquals
  else if Token = ';' then
    Result := 16
  else if Token = '(' then
    Result := TTokenTypes.tkLeftBracket
  else if Token = ')' then
    Result := TTokenTypes.tkRightBracket
  else if Token = '.' then
    Result := TTokenTypes.tkDotSeperator
  else if Token = 'AS' then
    Result := 20
  else if Token = 'ON' then
    Result := TTokenTypes.tkOn
  // Result := 22 // table
  // Result := 23 // tableref
  // Result := 24 // fieldname
  // Result := 25 // fieldnameref
  // Result := 26 // paramref
  // Result := 27 // Integer
  // Result := 28 // String
  else if Token = 'INSERT' then
    Result := 29
  else if Token = 'UPDATE' then
    Result := 31
  else if Token = 'SET' then
    Result := 32
  else if Token = 'ALTER' then
    Result := 33
  else if Token = 'CREATE' then
    Result := 34
  else if Token = 'TABLE' then
    Result := 35
  else if Token = 'BETWEEN' then
    Result := 36
  else if Token = 'CASE' then
    Result := 37
  else if Token = 'DELETE' then
    Result := 38
  else if Token = 'HAVING' then
    Result := 39
  else if Token = 'LIKE' then
    Result := 40
  else if Token = 'LIMIT' then
    Result := 41
  else if Token = 'DISTINCT' then
    Result := 42
  else if Token = 'WITH' then
    Result := 43
  else if Token = 'TOP' then
    Result := 44
  else if Token = 'DROP' then
    Result := 45
  else if Token = 'TRUNCATE' then
    Result := 46
  else if Token = 'COLUMN' then
    Result := 47
  else if Token = 'ADD' then
    Result := 48
  else if Token = 'UNIQUE' then
    Result := 49
  else if Token = 'CONSTRAINT' then
    Result := 50
  else if Token = 'INDEX' then
    Result := 51
  else if Token = 'VALUES' then
    Result := 52
  else if Token = 'ASC' then
    Result := 53
  else if Token = 'DESC' then
    Result := 54
  else if Token = 'ELSE' then
    Result := 55
  else if Token = 'END' then
    Result := 56
  else if Token = 'GRANT' then
    Result := 57
  else if Token = 'TO' then
    Result := 58
  else if Token = 'REVOKE' then
    Result := 59
  else if Token = 'VIEW' then
    Result := 60
  else if Token = 'REPLACE' then
    Result := 61
  else if Token = 'TRIGGER' then
    Result := 62
  else if Token = 'COMMIT' then
    Result := 63
  else if Token = 'ROLLBACK' then
    Result := 64
  else if Token = 'DATABASE' then
    Result := 65
  // Result := 66 database name
  // Result := tkViewName view name
  // Result := 68 some db object - table, view, etc
  // Result := 69 some db operation INSERT, DELETE, UPDATE, SELECT
  // Result := 70 user
  else if Token = 'USE' then
    Result := 73
  // Result := 74 constraint
  else if Token = 'USER' then
    Result := 75
  // Result := 76 GROUP BY
  // Result := 77 schema name
  // Result := 78 DROP TABLE
  // Result := 79 RIGHT OUTER JOIN
  // Result := 80 INNER JOIN
  // Result := 81 LEFT JOIN
  // Result := 82 RIGHT JOIN
  // Result := 83 OUTER JOIN
  // Result := 84 ORDER BY
  // Result := 85 CREATE DATABASE
  else if Token = 'SUM' then
    Result := 86 // SUM
  else if Token = 'COUNT' then
    Result := 87 // COUNT
  else if Token = 'AVG' then
    Result := 88 // AVG
  else if Token = 'EXISTS' then
    Result := 89 // EXISTS
  else if Token = 'OR' then
    Result := 90 // OR
  else if Token = 'AND' then
    Result := 91 // AND
  else if Token = 'NOT' then
    Result := 92 // OR
  else if Token = ',' then
    Result := 93 // ,
  else if Token = '<' then
    Result := 94 // <
  else if Token = '>' then
    Result := 95 // >
  else if Token = '/' then
    Result := 96 // /
  else if Token = '*' then
    Result := 97 // *
  else if Token = 'UNION' then
    Result := 98 // *
  else if Token = 'NULL' then
    Result := 99 // *
  else if Token = 'WHEN' then
    Result := 100 // *
  else if Token = 'IS' then
    Result := 101 // *
  else if Token = 'ELSE' then
    Result := 102 // *
  else if Token = 'THEN' then
    Result := 103 // *
  else if Token = 'BACKUP' then
    Result := 104 // *
  else if Token = 'DISK' then
    Result := 105 // *
  else if Token = 'VARCHAR' then
    Result := 106 // *
  else if Token = 'INT' then
    Result := 107 // *
  else if Token = 'MODIFY' then
    Result := 108 // *
  // Result := 109 CREATE VIEW
  // Result := 110 CREATE USER
  // Result := 111 CREATE TABLE
  // Result := 112 DROP DATABASE
  // Result := 113 DROP VIEW
  // Result := 114 DROP TABLE
  // Result := 115 DROP USER
  // Result := 116 TRUNCATE TABLE
  else if Token = 'IF' then
    Result := 117 // IF
  // Result := 118 IF EXISTS
  // Result := 119 ALTER TABLE
  else if Token = 'INDEX' then
    Result := 120 // INDEX
  // Result := 121 CREATE INDEX
  // Result := 122 DROP INDEX
  // Result := 123 DELETE FROM
  // Result := 124 <>
  // Result := 125 BACKUP DATABASE
  // Result := 126 TO DISK
  // Result := 127 INSERT INTO
  // Result := 128 DROP USER
  // Result := 129 DROP CONSTRAINT
  // Result := 130 indexname
  // Result := 131 constraintname
  // Result := 132 LOCK TABLES
  // Result := 133 UNLOCK TABLES
  else if Token = 'LOCK' then
    Result := 134 // LOCK
  else if Token = 'UNLOCK' then
    Result := 135 // UNLOCK
  else if Token = 'TABLES' then
    Result := 136 // TABLES
  else if Token = 'DOUBLE' then
    Result := 137 // DOUBLE
  else if Token = 'DEFAULT' then
    Result := 138 // DEFAULT
  else if Token = 'TEMPORARY' then
    Result := 139 // TEMPORARY
  else if Token = 'MAX' then
    Result := 140 // MAX
  // Result := 141 NOT NULL
  else if Token = 'ZEROFILL' then
    Result := 142 // MAX
  else if Token = 'UNSIGNED' then
    Result := 143 // UNSIGNED
  else if Token = 'MIN' then
    Result := 144 // MIN
  // Result := 145 indexname
  else if Token = 'READ' then
    Result := 146 // MIN
  //  Result := 147 // functionname
  else if Token = '-' then
    Result := 148 // MIN
  else if Token = '+' then
    Result := 149 // MIN
  else if Token = '[' then
    Result := 150 // [
  else if Token = ']' then
    Result := 151 // ]
  // Result := 152 // <=
  // Result := 153 // >=
 // else if Token = 'ALTER COLUMN' then
 //   Result := 154 // ]
  else if Token = 'CAST' then
    Result := 155
  else if Token = 'FLOAT' then
    Result := 156
  else if Token = 'CURRENT_DATE' then
    Result := 157
  else if Token = 'CURRENT_TIME' then
    Result := 158
  else if Token = 'IIF' then
    Result := 159
  else if Token = '--' then
    Result := 160
  else if Token = 'CONCAT' then
    Result := 161
  else if Token = 'SUBSTR' then
    Result := 162
  else
    Result := TTokenTypes.tkUnknownToken; // unknown token
end;

{ TTokenBucket }

procedure TTokenBucket.Join(istart: Integer);
begin
  Self[istart].token := Self[istart].token + ' ' + Self[istart + 1].token;
  Self.Delete(istart + 1);
end;

{ TTokenInfo }

function TTokenInfo.GetTokenSQL: Integer;
begin
  Result := FTokenSQL;
end;

procedure TTokenInfo.SetTokenSQL(const Value: Integer);
begin
  if (Token = '=') and (value <> TTokenTypes.tkEquals) then
  begin

  end
  else
  begin
    FTokenSQL := Value;
  end;
    //raise Exception.Create('Token <> TokenSQL');
end;

{ TSQLParser }

constructor TSQLParser.Create;
begin
  FTokens := TTokenBucket.Create;
end;

destructor TSQLParser.Destroy;
begin
  FreeAndNil(FTokens);
end;

function TSQLParser.StatementCount: Integer;
var
 count : Integer;
 i : Integer;
begin
  Result := 0;
  count := 0;
  for I := 0 to FTokens.Count - 1 do
  begin
    if FTokens[i].TokenSQL = 16 then
      Inc(count);
  end;
  Result := count;
end;

function TSQLParser.DoesDoubleConstantExpressionExist: Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to FTokens.Count - 1 do
  begin
    if i - 2 >= 0 then
    begin
      if (FTokens[i - 2].TokenSQL in [71,72]) and (FTokens[i - 1].TokenSQL in [TTokenTypes.tkEquals, 94, 95, 124, 152, 153]) and (FTokens[i].TokenSQL in [71,72]) then
      begin
        Result := True;
      end;
    end;
  end;
end;

function TSQLParser.DoesStatementModifyDB: Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to FTokens.Count - 1 do
  begin
    if FTokens[i].TokenSQL in [8,TTokenTypes.tkInto,29,31,32,33,34,38,45,48, 51,57,60, 85, 109,110,111,112,113,114,115,116, 119, 121, 122, 123,127,128,129,154] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TSQLParser.IsDDL: Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to FTokens.Count - 1 do
  begin
    if FTokens[i].TokenSQL in [TTokenTypes.tkInto, 34, 57, 85, 109, 111, 112, 113, 114, 119, 121, 122, 128, 129, 154] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TSQLParser.ProcessSQL(SQL: string);
var
  parser: TParser;
  strStrm: TStringStream;
  token: string;
  info: TTokenInfo;
  i: Integer;
begin
  strStrm := TStringStream.Create;
  try
    FTokens.Clear;
    strStrm.WriteString(SQL);
    strStrm.Position := 0;
    parser := TParser.Create(strStrm);
    repeat
      begin
        token := parser.TokenString;
  {
      case parser.Token of
        toSymbol:
          Memo2.Lines.Add('Symbol');
        toInteger:
          Memo2.Lines.Add('Integer');
        toFloat:
          Memo2.Lines.Add('Float');
        System.Classes.toString:
          Memo2.Lines.Add('String');
        toWString:
          Memo2.Lines.Add('WString');
      else
          Memo2.Lines.Add('Something Else');
      end;
  }
        info := TTokenInfo.Create;
        info.TokenType := parser.Token;
        info.Token := parser.TokenString;
        if (info.TokenType = #2) and (info.Token = #0) then
          info.Token := '#';
        info.TokenSQL := TokenStringToTokenSQL(info);
  //    Memo2.Lines.Add(token + ' ' + info.TokenSQL.ToString);
        FTokens.Add(info);
      end
    until (parser.NextToken = toEOF);

    i := 0;
    // Initial pass - join multi word commands together
    while i < FTokens.Count - 1 do
    begin
      if ((FTokens[i].TokenSQL = 4) and (FTokens[i + 1].TokenSQL = 7) and (FTokens[i + 2].TokenSQL = 8)) then
      begin
        FTokens.Join(i);
        FTokens.Join(i);  // RIGHT OUTER JOIN
        FTokens[i].TokenSQL := 79;
      end
      else if ((FTokens[i].TokenSQL = 5) and (FTokens[i + 1].TokenSQL = 8)) then
      begin
        FTokens.Join(i);  // INNER JOIN
        FTokens[i].TokenSQL := 80;
      end
      else if ((FTokens[i].TokenSQL = 3) and (FTokens[i + 1].TokenSQL = 8)) then
      begin
        FTokens.Join(i); // LEFT JOIN
        FTokens[i].TokenSQL := 81;
      end
      else if ((FTokens[i].TokenSQL = 4) and (FTokens[i + 1].TokenSQL = 8)) then
      begin
        FTokens.Join(i); // RIGHT JOIN
        FTokens[i].TokenSQL := 82;
      end
      else if ((FTokens[i].TokenSQL = 7) and (FTokens[i + 1].TokenSQL = 8)) then
      begin
        FTokens.Join(i); // OUTER JOIN
        FTokens[i].TokenSQL := 83;
      end
      else if ((FTokens[i].TokenSQL = 11) and (FTokens[i + 1].TokenSQL = 12)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 76;  // GROUP BY
      end
      else if ((FTokens[i].TokenSQL = 10) and (FTokens[i + 1].TokenSQL = 12)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 84;  // ORDER BY
      end
      else if ((FTokens[i].TokenSQL = 134) and (FTokens[i + 1].TokenSQL = 136)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 132;  // LOCK TABLES
      end
      else if ((FTokens[i].TokenSQL = 135) and (FTokens[i + 1].TokenSQL = 136)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 133;  // UNLOCK TABLES
      end
      else if ((FTokens[i].TokenSQL = 34) and (FTokens[i + 1].TokenSQL = 65)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 85;  // CREATE DATABASE
      end
      else if ((FTokens[i].TokenSQL = 45) and (FTokens[i + 1].TokenSQL = 65)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 112;  // DROP DATABASE
      end
      else if ((FTokens[i].TokenSQL = 34) and (FTokens[i + 1].TokenSQL = 60)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 109;  // CREATE VIEW
      end
      else if ((FTokens[i].TokenSQL = 45) and (FTokens[i + 1].TokenSQL = 60)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 113;  // DROP VIEW
      end
      else if ((FTokens[i].TokenSQL = 34) and (FTokens[i + 1].TokenSQL = 35)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 111;  // CREATE TABLE
      end
      else if ((FTokens[i].TokenSQL = 45) and (FTokens[i + 1].TokenSQL = 35)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 114;  // DROP TABLE
      end
      else if ((FTokens[i].TokenSQL = 29) and (FTokens[i + 1].TokenSQL = 9)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 127;  // INSERT INTO
      end
      else if ((FTokens[i].TokenSQL = 34) and (FTokens[i + 1].TokenSQL = 51)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 121;  // CREATE INDEX
      end
      else if ((FTokens[i].TokenSQL = 45) and (FTokens[i + 1].TokenSQL = 75)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 128;  // DROP USER
      end
      else if ((FTokens[i].TokenSQL = 46) and (FTokens[i + 1].TokenSQL = 35)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 116;  // TRUNCATE TABLE
      end
      else if ((FTokens[i].TokenSQL = 33) and (FTokens[i + 1].TokenSQL = 35)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 119;  // ALTER TABLE
      end
      else if ((FTokens[i].TokenSQL = 34) and (FTokens[i + 1].TokenSQL = 75)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 110;  // CREATE USER
      end
      else if ((FTokens[i].TokenSQL = 34) and (FTokens[i + 1].TokenSQL = 120)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 121;  // CREATE INDEX
      end
      else if ((FTokens[i].TokenSQL = 45) and (FTokens[i + 1].TokenSQL = 51)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 122;  // DROP INDEX
      end
      else if ((FTokens[i].TokenSQL = 45) and (FTokens[i + 1].TokenSQL = 50)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 129;  // DROP CONSTRAINT
      end

      else if ((FTokens[i].TokenSQL = 38) and (FTokens[i + 1].TokenSQL = 1)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 123;  // DELETE FROM
      end
      else if ((FTokens[i].TokenSQL = 94) and (FTokens[i + 1].TokenSQL = 95)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 124;  // <>
      end
      else if ((FTokens[i].TokenSQL = 94) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkEquals)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 152;  // <=
      end
      else if ((FTokens[i].TokenSQL = 95) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkEquals)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 153;  // >=
      end
      else if ((FTokens[i].TokenSQL = 33) and (FTokens[i + 1].TokenSQL = 47)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 154;  // ALTER COLUMN
      end
      else if ((FTokens[i].TokenSQL = 104) and (FTokens[i + 1].TokenSQL = 65)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 125;  // BACKUP DATABASE
      end
      else if ((FTokens[i].TokenSQL = 58) and (FTokens[i + 1].TokenSQL = 105)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 126;  // TO DISK
      end
      else if ((FTokens[i].TokenSQL = 92) and (FTokens[i + 1].TokenSQL = 99)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 141;  // NOT NULL
      end
      else if ((FTokens[i].TokenSQL = 117) and (FTokens[i + 1].TokenSQL = 89)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := 110;  // IF EXISTS
      end;
      Inc(i);
    end;

    for i := 1 to FTokens.Count - 1 do
    begin
      if FTokens[i - 1].TokenSQL = TTokenTypes.tkFROM  {'FROM'} then
        FTokens[i].TokenSQL := TTokenTypes.tkTableName;

      if FTokens[i - 1].TokenSQL = 123  {'DELETE FROM'} then
        FTokens[i].TokenSQL := TTokenTypes.tkTableName;


      if (i - 3 >= 0) and (FTokens[i - 3].TokenSQL = 1) and {'FROM3 schema2.1tablename0'}
         (FTokens[i - 1].TokenSQL = TTokenTypes.tkDotSeperator) then
      begin
        FTokens[i - 2].TokenSQL := 77;
        FTokens[i - 0].TokenSQL := TTokenTypes.tkTableName;
      end;

      if (i - 3 >= 0) and (FTokens[i - 3].TokenSQL = 122) and {'DROP INDEX3 table2.1indexname0'}
         (FTokens[i - 1].TokenSQL = TTokenTypes.tkDotSeperator) then
      begin
        FTokens[i - 2].TokenSQL := TTokenTypes.tkTableName;
        FTokens[i - 0].TokenSQL := 145;
      end;


      if (i - 2 >= 0) and (FTokens[i - 2].TokenSQL = TTokenTypes.tkInto) and {'INTO2 ????1 FROM0'}
         (FTokens[i].TokenSQL = 1) then
      begin
        FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
      end;



      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = 20) and (FTokens[i - 2].TokenSQL = TTokenTypes.tkTableName) {'AS'} then
          FTokens[i].TokenSQL := 23;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = 86) and (FTokens[i - 2].TokenSQL = TTokenTypes.tkLeftBracket) {'SUM('} then
          FTokens[i].TokenSQL := 86;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = 87) and (FTokens[i - 2].TokenSQL = TTokenTypes.tkLeftBracket) {'COUNT('} then
          FTokens[i].TokenSQL := 87;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = 88) and (FTokens[i - 2].TokenSQL = TTokenTypes.tkLeftBracket) {'AVG('} then
          FTokens[i].TokenSQL := 88;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = 89) and (FTokens[i - 2].TokenSQL = TTokenTypes.tkLeftBracket) {'EXISTS('} then
          FTokens[i].TokenSQL := 89;
      end;


      if (i - 2 >= 0) and (i < FTokens.Count - 2) then // SELECT
      begin
        if (FTokens[i - 1].token = 'SELECT') and
           (FTokens[i + 1].token = '.')
            then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;


      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 31) and (FTokens[i].tokenSQL = 32) then   // UPDATE ??? SET
          FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 2 >= 0 then
      begin   // BACKUPDATABASE2 ????1 TODISK0
        if (FTokens[i - 2].tokenSQL = 125) and (FTokens[i].tokenSQL = 126) then
          FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
      end;


      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].token = 'JOIN') or (FTokens[i - 1].token = 'INNER JOIN') or (FTokens[i - 1].token = 'LEFT JOIN') or (FTokens[i - 1].token = 'RIGHT JOIN') or (FTokens[i - 1].token = 'OUTER JOIN') or (FTokens[i - 1].token = 'FULL OUTER JOIN') then
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
        if FTokens[i].Token = '[' then
          FTokens[i].TokenSQL := 150;
      end;

      if i - 2 >= 0 then  // ON
      begin
        if (FTokens[i - 1].token = 'ON') and (FTokens[i + 1].token = '.') and//          (FTokens[i + 2].Token = fieldname)
          (FTokens[i + 3].token = '=') and//          (FTokens[i + 4].Token = tablename) and
          (FTokens[i + 5].token = '.') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
          FTokens[i + 4].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 6].TokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if i - 2 >= 0 then  // (
      begin
        if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = '.') and//          (FTokens[i + 2].Token = fieldname)
          (FTokens[i + 3].token = '=') and//          (FTokens[i + 4].Token = tablename) and
          (FTokens[i + 5].token = '.') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
          FTokens[i + 4].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 6].TokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if i - 2 >= 0 then // LOCK TABLES2 ????1 READ0
      begin
        if (FTokens[i - 2].TokenSQL = 132) and (FTokens[i].TokenSQL = 146) then
        begin
          FTokens[i-1].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;


      if i - 2 >= 0 then  // WHERE fieldname = ???
      begin
        if ((FTokens[i - 1].token = 'SET') or (FTokens[i - 1].token = 'WHERE')) and
          (FTokens[i + 1].token = '=')
        then
        begin
          if FTokens[i].TokenType = toSymbol then
            FTokens[i].TokenSQL := TTokenTypes.tkFieldName
          else if FTokens[i].TokenType = toInteger then
            FTokens[i].TokenSQL := 71;

          if FTokens[i + 2].TokenType = toSymbol then
            FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName
          else if FTokens[i + 2].TokenType = toInteger then
            FTokens[i + 2].TokenSQL := 71
          else if FTokens[i + 2].TokenType = toFloat then
            FTokens[i + 2].TokenSQL := 71
          else if FTokens[i + 2].TokenType = System.Classes.toString then
            FTokens[i + 2].TokenSQL := 72;

        end
      end;

      if i - 2 >= 0 then  // (
      begin
        if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = '.') and//          (FTokens[i + 2].Token = fieldname)
          ((FTokens[i + 3].token = '=') or (FTokens[i + 3].token = '<') or (FTokens[i + 3].token = '>')) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if i - 2 >= 0 then  // tablename MODIFY ?????
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkTableName) and (FTokens[i - 1].tokenSQL = 108) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if i - 4 >= 0 then  // =
      begin
        if (FTokens[i - 3].token = '=') and (FTokens[i - 1].token = '.') then
        begin
          FTokens[i -2 ].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 0].TokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if i - 1 >= 0 then  // USE dbname
      begin
        if (FTokens[i - 1].tokenSQL = 73) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkDatabaseName;
        end
      end;

      if i - 5 >= 0 then  // =
      begin
        if ((FTokens[i - 3].token = '<') or (FTokens[i - 3].token = '=') or (FTokens[i - 3].token = '>')) and (FTokens[i - 1].token = '.') then
        begin
          FTokens[i-2].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 0].TokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if i - 5 >= 0 then
      begin
        if (FTokens[i - 5].tokenSQL = 0 ) and (FTokens[i - 3].tokenSQL = TTokenTypes.tkDotSeperator)
           and (FTokens[i - 1].tokenSQL = 20) then
        begin  // SELECT5 ????4.3?????2 AS1 ?????0
          FTokens[i - 4].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i - 2].tokenSQL := TTokenTypes.tkFieldName;
          FTokens[i].tokenSQL := 25;
        end
      end;

      if i - 2 >= 0 then  // =
      begin
        if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = '.') and (FTokens[i + 3].token = ')') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if (FTokens[i - 1].tokenSQL = 0 ) and (FTokens[i + 1].tokenSQL = TTokenTypes.tkDotSeperator) then
      begin  // SELECT ????.????? ,
        FTokens[i].tokenSQL := TTokenTypes.tkTableName;
        FTokens[i + 2].tokenSQL := TTokenTypes.tkFieldName;
      end
      else if (FTokens[i - 1].tokenSQL = 0 ) and (FTokens[i + 1].tokenSQL = 93) then
      begin // SELECT ???? ,
        FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
      end
      else if (FTokens[i - 1].tokenSQL = 0 ) and (FTokens[i + 1].tokenSQL = 1) then
      begin // SELECT ???? FROM
        if FTokens[i].Token = '*' then
          FTokens[i].tokenSQL := TTokenTypes.tkAsterisk
        else
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkFieldName ) and (FTokens[i - 1].tokenSQL = 93) and (FTokens[i].tokenSQL = TTokenTypes.tkUnknownToken) then
        begin // field2 ,1 ????0
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkFieldName ) and (FTokens[i - 2].tokenSQL = 93) and (FTokens[i].tokenSQL = TTokenTypes.tkFROM) then
        begin // field3 ,2 ????1 FROM0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 2 ) and (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin  // WHERE ????.?????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if (FTokens[i - 2].tokenSQL = 2 ) then
        begin // WHERE ????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if ( (FTokens[i - 2].tokenSQL = 42 ) or (FTokens[i - 2].tokenSQL = 91 ) or (FTokens[i - 2].tokenSQL = 84 ) ) and (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin  // ORDER BY ????.?????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if ( (FTokens[i - 2].tokenSQL = 42 ) or (FTokens[i - 2].tokenSQL = 91 ) or (FTokens[i - 2].tokenSQL = 84 ) ) then
        begin // ORDER BY ????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 76 ) and (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin  // GROUP BY ????.?????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if (FTokens[i - 2].tokenSQL = 76 ) then
        begin // GROUP BY ????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = 84 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin  // ORDER BY3 ????2.1?????0
          FTokens[i - 2].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
        end
      end;
      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = 84 ) then
        begin // ORDER BY ????
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 32 ) and (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin  // SET ????.?????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if (FTokens[i - 2].tokenSQL = 32 ) then
        begin // SET ????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then  // =
      begin
        if (FTokens[i - 1].token = ',') and (FTokens[i + 1].token = '.') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if i - 2 >= 0 then  // =
      begin
        if (FTokens[i - 2].tokenSQL = 121) and (FTokens[i].tokenSQL = TTokenTypes.tkOn) then
        begin
          FTokens[i - 1].TokenSQL := 130;
        end;
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkOn) and (FTokens[i].tokenSQL = TTokenTypes.tkLeftBracket) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
        end;
        if (FTokens[i - 2].tokenSQL = 127) and (FTokens[i].tokenSQL = 0) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
        end;
      end;



      if i - 1 >= 0 then  // =
      begin
        if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = ')') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if i - 3 >= 0 then  // DROP INDEX3 ????2.1????0
      begin
        if (FTokens[i - 3].TokenSQL = 122) and (FTokens[i - 1].TokenSQL = TTokenTypes.tkDotSeperator) then
        begin
          FTokens[i - 2].TokenSQL := TTokenTypes.tkFieldName;
          FTokens[i - 0].TokenSQL := 130;
        end
      end;

      if i - 1 >= 0 then  // DROP CONSTRAINT ????
      begin
        if (FTokens[i - 1].TokenSQL = 129) then
        begin
          FTokens[i].TokenSQL := 131;
        end
      end;
      if i - 1 >= 0 then  // INSERT INTO ????
      begin
        if (FTokens[i - 1].TokenSQL = 127) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
        end
      end;

      if i - 1 >= 0 then  // parameter
      begin
        if (FTokens[i - 1].token = ':') then
        begin
          FTokens[i].TokenSQL := 26;
        end
      end;

      if i - 5 >= 0 then  // GRANT5 operation4 ON3 object2 TO1 user0
      begin
        if (FTokens[i - 5].tokenSQL = 57) and
           (FTokens[i - 3].tokenSQL = TTokenTypes.tkOn) and
           (FTokens[i - 1].tokenSQL = 58) and
           (MatchStr(FTokens[i - 4].token, ['SELECT', 'INSERT', 'DELETE']) = TRUE)
        then
        begin
          FTokens[i - 4].TokenSQL := 69;
          FTokens[i - 2].TokenSQL := 68;
          FTokens[i].TokenSQL := 70;
        end
      end;

     if i - 3 >= 0 then  // ON3 object2 TO1 user0
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkOn) and
           (FTokens[i - 1].tokenSQL = 58)
        then
        begin
          FTokens[i - 2].TokenSQL := 68;
          FTokens[i].TokenSQL := 70;
        end
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = 85) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkDatabaseName; // CREATE DATABASE
        end
        else if (FTokens[i - 1].tokenSQL = 112) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkDatabaseName; // DROP DATABASE
        end
        else if (FTokens[i - 1].tokenSQL = 109) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkViewName; // CREATE VIEW
        end
        else if (FTokens[i - 1].tokenSQL = 113) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkViewName; // DROP VIEW
        end
        else if (FTokens[i - 1].tokenSQL = 110) then
        begin
          FTokens[i].TokenSQL := 70; // CREATE USER
        end
        else if (FTokens[i - 1].tokenSQL = 110) then
        begin
          FTokens[i].TokenSQL := 115; // DROP USER
        end
        else if (FTokens[i - 1].tokenSQL = 111) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName; // CREATE TABLE
        end
        else if (FTokens[i - 1].tokenSQL = 114) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName; // DROP TABLE
        end
        else if (FTokens[i - 1].tokenSQL = 128) then
        begin
          FTokens[i].TokenSQL := 70; // DROP USER
        end
        else if (FTokens[i - 1].tokenSQL = 116) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName; // TRUNCATE TABLE
        end
        else if (FTokens[i - 1].tokenSQL = 119) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName; // ALTER TABLE
        end
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 93 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkFROM) then
        begin //  ,2 ????1 FROM0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkEquals ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 16) then
        begin //  ,2 ????1 FROM0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 0 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 20) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 20 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkFROM) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 48 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 106) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 20 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 93) then
        begin //  AS 2 ????1 ,0
          FTokens[i - 1].tokenSQL := 25;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 93 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 2) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkLeftBracket ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 107) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 93 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 106) then
        begin //  , 2 ????1 VARCHAR0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 93 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 107) then
        begin //  , 2 ????1 VARCHAR0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 154 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 106) then
        begin //  COLUMN 2 ????1 VARCHAR0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 93 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 137) then
        begin //  COLUMN 2 ????1 VARCHAR0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkFieldName ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkEquals) then
        begin //  NOT 2 ????1 =0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkLeftBracket ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 93) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 93 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkEquals) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 35 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkLeftBracket) then
        begin //  TABLE 2 ????1 (0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 100 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 101) then
        begin //  TABLE 2 ????1 (0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkUnknownToken ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkLeftBracket) and (FTokens[i-0].tokenSQL = TTokenTypes.tkRightBracket) then
        begin //  TABLE 2 ????1 (0
          FTokens[i - 2].tokenSQL := 147;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 55 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 56) then
        begin //  TABLE 2 ????1 (0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 20 ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkOn) then
        begin //  AS 2 ????1 ON0
          FTokens[i - 1].tokenSQL := 23;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkLeftBracket ) and (FTokens[i - 2].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-1].tokenSQL = TTokenTypes.tkDotSeperator) and (FTokens[i].tokenSQL = TTokenTypes.tkUnknownToken) then
        begin //  AS 2 ????1 ON0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkRightBracket ) and (FTokens[i - 2].tokenSQL = 20) and (FTokens[i-1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkLeftBracket) then
        begin //  AS 2 ????1 ON0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkRightBracket ) and (FTokens[i - 2].tokenSQL = 20) and (FTokens[i-1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkLeftBracket) then
        begin //  AS 2 ????1 ON0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;


//      if i - 3 >= 0 then
//      begin
//        if (FTokens[i - 3].tokenSQL <> TTokenTypes.tkDotSeperator ) and (FTokens[i - 2].Token = '[') and (FTokens[i-1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkLeftBracket) then
//        begin //  AS 2 ????1 ON0
//          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
//        end;
//      end;


      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 45) and (FTokens[i - 1].tokenSQL = 50) then
        begin
          FTokens[i].TokenSQL := 74; // DROP CONSTRAINT
        end
      end;
  end;
  finally
    FreeAndNil(parser);
  end;
end;

end.
