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

  TTokenBucket = class(TObjectList<TTokenInfo>)
  public
    procedure Join(istart: Integer);
  end;

  TSQLParser = class
  public
    FTokens: TTokenBucket;
    procedure ProcessSQL(SQL: string);
    constructor Create;
    destructor Destroy; override;
  end;



function TokenStringToTokenSQL(info: TTokenInfo): Integer;


implementation

uses
    System.SysUtils
  , System.Classes
  , System.StrUtils
  ;

function TokenStringToTokenSQL(info: TTokenInfo): Integer;
var
  token : string;
  intValue : Int64;
  floatValue : double;
begin
  token := info.Token.ToUpper;
  if Token = 'SELECT' then
    Result := 0
  else if (TryStrToInt64(token, intValue) = True) and not (Token = '.') then
    Result := 71 // number value
  else if (TryStrToFloat(token, floatValue) = True) and not (Token = '.') then
    Result := 71 // number value
  else if info.TokenType = System.Classes.toString then
    Result := 72 // string value
  else if Token = 'FROM' then
    Result := 1
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
    Result := 15
  else if Token = ';' then
    Result := 16
  else if Token = '(' then
    Result := 17
  else if Token = ')' then
    Result := 18
  else if Token = '.' then
    Result := 19
  else if Token = 'AS' then
    Result := 20
  else if Token = 'ON' then
    Result := 21
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
  // Result := 67 view name
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
    Result := 95 // <
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
  else
    Result := -199; // unknown token
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
  if (Token = '=') and (value <> 15) then
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
      if FTokens[i - 1].TokenSQL = 1  {'FROM'} then
        FTokens[i].TokenSQL := 22;

      if FTokens[i - 1].TokenSQL = 123  {'DELETE FROM'} then
        FTokens[i].TokenSQL := 22;


      if (i - 3 >= 0) and (FTokens[i - 3].TokenSQL = 1) and {'FROM3 schema2.1tablename0'}
         (FTokens[i - 1].TokenSQL = 19) then
      begin
        FTokens[i - 2].TokenSQL := 77;
        FTokens[i - 0].TokenSQL := 22;
      end;

      if (i - 3 >= 0) and (FTokens[i - 3].TokenSQL = 122) and {'DROP INDEX3 table2.1indexname0'}
         (FTokens[i - 1].TokenSQL = 19) then
      begin
        FTokens[i - 2].TokenSQL := 22;
        FTokens[i - 0].TokenSQL := 145;
      end;


      if (i - 2 >= 0) and (FTokens[i - 2].TokenSQL = 9) and {'INTO2 ????1 FROM0'}
         (FTokens[i].TokenSQL = 1) then
      begin
        FTokens[i - 1].TokenSQL := 22;
      end;



      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = 20) and (FTokens[i - 2].TokenSQL = 22) {'AS'} then
          FTokens[i].TokenSQL := 23;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = 86) and (FTokens[i - 2].TokenSQL = 17) {'SUM('} then
          FTokens[i].TokenSQL := 86;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = 87) and (FTokens[i - 2].TokenSQL = 17) {'COUNT('} then
          FTokens[i].TokenSQL := 87;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = 88) and (FTokens[i - 2].TokenSQL = 17) {'AVG('} then
          FTokens[i].TokenSQL := 88;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = 89) and (FTokens[i - 2].TokenSQL = 17) {'EXISTS('} then
          FTokens[i].TokenSQL := 89;
      end;


      if (i - 2 >= 0) and (i < FTokens.Count - 2) then // SELECT
      begin
        if (FTokens[i - 1].token = 'SELECT') and
           (FTokens[i + 1].token = '.')
            then
        begin
          FTokens[i].TokenSQL := 22;
          FTokens[i + 2].TokenSQL := 24;
        end;
      end;


      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 31) and (FTokens[i].tokenSQL = 32) then   // UPDATE ??? SET
          FTokens[i - 1].TokenSQL := 22;
      end;

      if i - 2 >= 0 then
      begin   // BACKUPDATABASE2 ????1 TODISK0
        if (FTokens[i - 2].tokenSQL = 125) and (FTokens[i].tokenSQL = 126) then
          FTokens[i - 1].TokenSQL := 22;
      end;


      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].token = 'JOIN') or (FTokens[i - 1].token = 'INNER JOIN') or (FTokens[i - 1].token = 'LEFT JOIN') or (FTokens[i - 1].token = 'RIGHT JOIN') or (FTokens[i - 1].token = 'OUTER JOIN') or (FTokens[i - 1].token = 'FULL OUTER JOIN') then
          FTokens[i].TokenSQL := 22;
      end;

      if i - 2 >= 0 then  // ON
      begin
        if (FTokens[i - 1].token = 'ON') and (FTokens[i + 1].token = '.') and//          (FTokens[i + 2].Token = fieldname)
          (FTokens[i + 3].token = '=') and//          (FTokens[i + 4].Token = tablename) and
          (FTokens[i + 5].token = '.') then
        begin
          FTokens[i].TokenSQL := 22;
          FTokens[i + 2].TokenSQL := 24;
          FTokens[i + 4].TokenSQL := 22;
          FTokens[i + 6].TokenSQL := 24;
        end
      end;

      if i - 2 >= 0 then  // (
      begin
        if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = '.') and//          (FTokens[i + 2].Token = fieldname)
          (FTokens[i + 3].token = '=') and//          (FTokens[i + 4].Token = tablename) and
          (FTokens[i + 5].token = '.') then
        begin
          FTokens[i].TokenSQL := 22;
          FTokens[i + 2].TokenSQL := 24;
          FTokens[i + 4].TokenSQL := 22;
          FTokens[i + 6].TokenSQL := 24;
        end
      end;

      if i - 2 >= 0 then // LOCK TABLES2 ????1 READ0
      begin
        if (FTokens[i - 2].TokenSQL = 132) and (FTokens[i].TokenSQL = 146) then
        begin
          FTokens[i-1].TokenSQL := 24;
        end;
      end;


      if i - 2 >= 0 then  // WHERE fieldname = ???
      begin
        if ((FTokens[i - 1].token = 'SET') or (FTokens[i - 1].token = 'WHERE')) and
          (FTokens[i + 1].token = '=')
        then
        begin
          if FTokens[i].TokenType = toSymbol then
            FTokens[i].TokenSQL := 24
          else if FTokens[i].TokenType = toInteger then
            FTokens[i].TokenSQL := 71;

          if FTokens[i + 2].TokenType = toSymbol then
            FTokens[i + 2].TokenSQL := 24
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
          FTokens[i].TokenSQL := 22;
          FTokens[i + 2].TokenSQL := 24;
        end
      end;

      if i - 2 >= 0 then  // tablename MODIFY ?????
      begin
        if (FTokens[i - 2].tokenSQL = 22) and (FTokens[i - 1].tokenSQL = 108) then
        begin
          FTokens[i].TokenSQL := 24;
        end
      end;

      if i - 2 >= 0 then  // =
      begin
        if (FTokens[i - 1].token = '=') and (FTokens[i + 1].token = '.') then
        begin
          FTokens[i].TokenSQL := 22;
          FTokens[i + 2].TokenSQL := 24;
        end
      end;

      if i - 1 >= 0 then  // USE dbname
      begin
        if (FTokens[i - 1].tokenSQL = 73) then
        begin
          FTokens[i].TokenSQL := 66;
        end
      end;

      if i - 2 >= 0 then  // =
      begin
        if ((FTokens[i - 1].token = '<') or (FTokens[i - 1].token = '=') or (FTokens[i - 1].token = '>')) and (FTokens[i + 1].token = '.') then
        begin
          FTokens[i].TokenSQL := 22;
          FTokens[i + 2].TokenSQL := 24;
        end
      end;

      if i - 5 >= 0 then
      begin
        if (FTokens[i - 5].tokenSQL = 0 ) and (FTokens[i - 3].tokenSQL = 19)
           and (FTokens[i - 1].tokenSQL = 20) then
        begin  // SELECT5 ????4.3?????2 AS1 ?????0
          FTokens[i - 4].tokenSQL := 22;
          FTokens[i - 2].tokenSQL := 24;
          FTokens[i].tokenSQL := 25;
        end
      end;

      if i - 2 >= 0 then  // =
      begin
        if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = '.') and (FTokens[i + 3].token = ')') then
        begin
          FTokens[i].TokenSQL := 22;
          FTokens[i + 2].TokenSQL := 24;
        end
      end;

      if (FTokens[i - 1].tokenSQL = 0 ) and (FTokens[i + 1].tokenSQL = 19) then
      begin  // SELECT ????.????? ,
        FTokens[i].tokenSQL := 22;
        FTokens[i + 2].tokenSQL := 24;
      end
      else if (FTokens[i - 1].tokenSQL = 0 ) and (FTokens[i + 1].tokenSQL = 93) then
      begin // SELECT ???? ,
        FTokens[i].tokenSQL := 24;
      end
      else if (FTokens[i - 1].tokenSQL = 0 ) and (FTokens[i + 1].tokenSQL = 1) then
      begin // SELECT ???? FROM
        FTokens[i].tokenSQL := 24;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 24 ) and (FTokens[i - 1].tokenSQL = 93) and (FTokens[i].tokenSQL = -199) then
        begin // field2 ,1 ????0
          FTokens[i].tokenSQL := 24;
        end
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = 24 ) and (FTokens[i - 2].tokenSQL = 93) and (FTokens[i].tokenSQL = 1) then
        begin // field3 ,2 ????1 FROM0
          FTokens[i - 1].tokenSQL := 24;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 2 ) and (FTokens[i].tokenSQL = 19) then
        begin  // WHERE ????.?????
          FTokens[i - 1 ].tokenSQL := 22;
          FTokens[i + 1].tokenSQL := 24;
        end
        else if (FTokens[i - 2].tokenSQL = 2 ) then
        begin // WHERE ????
          FTokens[i - 1].tokenSQL := 24;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 84 ) and (FTokens[i].tokenSQL = 19) then
        begin  // ORDER BY ????.?????
          FTokens[i - 1 ].tokenSQL := 22;
          FTokens[i + 1].tokenSQL := 24;
        end
        else if (FTokens[i - 2].tokenSQL = 84 ) then
        begin // ORDER BY ????
          FTokens[i - 1].tokenSQL := 24;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 76 ) and (FTokens[i].tokenSQL = 19) then
        begin  // GROUP BY ????.?????
          FTokens[i - 1 ].tokenSQL := 22;
          FTokens[i + 1].tokenSQL := 24;
        end
        else if (FTokens[i - 2].tokenSQL = 76 ) then
        begin // GROUP BY ????
          FTokens[i - 1].tokenSQL := 24;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = 32 ) and (FTokens[i].tokenSQL = 19) then
        begin  // SET ????.?????
          FTokens[i - 1 ].tokenSQL := 22;
          FTokens[i + 1].tokenSQL := 24;
        end
        else if (FTokens[i - 2].tokenSQL = 32 ) then
        begin // SET ????
          FTokens[i - 1].tokenSQL := 24;
        end;
      end;

      if i - 2 >= 0 then  // =
      begin
        if (FTokens[i - 1].token = ',') and (FTokens[i + 1].token = '.') then
        begin
          FTokens[i].TokenSQL := 22;
          FTokens[i + 2].TokenSQL := 24;
        end
      end;

      if i - 2 >= 0 then  // =
      begin
        if (FTokens[i - 2].tokenSQL = 121) and (FTokens[i].tokenSQL = 21) then
        begin
          FTokens[i - 1].TokenSQL := 130;
        end;
        if (FTokens[i - 2].tokenSQL = 21) and (FTokens[i].tokenSQL = 17) then
        begin
          FTokens[i - 1].TokenSQL := 22;
        end;
        if (FTokens[i - 2].tokenSQL = 127) and (FTokens[i].tokenSQL = 0) then
        begin
          FTokens[i - 1].TokenSQL := 22;
        end;
      end;



      if i - 1 >= 0 then  // =
      begin
        if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = ')') then
        begin
          FTokens[i].TokenSQL := 24;
        end
      end;

      if i - 3 >= 0 then  // DROP INDEX3 ????2.1????0
      begin
        if (FTokens[i - 3].TokenSQL = 122) and (FTokens[i - 1].TokenSQL = 19) then
        begin
          FTokens[i - 2].TokenSQL := 24;
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
          FTokens[i].TokenSQL := 22;
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
           (FTokens[i - 3].tokenSQL = 21) and
           (FTokens[i - 1].tokenSQL = 58) and
           (MatchStr(FTokens[i - 4].token, ['SELECT', 'INSERT', 'DELETE']) = TRUE)
        then
        begin
          FTokens[i - 4].TokenSQL := 69;
          FTokens[i - 2].TokenSQL := 68;
          FTokens[i].TokenSQL := 70;
        end
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = 85) then
        begin
          FTokens[i].TokenSQL := 66; // CREATE DATABASE
        end
        else if (FTokens[i - 1].tokenSQL = 112) then
        begin
          FTokens[i].TokenSQL := 66; // DROP DATABASE
        end
        else if (FTokens[i - 1].tokenSQL = 109) then
        begin
          FTokens[i].TokenSQL := 67; // CREATE VIEW
        end
        else if (FTokens[i - 1].tokenSQL = 113) then
        begin
          FTokens[i].TokenSQL := 67; // DROP VIEW
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
          FTokens[i].TokenSQL := 22; // CREATE TABLE
        end
        else if (FTokens[i - 1].tokenSQL = 114) then
        begin
          FTokens[i].TokenSQL := 22; // DROP TABLE
        end
        else if (FTokens[i - 1].tokenSQL = 128) then
        begin
          FTokens[i].TokenSQL := 70; // DROP USER
        end
        else if (FTokens[i - 1].tokenSQL = 116) then
        begin
          FTokens[i].TokenSQL := 22; // TRUNCATE TABLE
        end
        else if (FTokens[i - 1].tokenSQL = 119) then
        begin
          FTokens[i].TokenSQL := 22; // ALTER TABLE
        end
      end;


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
