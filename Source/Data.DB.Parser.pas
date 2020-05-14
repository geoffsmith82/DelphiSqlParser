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

function TokenStringToTokenSQL(info: TTokenInfo): Integer;


implementation

uses
    SysUtils
  ;

function TokenStringToTokenSQL(info: TTokenInfo): Integer;
var
  token : string;
begin
  token := info.Token.ToUpper;
  if Token = 'SELECT' then
    Result := 0
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
  else if Token = 'INTO' then
    Result := 30
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
  // Result := 71 number value
  // Result := 72 string value
  else if Token = 'USE' then
    Result := 73
  // Result := 74 constraint
  else if Token = 'USER' then
    Result := 75
  // Result := 76 GROUP BY
  // Result := 77 schema name
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

end.
