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

function TokenStringToTokenSQL(info: TTokenInfo): Integer;
begin
  if info.Token = 'SELECT' then
    Result := 0
  else if info.Token = 'FROM' then
    Result := 1
  else if info.Token = 'WHERE' then
    Result := 2
  else if info.Token = 'LEFT' then
    Result := 3
  else if info.Token = 'RIGHT' then
    Result := 4
  else if info.Token = 'INNER' then
    Result := 5
  else if info.Token = 'FULL' then
    Result := 6
  else if info.Token = 'OUTER' then
    Result := 7
  else if info.Token = 'JOIN' then
    Result := 8
  else if info.Token = 'INTO' then
    Result := 9
  else if info.Token = 'ORDER' then
    Result := 10
  else if info.Token = 'GROUP' then
    Result := 11
  else if info.Token = 'BY' then
    Result := 12
  else if info.Token = 'IN' then
    Result := 13
  else if info.Token = '*' then
    Result := 14
  else if info.Token = '=' then
    Result := 15
  else if info.Token = ';' then
    Result := 16
  else if info.Token = '(' then
    Result := 17
  else if info.Token = ')' then
    Result := 18
  else if info.Token = '.' then
    Result := 19
  else if info.Token = 'AS' then
    Result := 20
  else if info.Token = 'ON' then
    Result := 21
  // Result := 22 // table
  // Result := 23 // tableref
  // Result := 24 // fieldname
  // Result := 25 // fieldnameref
  // Result := 26 // paramref
  // Result := 27 // Integer
  // Result := 28 // String
  else if info.Token = 'INSERT' then
    Result := 29
  else if info.Token = 'INTO' then
    Result := 30
  else if info.Token = 'UPDATE' then
    Result := 31
  else if info.Token = 'SET' then
    Result := 32
  else if info.Token = 'ALTER' then
    Result := 33
  else if info.Token = 'CREATE' then
    Result := 34
  else if info.Token = 'TABLE' then
    Result := 35
  else if info.Token = 'BETWEEN' then
    Result := 36
  else if info.Token = 'CASE' then
    Result := 37
  else if info.Token = 'DELETE' then
    Result := 38
  else if info.Token = 'HAVING' then
    Result := 39
  else if info.Token = 'LIKE' then
    Result := 40
  else if info.Token = 'LIMIT' then
    Result := 41
  else if info.Token = 'DISTINCT' then
    Result := 42
  else if info.Token = 'WITH' then
    Result := 43
  else if info.Token = 'TOP' then
    Result := 44
  else if info.Token = 'DROP' then
    Result := 45
  else if info.Token = 'TRUNCATE' then
    Result := 46
  else if info.Token = 'COLUMN' then
    Result := 47
  else if info.Token = 'ADD' then
    Result := 48
  else if info.Token = 'UNIQUE' then
    Result := 49
  else if info.Token = 'CONSTRAINT' then
    Result := 50
  else if info.Token = 'INDEX' then
    Result := 51
  else if info.Token = 'VALUES' then
    Result := 52
  else if info.Token = 'ASC' then
    Result := 53
  else if info.Token = 'DESC' then
    Result := 54
  else if info.Token = 'ELSE' then
    Result := 55
  else if info.Token = 'END' then
    Result := 56
  else if info.Token = 'GRANT' then
    Result := 57
  else if info.Token = 'TO' then
    Result := 58
  else if info.Token = 'REVOKE' then
    Result := 59
  else if info.Token = 'VIEW' then
    Result := 60
  else if info.Token = 'REPLACE' then
    Result := 61
  else if info.Token = 'TRIGGER' then
    Result := 62
  else if info.Token = 'COMMIT' then
    Result := 63
  else if info.Token = 'ROLLBACK' then
    Result := 64
  else if info.Token = 'DATABASE' then
    Result := 65
  // Result := 66 database name
  // Result := 67 view name
  // Result := 68 some db object - table, view, etc
  // Result := 69 some db operation INSERT, DELETE, UPDATE, SELECT
  // Result := 70 user
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
