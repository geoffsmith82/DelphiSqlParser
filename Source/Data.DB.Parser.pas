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
