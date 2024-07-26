unit Data.DB.Parser;

interface

uses
  System.Generics.Collections,
  System.Classes,
  Data.DB.Lexer;

type
  TTokenTypes = (
    tkSELECT = 0, tkFROM, tkWHERE, tkLeft, tkRight, tkInner, tkFull, tkOuter,
    tkJoin, tkInto, tkOrder, tkGroup, tkBy, tkIn, tkAsterisk, tkEquals,
    tkEndStatement, tkLeftBracket, tkRightBracket, tkDotSeperator, tkAS, tkOn,
    tkTableName, tkTableRef, tkFieldName, tkFieldRefName, tkInsert, tkUpdate,
    tkSet, tkAlter, tkCreate, tkTable, tkBetween, tkCase, tkDelete, tkHaving,
    tkLIKE, tkLimit, tkDISTINCT, tkWith, tkTop, tkDrop, tkTRUNCATE, tkCOLUMN,
    tkAdd, tkUnique, tkConstraint, tkIndex, tkValues, tkAsc, tkDesc, tkElse,
    tkEnd, tkGrant, tkTo, tkRevoke, tkView, tkReplace, tkTrigger, tkCommit,
    tkRollback, tkDatabase, tkDatabaseName, tkViewName,
    tkDBOperation, tkUsername, tkConstantNumber, tkConstantString, tkUse, tkUser,
    tkGroupBy, tkSchemaName, tkRightOuterJoin, tkInnerJoin, tkLeftJoin, tkRightJoin,
    tkOuterJoin, tkOrderBy, tkCreateDatabase, tkSum, tkCount, tkAvg, tkExists, tkOr,
    tkAnd, tkNot, tkComma, tkLessThan, tkGreaterThan, tkDivide, tkUNION, tkNULL,
    tkWhen, tkIs, tkThen, tkBackup, tkDisk, tkVarchar, tkInt, tkModify, tkCreateView,
    tkCreateUser, tkCreateTable, tkDropDatabase, tkDropView, tkDropTable, tkDropUser,
    tkTruncateTable, tkIf, tkIfExists, tkAlterTable, tkCreateIndex, tkDropIndex,
    tkDeleteFrom, tkNotEqual, tkBackupDatabase, tkToDisk, tkInsertInto, tkDropConstraint,
    tkIndexName, tkConstraintName, tkLockTables, tkUnlockTables, tkLock, tkUnLock, tkTables,
    tkDouble, tkDefault, tkTemporary, tkMax, tkNotNull, tkZeroFill, tkUnsigned, tkMin,
    tkRead, tkFunctionName, tkMinus, tkPlus, tkLessThanOrEqual, tkGreaterThanOrEqual,
    tkAlterColumn, tkCast, tkFloat, tkCurrentDate, tkCurrentTime, tkIIf, tkComment,
    tkConcat, tkSubstr, tkCreateTemporaryTable, tkParam, tkAll, tkUnionAll, tkPrimary,
    tkKey, tkPrimaryKey, tkDropColumn, tkCreateOrReplaceView, tkCheck, tkCreateUniqueIndex,
    tkAutoIncrement, tkForeign, tkReferences, tkForeignKey, tkOpenSquareBracket, tkCloseSquareBracket,
    tkMultiply, tkHash, tkUnknownToken, tk68, tk150, tkYear, tkDateFunction, tkCross, tkCrossJoin,
    tkDate, tkCreateTrigger, tkCreateOrAlterTrigger, tkDropTrigger, tkDropTriggerIfExists,
    tkEnableTrigger, tkDisableTrigger, tkCreateDefiner, tkCreateFunction, tkDropProcedure,
    tkDropFunction, tkDropFunctionIfExists, tkDropProcedureIfExists, tkRenameTable,
    tkShowBinaryLogStatus, tkShowBinaryLogs, tkShowBinLogEvents, tkShowCollation,
    tkShowCollumns, tkShowCreateDatabase, tkShowCreateEvent, tkShowCreateFunction,
    tkShowCreateProcedure, tkShowCreateTable, tkShowCreateTrigger, tkShowCreateUser,
    tkShowCreateView, tkShowDatabases, tkShowEngine, tkShowEngines, tkShowErrors,
    tkShowEvents, tkShowFunctionCode, tkShowFunctionStatus, tkShowGrants, tkShowIndex,
    tkOpenTables, tkShowTriggers, tkShowStatus, tkShowCharSet, tkShowEngineInnoDBMutex,
    tkShowFullProcessList, tkShowFullTables, tkShowPlugins, tkShowProcedureStatus,
    tkShowProcessList, tkShowProfile, tkShowProfiles, tkShowSchemas,
    tkShowStorageEngines, tkShowTableStatus, tkShowTables, tkShowEngineInnoDBStatus
    );

  TTokenInfo = class
  strict private
    FTokenSQL: TTokenTypes;
  private
    function GetTokenSQL: TTokenTypes;
    procedure SetTokenSQL(const Value: TTokenTypes; overrides: Boolean); overload;
    procedure SetTokenSQL(const Value: TTokenTypes); overload;
  public
    TokenType: TTokenType;
    Token: string;
    SourcePos: Int64;
    property TokenSQL: TTokenTypes read GetTokenSQL write SetTokenSQL;
  end;

  TTokenBucket = class(TObjectList<TTokenInfo>)
  public
    procedure Join(istart: Integer);
    function LookupTokenByPosition(iPosition: Integer): TTokenInfo;
  end;

  TLexerParser = class(TParser)
  end;

  TSQLParser = class
  private
    FTokenDict: TDictionary<string, TTokenTypes>;

    procedure InitializeTokenDict;
    procedure CombineTokens;
    procedure DropIndex(i: Integer);
  public
    FTokens: TTokenBucket;
    function TokenStringToTokenSQL(info: TTokenInfo): TTokenTypes;
    function TokenIdToTokenString(inTokenId: TTokenTypes): string;
    function ProcessSQL(SQL: string): Integer;
    function DoesStatementModifyDB: Boolean;
    function IsDDL: Boolean;
    function DoesDoubleConstantExpressionExist: Boolean;
    function StatementCount: Integer;
    function TokenCount: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,  System.StrUtils, Winapi.Windows, System.Rtti,
  System.TypInfo;

type
  TTokenMatch = record
    Tokens: TArray<string>;
    ResultTokenSQL: TTokenTypes;
  end;




function TSQLParser.TokenIdToTokenString(inTokenId: TTokenTypes): string;
begin
  Result := GetEnumName(TypeInfo(TTokenTypes), Integer(inTokenId));
end;

{ TTokenBucket }

procedure TTokenBucket.Join(istart: Integer);
begin
  Self[istart].Token := Self[istart].Token + ' ' + Self[istart + 1].Token;
  Self.Delete(istart + 1);
end;

function TTokenBucket.LookupTokenByPosition(iPosition: Integer): TTokenInfo;
var
  i: Integer;
begin
  Result := Self[iPosition];
end;

{ TTokenInfo }

function TTokenInfo.GetTokenSQL: TTokenTypes;
begin
  Result := FTokenSQL;
end;

procedure TTokenInfo.SetTokenSQL(const Value: TTokenTypes; overrides: Boolean);
begin
  if ((TokenSQL = tkConstantNumber) or
     (TokenSQL = tkConstantString)) and not overrides then
  begin
    Exit;
  end;
  if (Token = '=') and (Value <> tkEquals) then
  begin
    // Do nothing
  end
  else
  begin
    FTokenSQL := Value;
  end;
end;

procedure TTokenInfo.SetTokenSQL(const Value: TTokenTypes);
begin
  SetTokenSQL(value, False);
end;

{ TSQLParser }

constructor TSQLParser.Create;
begin
  FTokens := TTokenBucket.Create;
  FTokenDict := TDictionary<string, TTokenTypes>.Create;
  InitializeTokenDict;
end;

destructor TSQLParser.Destroy;
begin
  FTokens.Free;
  FTokenDict.Free;
  inherited;
end;

procedure TSQLParser.InitializeTokenDict;
begin
  FTokenDict.Add('SELECT', tkSELECT);
  FTokenDict.Add('FROM', tkFROM);
  FTokenDict.Add('WHERE', tkWHERE);
  FTokenDict.Add('LEFT', tkLeft);
  FTokenDict.Add('RIGHT', tkRight);
  FTokenDict.Add('INNER', tkInner);
  FTokenDict.Add('FULL', tkFull);
  FTokenDict.Add('OUTER', tkOuter);
  FTokenDict.Add('JOIN', tkJoin);
  FTokenDict.Add('INTO', tkInto);
  FTokenDict.Add('ORDER', tkOrder);
  FTokenDict.Add('GROUP', tkGroup);
  FTokenDict.Add('BY', tkBy);
  FTokenDict.Add('IN', tkIn);
  FTokenDict.Add('*', tkAsterisk);
  FTokenDict.Add('=', tkEquals);
  FTokenDict.Add(';', tkEndStatement);
  FTokenDict.Add('(', tkLeftBracket);
  FTokenDict.Add(')', tkRightBracket);
  FTokenDict.Add('.', tkDotSeperator);
  FTokenDict.Add('AS', tkAS);
  FTokenDict.Add('ON', tkOn);
  FTokenDict.Add('INSERT', tkInsert);
  FTokenDict.Add('UPDATE', tkUpdate);
  FTokenDict.Add('SET', tkSet);
  FTokenDict.Add('ALTER', tkAlter);
  FTokenDict.Add('CREATE', tkCreate);
  FTokenDict.Add('TABLE', tkTable);
  FTokenDict.Add('BETWEEN', tkBetween);
  FTokenDict.Add('CASE', tkCase);
  FTokenDict.Add('DELETE', tkDelete);
  FTokenDict.Add('HAVING', tkHaving);
  FTokenDict.Add('LIKE', tkLIKE);
  FTokenDict.Add('LIMIT', tkLimit);
  FTokenDict.Add('DISTINCT', tkDISTINCT);
  FTokenDict.Add('WITH', tkWith);
  FTokenDict.Add('TOP', tkTop);
  FTokenDict.Add('DROP', tkDrop);
  FTokenDict.Add('TRUNCATE', tkTRUNCATE);
  FTokenDict.Add('COLUMN', tkCOLUMN);
  FTokenDict.Add('ADD', tkAdd);
  FTokenDict.Add('UNIQUE', tkUnique);
  FTokenDict.Add('CONSTRAINT', tkConstraint);
  FTokenDict.Add('INDEX', tkIndex);
  FTokenDict.Add('VALUES', tkValues);
  FTokenDict.Add('ASC', tkAsc);
  FTokenDict.Add('DESC', tkDesc);
  FTokenDict.Add('ELSE', tkElse);
  FTokenDict.Add('END', tkEnd);
  FTokenDict.Add('GRANT', tkGrant);
  FTokenDict.Add('TO', tkTo);
  FTokenDict.Add('REVOKE', tkRevoke);
  FTokenDict.Add('VIEW', tkView);
  FTokenDict.Add('REPLACE', tkReplace);
  FTokenDict.Add('TRIGGER', tkTrigger);
  FTokenDict.Add('COMMIT', tkCommit);
  FTokenDict.Add('ROLLBACK', tkRollback);
  FTokenDict.Add('DATABASE', tkDatabase);
  FTokenDict.Add('USE', tkUse);
  FTokenDict.Add('USER', tkUser);
  FTokenDict.Add('GROUP BY', tkGroupBy);
  FTokenDict.Add('RIGHT OUTER JOIN', tkRightOuterJoin);
  FTokenDict.Add('INNER JOIN', tkInnerJoin);
  FTokenDict.Add('LEFT JOIN', tkLeftJoin);
  FTokenDict.Add('RIGHT JOIN', tkRightJoin);
  FTokenDict.Add('OUTER JOIN', tkOuterJoin);
  FTokenDict.Add('ORDER BY', tkOrderBy);
  FTokenDict.Add('CREATE DATABASE', tkCreateDatabase);
  FTokenDict.Add('SUM', tkSum);
  FTokenDict.Add('COUNT', tkCount);
  FTokenDict.Add('AVG', tkAvg);
  FTokenDict.Add('EXISTS', tkExists);
  FTokenDict.Add('OR', tkOr);
  FTokenDict.Add('AND', tkAnd);
  FTokenDict.Add('NOT', tkNot);
  FTokenDict.Add(',', tkComma);
  FTokenDict.Add('<', tkLessThan);
  FTokenDict.Add('>', tkGreaterThan);
  FTokenDict.Add('/', tkDivide);
  FTokenDict.Add('UNION', tkUNION);
  FTokenDict.Add('NULL', tkNULL);
  FTokenDict.Add('WHEN', tkWhen);
  FTokenDict.Add('IS', tkIs);
  FTokenDict.Add('THEN', tkThen);
  FTokenDict.Add('BACKUP', tkBackup);
  FTokenDict.Add('DISK', tkDisk);
  FTokenDict.Add('VARCHAR', tkVarchar);
  FTokenDict.Add('INT', tkInt);
  FTokenDict.Add('MODIFY', tkModify);
  FTokenDict.Add('IF', tkIf);
  FTokenDict.Add('IF EXISTS', tkIfExists);
  FTokenDict.Add('ALTER TABLE', tkAlterTable);
  FTokenDict.Add('CREATE INDEX', tkCreateIndex);
  FTokenDict.Add('DROP INDEX', tkDropIndex);
  FTokenDict.Add('DELETE FROM', tkDeleteFrom);
  FTokenDict.Add('<>', tkNotEqual);
  FTokenDict.Add('BACKUP DATABASE', tkBackupDatabase);
  FTokenDict.Add('TO DISK', tkToDisk);
  FTokenDict.Add('INSERT INTO', tkInsertInto);
  FTokenDict.Add('DROP CONSTRAINT', tkDropConstraint);
  FTokenDict.Add('LOCK TABLES', tkLockTables);
  FTokenDict.Add('UNLOCK TABLES', tkUnlockTables);
  FTokenDict.Add('LOCK', tkLock);
  FTokenDict.Add('UNLOCK', tkUnLock);
  FTokenDict.Add('TABLES', tkTables);
  FTokenDict.Add('DOUBLE', tkDouble);
  FTokenDict.Add('DEFAULT', tkDefault);
  FTokenDict.Add('TEMPORARY', tkTemporary);
  FTokenDict.Add('MAX', tkMax);
  FTokenDict.Add('NOT NULL', tkNotNull);
  FTokenDict.Add('ZEROFILL', tkZeroFill);
  FTokenDict.Add('UNSIGNED', tkUnsigned);
  FTokenDict.Add('MIN', tkMin);
  FTokenDict.Add('READ', tkRead);
  FTokenDict.Add('FUNCTION NAME', tkFunctionName);
  FTokenDict.Add('-', tkMinus);
  FTokenDict.Add('+', tkPlus);
  FTokenDict.Add('<=', tkLessThanOrEqual);
  FTokenDict.Add('>=', tkGreaterThanOrEqual);
  FTokenDict.Add('ALTER COLUMN', tkAlterColumn);
  FTokenDict.Add('CAST', tkCast);
  FTokenDict.Add('FLOAT', tkFloat);
  FTokenDict.Add('CURRENT_DATE', tkCurrentDate);
  FTokenDict.Add('CURRENT_TIME', tkCurrentTime);
  FTokenDict.Add('IIF', tkIIf);
  FTokenDict.Add('--', tkComment);
  FTokenDict.Add('CONCAT', tkConcat);
  FTokenDict.Add('SUBSTR', tkSubstr);
  FTokenDict.Add('CREATE TEMPORARY TABLE', tkCreateTemporaryTable);
  FTokenDict.Add('PARAM', tkParam);
  FTokenDict.Add('ALL', tkAll);
  FTokenDict.Add('UNION ALL', tkUnionAll);
  FTokenDict.Add('PRIMARY', tkPrimary);
  FTokenDict.Add('KEY', tkKey);
  FTokenDict.Add('PRIMARY KEY', tkPrimaryKey);
  FTokenDict.Add('DROP COLUMN', tkDropColumn);
  FTokenDict.Add('CREATE OR REPLACE VIEW', tkCreateOrReplaceView);
  FTokenDict.Add('CHECK', tkCheck);
  FTokenDict.Add('CREATE UNIQUE INDEX', tkCreateUniqueIndex);
  FTokenDict.Add('AUTO_INCREMENT', tkAutoIncrement);
  FTokenDict.Add('FOREIGN', tkForeign);
  FTokenDict.Add('REFERENCES', tkReferences);
  FTokenDict.Add('FOREIGN KEY', tkForeignKey);
  FTokenDict.Add('[', tkOpenSquareBracket);
  FTokenDict.Add(']', tkCloseSquareBracket);
  FTokenDict.Add('#', tkHash);
  FTokenDict.Add('YEAR', tkYear);
  FTokenDict.Add('CROSS', tkCross);
end;

function TSQLParser.TokenStringToTokenSQL(info: TTokenInfo): TTokenTypes;
var
  token: string;
  intValue : Int64;
  floatValue: Double;
begin
  token := info.Token.ToUpper;

  if FTokenDict.TryGetValue(token, Result) then
    Exit;

  if (TryStrToInt64(token, intValue)) or (TryStrToFloat(token, floatValue)) then
    Result := TTokenTypes.tkConstantNumber
  else if info.TokenType = ttString then
    Result := TTokenTypes.tkConstantString
  else
    Result := TTokenTypes.tkUnknownToken;
end;

function TSQLParser.StatementCount: Integer;
var
  count: Integer;
  i: Integer;
begin
  count := 0;
  for i := 0 to FTokens.Count - 1 do
  begin
    if FTokens[i].TokenSQL = TTokenTypes.tkEndStatement then
      Inc(count);
  end;
  Result := count;
end;

function TSQLParser.TokenCount: Integer;
begin
  Result := FTokens.Count;
end;

function TSQLParser.DoesDoubleConstantExpressionExist: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FTokens.Count - 1 do
  begin
    if i - 2 >= 0 then
    begin
      if (FTokens[i - 2].TokenSQL in [TTokenTypes.tkConstantNumber, TTokenTypes.tkConstantString]) and
         (FTokens[i - 1].TokenSQL in [TTokenTypes.tkEquals, TTokenTypes.tkLessThan, TTokenTypes.tkLessThanOrEqual, TTokenTypes.tkNotEqual, TTokenTypes.tkGreaterThan, TTokenTypes.tkGreaterThanOrEqual]) and
         (FTokens[i].TokenSQL in [TTokenTypes.tkConstantNumber, TTokenTypes.tkConstantString]) then
      begin
        Result := True;
      end;
    end;
  end;
end;

function TSQLParser.DoesStatementModifyDB: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FTokens.Count - 1 do
  begin
    if FTokens[i].TokenSQL in [
                               TTokenTypes.tkJoin,
                               TTokenTypes.tkInto,
                               TTokenTypes.tkInsert,
                               TTokenTypes.tkUpdate,
                               TTokenTypes.tkSet,
                               TTokenTypes.tkAlter,
                               TTokenTypes.tkCreate,
                               TTokenTypes.tkDelete,
                               TTokenTypes.tkDrop,
                               TTokenTypes.tkAdd,
                               TTokenTypes.tkGrant,
                               TTokenTypes.tkView,
                               TTokenTypes.tkCreateDatabase,
                               TTokenTypes.tkCreateView,
                               TTokenTypes.tkCreateUser,
                               TTokenTypes.tkCreateTable,
                               TTokenTypes.tkCreateIndex,
                               TTokenTypes.tkCreateUniqueIndex,
                               TTokenTypes.tkCreateOrReplaceView,
                               TTokenTypes.tkDropDatabase,
                               TTokenTypes.tkDropView,
                               TTokenTypes.tkDropTable,
                               TTokenTypes.tkDropUser,
                               TTokenTypes.tkTruncateTable,
                               TTokenTypes.tkAlterTable,
                               TTokenTypes.tkDropIndex,
                               TTokenTypes.tkDeleteFrom,
                               TTokenTypes.tkInsertInto,
                               TTokenTypes.tkDropConstraint,
                               TTokenTypes.tkAlterColumn
                               ] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TSQLParser.IsDDL: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FTokens.Count - 1 do
  begin
    if FTokens[i].TokenSQL in [
                               tkInto,
                               tkCreate,
                               tkGrant,
                               tkCreateDatabase,
                               tkCreateView,
                               tkCreateOrReplaceView,
                               tkCreateTable,
                               tkCreateUniqueIndex,
                               tkDropDatabase,
                               tkDropView,
                               tkDropTable,
                               tkAlterTable,
                               tkCreateIndex,
                               tkDropIndex,
                               tkDropConstraint,
                               tkAlterColumn
                               ] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TSQLParser.CombineTokens;
var
  i: Integer;
  TokenCombinations: array of TTokenMatch;

  procedure CombineIfMatch(const ATokenMatch: TTokenMatch);
  var
    j: Integer;
    match: Boolean;
  begin
    match := True;
    for j := 0 to High(ATokenMatch.Tokens) do
    begin
      if FTokens[i + j].Token <> ATokenMatch.Tokens[j] then
      begin
        match := False;
        Break;
      end;
    end;

    if match then
    begin
      for j := 1 to High(ATokenMatch.Tokens) do
        FTokens.Join(i);
      FTokens[i].TokenSQL := ATokenMatch.ResultTokenSQL;
    end;
  end;

  procedure AddTokenCombination(const Tokens: TArray<string>; ResultTokenSQL: TTokenTypes);
  var
    CurrentIndex: Integer;
  begin
    CurrentIndex := Length(TokenCombinations);
    SetLength(TokenCombinations, CurrentIndex + 1);
    TokenCombinations[CurrentIndex].Tokens := Copy(Tokens);
    TokenCombinations[CurrentIndex].ResultTokenSQL := ResultTokenSQL;
  end;

begin
  i := 0;

  // Initialize token combinations
  AddTokenCombination(['`', '`'], tkUnknownToken);
  AddTokenCombination(['[', ']'], tkUnknownToken);
  AddTokenCombination(['<', '='], tkLessThanOrEqual);
  AddTokenCombination(['<', '>'], tkNotEqual);
  AddTokenCombination(['>', '='], tkGreaterThanOrEqual);
  AddTokenCombination(['ALTER', 'COLUMN'], tkAlterColumn);
  AddTokenCombination(['ALTER', 'TABLE'], tkAlterTable);
  AddTokenCombination(['BACKUP', 'DATABASE'], tkBackupDatabase);
  AddTokenCombination(['CREATE', 'DATABASE'], tkCreateDatabase);
  AddTokenCombination(['CREATE', 'DEFINER'], tkCreateDefiner);
  AddTokenCombination(['CREATE', 'FUNCTION'], tkCreateFunction);
  AddTokenCombination(['CREATE', 'INDEX'], tkCreateIndex);
  AddTokenCombination(['CREATE', 'OR', 'REPLACE', 'TRIGGER'], tkCreateOrAlterTrigger);
  AddTokenCombination(['CREATE', 'OR', 'REPLACE', 'VIEW'], tkCreateOrReplaceView);
  AddTokenCombination(['CREATE', 'TABLE'], tkCreateTable);
  AddTokenCombination(['CREATE', 'TEMPORARY', 'TABLE'], tkCreateTemporaryTable);
  AddTokenCombination(['CREATE', 'TRIGGER'], tkCreateTrigger);
  AddTokenCombination(['CREATE', 'UNIQUE', 'INDEX'], tkCreateUniqueIndex);
  AddTokenCombination(['CREATE', 'USER'], tkCreateUser);
  AddTokenCombination(['CREATE', 'VIEW'], tkCreateView);
  AddTokenCombination(['DELETE', 'FROM'], tkDeleteFrom);
  AddTokenCombination(['DISABLE', 'TRIGGER'], tkDisableTrigger);
  AddTokenCombination(['DROP', 'COLUMN'], tkDropColumn);
  AddTokenCombination(['DROP', 'CONSTRAINT'], tkDropConstraint);
  AddTokenCombination(['DROP', 'DATABASE'], tkDropDatabase);
  AddTokenCombination(['DROP', 'FUNCTION', 'IF', 'EXISTS'], tkDropFunctionIfExists);
  AddTokenCombination(['DROP', 'FUNCTION'], tkDropFunction);
  AddTokenCombination(['DROP', 'INDEX'], tkDropIndex);
  AddTokenCombination(['DROP', 'PROCEDURE', 'IF', 'EXISTS'], tkDropProcedureIfExists);
  AddTokenCombination(['DROP', 'PROCEDURE'], tkDropProcedure);
  AddTokenCombination(['DROP', 'TABLE'], tkDropTable);
  AddTokenCombination(['DROP', 'TRIGGER', 'IF', 'EXISTS'], tkDropTriggerIfExists);
  AddTokenCombination(['DROP', 'TRIGGER'], tkDropTrigger);
  AddTokenCombination(['DROP', 'USER'], tkDropUser);
  AddTokenCombination(['DROP', 'VIEW'], tkDropView);
  AddTokenCombination(['ENABLE', 'TRIGGER'], tkEnableTrigger);
  AddTokenCombination(['FOREIGN', 'KEY'], tkForeignKey);
  AddTokenCombination(['GROUP', 'BY'], tkGroupBy);
  AddTokenCombination(['IF', 'EXISTS'], tkIfExists);
  AddTokenCombination(['INNER', 'JOIN'], tkInnerJoin);
  AddTokenCombination(['INSERT', 'INTO'], tkInsertInto);
  AddTokenCombination(['LEFT', 'JOIN'], tkLeftJoin);
  AddTokenCombination(['LOCK', 'TABLES'], tkLockTables);
  AddTokenCombination(['NOT', 'NULL'], tkNotNull);
  AddTokenCombination(['ORDER', 'BY'], tkOrderBy);
  AddTokenCombination(['OUTER', 'JOIN'], tkOuterJoin);
  AddTokenCombination(['PRIMARY', 'KEY'], tkPrimaryKey);
  AddTokenCombination(['RENAME', 'TABLE'], tkRenameTable);
  AddTokenCombination(['RIGHT', 'JOIN'], tkRightJoin);
  AddTokenCombination(['RIGHT', 'OUTER', 'JOIN'], tkRightOuterJoin);
  AddTokenCombination(['SHOW', 'BINARY', 'LOG', 'STATUS'], tkShowBinaryLogStatus);
  AddTokenCombination(['SHOW', 'BINARY', 'LOGS'], tkShowBinaryLogs);
  AddTokenCombination(['SHOW', 'BINLOG', 'EVENTS'], tkShowBinLogEvents);
  AddTokenCombination(['SHOW', 'CHARACTER', 'SET'], tkShowCharset);
  AddTokenCombination(['SHOW', 'CHARSET'], tkShowCharset);
  AddTokenCombination(['SHOW', 'COLLATION'], tkShowCollation);
  AddTokenCombination(['SHOW', 'COLUMNS'], tkShowCollumns);
  AddTokenCombination(['SHOW', 'CREATE', 'DATABASE'], tkShowCreateDatabase);
  AddTokenCombination(['SHOW', 'CREATE', 'EVENT'], tkShowCreateEvent);
  AddTokenCombination(['SHOW', 'CREATE', 'FUNCTION'], tkShowCreateFunction);
  AddTokenCombination(['SHOW', 'CREATE', 'PROCEDURE'], tkShowCreateProcedure);
  AddTokenCombination(['SHOW', 'CREATE', 'TABLE'], tkShowCreateTable);
  AddTokenCombination(['SHOW', 'CREATE', 'TRIGGER'], tkShowCreateTrigger);
  AddTokenCombination(['SHOW', 'CREATE', 'USER'], tkShowCreateUser);
  AddTokenCombination(['SHOW', 'CREATE', 'VIEW'], tkShowCreateView);
  AddTokenCombination(['SHOW', 'DATABASES'], tkShowDatabases);
  AddTokenCombination(['SHOW', 'ENGINES'], tkShowEngines);
  AddTokenCombination(['SHOW', 'ENGINE', 'INNODB', 'MUTEX'], tkShowEngineInnoDBMutex);
  AddTokenCombination(['SHOW', 'ENGINE', 'INNODB', 'STATUS'], tkShowEngineInnoDBStatus);
  AddTokenCombination(['SHOW', 'ENGINE'], tkShowEngine);
  AddTokenCombination(['SHOW', 'ERRORS'], tkShowErrors);
  AddTokenCombination(['SHOW', 'EVENTS'], tkShowEvents);
  AddTokenCombination(['SHOW', 'FULL', 'PROCESSLIST'], tkShowFullProcessList);
  AddTokenCombination(['SHOW', 'FULL', 'TABLES'], tkShowFullTables);
  AddTokenCombination(['SHOW', 'FUNCTION', 'STATUS'], tkShowFunctionStatus);
  AddTokenCombination(['SHOW', 'FUNCTION', 'CODE'], tkShowFunctionCode);
  AddTokenCombination(['SHOW', 'GRANTS'], tkShowGrants);
  AddTokenCombination(['SHOW', 'INDEX'], tkShowIndex);
  AddTokenCombination(['SHOW', 'OPEN', 'TABLES'], tkOpenTables);
  AddTokenCombination(['SHOW', 'PLUGINS'], tkShowPlugins);
  AddTokenCombination(['SHOW', 'PROCEDURE', 'STATUS'], tkShowProcedureStatus);
  AddTokenCombination(['SHOW', 'PROCESSLIST'], tkShowProcessList);
  AddTokenCombination(['SHOW', 'PROFILE'], tkShowProfile);
  AddTokenCombination(['SHOW', 'PROFILES'], tkShowProfiles);
  AddTokenCombination(['SHOW', 'SCHEMAS'], tkShowSchemas);
  AddTokenCombination(['SHOW', 'STATUS'], tkShowStatus);
  AddTokenCombination(['SHOW', 'STORAGE', 'ENGINES'], tkShowStorageEngines);
  AddTokenCombination(['SHOW', 'TABLE', 'STATUS'], tkShowTableStatus);
  AddTokenCombination(['SHOW', 'TABLES'], tkShowTables);
  AddTokenCombination(['SHOW', 'TRIGGERS'], tkShowTriggers);
  AddTokenCombination(['TO', 'DISK'], tkToDisk);
  AddTokenCombination(['TRUNCATE', 'TABLE'], tkTruncateTable);
  AddTokenCombination(['UNION', 'ALL'], tkUnionAll);
  AddTokenCombination(['UNLOCK', 'TABLES'], tkUnlockTables);

  while i < FTokens.Count - 1 do
  begin
    for var TokenMatch in TokenCombinations do
      CombineIfMatch(TokenMatch);
    Inc(i);
  end;
end;


procedure TSQLParser.DropIndex(i: Integer);
begin
  FTokens.Delete(i);
end;

function ExtractNumberAsString(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    if CharInSet(S[I], ['0'..'9']) then
      Result := Result + S[I]
    else //if Result <> '' then
      Break;
  end;
end;

function TSQLParser.ProcessSQL(SQL: string): Integer;
var
  parser: TSQLLexer;
  strStrm: TStringStream;
  tokeninf : TToken;
  token: string;
  info: TTokenInfo;
  i: Integer;
  tmp: string;
begin
  Result := -1;
  strStrm := TStringStream.Create;
  try
    FTokens.Clear;
    strStrm.WriteString(SQL);
    strStrm.Position := 0;
    try
      parser := TSQLLexer .Create(strStrm);
    except
      on e: Exception do
      begin
        OutputDebugString(PChar('LinePos:' + parser.LinePos.ToString));
      end;
    end;
    try
      tokeninf := parser.GetNextToken;
      repeat
          info := TTokenInfo.Create;
          info.SetTokenSQL(tkUnknownToken);
          info.TokenType := ttUnknown;
          info.TokenType := tokeninf.TokenType;
          info.Token := tokeninf.Value;
          info.SourcePos := tokeninf.Position;
          OutputDebugString(PChar(info.Token));
          info.TokenSQL := TokenStringToTokenSQL(info);
          if info.Token.StartsWith('--') then
          begin
            info.TokenSQL := tkComment;
          end;
          FTokens.Add(info);
          tokeninf := parser.GetNextToken;
      until (tokeninf.TokenType = ttEOF);
      OutputDebugString(PChar('Tokens: ' + FTokens.Count.ToString));
    except
      on e: Exception do
      begin
        OutputDebugString(PChar('LinePos:' + parser.LinePos.ToString));
        Result := parser.LinePos;
      end;
    end;
    i := 0;

    CombineTokens;

    for i := 1 to FTokens.Count - 1 do
    begin
      if FTokens[i - 1].TokenSQL = TTokenTypes.tkFROM then
        FTokens[i].TokenSQL := TTokenTypes.tkTableName;

      if FTokens[i - 1].TokenSQL = TTokenTypes.tkDeleteFrom then
        FTokens[i].TokenSQL := TTokenTypes.tkTableName;

      if (i - 3 >= 0) and (FTokens[i - 3].TokenSQL = TTokenTypes.tkFROM) and
         (FTokens[i - 1].TokenSQL = TTokenTypes.tkDotSeperator) then
      begin
        FTokens[i - 2].TokenSQL := TTokenTypes.tkSchemaName;
        FTokens[i].TokenSQL := TTokenTypes.tkTableName;
      end;

      if (i - 3 >= 0) and (FTokens[i - 3].TokenSQL = TTokenTypes.tkDropIndex) and
         (FTokens[i - 1].TokenSQL = TTokenTypes.tkDotSeperator) then
      begin
        FTokens[i - 2].TokenSQL := TTokenTypes.tkSchemaName;
        FTokens[i].TokenSQL := TTokenTypes.tkIndexName;
      end;

      if (i - 2 >= 0) and (FTokens[i - 2].TokenSQL = TTokenTypes.tkInto) and
         (FTokens[i].TokenSQL = tkFrom) then
      begin
        FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = TTokenTypes.tkAS) and
           (FTokens[i - 2].TokenSQL = TTokenTypes.tkTableName) then
          FTokens[i].TokenSQL := TTokenTypes.tkTableRef;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = tkSum) and
           (FTokens[i - 2].TokenSQL = TTokenTypes.tkLeftBracket) then
          FTokens[i].TokenSQL := tkSum;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = tkCount) and
           (FTokens[i - 2].TokenSQL = TTokenTypes.tkLeftBracket) then
          FTokens[i].TokenSQL := tkCount;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = tkAvg) and
           (FTokens[i - 2].TokenSQL = TTokenTypes.tkLeftBracket) then
          FTokens[i].TokenSQL := tkAvg;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = tkExists) and
           (FTokens[i - 2].TokenSQL = TTokenTypes.tkLeftBracket) then
          FTokens[i].TokenSQL := tkExists;
      end;

      if (i - 2 >= 0) and (i < FTokens.Count - 2) then
      begin
        if (FTokens[i - 1].token = 'SELECT') and
           (FTokens[i + 1].token = '.') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkUpdate) and
           (FTokens[i].tokenSQL = TTokenTypes.tkSet) then
          FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkBackupDatabase) and
           (FTokens[i].tokenSQL = TTokenTypes.tkToDisk) then
          FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].token = 'JOIN') or
           (FTokens[i - 1].token = 'INNER JOIN') or
           (FTokens[i - 1].token = 'LEFT JOIN') or
           (FTokens[i - 1].token = 'RIGHT JOIN') or
           (FTokens[i - 1].token = 'OUTER JOIN') or
           (FTokens[i - 1].token = 'FULL OUTER JOIN') then
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
        if FTokens[i].Token = '[' then
          FTokens[i].TokenSQL := tk150;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].token = 'ON') and
           (FTokens[i + 1].token = '.') and
           (FTokens[i + 3].token = '=') and
           (FTokens[i + 5].token = '.') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
          FTokens[i + 4].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 6].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].token = '(') and
           (FTokens[i + 1].token = '.') and
           ((FTokens[i + 3].token = '=') or
            (FTokens[i + 3].token = '<') or
            (FTokens[i + 3].token = '>')) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkTableName) and
           (FTokens[i - 1].tokenSQL = TTokenTypes.tkModify) then
          FTokens[i].TokenSQL := TTokenTypes.tkFieldName;
      end;

      if i - 4 >= 0 then
      begin
        if (FTokens[i - 3].token = '=') and
           (FTokens[i - 1].token = '.') then
        begin
          FTokens[i - 2].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkUse) then
          FTokens[i].TokenSQL := TTokenTypes.tkDatabaseName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropTable) then
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkTruncateTable) then
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkAlterTable) then
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkAlterTable) then
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkCreateTemporaryTable) then
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkLockTables) then
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropView) then
          FTokens[i].TokenSQL := TTokenTypes.tkViewName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropUser) then
          FTokens[i].TokenSQL := TTokenTypes.tkUsername;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropIndex) then
          FTokens[i].TokenSQL := TTokenTypes.tkIndexName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkCreateTable) then
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkCreateView) then
          FTokens[i].TokenSQL := TTokenTypes.tkViewName;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropColumn) then
          FTokens[i].TokenSQL := TTokenTypes.tkFieldName;
      end;


      if i - 5 >= 0 then
      begin
        if ((FTokens[i - 3].token = '<') or
            (FTokens[i - 3].token = '=') or
            (FTokens[i - 3].token = '>')) and
           (FTokens[i - 1].token = '.') then
        begin
          FTokens[i - 2].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 5 >= 0 then
      begin
        if (FTokens[i - 5].tokenSQL = TTokenTypes.tkSELECT) and
           (FTokens[i - 3].tokenSQL = TTokenTypes.tkDotSeperator) and
           (FTokens[i - 1].tokenSQL = TTokenTypes.tkAS) then
        begin
          FTokens[i - 4].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i - 2].tokenSQL := TTokenTypes.tkFieldName;
          FTokens[i].tokenSQL := TTokenTypes.tkFieldRefName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].token = '(') and
           (FTokens[i + 1].token = '.') and
           (FTokens[i + 3].token = ')') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

    if i - 2 >= 0 then
    begin
      if (FTokens[i - 1].tokenSQL = TTokenTypes.tkSELECT) and
         (FTokens[i + 1].tokenSQL = TTokenTypes.tkDotSeperator) then
      begin
        FTokens[i].tokenSQL := TTokenTypes.tkTableName;
        FTokens[i + 2].tokenSQL := TTokenTypes.tkFieldName;
      end
      else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkSELECT) and
              (FTokens[i + 1].tokenSQL = TTokenTypes.tkComma) then
      begin
        FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
      end
      else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkSELECT) and
              (FTokens[i + 1].tokenSQL = tkFROM) then
      begin
        if FTokens[i].Token = '*' then
          FTokens[i].tokenSQL := TTokenTypes.tkAsterisk
        else
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
      end;
    end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkFieldName) and
           (FTokens[i - 1].tokenSQL = TTokenTypes.tkComma) and
           (FTokens[i].tokenSQL = TTokenTypes.tkUnknownToken) then
        begin
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkFieldName) and
           (FTokens[i - 2].tokenSQL = TTokenTypes.tkComma) and
           (FTokens[i].tokenSQL = TTokenTypes.tkFROM) then
        begin
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkWHERE) and
           (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if (FTokens[i - 2].tokenSQL = TTokenTypes.tkWHERE) then
        begin
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if ((FTokens[i - 2].tokenSQL = TTokenTypes.tkDISTINCT) or
            (FTokens[i - 2].tokenSQL = tkAnd) or
            (FTokens[i - 2].tokenSQL = TTokenTypes.tkOrderBy)) and
           (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if ((FTokens[i - 2].tokenSQL = TTokenTypes.tkDISTINCT) or
                 (FTokens[i - 2].tokenSQL = tkAnd) or
                 (FTokens[i - 2].tokenSQL = TTokenTypes.tkOrderBy)) then
        begin
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkGroupBy) and
           (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if (FTokens[i - 2].tokenSQL = TTokenTypes.tkGroupBy) then
        begin
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkOrderBy) and
           (FTokens[i - 1].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin
          FTokens[i - 2].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkOrderBy) then
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkSet) and
           (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if (FTokens[i - 2].tokenSQL = TTokenTypes.tkSet) then
        begin
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].token = ',') and
           (FTokens[i + 1].token = '.') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkCreateIndex) and
           (FTokens[i].tokenSQL = TTokenTypes.tkOn) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkIndexName;
        end;
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkCreateOrReplaceView) and
           (FTokens[i].tokenSQL = TTokenTypes.tkAs) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkViewName;
        end;
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkCreateUniqueIndex) and
           (FTokens[i].tokenSQL = TTokenTypes.tkOn) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkIndexName;
        end;
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkOn) and
           (FTokens[i].tokenSQL = TTokenTypes.tkLeftBracket) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
        end;
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkInsertInto) and
           (FTokens[i].tokenSQL = TTokenTypes.tkSELECT) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
        end;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].token = '(') and
           (FTokens[i + 1].token = ')') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = TTokenTypes.tkDropConstraint) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkConstraintName;
        end;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = TTokenTypes.tkInsertInto) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
        end;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].token = ':') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkParam;
        end;
      end;

      if i - 5 >= 0 then
      begin
        if (FTokens[i - 5].tokenSQL = TTokenTypes.tkGrant) and
           (FTokens[i - 3].tokenSQL = TTokenTypes.tkOn) and
           (FTokens[i - 1].tokenSQL = TTokenTypes.tkTo) and
           (MatchStr(FTokens[i - 4].token, ['SELECT', 'INSERT', 'DELETE']) = TRUE) then
        begin
          FTokens[i - 4].TokenSQL := TTokenTypes.tkDBOperation;
          FTokens[i - 2].TokenSQL := tk68;
          FTokens[i].TokenSQL := TTokenTypes.tkUsername;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkOn) and
           (FTokens[i - 1].tokenSQL = TTokenTypes.tkTo) then
        begin
          FTokens[i - 2].TokenSQL := tk68;
          FTokens[i].TokenSQL := TTokenTypes.tkUsername;
        end;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDatabase) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkDatabaseName;
        end;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropDatabase) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkDatabaseName;
        end;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkCreateDatabase) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkDatabaseName;
        end;
      end;
    end;
  finally
    parser.Free;
    strStrm.Free;
  end;
end;
end.

