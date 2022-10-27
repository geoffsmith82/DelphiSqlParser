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
    SourcePos : Int64;
    property TokenSQL: Integer read GetTokenSQL write SetTokenSQL;
  end;

  TTokenTypes = record
  const
    tkSELECT = 0;
    tkFROM = 1;
    tkWHERE = 2;
    tkLeft = 3;
    tkRight = 4;
    tkInner = 5;
    tkFull = 6;
    tkOuter = 7;
    tkJoin = 8;
    tkInto = 9;
    tkOrder = 10;
    tkGroup = 11;
    tkBy = 12;
    tkIn = 13;
    tkAsterisk = 14;
    tkEquals = 15;
    tkEndStatement = 16;
    tkLeftBracket = 17;
    tkRightBracket = 18;
    tkDotSeperator = 19;
    tkAS = 20;
    tkOn = 21;
    tkTableName = 22;
    tkTableRef = 23;
    tkFieldName = 24;
    tkFieldRefName = 25;
    tkInsert = 29;
    tkUpdate = 31;
    tkSet = 32;
    tkAlter = 33;
    tkCreate = 34;
    tkTable = 35;
    tkBetween = 36; // : Result := 'tkBetween';
    tkCase = 37; // : Result := 'tkCase';
    tkDelete = 38; // : Result := 'tkDelete';
    tkHaving = 39; // : Result := 'tkHaving';
    tkLIKE = 40; // : Result := 'tkLIKE';
    tkLimit = 41; // : Result := 'tkLimit';
    tkDISTINCT = 42;
    tkWith = 43;
    tkTop = 44;
    tkDrop = 45;
    tkTRUNCATE = 46; // Result := 46
    tkCOLUMN = 47; // Result := 47
    tkAdd = 48;
    tkUnique = 49; // Result := 49
    tkConstraint = 50; // Result := 50
//    tkIndex = 51; // Result := 51
    tkValues = 52; // Result := 52
    tkAsc = 53; // Result := 53
    tkDesc = 54; // Result := 54
    tkElse = 55;
    tkEnd = 56;
    tkGrant = 57;
    tkTo = 58;
    tkRevoke = 59;
    tkView = 60;
    tkReplace = 61;
    tkTrigger = 62;
    tkCommit = 63;
    tkRollback = 64;
    tkDatabase = 65;
    tkDatabaseName = 66;
    tkViewName = 67;
    tkDBOperation = 69;
    tkUsername = 70;
    tkConstantNumber = 71;
    tkConstantString = 72;
    tkUse = 73;
    tkUser = 75;
    tkGroupBy = 76;
    tkSchemaName = 77;
    tkRightOuterJoin = 79;
    tkInnerJoin = 80;
    tkLeftJoin = 81;
    tkRightJoin = 82;
    tkOuterJoin = 83;
    tkOrderBy = 84;
    tkCreateDatabase = 85;
    tkSum = 86; // : Result := 'tkSum';
    tkCount = 87; // : Result := 'tkCount';
    tkAvg = 88; // : Result := 'tkAvg';
    tkExists = 89;// : Result := 'tkExists';
    tkOr = 90; // : Result := 'tkOr';
    tkAnd = 91; // : Result := 'tkAnd';
    tkNot = 92;
    tkComma = 93; // : Result := 'tkComma';
    tkLessThan = 94; //: Result := 'tkLessThan';
    tkGreaterThan = 95; //: Result := 'tkGreaterThan';
    tkDivide = 96; // : Result := 'tkDivide';
    tkUNION = 98; // : Result := 'tkUNION';
    tkNULL = 99; // : Result := 'tkNULL';
    tkWhen = 100;
    tkIs = 101;
//    tkElse = 102;
    tkThen = 103;
    tkBackup = 104;
    tkDisk = 105;
    tkVarchar = 106;
    tkInt = 107;
    tkModify = 108;
    tkCreateView = 109; // Result := 109 CREATE VIEW
    tkCreateUser = 110; // Result := 110 CREATE USER
    tkCreateTable = 111; // Result := 111 CREATE TABLE
    tkDropDatabase = 112; // Result := TTokenTypes.tkDropDatabase DROP DATABASE
    tkDropView = 113; // Result := 113 DROP VIEW
    tkDropTable = 114; // Result := 114 DROP TABLE
    tkDropUser = 115; // Result := 115 DROP USER
    tkTruncateTable = 116; // Result := 116 TRUNCATE TABLE
    tkIf = 117; //  Result := 117 // IF
    tkIfExists = 118; // Result := 118 IF EXISTS
    tkAlterTable = 119; // Result := 119 ALTER TABLE
    tkIndex = 120; // Result := 120 // INDEX
    tkCreateIndex = 121; // Result := 121 CREATE INDEX
    tkDropIndex = 122; // Result := 122 DROP INDEX
    tkDeleteFrom = 123; // Result := 123 DELETE FROM
    tkNotEqual = 124;// Result := 124 <>
    tkBackupDatabase = 125;
    tkToDisk = 126;
    tkInsertInto = 127;
    tkDropConstraint = 129;
    tkIndexName = 130;//: Result := 'tkIndexName';
    tkConstraintName = 131;// : Result := 'tkConstraintName';
    tkLockTables = 132; //: Result := 'tkLockTables';
    tkUnlockTables = 133; //: Result := 'tkUnlockTables';
    tkLock = 134;
    tkUnLock = 135;
    tkTables = 136;
    tkDouble = 137;//: Result := 'tkDouble';
    tkDefault = 138; //: Result := 'tkDefault';
    tkTemporary = 139;
    tkMax = 140; //: Result := 'tkMax';
    tkNotNull = 141; //: Result := 'tkNotNull';
    tkZeroFill = 142;// : Result := 'tkZeroFill';
    tkUnsigned = 143;//: Result := 'tkUnsigned';
    tkMin = 144; //: Result := 'tkMin';
//    tkIndexName = 145;
    tkRead = 146;
    tkFunctionName = 147;
    tkMinus = 148;
    tkPlus = 149; //: Result := 'tkPlus';
    tkLessThanOrEqual = 152;// : Result := 'tkLessThanOrEqual';
    tkGreaterThanOrEqual = 153;//: Result := 'tkGreaterThanOrEqual';
    tkAlterColumn = 154;
    tkCast = 155;//: Result := 'tkCast';
    tkFloat = 156; //: Result := 'tkFloat';
    tkCurrentDate = 157;
    tkCurrentTime = 158;
    tkIIf = 159;
    tkComment = 160; //: Result := 'tkComment';
    tkConcat = 161; //: Result := 'tkConcat';
    tkSubstr = 162; //: Result := 'tkSubstr';
    tkCreateTemporaryTable = 164;
    tkParam = 165;
    tkAll = 166;
    tkUnionAll = 167;
    tkPrimary = 168;
    tkKey = 169;
    tkPrimaryKey = 170;
    tkDropColumn = 171;
    tkCreateOrReplaceView = 172;
    tkCheck = 173;
    tkCreateUniqueIndex = 174;
    tkAutoIncrement = 175;
    tkForeign = 176;
    tkReferences = 177;
    tkForeignKey = 178;
    tkUnknownToken = -199;
  end;

  TTokenBucket = class(TObjectList<TTokenInfo>)
  public
    procedure Join(istart: Integer);
  end;

  TSQLParser = class
  private
    procedure CombineTokens;
    procedure DropIndex(i: Integer);
  public
    FTokens: TTokenBucket;
    function ProcessSQL(SQL: string):Integer;
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
    TTokenTypes.tkSELECT : Result := 'tkSELECT';
    TTokenTypes.tkFROM : Result := 'tkFROM';
    TTokenTypes.tkWHERE : Result := 'tkWHERE';
    TTokenTypes.tkJoin : Result := 'tkJoin';
    TTokenTypes.tkInto : Result := 'tkInto';
    TTokenTypes.tkOrder : Result := 'tkOrder';
    TTokenTypes.tkIn : Result := 'tkIn';
    TTokenTypes.tkAsterisk : Result := 'tkAsterisk';
    TTokenTypes.tkEquals : Result := 'tkEquals';
    TTokenTypes.tkEndStatement : Result := 'tkEndStatement';
    TTokenTypes.tkLeftBracket : Result := 'tkLeftBracket';
    TTokenTypes.tkRightBracket : Result := 'tkRightBracket';
    TTokenTypes.tkDotSeperator : Result := 'tkDotSeperator';
    TTokenTypes.tkAS : Result := 'tkAS';
    TTokenTypes.tkOn : Result := 'tkOn';
    TTokenTypes.tkTableName : Result := 'tkTableName';
    TTokenTypes.tkTableRef : Result := 'tkTableRef';
    TTokenTypes.tkFieldName : Result := 'tkFieldName';
    TTokenTypes.tkFieldRefName : Result := 'tkFieldRefName';
    TTokenTypes.tkUpdate : Result := 'tkUpdate';
    TTokenTypes.tkSet : Result := 'tkSet';
    TTokenTypes.tkTable : Result := 'tkTable';
    TTokenTypes.tkBetween : Result := 'tkBetween';
    TTokenTypes.tkCase : Result := 'tkCase';
    TTokenTypes.tkDelete : Result := 'tkDelete';
    TTokenTypes.tkHaving : Result := 'tkHaving';
    TTokenTypes.tkLIKE : Result := 'tkLIKE';
    TTokenTypes.tkLimit : Result := 'tkLimit';
    TTokenTypes.tkDISTINCT : Result := 'tkDISTINCT';
    TTokenTypes.tkTop : Result := 'tkTop';
    TTokenTypes.tkAdd : Result := 'tkAdd';
    TTokenTypes.tkUnique : Result := 'tkUnique';
    TTokenTypes.tkConstraint : Result := 'tkConstraint';
    TTokenTypes.tkValues : Result := 'tkValues';
    TTokenTypes.tkAsc : Result := 'tkAsc';
    TTokenTypes.tkDesc : Result := 'tkDesc';
    TTokenTypes.tkElse : Result := 'tkElse';
    TTokenTypes.tkEnd : Result := 'tkEnd';
    TTokenTypes.tkGrant : Result := 'tkGrant';
    TTokenTypes.tkTo : Result := 'tkTo';
    TTokenTypes.tkDatabaseName : Result := 'tkDatabaseName';
    TTokenTypes.tkViewName : Result := 'tkViewName';
    TTokenTypes.tkUsername : Result := 'tkUsername';
    TTokenTypes.tkConstantNumber : Result := 'tkConstantNumber';
    TTokenTypes.tkConstantString : Result := 'tkConstantString';
    TTokenTypes.tkUse : Result := 'tkUse';
    TTokenTypes.tkGroupBy : Result := 'tkGroupBy';
    TTokenTypes.tkSchemaName : Result := 'tkSchemaName';
    TTokenTypes.tkInnerJoin : Result := 'tkInnerJoin';
    TTokenTypes.tkLeftJoin : Result := 'tkLeftJoin';
    TTokenTypes.tkRightJoin : Result := 'tkRightJoin';
    TTokenTypes.tkOrderBy : Result := 'tkOrderBy';
    TTokenTypes.tkCreateDatabase : Result := 'tkCreateDatabase';
    TTokenTypes.tkSum : Result := 'tkSum';
    TTokenTypes.tkCount : Result := 'tkCount';
    TTokenTypes.tkAvg : Result := 'tkAvg';
    TTokenTypes.tkExists : Result := 'tkExists';
    TTokenTypes.tkOr : Result := 'tkOr';
    TTokenTypes.tkAnd : Result := 'tkAnd';
    TTokenTypes.tkComma : Result := 'tkComma';
    TTokenTypes.tkLessThan : Result := 'tkLessThan';
    TTokenTypes.tkGreaterThan : Result := 'tkGreaterThan';
    TTokenTypes.tkDivide : Result := 'tkDivide';
    TTokenTypes.tkUNION : Result := 'tkUNION';
    TTokenTypes.tkNULL : Result := 'tkNULL';
    TTokenTypes.tkVarchar : Result := 'tkVarchar';
    TTokenTypes.tkInt: Result := 'tkInt';
    TTokenTypes.tkModify: Result := 'tkModify';
    TTokenTypes.tkCreateView: Result := 'tkCreateView';
    TTokenTypes.tkCreateTable: Result := 'tkCreateTable';
    TTokenTypes.tkDropDatabase: Result := 'tkDropDatabase';
    TTokenTypes.tkDropView: Result := 'tkDropView';
    TTokenTypes.tkDropTable: Result := 'tkDropTable';
    TTokenTypes.tkDropIndex: Result := 'tkDropIndex';
    TTokenTypes.tkTruncateTable: Result := 'tkTruncateTable';
    TTokenTypes.tkAlterTable: Result := 'tkAlterTable';
    TTokenTypes.tkCreateIndex: Result := 'tkCreateIndex';
    TTokenTypes.tkDeleteFrom: Result := 'tkDeleteFrom';
    TTokenTypes.tkNotEqual: Result := 'tkNotEqual';
    TTokenTypes.tkBackupDatabase: Result := 'tkBackupDatabase';
    TTokenTypes.tkToDisk: Result := 'tkToDisk';
    TTokenTypes.tkInsertInto: Result := 'tkInsertInto';
    TTokenTypes.tkDropUser: Result := 'tkDropUser';
    TTokenTypes.tkDropConstraint: Result := 'tkDropConstraint';
    TTokenTypes.tkIndexName: Result := 'tkIndexName';
    TTokenTypes.tkConstraintName: Result := 'tkConstraintName';
    TTokenTypes.tkLockTables: Result := 'tkLockTables';
    TTokenTypes.tkUnlockTables: Result := 'tkUnlockTables';
    TTokenTypes.tkDouble: Result := 'tkDouble';
    TTokenTypes.tkDefault: Result := 'tkDefault';
    TTokenTypes.tkMax: Result := 'tkMax';
    TTokenTypes.tkNotNull : Result := 'tkNotNull';
    TTokenTypes.tkZeroFill : Result := 'tkZeroFill';
    TTokenTypes.tkUnsigned : Result := 'tkUnsigned';
    TTokenTypes.tkMin : Result := 'tkMin';
    TTokenTypes.tkPlus: Result := 'tkPlus';
    TTokenTypes.tkLessThanOrEqual : Result := 'tkLessThanOrEqual';
    TTokenTypes.tkGreaterThanOrEqual : Result := 'tkGreaterThanOrEqual';
    TTokenTypes.tkCast : Result := 'tkCast';
    TTokenTypes.tkFloat : Result := 'tkFloat';
    TTokenTypes.tkComment : Result := 'tkComment';
    TTokenTypes.tkConcat : Result := 'tkConcat';
    TTokenTypes.tkSubstr : Result := 'tkSubstr';
    TTokenTypes.tkParam : Result := 'tkParam';
    TTokenTypes.tkUnionAll: Result := 'tkUnionAll';
    TTokenTypes.tkPrimary: Result := 'tkPrimary';
    TTokenTypes.tkKey: Result := 'tkKey';
    TTokenTypes.tkPrimaryKey: Result := 'tkPrimaryKey';
    TTokenTypes.tkDropColumn: Result := 'tkDropColumn';
    TTokenTypes.tkAlterColumn: Result := 'tkAlterColumn';
    TTokenTypes.tkCreateOrReplaceView: Result := 'tkCreateOrReplaceView';
    TTokenTypes.tkCreateTemporaryTable: Result := 'tkCreateTemporaryTable';
    TTokenTypes.tkCheck: Result := 'tkCheck';
    TTokenTypes.tkCreateUniqueIndex: Result := 'tkCreateUniqueIndex';
    TTokenTypes.tkForeignKey: Result := 'tkForeignKey';
    TTokenTypes.tkRead: Result := 'tkRead';

    TTokenTypes.tkUnknownToken: Result := '';
  else
    Result := 'Token Name !Defined';
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
    Result := TTokenTypes.tkConstantNumber // number value
  else if (TryStrToFloat(token, floatValue) = True) and not (Token = '.') then
    Result := TTokenTypes.tkConstantNumber // number value
  else if info.TokenType = System.Classes.toString then
    Result := TTokenTypes.tkConstantString // string value
  else if Token = 'FROM' then
    Result := TTokenTypes.tkFROM
  else if Token = 'WHERE' then
    Result := TTokenTypes.tkWHERE
  else if Token = 'LEFT' then
    Result := TTokenTypes.tkLeft
  else if Token = 'RIGHT' then
    Result := TTokenTypes.tkRight
  else if Token = 'INNER' then
    Result := TTokenTypes.tkInner
  else if Token = 'FULL' then
    Result := TTokenTypes.tkFull
  else if Token = 'OUTER' then
    Result := TTokenTypes.tkOuter
  else if Token = 'JOIN' then
    Result := TTokenTypes.tkJoin
  else if Token = 'INTO' then
    Result := TTokenTypes.tkInto
  else if Token = 'ORDER' then
    Result := TTokenTypes.tkOrder
  else if Token = 'GROUP' then
    Result := TTokenTypes.tkGroup
  else if Token = 'BY' then
    Result := TTokenTypes.tkBy
  else if Token = 'IN' then
    Result := TTokenTypes.tkIn
  else if Token = '*' then
    Result := TTokenTypes.tkAsterisk
  else if Token = '=' then
    Result := TTokenTypes.tkEquals
  else if Token = ';' then
    Result := TTokenTypes.tkEndStatement
  else if Token = '(' then
    Result := TTokenTypes.tkLeftBracket
  else if Token = ')' then
    Result := TTokenTypes.tkRightBracket
  else if Token = '.' then
    Result := TTokenTypes.tkDotSeperator
  else if Token = 'AS' then
    Result := TTokenTypes.tkAS
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
    Result := TTokenTypes.tkInsert
  else if Token = 'UPDATE' then
    Result := TTokenTypes.tkUpdate
  else if Token = 'SET' then
    Result := TTokenTypes.tkSet
  else if Token = 'ALTER' then
    Result := TTokenTypes.tkAlter
  else if Token = 'CREATE' then
    Result := TTokenTypes.tkCreate
  else if Token = 'TABLE' then
    Result := TTokenTypes.tkTable
  else if Token = 'BETWEEN' then
    Result := TTokenTypes.tkBetween
  else if Token = 'CASE' then
    Result := TTokenTypes.tkCase
  else if Token = 'DELETE' then
    Result := TTokenTypes.tkDelete
  else if Token = 'HAVING' then
    Result := TTokenTypes.tkHaving
  else if Token = 'LIKE' then
    Result := TTokenTypes.tkLIKE
  else if Token = 'LIMIT' then
    Result := TTokenTypes.tkLimit
  else if Token = 'DISTINCT' then
    Result := TTokenTypes.tkDISTINCT
  else if Token = 'WITH' then
    Result := TTokenTypes.tkWith
  else if Token = 'TOP' then
    Result := TTokenTypes.tkTop
  else if Token = 'DROP' then
    Result := TTokenTypes.tkDrop
  else if Token = 'TRUNCATE' then
    Result := TTokenTypes.tkTRUNCATE
  else if Token = 'COLUMN' then
    Result := TTokenTypes.tkCOLUMN
  else if Token = 'ADD' then
    Result := TTokenTypes.tkAdd
  else if Token = 'UNIQUE' then
    Result := TTokenTypes.tkUnique
  else if Token = 'CONSTRAINT' then
    Result := TTokenTypes.tkConstraint
  else if Token = 'INDEX' then
    Result := TTokenTypes.tkIndex
  else if Token = 'VALUES' then
    Result := TTokenTypes.tkValues
  else if Token = 'ASC' then
    Result := TTokenTypes.tkAsc
  else if Token = 'DESC' then
    Result := TTokenTypes.tkDesc
  else if Token = 'ELSE' then
    Result := TTokenTypes.tkElse
  else if Token = 'END' then
    Result := TTokenTypes.tkEnd
  else if Token = 'GRANT' then
    Result := TTokenTypes.tkGrant
  else if Token = 'TO' then
    Result := TTokenTypes.tkTo
  else if Token = 'REVOKE' then
    Result := TTokenTypes.tkRevoke
  else if Token = 'VIEW' then
    Result := TTokenTypes.tkView
  else if Token = 'REPLACE' then
    Result := TTokenTypes.tkReplace
  else if Token = 'TRIGGER' then
    Result := TTokenTypes.tkTrigger
  else if Token = 'COMMIT' then
    Result := TTokenTypes.tkCommit
  else if Token = 'ROLLBACK' then
    Result := TTokenTypes.tkRollback
  else if Token = 'DATABASE' then
    Result := TTokenTypes.tkDatabase
  // Result := 66 database name
  // Result := tkViewName view name
  // Result := 68 some db object - table, view, etc
  // Result := TTokenTypes.tkDBOperation some db operation INSERT, DELETE, UPDATE, SELECT
  // Result := tkUsername user
  else if Token = 'USE' then
    Result := TTokenTypes.tkUse
  // Result := 74 constraint
  else if Token = 'USER' then
    Result := TTokenTypes.tkUser
  // Result := TTokenTypes.tkGroupBy GROUP BY
  // Result := 77 schema name
  // Result := 78 DROP TABLE
  // Result := TTokenTypes.tkRightOuterJoin RIGHT OUTER JOIN
  // Result := 80 INNER JOIN
  // Result := 81 LEFT JOIN
  // Result := 82 RIGHT JOIN
  // Result := 83 OUTER JOIN
  // Result := 84 ORDER BY
  // Result := tkCreateDatabase CREATE DATABASE
  else if Token = 'SUM' then
    Result := TTokenTypes.tkSum // SUM
  else if Token = 'COUNT' then
    Result := TTokenTypes.tkCount // COUNT
  else if Token = 'AVG' then
    Result := TTokenTypes.tkAvg // AVG
  else if Token = 'EXISTS' then
    Result := TTokenTypes.tkExists // EXISTS
  else if Token = 'OR' then
    Result := TTokenTypes.tkOr // OR
  else if Token = 'AND' then
    Result := TTokenTypes.tkAnd // AND
  else if Token = 'NOT' then
    Result := TTokenTypes.tkNot // OR
  else if Token = ',' then
    Result := TTokenTypes.tkComma // ,
  else if Token = '<' then
    Result := TTokenTypes.tkLessThan // <
  else if Token = '>' then
    Result := TTokenTypes.tkGreaterThan // >
  else if Token = '/' then
    Result := 96 // /
  else if Token = '*' then
    Result := 97 // *
  else if Token = 'UNION' then
    Result := TTokenTypes.tkUNION // UNION
  else if Token = 'NULL' then
    Result := TTokenTypes.tkNULL // *
  else if Token = 'WHEN' then
    Result := TTokenTypes.tkWhen // *
  else if Token = 'IS' then
    Result := TTokenTypes.tkIs // *
  else if Token = 'ELSE' then
    Result := TTokenTypes.tkElse // *
  else if Token = 'THEN' then
    Result := TTokenTypes.tkThen // *
  else if Token = 'BACKUP' then
    Result := TTokenTypes.tkBackup // *
  else if Token = 'DISK' then
    Result := TTokenTypes.tkDisk // *
  else if Token = 'VARCHAR' then
    Result := TTokenTypes.tkVarchar // *
  else if Token = 'INT' then
    Result := TTokenTypes.tkInt // *
  else if Token = 'MODIFY' then
    Result := 108 // *
  // Result := 109 CREATE VIEW
  // Result := 110 CREATE USER
  // Result := 111 CREATE TABLE
  // Result := TTokenTypes.tkDropDatabase DROP DATABASE
  // Result := 113 DROP VIEW
  // Result := 114 DROP TABLE
  // Result := 115 DROP USER
  // Result := 116 TRUNCATE TABLE
  else if Token = 'IF' then
    Result := TTokenTypes.tkIf // IF
  // Result := 118 IF EXISTS
  // Result := 119 ALTER TABLE
  else if Token = 'INDEX' then
    Result := TTokenTypes.tkIndex // INDEX
  // Result := 121 CREATE INDEX
  // Result := 122 DROP INDEX
  // Result := 123 DELETE FROM
  // Result := 124 <>
  // Result := TTokenTypes.tkBackupDatabase BACKUP DATABASE
  // Result := 126 TO DISK
  // Result := 127 INSERT INTO
  // Result := 128 DROP USER
  // Result := 129 DROP CONSTRAINT
  // Result := 130 indexname
  // Result := 131 constraintname
  // Result := 132 LOCK TABLES
  // Result := 133 UNLOCK TABLES
  else if Token = 'LOCK' then
    Result := TTokenTypes.tkLock // LOCK
  else if Token = 'UNLOCK' then
    Result := TTokenTypes.tkUnLock // UNLOCK
  else if Token = 'TABLES' then
    Result := TTokenTypes.tkTables // TABLES
  else if Token = 'DOUBLE' then
    Result := TTokenTypes.tkDouble // DOUBLE
  else if Token = 'DEFAULT' then
    Result := TTokenTypes.tkDefault // DEFAULT
  else if Token = 'TEMPORARY' then
    Result := TTokenTypes.tkTemporary // TEMPORARY
  else if Token = 'MAX' then
    Result := TTokenTypes.tkMax // MAX
  // Result := 141 NOT NULL
  else if Token = 'ZEROFILL' then
    Result := TTokenTypes.tkZeroFill // MAX
  else if Token = 'UNSIGNED' then
    Result := TTokenTypes.tkUnsigned // UNSIGNED
  else if Token = 'MIN' then
    Result := TTokenTypes.tkMin // MIN
  // Result := 145 indexname
  else if Token = 'READ' then
    Result := TTokenTypes.tkRead // MIN
  //  Result := 147 // functionname
  else if Token = '-' then
    Result := TTokenTypes.tkMinus // MIN
  else if Token = '+' then
    Result := TTokenTypes.tkPlus // MIN
  else if Token = '[' then
    Result := 150 // [
  else if Token = ']' then
    Result := 151 // ]
  // Result := 152 // <=
  // Result := 153 // >=
 // else if Token = 'ALTER COLUMN' then
 //   Result := TTokenTypes.tkAlterColumn // ]
  else if Token = 'CAST' then
    Result := TTokenTypes.tkCast
  else if Token = 'FLOAT' then
    Result := TTokenTypes.tkFloat
  else if Token = 'CURRENT_DATE' then
    Result := TTokenTypes.tkCurrentDate
  else if Token = 'CURRENT_TIME' then
    Result := TTokenTypes.tkCurrentTime
  else if Token = 'IIF' then
    Result := TTokenTypes.tkIIf
  else if Token = '--' then
    Result := TTokenTypes.tkComment
  else if Token = 'CONCAT' then
    Result := TTokenTypes.tkConcat
  else if Token = 'SUBSTR' then
    Result := TTokenTypes.tkSubstr
  else if Token = 'ALL' then
    Result := TTokenTypes.tkAll
  else if Token = 'PRIMARY' then
    Result := TTokenTypes.tkPrimary
  else if token = 'FOREIGN' then
    Result := TTokenTypes.tkForeign
  else if token = 'REFERENCES' then
    Result := TTokenTypes.tkReferences
  else if Token = 'KEY' then
    Result := TTokenTypes.tkKey
  else if Token = 'CHECK' then
    Result := TTokenTypes.tkCheck
  else if Token = 'AUTO_INCREMENT' then
    Result := TTokenTypes.tkAutoIncrement
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
  if (TokenSQL = TTokenTypes.tkConstantNumber) or
     (TokenSQL = TTokenTypes.tkConstantString) then
  begin
    Exit;
  end;
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
  inherited;
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
    if FTokens[i].TokenSQL = TTokenTypes.tkEndStatement then
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
      if (FTokens[i - 2].TokenSQL in [TTokenTypes.tkConstantNumber, TTokenTypes.tkConstantString]) and
         (FTokens[i - 1].TokenSQL in [TTokenTypes.tkEquals, TTokenTypes.tkLessThan, TTokenTypes.tkLessThanOrEqual, TTokenTypes.tkNotEqual, TTokenTypes.tkGreaterThan, TTokenTypes.tkGreaterThanOrEqual]) and
         (FTokens[i].TokenSQL in [TTokenTypes.tkConstantNumber,TTokenTypes.tkConstantString]) then
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
  i : Integer;
begin
  Result := False;
  for i := 0 to FTokens.Count - 1 do
  begin
    if FTokens[i].TokenSQL in [
                               TTokenTypes.tkInto,
                               TTokenTypes.tkCreate,
                               TTokenTypes.tkGrant,
                               TTokenTypes.tkCreateDatabase,
                               TTokenTypes.tkCreateView,
                               TTokenTypes.tkCreateOrReplaceView,
                               TTokenTypes.tkCreateTable,
                               TTokenTypes.tkCreateUniqueIndex,
                               TTokenTypes.tkDropDatabase,
                               TTokenTypes.tkDropView,
                               TTokenTypes.tkDropTable,
                               TTokenTypes.tkAlterTable,
                               TTokenTypes.tkCreateIndex,
                               TTokenTypes.tkDropIndex,
                               TTokenTypes.tkDropConstraint,
                               TTokenTypes.tkAlterColumn
                               ] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TSQLParser.CombineTokens;
var
  i : Integer;
  token : string;
begin
  i := 0;
    // Initial pass - join multi word commands together
    while i < FTokens.Count - 1 do
    begin
      if (FTokens[i].Token.Trim = '`') then
      begin
        token := FTokens[i + 1].Token;
        FTokens.Join(i);
        FTokens[i].Token := '`' + token;
      end
      else if (i > 0) and (FTokens[i - 1].Token = '`') then
      begin
        token := FTokens[i - 2].Token;
        FTokens.Join(i - 2);
        FTokens[i - 2].Token := token + '`';
      end
      else if (i > 0) and FTokens[i - 1].Token.Trim.StartsWith('`') and ((FTokens[i - 1].Token.EndsWith('`') = False) or (FTokens[i - 1].Token <> '`')) then
      begin
        FTokens.Join(i - 1);
      end

      else if (FTokens[i].Token.Trim = '[') then
      begin
        token := FTokens[i + 1].Token;
        FTokens.Join(i);
        FTokens[i].Token := '[' + token;
      end
      else if (i > 0) and (FTokens[i - 1].Token = ']') then
      begin
        token := FTokens[i - 2].Token;
        FTokens.Join(i - 2);
        FTokens[i - 2].Token := token + ']';
      end
      else if (i > 0) and FTokens[i - 1].Token.Trim.StartsWith('[') and ((FTokens[i - 1].Token.EndsWith(']') = False) or (FTokens[i - 1].Token <> '[')) then
      begin
        FTokens.Join(i - 1);
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkRight) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkOuter) and (FTokens[i + 2].TokenSQL = TTokenTypes.tkJoin)) then
      begin
        FTokens.Join(i);
        FTokens.Join(i);  // RIGHT OUTER JOIN
        FTokens[i].TokenSQL := TTokenTypes.tkRightOuterJoin;
      end
      else if (FTokens[i].Token = ':') then
      begin
        token := FTokens[i + 1].Token;
        FTokens.Join(i);
        FTokens[i].Token := ':' + token;
        FTokens[i].TokenSQL := TTokenTypes.tkParam;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkCreate) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkTemporary) and (FTokens[i + 2].TokenSQL = TTokenTypes.tkTable)) then
      begin
        FTokens.Join(i);
        FTokens.Join(i);  // CREATE TEMPORARY TABLE
        FTokens[i].TokenSQL := TTokenTypes.tkCreateTemporaryTable;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkCreate) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkOr) and (FTokens[i + 2].TokenSQL = TTokenTypes.tkReplace) and (FTokens[i + 3].TokenSQL = TTokenTypes.tkView)) then
      begin
        FTokens.Join(i);
        FTokens.Join(i);
        FTokens.Join(i);  // CREATE OR REPLACE VIEW
        FTokens[i].TokenSQL := TTokenTypes.tkCreateOrReplaceView;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkCreate) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkUnique) and (FTokens[i + 2].TokenSQL = TTokenTypes.tkIndex) ) then
      begin
        FTokens.Join(i);
        FTokens.Join(i);  // CREATE UNIQUE INDEX
        FTokens[i].TokenSQL := TTokenTypes.tkCreateUniqueIndex;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkInner) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkJoin)) then
      begin
        FTokens.Join(i);  // INNER JOIN
        FTokens[i].TokenSQL := TTokenTypes.tkInnerJoin;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkPrimary) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkKey)) then
      begin
        FTokens.Join(i);  // PRIMARY KEY
        FTokens[i].TokenSQL := TTokenTypes.tkPrimaryKey;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkForeign) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkKey)) then
      begin
        FTokens.Join(i);  // FOREIGN KEY
        FTokens[i].TokenSQL := TTokenTypes.tkForeignKey;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkDrop) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkCOLUMN)) then
      begin
        FTokens.Join(i);  // DROP COLUMN
        FTokens[i].TokenSQL := TTokenTypes.tkDropColumn;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkUNION) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkAll)) then
      begin
        FTokens.Join(i);  // UNION ALL
        FTokens[i].TokenSQL := TTokenTypes.tkUnionAll;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkLeft) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkJoin)) then
      begin
        FTokens.Join(i); // LEFT JOIN
        FTokens[i].TokenSQL := TTokenTypes.tkLeftJoin;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkRight) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkJoin)) then
      begin
        FTokens.Join(i); // RIGHT JOIN
        FTokens[i].TokenSQL := TTokenTypes.tkRightJoin;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkOuter) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkJoin)) then
      begin
        FTokens.Join(i); // OUTER JOIN
        FTokens[i].TokenSQL := TTokenTypes.tkOuterJoin;
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkGroup) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkBy)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkGroupBy;  // GROUP BY
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkOrder) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkBy)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkOrderBy;  // ORDER BY
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkLock) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkTables)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkLockTables;  // LOCK TABLES
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkUnLock) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkTables)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkUnlockTables;  // UNLOCK TABLES
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkCreate) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkDatabase)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkCreateDatabase;  // CREATE DATABASE
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkDrop) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkDatabase)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkDropDatabase;  // DROP DATABASE
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkCreate) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkView)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkCreateView;  // CREATE VIEW
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkDrop) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkView)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkDropView;  // DROP VIEW
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkCreate) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkTable)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkCreateTable;  // CREATE TABLE
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkDrop) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkTable)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkDropTable;  // DROP TABLE
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkInsert) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkInto)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkInsertInto;  // INSERT INTO
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkCreate) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkIndex)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkCreateIndex;  // CREATE INDEX
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkDrop) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkUser)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkDropUser;  // DROP USER
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkTRUNCATE) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkTable)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkTruncateTable;  // TRUNCATE TABLE
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkAlter) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkTable)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkAlterTable;  // ALTER TABLE
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkCreate) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkUser)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkCreateUser;  // CREATE USER
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkCreate) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkIndex)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkCreateIndex;  // CREATE INDEX
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkDrop) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkIndex)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkDropIndex;  // DROP INDEX
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkDrop) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkConstraint)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkDropConstraint;  // DROP CONSTRAINT
      end

      else if ((FTokens[i].TokenSQL = TTokenTypes.tkDelete) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkFROM)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkDeleteFrom;  // DELETE FROM
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkLessThan) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkGreaterThan)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkNotEqual;  // <>
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkLessThan) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkEquals)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkLessThanOrEqual;  // <=
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkGreaterThan) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkEquals)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkGreaterThanOrEqual;  // >=
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkAlter) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkCOLUMN)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkAlterColumn;  // ALTER COLUMN
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkBackup) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkDatabase)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkBackupDatabase;  // BACKUP DATABASE
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkTo) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkDisk)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkToDisk;  // TO DISK
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkNot) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkNULL)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkNotNull;  // NOT NULL
      end
      else if ((FTokens[i].TokenSQL = TTokenTypes.tkIf) and (FTokens[i + 1].TokenSQL = TTokenTypes.tkExists)) then
      begin
        FTokens.Join(i);
        FTokens[i].TokenSQL := TTokenTypes.tkIfExists;  // IF EXISTS
      end;
      Inc(i);
    end;
end;

procedure TSQLParser.DropIndex(i:Integer);
begin
  if i - 2 >= 0 then // DROP INDEX2 ????1 ;0
  begin
    if (FTokens[i - 2].TokenSQL = TTokenTypes.tkDropIndex) and (FTokens[i].TokenSQL = TTokenTypes.tkEndStatement) then
    begin
      FTokens[i - 1].TokenSQL := TTokenTypes.tkIndexName;
    end;
  end;

  if i - 4 >= 0 then // DROP INDEX3 ????2.????1 ;0
  begin
    if (FTokens[i - 4].TokenSQL = TTokenTypes.tkDropIndex) and (FTokens[i - 2].TokenSQL = TTokenTypes.tkDotSeperator) and (FTokens[i].TokenSQL = TTokenTypes.tkEndStatement) then
    begin
      FTokens[i - 3].TokenSQL := TTokenTypes.tkSchemaName;
      FTokens[i - 1].TokenSQL := TTokenTypes.tkIndexName;
    end;
  end;

  if i - 3 >= 0 then  // DROP INDEX3 ????2.1????0
  begin
    if (FTokens[i - 3].TokenSQL = TTokenTypes.tkDropIndex) and (FTokens[i - 1].TokenSQL = TTokenTypes.tkDotSeperator) then
    begin
      FTokens[i - 2].TokenSQL := TTokenTypes.tkFieldName;
      FTokens[i].TokenSQL := TTokenTypes.tkIndexName;
    end
  end;

end;

function TSQLParser.ProcessSQL(SQL: string): Integer;
var
  parser: TParser;
  strStrm: TStringStream;
  token: string;
  info: TTokenInfo;
  i: Integer;
begin
  Result := -1;
  strStrm := TStringStream.Create;
  try
    FTokens.Clear;
    strStrm.WriteString(SQL);
    strStrm.Position := 0;
    try
      parser := TParser.Create(strStrm);
    except
      on e : Exception do
      begin
        OutputDebugString(PChar('LinePos:'+ parser.LinePos.ToString));
    //    OutputDebugString(PChar(e.Message));
      end;

    end;
  try
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
        info.SourcePos := parser.SourcePos;

        if (info.TokenType = #2) and (info.Token = #0) then
          info.Token := '#';
        info.TokenSQL := TokenStringToTokenSQL(info);

  //    Memo2.Lines.Add(token + ' ' + info.TokenSQL.ToString);
        FTokens.Add(info);
      end
    until (parser.NextToken = toEOF);
         except
          on e : Exception do
          begin
            OutputDebugString(PChar('LinePos:'+ parser.LinePos.ToString));
            Result := parser.LinePos;
        //    OutputDebugString(PChar(e.Message));
          end;
        end;
    i := 0;

    CombineTokens;

    for i := 1 to FTokens.Count - 1 do
    begin
      if FTokens[i - 1].TokenSQL = TTokenTypes.tkFROM  {'FROM'} then
        FTokens[i].TokenSQL := TTokenTypes.tkTableName;

      if FTokens[i - 1].TokenSQL = TTokenTypes.tkDeleteFrom  {'DELETE FROM'} then
        FTokens[i].TokenSQL := TTokenTypes.tkTableName;


      if (i - 3 >= 0) and (FTokens[i - 3].TokenSQL = TTokenTypes.tkFROM) and {'FROM3 schema2.1tablename0'}
         (FTokens[i - 1].TokenSQL = TTokenTypes.tkDotSeperator) then
      begin
        FTokens[i - 2].TokenSQL := TTokenTypes.tkSchemaName;
        FTokens[i].TokenSQL := TTokenTypes.tkTableName;
      end;

      if (i - 3 >= 0) and (FTokens[i - 3].TokenSQL = TTokenTypes.tkDropIndex) and {'DROP INDEX3 table2.1indexname0'}
         (FTokens[i - 1].TokenSQL = TTokenTypes.tkDotSeperator) then
      begin
        FTokens[i - 2].TokenSQL := TTokenTypes.tkSchemaName;
        FTokens[i].TokenSQL := TTokenTypes.tkIndexName;
      end;

      if (i - 2 >= 0) and (FTokens[i - 2].TokenSQL = TTokenTypes.tkInto) and {'INTO2 ????1 FROM0'}
         (FTokens[i].TokenSQL = 1) then
      begin
        FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].TokenSQL = TTokenTypes.tkAS) and (FTokens[i - 2].TokenSQL = TTokenTypes.tkTableName) {'AS'} then
          FTokens[i].TokenSQL := TTokenTypes.tkTableRef;
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
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkUpdate) and (FTokens[i].tokenSQL = TTokenTypes.tkSet) then   // UPDATE ??? SET
          FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
      end;

      if i - 2 >= 0 then
      begin   // BACKUPDATABASE2 ????1 TODISK0
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkBackupDatabase) and (FTokens[i].tokenSQL = TTokenTypes.tkToDisk) then
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
        end;
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
        end;
      end;

      if i - 2 >= 0 then // LOCK TABLES2 ????1 READ0
      begin
        if (FTokens[i - 2].TokenSQL = TTokenTypes.tkLockTables) and (FTokens[i].TokenSQL = 146) then
        begin
          FTokens[i-1].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 1 >= 0 then // DROP USER1 ????0
      begin
        if (FTokens[i - 1].TokenSQL = TTokenTypes.tkDropUser) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkUsername;
        end;
      end;

      if i - 1 >= 0 then // DROP COLUMN ????0
      begin
        if (FTokens[i - 1].TokenSQL = TTokenTypes.tkDropColumn) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      DropIndex(i);

      if i - 2 >= 0 then  // WHERE fieldname = ???
      begin
        if ((FTokens[i - 1].token = 'SET') or (FTokens[i - 1].token = 'WHERE')) and
          (FTokens[i + 1].token = '=')
        then
        begin
          if FTokens[i].TokenType = toSymbol then
            FTokens[i].TokenSQL := TTokenTypes.tkFieldName
          else if FTokens[i].TokenType = toInteger then
            FTokens[i].TokenSQL := TTokenTypes.tkConstantNumber;

          if FTokens[i + 2].TokenType = toSymbol then
            FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName
          else if FTokens[i + 2].TokenType = toInteger then
            FTokens[i + 2].TokenSQL := TTokenTypes.tkConstantNumber
          else if FTokens[i + 2].TokenType = toFloat then
            FTokens[i + 2].TokenSQL := TTokenTypes.tkConstantNumber
          else if FTokens[i + 2].TokenType = System.Classes.toString then
            FTokens[i + 2].TokenSQL := TTokenTypes.tkConstantString;

        end;
      end;

      if i - 2 >= 0 then  // (
      begin
        if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = '.') and//          (FTokens[i + 2].Token = fieldname)
          ((FTokens[i + 3].token = '=') or (FTokens[i + 3].token = '<') or (FTokens[i + 3].token = '>')) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then  // tablename MODIFY ?????
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkTableName) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkModify) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 4 >= 0 then  // =
      begin
        if (FTokens[i - 3].token = '=') and (FTokens[i - 1].token = '.') then
        begin
          FTokens[i -2 ].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 0].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 1 >= 0 then  // USE dbname
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkUse) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkDatabaseName;
        end;
      end;

      if i - 5 >= 0 then  // =
      begin
        if ((FTokens[i - 3].token = '<') or (FTokens[i - 3].token = '=') or (FTokens[i - 3].token = '>')) and (FTokens[i - 1].token = '.') then
        begin
          FTokens[i-2].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 0].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 5 >= 0 then
      begin
        if (FTokens[i - 5].tokenSQL = TTokenTypes.tkSELECT) and (FTokens[i - 3].tokenSQL = TTokenTypes.tkDotSeperator)
           and (FTokens[i - 1].tokenSQL = TTokenTypes.tkAS) then
        begin  // SELECT5 ????4.3?????2 AS1 ?????0
          FTokens[i - 4].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i - 2].tokenSQL := TTokenTypes.tkFieldName;
          FTokens[i].tokenSQL := TTokenTypes.tkFieldRefName;
        end;
      end;

      if i - 2 >= 0 then  // =
      begin
        if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = '.') and (FTokens[i + 3].token = ')') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 2].TokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if (FTokens[i - 1].tokenSQL = TTokenTypes.tkSELECT) and (FTokens[i + 1].tokenSQL = TTokenTypes.tkDotSeperator) then
      begin  // SELECT ????.????? ,
        FTokens[i].tokenSQL := TTokenTypes.tkTableName;
        FTokens[i + 2].tokenSQL := TTokenTypes.tkFieldName;
      end
      else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkSELECT) and (FTokens[i + 1].tokenSQL = TTokenTypes.tkComma) then
      begin // SELECT ???? ,
        FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
      end
      else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkSELECT) and (FTokens[i + 1].tokenSQL = 1) then
      begin // SELECT ???? FROM
        if FTokens[i].Token = '*' then
          FTokens[i].tokenSQL := TTokenTypes.tkAsterisk
        else
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkFieldName) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkComma) and (FTokens[i].tokenSQL = TTokenTypes.tkUnknownToken) then
        begin // field2 ,1 ????0
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
        end
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkFieldName) and (FTokens[i - 2].tokenSQL = TTokenTypes.tkComma) and (FTokens[i].tokenSQL = TTokenTypes.tkFROM) then
        begin // field3 ,2 ????1 FROM0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkWHERE) and (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin  // WHERE ????.?????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if (FTokens[i - 2].tokenSQL = TTokenTypes.tkWHERE) then
        begin // WHERE ????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if ( (FTokens[i - 2].tokenSQL = TTokenTypes.tkDISTINCT) or (FTokens[i - 2].tokenSQL = 91 ) or (FTokens[i - 2].tokenSQL = TTokenTypes.tkOrderBy ) ) and (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin  // ORDER BY ????.?????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if ( (FTokens[i - 2].tokenSQL = TTokenTypes.tkDISTINCT) or (FTokens[i - 2].tokenSQL = 91 ) or (FTokens[i - 2].tokenSQL = TTokenTypes.tkOrderBy ) ) then
        begin // ORDER BY ????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkGroupBy) and (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin  // GROUP BY ????.?????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if (FTokens[i - 2].tokenSQL = TTokenTypes.tkGroupBy) then
        begin // GROUP BY ????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkOrderBy) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin  // ORDER BY3 ????2.1?????0
          FTokens[i - 2].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;
      if i - 2 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkOrderBy) then
        begin // ORDER BY ????
          FTokens[i].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkSet) and (FTokens[i].tokenSQL = TTokenTypes.tkDotSeperator) then
        begin  // SET ????.?????
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
          FTokens[i + 1].tokenSQL := TTokenTypes.tkFieldName;
        end
        else if (FTokens[i - 2].tokenSQL = TTokenTypes.tkSet) then
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
        end;
      end;

      if i - 2 >= 0 then  // =
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkCreateIndex) and (FTokens[i].tokenSQL = TTokenTypes.tkOn) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkIndexName;
        end;
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkCreateOrReplaceView) and (FTokens[i].tokenSQL = TTokenTypes.tkAs) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkViewName;
        end;
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkCreateUniqueIndex) and (FTokens[i].tokenSQL = TTokenTypes.tkOn) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkIndexName;
        end;
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkOn) and (FTokens[i].tokenSQL = TTokenTypes.tkLeftBracket) then
        begin
          FTokens[i - 1].TokenSQL := TTokenTypes.tkTableName;
        end;
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkInsertInto) and (FTokens[i].tokenSQL = TTokenTypes.tkSELECT) then
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

      if i - 1 >= 0 then  // DROP CONSTRAINT ????
      begin
        if (FTokens[i - 1].TokenSQL = TTokenTypes.tkDropConstraint) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkConstraintName;
        end
      end;
      if i - 1 >= 0 then  // INSERT INTO ????
      begin
        if (FTokens[i - 1].TokenSQL = TTokenTypes.tkInsertInto) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName;
        end
      end;

      if i - 1 >= 0 then  // parameter
      begin
        if (FTokens[i - 1].token = ':') then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkParam;
        end
      end;

      if i - 5 >= 0 then  // GRANT5 operation4 ON3 object2 TO1 user0
      begin
        if (FTokens[i - 5].tokenSQL = TTokenTypes.tkGrant) and
           (FTokens[i - 3].tokenSQL = TTokenTypes.tkOn) and
           (FTokens[i - 1].tokenSQL = TTokenTypes.tkTo) and
           (MatchStr(FTokens[i - 4].token, ['SELECT', 'INSERT', 'DELETE']) = TRUE)
        then
        begin
          FTokens[i - 4].TokenSQL := TTokenTypes.tkDBOperation;
          FTokens[i - 2].TokenSQL := 68;
          FTokens[i].TokenSQL := TTokenTypes.tkUsername;
        end
      end;

     if i - 3 >= 0 then  // ON3 object2 TO1 user0
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkOn) and
           (FTokens[i - 1].tokenSQL = TTokenTypes.tkTo)
        then
        begin
          FTokens[i - 2].TokenSQL := 68;
          FTokens[i].TokenSQL := TTokenTypes.tkUsername;
        end
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkCreateDatabase) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkDatabaseName; // CREATE DATABASE
        end
        else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropDatabase) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkDatabaseName; // DROP DATABASE
        end
        else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkCreateView) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkViewName; // CREATE VIEW
        end
        else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropView) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkViewName; // DROP VIEW
        end
        else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkCreateUser) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkUsername; // CREATE USER
        end
        else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropUser) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkUsername; // DROP USER
        end
        else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkCreateTable) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName; // CREATE TABLE
        end
        else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropTable) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName; // DROP TABLE
        end
        else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkDropUser) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkUsername; // DROP USER
        end
        else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkTruncateTable) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName; // TRUNCATE TABLE
        end
        else if (FTokens[i - 1].tokenSQL = TTokenTypes.tkAlterTable) then
        begin
          FTokens[i].TokenSQL := TTokenTypes.tkTableName; // ALTER TABLE
        end
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkComma) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkFROM) then
        begin //  ,2 ????1 FROM0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkEquals) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkEndStatement) then
        begin //  ,2 ????1 FROM0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkSELECT) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkAS) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 1 >= 0 then
      begin
        if (FTokens[i - 1].tokenSQL = TTokenTypes.tkCreateTemporaryTable) and (FTokens[i].tokenSQL = TTokenTypes.tkUnknownToken) then
        begin //  CREATE TEMPORARY TABLE 1 ???0
          FTokens[i].tokenSQL := TTokenTypes.tkTableName;
        end;
      end;


      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkAS) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkFROM) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkAdd) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkVarchar) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkAS) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkComma) then
        begin //  AS 2 ????1 ,0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldRefName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkComma) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = 2) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkLeftBracket) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkInt) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkComma) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkVarchar) then
        begin //  , 2 ????1 VARCHAR0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkComma) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-0].tokenSQL = TTokenTypes.tkInt) then
        begin //  , 2 ????1 VARCHAR0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkAlterColumn) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkVarchar) then
        begin //  COLUMN 2 ????1 VARCHAR0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkComma) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = 137) then
        begin //  COLUMN 2 ????1 VARCHAR0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkFieldName) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkEquals) then
        begin //  NOT 2 ????1 =0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkLeftBracket ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkComma) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkComma) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkEquals) then
        begin //  SELECT 2 ????1 AS0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkTable) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkLeftBracket) then
        begin //  TABLE 2 ????1 (0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkTable) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = 101) then
        begin //  TABLE 2 ????1 (0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkLeftBracket) and (FTokens[i].tokenSQL = TTokenTypes.tkRightBracket) then
        begin //  TABLE 2 ????1 (0
          FTokens[i - 2].tokenSQL := TTokenTypes.tkFunctionName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkElse) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = 56) then
        begin //  TABLE 2 ????1 (0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 2 >= 0 then
      begin
        if (FTokens[i - 2].tokenSQL = TTokenTypes.tkAS ) and (FTokens[i - 1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkOn) then
        begin //  AS 2 ????1 ON0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkTableRef;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkLeftBracket) and (FTokens[i - 2].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i-1].tokenSQL = TTokenTypes.tkDotSeperator) and (FTokens[i].tokenSQL = TTokenTypes.tkUnknownToken) then
        begin //  AS 2 ????1 ON0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkRightBracket) and (FTokens[i - 2].tokenSQL = TTokenTypes.tkAS) and (FTokens[i-1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkLeftBracket) then
        begin //  AS 2 ????1 ON0
          FTokens[i - 1].tokenSQL := TTokenTypes.tkFieldName;
        end;
      end;

      if i - 3 >= 0 then
      begin
        if (FTokens[i - 3].tokenSQL = TTokenTypes.tkRightBracket) and (FTokens[i - 2].tokenSQL = TTokenTypes.tkAS) and (FTokens[i-1].tokenSQL = TTokenTypes.tkUnknownToken) and (FTokens[i].tokenSQL = TTokenTypes.tkLeftBracket) then
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

  end;
  finally
    FreeAndNil(parser);
  end;
end;

end.
