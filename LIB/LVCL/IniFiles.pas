{-----------------------------------------------------------------------------
 Unit Name: IniFiles
 Date:      2010-07-07
 Purpose:
  DKW 제작을 위해 IniFiles 유니트가 필요해짐. TINIFile 객체 기본기능만 재정의. 

 History:
-----------------------------------------------------------------------------}


unit IniFiles;


interface

uses SysUtils, Classes;

type
  EIniFileException = class(Exception);

  TCustomIniFile = class(TObject)
  private
    FFileName: string;
  public
    constructor Create(const FileName: string);
    function SectionExists(const Section: string): Boolean;
    function ReadString(const Section, Ident, Default: string): string; virtual; abstract;
    procedure WriteString(const Section, Ident, Value: String); virtual; abstract;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; virtual;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); virtual;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; virtual;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); virtual;
    procedure ReadSection(const Section: string; Strings: TStrings); virtual; abstract;
    procedure ReadSections(Strings: TStrings); overload; virtual; abstract;
    procedure ReadSections(const Section: string; Strings: TStrings); overload; virtual;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); virtual; abstract;
    procedure EraseSection(const Section: string); virtual; abstract;
    procedure DeleteKey(const Section, Ident: String); virtual; abstract;
    procedure UpdateFile; virtual; abstract;
    function ValueExists(const Section, Ident: string): Boolean; virtual;
    property FileName: string read FFileName;
  end;

  TIniFile = class(TCustomIniFile)
  public
    destructor Destroy; override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;
  end;


implementation

uses
  Windows;

{ TCustomIniFile }

constructor TCustomIniFile.Create(const FileName: string);
begin
  FFileName := FileName;
end;

function TCustomIniFile.SectionExists(const Section: string): Boolean;
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    ReadSection(Section, S);
    Result := S.Count > 0;
  finally
    S.Free;
  end;
end;

function TCustomIniFile.ReadInteger(const Section, Ident: string;
  Default: Longint): Longint;
var
  IntStr: string;
begin
  IntStr := ReadString(Section, Ident, '');
  if (Length(IntStr) > 2) and (IntStr[1] = '0') and
     ((IntStr[2] = 'X') or (IntStr[2] = 'x')) then
    IntStr := '$' + Copy(IntStr, 3, Maxint);
  Result := StrToIntDef(IntStr, Default);
end;

procedure TCustomIniFile.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

function TCustomIniFile.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;


procedure TCustomIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
const
  Values: array[Boolean] of string = ('0', '1');
begin
  WriteString(Section, Ident, Values[Value]);
end;

function TCustomIniFile.ValueExists(const Section, Ident: string): Boolean;
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    ReadSection(Section, S);
    Result := S.IndexOf(Ident) > -1;
  finally
    S.Free;
  end;
end;

procedure TCustomIniFile.ReadSections(const Section: string;
  Strings: TStrings);
var
  I: Integer;
begin
  ReadSections(Strings);
  for I := Strings.Count - 1 downto 0 do
    if not SameText(Section, Copy(Strings[I], 1, Length(Section))) then
      Strings.Delete(I);
end;


{ TIniFile }

const
  SIniFileWriteError = 'Unable to write to %s';

destructor TIniFile.Destroy;
begin
  UpdateFile;         // flush changes to disk
  inherited Destroy;
end;

function TIniFile.ReadString(const Section, Ident, Default: string): string;
var
  Buffer: array[0..2047] of Char;
begin
  SetString(Result, Buffer, GetPrivateProfileString(PChar(Section),
    PChar(Ident), PChar(Default), Buffer, SizeOf(Buffer), PChar(FFileName)));
end;

procedure TIniFile.WriteString(const Section, Ident, Value: string);
begin
  if not WritePrivateProfileString(
    PChar(Section), PChar(Ident),
    PChar(Value), PChar(FFileName)
  ) then
    raise EIniFileException.CreateFmt(SIniFileWriteError, [FileName]);
end;

procedure TIniFile.ReadSections(Strings: TStrings);
const
  BufSize = 16384;
var
  Buffer, P: PChar;
begin
  GetMem(Buffer, BufSize);
  try
    Strings.Clear;
    if GetPrivateProfileString(nil, nil, nil, Buffer, BufSize,
      PChar(FFileName)) <> 0 then
    begin
      P := Buffer;
      while P^ <> #0 do
      begin
        Strings.Add(P);
        Inc(P, StrLen(P) + 1);
      end;
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

procedure TIniFile.ReadSection(const Section: string; Strings: TStrings);
const
  BufSize = 16384;
var
  Buffer, P: PChar;
begin
  GetMem(Buffer, BufSize);
  try
    Strings.Clear;
    if GetPrivateProfileString(PChar(Section), nil, nil, Buffer, BufSize,
      PChar(FFileName)) <> 0 then
    begin
      P := Buffer;
      while P^ <> #0 do
      begin
        Strings.Add(P);
        Inc(P, StrLen(P) + 1);
      end;
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

procedure TIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  KeyList: TStringList;
  I: Integer;
begin
  KeyList := TStringList.Create;
  try
    ReadSection(Section, KeyList);
    Strings.Clear;
    for I := 0 to KeyList.Count - 1 do
      Strings.Add(KeyList[I] + '=' + ReadString(Section, KeyList[I], ''))
  finally
    KeyList.Free;
  end;
end;

procedure TIniFile.EraseSection(const Section: string);
begin
  if not WritePrivateProfileString(PChar(Section), nil, nil, PChar(FFileName)) then
    raise EIniFileException.CreateFmt(SIniFileWriteError, [FileName]);
end;

procedure TIniFile.DeleteKey(const Section, Ident: String);
begin
  WritePrivateProfileString(PChar(Section), PChar(Ident), nil, PChar(FFileName));
end;

procedure TIniFile.UpdateFile;
begin
  WritePrivateProfileString(nil, nil, nil, PChar(FFileName));
end;

end.
