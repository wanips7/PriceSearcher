// Simple translator
// Author: wanips
// Version: 0.9

unit uTranslator;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.IniFiles, System.IOUtils,
  System.NetEncoding;

const
  SECTION_KEY_DELIM = '.';
  LOCALE_FILE_EXT = 'lng';

const
  DELIM_NOT_FOUND = 'Delimiter not found (string: %s)';
  LOCALES_PATH_IS_NOT_SET = 'Locales path is not set';
  LOCALE_IS_NOT_LOADED = 'Locale file is not loaded';
  TRANSLATION_NOT_FOUND = 'Translation not found';

type
  TStringArray = TArray<string>;

type
  ETranslatorError = class(Exception);

type
  PLocaleInfo = ^TLocaleInfo;
  TLocaleInfo = record
    Author: string;
    Icon: TBytes;
    FileName: string;
    Lang: string;
    Version: string;
  end;

type
  TOnLoadLocale = procedure(const LocaleInfo: TLocaleInfo) of object;
  TOnNoTranslation = procedure(const Section, Key: string) of object;

type
  TLocaleInfoList = TArray<TLocaleInfo>;

type
  TTranslator = record
  strict private
    FLocaleFile: TMemIniFile;
    FLocaleInfo: TLocaleInfo;
    FLocalesPath: string;
    FOnLoadLocale: TOnLoadLocale;
    FOnNoTranslation: TOnNoTranslation;
    function HasLocalesPath: Boolean;
    procedure DoLoadLocale(const Value: TLocaleInfo);
    procedure DoNoTranslation(const Section, Key: string);
    function GetLocaleInfo: PLocaleInfo;
    procedure SetLocalesPath(const Value: string);
  public
    property OnLoadLocale: TOnLoadLocale read FOnLoadLocale write FOnLoadLocale;
    property OnNoTranslation: TOnNoTranslation read FOnNoTranslation write FOnNoTranslation;
    property LocalesPath: string read FLocalesPath write SetLocalesPath;
    property LocaleInfo: PLocaleInfo read GetLocaleInfo;
    class operator Initialize(out Dest: TTranslator);
    class operator Finalize(var Dest: TTranslator);
    function GetFileNameByLang(const Value: string): string;
    function GetLangList: TStringArray;
    function GetLocaleInfoList: TLocaleInfoList;
    function HasLocale: Boolean;
    function Translate(const Section, Key: string): string;
    function TranslateF(const Section, Key: string; const Args: array of const): string;
    function TryGetLocaleInfo(const FileName: string; out LocaleInfo: TLocaleInfo): Boolean; overload;
    function TryLoadLocale(const FileName: string): Boolean;

  end;

function Translate(const SectionDotKey: string): string;
function TranslateF(const SectionDotKey: string; const Args: array of const): string;

var
  Translator: TTranslator;

implementation

function Translate(const SectionDotKey: string): string;
var
  Section, Key: string;
  DotPos: Integer;
begin
  Result := EmptyStr;

  DotPos := Pos(SECTION_KEY_DELIM, SectionDotKey);
  if DotPos > 0 then
  begin
    Section := SectionDotKey.Substring(0, DotPos - 1);
    Key := SectionDotKey.Substring(DotPos, SectionDotKey.Length - DotPos + 1);

    if not Section.IsEmpty and not Key.IsEmpty then
      Result := Translator.Translate(Section, Key);
  end
    else
  raise ETranslatorError.CreateFmt(DELIM_NOT_FOUND, [SectionDotKey]);

end;

function TranslateF(const SectionDotKey: string; const Args: array of const): string;
begin
  Result := Format(Translate(SectionDotKey), Args);
end;

{ TTranslator }

function TTranslator.HasLocale: Boolean;
begin
  Result := Assigned(FLocaleFile);
end;

function TTranslator.HasLocalesPath: Boolean;
begin
  Result := not FLocalesPath.IsEmpty;
end;

class operator TTranslator.Initialize(out Dest: TTranslator);
begin
  Dest.FOnLoadLocale := nil;
  Dest.FOnNoTranslation := nil;
  Dest.FLocaleFile := nil;
  Dest.FLocaleInfo := Default(TLocaleInfo);
  Dest.FLocalesPath := EmptyStr;
end;

class operator TTranslator.Finalize(var Dest: TTranslator);
begin
  FreeAndNil(Dest.FLocaleFile);
end;

procedure TTranslator.DoLoadLocale(const Value: TLocaleInfo);
begin
  if Assigned(FOnLoadLocale) then
    FOnLoadLocale(Value);
end;

procedure TTranslator.DoNoTranslation(const Section, Key: string);
begin
  if Assigned(FOnNoTranslation) then
    FOnNoTranslation(Section, Key);
end;

function TTranslator.GetLocaleInfo: PLocaleInfo;
begin
  Result := @FLocaleInfo;
end;

procedure TTranslator.SetLocalesPath(const Value: string);
begin
  if DirectoryExists(Value) then
    FLocalesPath := IncludeTrailingBackslash(Value);
end;

function TTranslator.Translate(const Section, Key: string): string;
begin
  if HasLocale then
  begin
    Result := FLocaleFile.ReadString(Section, Key, EmptyStr);

    if Result = EmptyStr then
    begin
      Result := TRANSLATION_NOT_FOUND;
      DoNoTranslation(Section, Key);
    end;
  end
    else
  raise ETranslatorError.Create(LOCALE_IS_NOT_LOADED);
end;

function TTranslator.TranslateF(const Section, Key: string; const Args: array of const): string;
begin
  Translate(Section, Format(Key, Args));
end;

function TTranslator.GetFileNameByLang(const Value: string): string;
var
  LocaleInfo: TLocaleInfo;
begin
  Result := EmptyStr;

  if HasLocalesPath then
    for LocaleInfo in GetLocaleInfoList do
      if LocaleInfo.Lang = Value then
      begin
        Result := LocaleInfo.FileName;
        Break;
      end;
end;

function TTranslator.GetLangList: TStringArray;
var
  LocaleInfo: TLocaleInfo;
begin
  Result := [];

  if HasLocalesPath then
    for LocaleInfo in GetLocaleInfoList do
    begin
      Result := Result + [LocaleInfo.Lang];
    end;
end;

function TTranslator.TryLoadLocale(const FileName: string): Boolean;
begin
  Result := False;
  FreeAndNil(FLocaleFile);

  if HasLocalesPath then
  begin
    FLocaleInfo := Default(TLocaleInfo);

    Result := TryGetLocaleInfo(FileName, FLocaleInfo);

    if Result then
    begin
      FLocaleFile := TMemIniFile.Create(FLocalesPath + FileName);
      DoLoadLocale(FLocaleInfo);
    end;

  end
    else
  raise ETranslatorError.Create(LOCALES_PATH_IS_NOT_SET);
end;

function TTranslator.TryGetLocaleInfo(const FileName: string; out LocaleInfo: TLocaleInfo): Boolean;
var
  LocaleFile: TMemIniFile;
  IconBase64: string;
begin
  Result := False;
  LocaleInfo := Default(TLocaleInfo);

  if HasLocalesPath then
  begin
    LocaleInfo.FileName := FileName;
    if FileExists(FLocalesPath + FileName) then
    begin
      LocaleFile := TMemIniFile.Create(FLocalesPath + FileName);

      LocaleInfo.Lang := LocaleFile.ReadString('Info', 'Lang', EmptyStr);
      if not LocaleInfo.Lang.IsEmpty then
      begin
        LocaleInfo.Version := LocaleFile.ReadString('Info', 'Version', EmptyStr);
        LocaleInfo.Author := LocaleFile.ReadString('Info', 'Author', EmptyStr);

        IconBase64 := LocaleFile.ReadString('Info', 'Icon', EmptyStr);
        if not IconBase64.IsEmpty then
          LocaleInfo.Icon := TNetEncoding.Base64.DecodeStringToBytes(IconBase64);

        Result := True;
      end;

      LocaleFile.Free;
    end;

  end;
end;

function TTranslator.GetLocaleInfoList: TLocaleInfoList;
var
  LocaleInfo: TLocaleInfo;
  FilePath: string;
  FileName: string;
begin
  Result := [];

  if HasLocalesPath then
    for FilePath in TDirectory.GetFiles(FLocalesPath, '*.' + LOCALE_FILE_EXT, TSearchOption.soTopDirectoryOnly) do
    begin
      FileName := ExtractFileName(FilePath);

      if TryGetLocaleInfo(FileName, LocaleInfo) then
        Result := Result + [LocaleInfo];
    end;
end;

end.
