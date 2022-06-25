(*

  A program that searches for prices of goods

  Version: 0.8
  Author:  wanips

*)

unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.ExtCtrls,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.SyncObjs, System.IniFiles, Vcl.Menus, Vcl.Grids,
  uPriceSearch, uTranslator, XSuperJSON, XSuperObject, System.Generics.Collections, Vcl.ComCtrls,
  smtpsend, ssl_openssl, mimemess, mimepart, System.Net.HttpClient, Vcl.Samples.Spin, Generics.Defaults;

const
  APP_VERSION = '0.8';
  GOODS_FILE_NAME = 'goods.dat';
  SETTINGS_FILE_NAME = 'settings.ini';
  BLACKLIST_FILE_NAME = 'blacklist.dat';
  SEARCH_ENGINES_FILE_NAME = 'search.dat';
  PARSE_RULES_FILE_NAME = 'parse_rules.dat';
  LOCALIZE_FOLDER = 'Localizations';
  LOG_LIMIT = 3000;

type
  TGoodsData = class
  private
    FJsonData: ISuperObject;
    function GetCount: Integer;
  public
    property Count: Integer read GetCount;
    constructor Create;
    procedure First;
    function GetNext(out Value: TGoods): Boolean;
    procedure Clear;
    procedure Load(const FileName: string);
    procedure Save(const FileName: string);
    procedure Add(const Value: TGoods);
    procedure Remove(const Name: string);
    function Contains(const Name: string): Boolean;
    function TryGetGoods(const Name: string; out Goods: TGoods): Boolean;
  end;

  PSettingsData = ^TSettingsData;
  TSettingsData = record
    LangFile: string;
    SearchRequestCount: Integer;
    AutoModeInterval: Cardinal;
    PageLoadWaitTime: Cardinal;
    LowestPricesCount: Cardinal;
    SendNotifToTelegram: Boolean;
    SendNotifToEmail: Boolean;
    WindowPos: TPoint;
    MailTo: string;
    MailHost: string;
    MailPort: string;
    MailLogin: string;
    MailPass: string;
    TelegUserID: string;
    TelegBotToken: string;
  end;

  TSettings = class
  private
    FFile: TIniFile;
    FData: TSettingsData;
    function GetSettingsData: PSettingsData;
  public
    property Data: PSettingsData read GetSettingsData;
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  end;

  TAutoMode = class
  private
    FOnTic: TNotifyEvent;
    FOnPeriod: TNotifyEvent;
    FTimer: TTimer;
    FCounter: Integer;
    FPeriod: Integer;
    procedure ResetCounter;
    function GetIsEnabled: Boolean;
    procedure OnTimer(Sender: TObject);
    procedure DoTic;
    procedure DoPeriod;
  public
    property OnTic: TNotifyEvent read FOnTic write FOnTic;
    property OnPeriod: TNotifyEvent read FOnPeriod write FOnPeriod;
    property IsEnabled: Boolean read GetIsEnabled;
    property Counter: Integer read FCounter;
    constructor Create;
    destructor Destroy; override;
    procedure Enable(const PeriodSec: Cardinal);
    procedure Disable;
  end;

type
  TFormMain = class(TForm)
    ButtonAddGoods: TButton;
    ButtonRemoveGoods: TButton;
    ButtonStart: TButton;
    LabelAutoInterval: TLabel;
    MemoLog: TMemo;
    ButtonStartAuto: TButton;
    ButtonBlackList: TButton;
    ButtonSettings: TButton;
    ButtonStop: TButton;
    ButtonStartSelected: TButton;
    StatusBar: TStatusBar;
    MemoReports: TMemo;
    SpinEditAutoInterval: TSpinEdit;
    ButtonClearLog: TButton;
    ButtonClearReports: TButton;
    ButtonEditGoods: TButton;
    StringGridGoods: TStringGrid;
    procedure OnMessage(Sender: TObject; const Msg: TMessage; const Args: array of const);
    procedure OnReport(Sender: TObject; const Report: TReport);
    procedure OnGoodsProgress(Sender: TObject; const Complete, Total: Integer);
    procedure OnLinksProgress(Sender: TObject; const Complete, Total: Integer);
    procedure OnLoad(Sender: TObject; const Url: string);
    procedure OnLoadEnd(Sender: TObject; const Url, ResponseBody: string; const StatusCode: Integer);
    procedure OnLoadError(Sender: TObject; const Url: string; const StatusCode: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetTranslation;
    procedure Log(const Msg: string);
    procedure LogF(const Msg: string; const Args: array of const);
    procedure SendMail(const Subject, pTo, From, TextBody: string);
    procedure SendTelegramMessage(const UserID, BotToken, Text: string);
    procedure SetMailClientSettings(const TargetHost, TargetPort, UserName, Password: string);
    procedure ButtonAddGoodsClick(Sender: TObject);
    procedure AddGoods(const Value: TGoods);
    procedure RemoveGoods(const Value: TGoods);
    procedure ButtonRemoveGoodsClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonBlackListClick(Sender: TObject);
    procedure ButtonSettingsClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonStartSelectedClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ButtonStartAutoClick(Sender: TObject);
    procedure ButtonClearLogClick(Sender: TObject);
    procedure ButtonClearReportsClick(Sender: TObject);
    procedure ButtonEditGoodsClick(Sender: TObject);
    procedure SpinEditAutoIntervalChange(Sender: TObject);
  private
    FAutoMode: TAutoMode;
    FGoods: TGoodsData;
    FSMTPClient: TSMTPSend;
    FSearchEngineList: TStringList;
    procedure OnTimer(Sender: TObject);
    procedure OnPeriod(Sender: TObject);
    procedure OnStatus(Sender: TObject; const Status: TMainStatus);
    procedure OnStart(Sender: TObject);
    function ReportToText(const Value: TReport): string;
    function CanStart: Boolean;
    function GetCurrentTime: string;
    procedure StartFullSearch;
    procedure OnLowestPriceFound(Sender: TObject; const Report: TReport);
    function CanSendMessageToTelegram: Boolean;
    function CanSendEmail: Boolean;
    procedure SetStatus(const Value: string);
    procedure SetAutoModeStatus(const Text: string);
    procedure SetProgressLinks(const Text: string);
    procedure SetProgressGoods(const Text: string);
    function GetSelectedGoods(out Value: TGoods): Boolean;
    procedure UpdateGoodsGrid;
    procedure AddGoodsToGrid(const Value: TGoods);
  public
    { Public declarations }
  end;

function SearchEngineListToText(const Source: TSearchEngineList): string;
procedure TextToSearchEngineList(const Source: string; SearchEngineList: TSearchEngineList);

var
  FormMain: TFormMain;
  AppPath: string;
  Settings: TSettings;
  PriceSearcher: TPriceSearcher;

implementation

{$R *.dfm}

uses
  uEditGood, uBlackList, uSettings;

function SearchEngineDataToString(const SearchEngineData: TSearchEngineData): string;
begin
  Result := SearchEngineData.Url + ' ' + SearchEngineData.Regex;
end;

function StringToSearchEngineData(const Source: string; out SearchEngineData: TSearchEngineData): Boolean;
var
  DelimPos: Integer;
begin
  DelimPos := Pos(' ', Source);

  if DelimPos > 0 then
  begin
    SearchEngineData.Url := Source.Substring(0, DelimPos - 1);
    SearchEngineData.Regex := Source.Substring(DelimPos, Source.Length - DelimPos + 1);
  end;

  Result := (SearchEngineData.Url <> '') and (SearchEngineData.Regex <> '');
end;

function SearchEngineListToText(const Source: TSearchEngineList): string;
var
  SearchEngineData: TSearchEngineData;
begin
  Result := '';
  for SearchEngineData in Source do
  begin
    Result := Result + SearchEngineDataToString(SearchEngineData) + sLineBreak;
  end;
end;

procedure TextToSearchEngineList(const Source: string; SearchEngineList: TSearchEngineList);
var
  SearchEngineData: TSearchEngineData;
  Line: string;
begin
  SearchEngineList.Clear;
  for Line in Source.Split([#13, #10]) do
  begin
    if StringToSearchEngineData(Line, SearchEngineData) then
    begin
      SearchEngineList.Add(SearchEngineData);
    end;
  end;
end;

function StringListCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  i1, i2 : Integer;
begin
  i1 := StrToIntDef(List.Names[Index1], 0);
  i2 := StrToIntDef(List.Names[Index2], 0);
  Result := i1 - i2;
end;

procedure TFormMain.SetMailClientSettings(const TargetHost, TargetPort,
  UserName, Password: string);
begin
  FSMTPClient.TargetHost := TargetHost;
  FSMTPClient.TargetPort := TargetPort;
  FSMTPClient.UserName := UserName;
  FSMTPClient.Password := Password;
end;

procedure TFormMain.SendMail(const Subject, pTo, From, TextBody: string);
var
  Msg: TMimeMess;
  Body: TStringList;
  MIMEPart: TMimePart;
  IsSended: Boolean;
begin
  if (Subject <> '') and (pTo <> '') and (From <> '') and (TextBody <> '') then
  begin
    LogF(Translate('Messages.SendingEmailToTheRecipient'), [pTo]);

    Body := TStringList.Create;
    Body.Text := TextBody;

    Msg := TMimeMess.Create;
    Msg.Header.Subject := Subject;
    Msg.Header.From := From;
    Msg.Header.ToList.Add(pTo);

    MIMEPart := Msg.AddPartMultipart('alternative', nil);

    Msg.AddPartText(Body, MIMEPart);
    Msg.EncodeMessage;

    if FSMTPClient.Login then
      if FSMTPClient.AuthDone then
      begin
        IsSended := False;

        if FSMTPClient.MailFrom(From, From.Length) then
          if FSMTPClient.MailTo(pTo) then
            IsSended := FSMTPClient.MailData(Msg.Lines);

        if IsSended then
          Log(Translate('Messages.TheEmailIsSuccessfullySent'))
        else
          Log(Translate('Messages.ErrorOccurredWhileSendingEmail'));

        FSMTPClient.Logout;
      end;

    Msg.Free;
    Body.Free;
  end;
end;

procedure TFormMain.SendTelegramMessage(const UserID, BotToken, Text: string);
const
  TelegUrl = 'https://api.telegram.org/bot%s/sendmessage?chat_id=%s&text=%s';
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Url: string;
begin
  if (UserID <> '') and (BotToken <> '') and (Text <> '') then
  begin
    HttpClient := THTTPClient.Create;

    Url := Format(TelegUrl, [BotToken, UserID, Text]);
    Response := HttpClient.Get(Url);

    if Response.StatusCode = 200 then
      Log(Translate('Messages.MessageSuccessfullySentToTelegram'))
    else
      LogF(Translate('Messages.ErrorWhenSendingMessageToTelegram'), [Response.StatusCode]);

    HttpClient.Free;
  end;
end;

{ TGoods }

procedure TGoodsData.Add(const Value: TGoods);
var
  Info: ISuperArray;
begin
  Remove(Value.Name);

  Info := FJsonData.A[Value.Name];
  Info.S[0] := Value.Question;
  Info.I[1] := Value.Price;
end;

procedure TGoodsData.Clear;
begin
  FJsonData := TSuperObject.Create('{}');
end;

function TGoodsData.Contains(const Name: string): Boolean;
var
  Good: TGoods;
begin
  Result := TryGetGoods(Name, Good);
end;

constructor TGoodsData.Create;
begin
  Clear;
end;

procedure TGoodsData.First;
begin
  FJsonData.First;
end;

function TGoodsData.GetCount: Integer;
begin
  Result := FJsonData.Count;
end;

function TGoodsData.GetNext(out Value: TGoods): Boolean;
begin
  Result := not FJsonData.EoF;
  if Result then
  begin
    TryGetGoods(FJsonData.CurrentKey, Value);
    FJsonData.Next;
  end;
end;

procedure TGoodsData.Load(const FileName: string);
begin
  if FileExists(FileName) then
  begin
    FJsonData := TSuperObject.ParseFile(FileName);
  end;
end;

procedure TGoodsData.Remove(const Name: string);
begin
  FJsonData.Remove(Name);
end;

procedure TGoodsData.Save(const FileName: string);
begin
  FJsonData.SaveTo(FileName);
end;

function TGoodsData.TryGetGoods(const Name: string; out Goods: TGoods): Boolean;
var
  Info: ISuperArray;
begin
  Result := FJsonData.Contains(Name);
  if Result then
  begin
    Info := FJsonData.A[Name];
    Goods.Name := Name;
    Goods.Question := Info.S[0];
    Goods.Price := Info.I[1];
  end;
end;

procedure TFormMain.Log(const Msg: string);
begin
  if MemoLog.Lines.Count > LOG_LIMIT then
    MemoLog.Clear;

  if Msg <> '' then
    MemoLog.Lines.Add('[' + GetCurrentTime + '] ' + Msg);
end;

procedure TFormMain.LogF(const Msg: string; const Args: array of const);
begin
  Log(Format(Msg, Args));
end;

procedure TFormMain.OnGoodsProgress(Sender: TObject; const Complete, Total: Integer);
begin
  SetProgressGoods(TranslateF('Messages.GoodsProgress', [Complete, Total]));
end;

procedure TFormMain.OnLinksProgress(Sender: TObject; const Complete, Total: Integer);
begin
  SetProgressLinks(TranslateF('Messages.LinkProgress', [Complete, Total]));
end;

procedure TFormMain.OnLoad(Sender: TObject; const Url: string);
begin
  LogF(Translate('Messages.LoadingWebPage'), [Url]);
end;

procedure TFormMain.OnLoadEnd(Sender: TObject; const Url, ResponseBody: string; const StatusCode: Integer);
begin
  //
end;

procedure TFormMain.OnLoadError(Sender: TObject; const Url: string; const StatusCode: Integer);
begin
  Log(Translate('Messages.WebPageLoadingError'));
end;

procedure TFormMain.OnLowestPriceFound(Sender: TObject; const Report: TReport);
var
  Text: string;
begin
  if Settings.Data.SendNotifToTelegram or Settings.Data.SendNotifToEmail then
    LogF(Translate('Messages.PricesBelowTheLimitWereFound'), [Report.GoodsName]);

  Text := ReportToText(Report);

  if Settings.Data.SendNotifToTelegram then
  begin
    SendTelegramMessage(Settings.Data.TelegUserID, Settings.Data.TelegBotToken, Text);
  end;

  if Settings.Data.SendNotifToEmail then
  begin
    SendMail(Translate('Messages.TheLowestPriceIsFound'), Settings.Data.MailTo, Settings.Data.MailLogin, Text);
  end;

end;

procedure TFormMain.OnMessage(Sender: TObject; const Msg: TMessage; const Args: array of const);
begin
  case Msg of
    msgFoundLinks: LogF(Translate('Messages.FoundLinks'), Args);
    msgTotalLinksFound: LogF(Translate('Messages.TotalLinksFound'), Args);
    msgParseUrlTemplateError: LogF(Translate('Messages.TemplateParsingError'), Args);
    msgPriceFound: LogF(Translate('Messages.FoundPrice'), Args);
    msgPriceNotFound: LogF(Translate('Messages.PriceNotFound'), Args);
    msgGoodsPriceFoundCount: LogF(Translate('Messages.PricesFoundForGoods'), Args);
    msgGoodsPriceNotFound: LogF(Translate('Messages.NoPricesFoundForGoods'), Args);
    msgCantStart: Log(Translate('Messages.UnableToStart'));
    msgPriceSearching: LogF(Translate('Messages.SearchingForPrices'), Args);
  end
end;

procedure TFormMain.OnReport(Sender: TObject; const Report: TReport);
var
  Text: string;
begin
  Text := ReportToText(Report);
  MemoReports.Lines.Add(Text);
end;

procedure TFormMain.OnStart(Sender: TObject);
var
  SearchSettings: TSearchSettings;
begin
  SearchSettings.ResultCount := Settings.Data.SearchRequestCount;
  SearchSettings.LowestPricesCount := Settings.Data.LowestPricesCount;

  PriceSearcher.SearchSettings := SearchSettings;
  PriceSearcher.Browser.AfterLoadTimeout := Settings.Data.PageLoadWaitTime;
end;

procedure TFormMain.OnStatus(Sender: TObject; const Status: TMainStatus);
begin
  case Status of
    TMainStatus.Free:
      SetStatus(Translate('Status.Ready'));

    TMainStatus.Running:
      SetStatus(Translate('Status.Running'));

    TMainStatus.Completed:
      SetStatus(Translate('Status.Completed'));

    TMainStatus.Stopped:
      SetStatus(Translate('Status.Stopped'));
  end;
end;

procedure TFormMain.OnTimer(Sender: TObject);
var
  Remain: string;
begin
  Remain := FormatDateTime('hh:nn:ss', FAutoMode.Counter / SecsPerDay);
  SetAutoModeStatus(TranslateF('Messages.AutoModeIsTurnedOn', [Remain]));
end;

procedure TFormMain.OnPeriod(Sender: TObject);
begin
  StartFullSearch;
end;

procedure TFormMain.AddGoods(const Value: TGoods);
begin
  FGoods.Add(Value);
  UpdateGoodsGrid;
end;

procedure TFormMain.AddGoodsToGrid(const Value: TGoods);
var
  Row: Integer;
begin
  StringGridGoods.RowCount := StringGridGoods.RowCount + 1;
  Row := StringGridGoods.RowCount - 1;

  StringGridGoods.Cells[0, Row] := Value.Name;
  StringGridGoods.Cells[1, Row] := Value.Question;
  StringGridGoods.Cells[2, Row] := Value.Price.ToString;
end;

procedure TFormMain.ButtonAddGoodsClick(Sender: TObject);
begin
  FormEditGood.Clear;
  FormEditGood.ShowModal;
end;

procedure TFormMain.ButtonBlackListClick(Sender: TObject);
begin
  FormBlackList.Load(PriceSearcher.BlackList);
  FormBlackList.ShowModal;
end;

procedure TFormMain.ButtonClearLogClick(Sender: TObject);
begin
  MemoLog.Clear;
end;

procedure TFormMain.ButtonClearReportsClick(Sender: TObject);
begin
  MemoReports.Clear;
end;

procedure TFormMain.ButtonEditGoodsClick(Sender: TObject);
var
  Goods: TGoods;
begin
  if GetSelectedGoods(Goods) then
  begin
    FormEditGood.Load(Goods);
    FormEditGood.ShowModal;
  end;
end;

procedure TFormMain.ButtonRemoveGoodsClick(Sender: TObject);
var
  Goods: TGoods;
begin
  if GetSelectedGoods(Goods) then
    if MessageBox(Self.Handle, PChar(Translate('Messages.DeleteThisGood')),
      PChar(Translate('Messages.Removing')),
      MB_YESNO or MB_ICONINFORMATION) = IDYES then
      begin
        FGoods.Remove(Goods.Name);
        UpdateGoodsGrid;
      end;
end;

procedure TFormMain.ButtonSettingsClick(Sender: TObject);
begin
  FormSettings.Load;
  FormSettings.Show;
end;

procedure TFormMain.ButtonStartAutoClick(Sender: TObject);
begin
  FAutoMode.Enable(Settings.Data.AutoModeInterval * 60);
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);
begin
  StartFullSearch;
end;

procedure TFormMain.ButtonStartSelectedClick(Sender: TObject);
var
  Goods: TGoods;
begin
  if GetSelectedGoods(Goods) then
  begin
    if CanStart then
      PriceSearcher.Start([Goods]);
  end;
end;

procedure TFormMain.ButtonStopClick(Sender: TObject);
begin
  FAutoMode.Disable;
  SetAutoModeStatus('');

  if PriceSearcher.Status = TMainStatus.Running then
  begin
    PriceSearcher.Stop;
    Log(Translate('Messages.Stopped'));
  end;
end;

function TFormMain.CanSendEmail: Boolean;
begin
  Result := (FSMTPClient.TargetHost <> '') and (FSMTPClient.TargetPort <> '') and
    (FSMTPClient.UserName <> '') and (FSMTPClient.Password <> '');
end;

function TFormMain.CanSendMessageToTelegram: Boolean;
begin
  Result := (Settings.Data.TelegUserID <> '') and
    (Settings.Data.TelegBotToken <> '');
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if PriceSearcher.Status = Running then
  begin
    CanClose := False;
    Log(Translate('Messages.StopCheckBeforeClosing'));
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  AppPath := ExtractFilePath(ParamStr(0));

  Settings := TSettings.Create(AppPath + SETTINGS_FILE_NAME);
  Settings.Load;

  SpinEditAutoInterval.Value := Settings.Data.AutoModeInterval;

  FGoods := TGoodsData.Create;
  FGoods.Load(AppPath + GOODS_FILE_NAME);

  PriceSearcher := TPriceSearcher.Create;
  PriceSearcher.OnMessage := OnMessage;
  PriceSearcher.OnStatus := OnStatus;
  PriceSearcher.OnGoodsProgress := OnGoodsProgress;
  PriceSearcher.OnLinksProgress := OnLinksProgress;
  PriceSearcher.Browser.OnLoad := OnLoad;
  PriceSearcher.Browser.OnLoadEnd := OnLoadEnd;
  PriceSearcher.Browser.OnLoadError := OnLoadError;

  PriceSearcher.Reports.OnAdd := OnReport;
  PriceSearcher.OnLowestPriceFound := OnLowestPriceFound;
  PriceSearcher.Browser.AfterLoadTimeout := Settings.Data.PageLoadWaitTime;

  FSearchEngineList := TStringList.Create;
  if FileExists(AppPath + SEARCH_ENGINES_FILE_NAME) then
  begin
    FSearchEngineList.LoadFromFile(AppPath + SEARCH_ENGINES_FILE_NAME);
    TextToSearchEngineList(FSearchEngineList.Text, PriceSearcher.SearchEngineList);
  end;

  if FileExists(AppPath + PARSE_RULES_FILE_NAME) then
    PriceSearcher.PriceSearchRegexList.LoadFromFile(AppPath + PARSE_RULES_FILE_NAME);

  if FileExists(AppPath + BLACKLIST_FILE_NAME) then
    PriceSearcher.BlackList.LoadFromFile(AppPath + BLACKLIST_FILE_NAME);

  UpdateGoodsGrid;

  FAutoMode := TAutoMode.Create;
  FAutoMode.OnTic := OnTimer;
  FAutoMode.OnPeriod := OnPeriod;

  FSMTPClient := TSMTPSend.Create;
  FSMTPClient.Timeout := 3000;
  FSMTPClient.AutoTLS := False;
  FSMTPClient.FullSSL := true;

  SetMailClientSettings(Settings.Data.MailHost, Settings.Data.MailPort,
    Settings.Data.MailLogin, Settings.Data.MailPass);

  Translator.LocalesPath := AppPath + LOCALIZE_FOLDER;
  Translator.TryLoadLocale(Settings.Data.LangFile);

  SetTranslation;

  Caption := Caption + ' ' + APP_VERSION;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FSMTPClient.Free;

  FAutoMode.Free;

  FSearchEngineList.Text := SearchEngineListToText(PriceSearcher.SearchEngineList);
  FSearchEngineList.SaveToFile(AppPath + SEARCH_ENGINES_FILE_NAME);
  FSearchEngineList.Free;

  PriceSearcher.PriceSearchRegexList.SaveToFile(AppPath + PARSE_RULES_FILE_NAME);
  PriceSearcher.BlackList.SaveToFile(AppPath + BLACKLIST_FILE_NAME);
  PriceSearcher.Free;

  FGoods.Save(AppPath + GOODS_FILE_NAME);
  FGoods.Free;

  Settings.Save;
  Settings.Free;

end;

function TFormMain.GetCurrentTime: string;
begin
  Result := FormatDateTime('hh:nn:ss', Now)
end;

function TFormMain.GetSelectedGoods(out Value: TGoods): Boolean;
var
  Index: Integer;
begin
  Index := StringGridGoods.Row;

  if Index >= 0 then
  begin
    Result := FGoods.TryGetGoods(StringGridGoods.Cells[0, Index], Value);
  end
    else
  Log(Translate('Messages.NoSelectedGoods'));
end;

procedure TFormMain.RemoveGoods(const Value: TGoods);
begin
  FGoods.Remove(Value.Name);
  UpdateGoodsGrid;
end;

function TFormMain.ReportToText(const Value: TReport): string;
var
  PriceUrl: TPriceUrl;
  Title: string;
  Body: string;
begin
  Title := '[' + GetCurrentTime + ']' + sLineBreak + TranslateF('Messages.LowPricesForTheGoodsWasFound', [Value.GoodsName]);

  Body := '';
  for PriceUrl in Value.PriceUrlList do
    Body := Body + PriceUrl.Price.ToString + ' ' + PriceUrl.Url + sLineBreak;

  Result := Title + sLineBreak + Body;
end;

procedure TFormMain.SetAutoModeStatus(const Text: string);
begin
  StatusBar.Panels[3].Text := Text;
end;

procedure TFormMain.SetProgressGoods(const Text: string);
begin
  StatusBar.Panels[1].Text := Text;
end;

procedure TFormMain.SetProgressLinks(const Text: string);
begin
  StatusBar.Panels[2].Text := Text;
end;

procedure TFormMain.SetStatus(const Value: string);
begin
  StatusBar.Panels[0].Text := Value;
end;

procedure TFormMain.SpinEditAutoIntervalChange(Sender: TObject);
begin
  Settings.Data.AutoModeInterval := SpinEditAutoInterval.Value;
end;

function TFormMain.CanStart: Boolean;
begin
  Result := PriceSearcher.Status <> Running;

  if not Result then
    Log(Translate('Messages.CantStartWork'));
end;

procedure TFormMain.StartFullSearch;
var
  Goods: TGoods;
  List: TGoodsList;
begin
  if CanStart then
  begin
    List := [];
    FGoods.First;
    while FGoods.GetNext(Goods) do
    begin
      List := List + [Goods];
    end;

    if Length(List) > 0 then
    begin
      PriceSearcher.Start(List);
    end;
  end;
end;

procedure TFormMain.UpdateGoodsGrid;
var
  Goods: TGoods;
begin
  StringGridGoods.RowCount := 0;

  if FGoods.Count > 0 then
  begin
    FGoods.First;

    while FGoods.GetNext(Goods) do
    begin
      AddGoodsToGrid(Goods);
    end;
  end;
end;

procedure TFormMain.SetTranslation;
begin
  ButtonAddGoods.Caption := Translate('Buttons.Add');
  ButtonEditGoods.Caption := Translate('Buttons.Edit');
  ButtonRemoveGoods.Caption := Translate('Buttons.Remove');
  ButtonStart.Caption := Translate('Buttons.Start');
  ButtonStartAuto.Caption := Translate('Buttons.StartAuto');
  ButtonStop.Caption := Translate('Buttons.Stop');
  ButtonStartSelected.Caption := Translate('Buttons.StartSelected');
  ButtonSettings.Caption := Translate('Buttons.Settings');
  ButtonBlackList.Caption := Translate('Buttons.BlackList');

  LabelAutoInterval.Caption := Translate('Labels.AutoInterval');

  StringGridGoods.Cells[0, 0] := Translate('Labels.Name');
  StringGridGoods.Cells[1, 0] := Translate('Labels.Request');
  StringGridGoods.Cells[2, 0] := Translate('Labels.Price');

end;

{ TSettings }

constructor TSettings.Create(const FileName: string);
begin
  FFile := TIniFile.Create(FileName);
end;

destructor TSettings.Destroy;
begin
  FFile.Free;
  inherited;
end;

function TSettings.GetSettingsData: PSettingsData;
begin
  Result := @FData;
end;

procedure TSettings.Load;
begin
  FData.LangFile := FFile.ReadString('Main', 'LangFile', 'english.lng');
  FData.SearchRequestCount := FFile.ReadInteger('Main', 'SearchRequestCount', 10);
  FData.AutoModeInterval := FFile.ReadInteger('Main', 'AutoModeInterval', 60);
  FData.PageLoadWaitTime := FFile.ReadInteger('Main', 'PageLoadWaitTime', 1000);
  FData.LowestPricesCount := FFile.ReadInteger('Main', 'LowestPricesCount', 3);
  FData.SendNotifToTelegram := FFile.ReadBool('Main', 'SendNotifToTelegram', False);
  FData.SendNotifToEmail := FFile.ReadBool('Main', 'SendNotifToEmail', False);

  FData.WindowPos.X := FFile.ReadInteger('Main', 'WindowPosX', 100);
  FData.WindowPos.Y := FFile.ReadInteger('Main', 'WindowPosY', 100);

  FData.MailTo := FFile.ReadString('Mail', 'MailTo', '');
  FData.MailHost := FFile.ReadString('Mail', 'Host', '');
  FData.MailPort := FFile.ReadString('Mail', 'Port', '');
  FData.MailLogin := FFile.ReadString('Mail', 'Login', '');
  FData.MailPass := FFile.ReadString('Mail', 'Pass', '');

  FData.TelegUserID := FFile.ReadString('Telegram', 'UserID', '');
  FData.TelegBotToken := FFile.ReadString('Telegram', 'BotToken', '');
end;

procedure TSettings.Save;
begin
  FFile.WriteString('Main', 'LangFile', FData.LangFile);
  FFile.WriteInteger('Main', 'SearchRequestCount', FData.SearchRequestCount);
  FFile.WriteInteger('Main', 'AutoModeInterval', FData.AutoModeInterval);
  FFile.WriteInteger('Main', 'PageLoadWaitTime', FData.PageLoadWaitTime);
  FFile.WriteInteger('Main', 'LowestPricesCount', FData.LowestPricesCount);
  FFile.WriteBool('Main', 'SendNotifToTelegram', FData.SendNotifToTelegram);
  FFile.WriteBool('Main', 'SendNotifToEmail', FData.SendNotifToEmail);

  FFile.WriteInteger('Main', 'WindowPosX', FData.WindowPos.X);
  FFile.WriteInteger('Main', 'WindowPosY', FData.WindowPos.Y);

  FFile.WriteString('Mail', 'MailTo', FData.MailTo);
  FFile.WriteString('Mail', 'Host', FData.MailHost);
  FFile.WriteString('Mail', 'Port', FData.MailPort);
  FFile.WriteString('Mail', 'Login', FData.MailLogin);
  FFile.WriteString('Mail', 'Pass', FData.MailPass);

  FFile.WriteString('Telegram', 'UserID', FData.TelegUserID);
  FFile.WriteString('Telegram', 'BotToken', FData.TelegBotToken);
end;

{ TAutoMode }

constructor TAutoMode.Create;
begin
  FOnTic := nil;
  FOnPeriod := nil;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := MSecsPerSec;
  FTimer.OnTimer := OnTimer;

  FPeriod := 0;
  Disable;
end;

destructor TAutoMode.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TAutoMode.Disable;
begin
  ResetCounter;
  FTimer.Enabled := False;
end;

procedure TAutoMode.DoPeriod;
begin
  if Assigned(FOnPeriod) then
    FOnPeriod(Self);
end;

procedure TAutoMode.DoTic;
begin
  if Assigned(FOnTic) then
    FOnTic(Self);
end;

procedure TAutoMode.Enable(const PeriodSec: Cardinal);
begin
  if PeriodSec > 1 then
  begin
    FPeriod := PeriodSec;
    ResetCounter;
    FTimer.Enabled := True;
  end;
end;

function TAutoMode.GetIsEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TAutoMode.ResetCounter;
begin
  FCounter := FPeriod;
end;

procedure TAutoMode.OnTimer(Sender: TObject);
begin
  Dec(FCounter);

  DoTic;

  if FCounter <= 0 then
  begin
    DoPeriod;
    ResetCounter;
  end;
end;

end.
