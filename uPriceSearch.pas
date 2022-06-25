unit uPriceSearch;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Threading, System.SyncObjs,
  RegularExpressions, NetEncoding, Generics.Collections, System.Generics.Defaults, uSyncCEFBrowser;

const
  URL_TEMPLATE_REQUEST = '{request}';
  URL_TEMPLATE_COUNT = '{count}';

type
  TMainStatus = (Free, Running, Completed, Stopped);

  TMessage = (
    msgCantStart, // Error: Unable to start price collection
    msgFoundLinks, // 'Found %d links'
    msgGoodsPriceFoundCount, // %d prices found for goods %s
    msgGoodsPriceNotFound, // No prices found for goods %s
    msgParseUrlTemplateError, // 'Parse url template error: %s'
    msgPriceFound, // Found price (%d)
    msgPriceNotFound, // Price not found
    msgPriceSearching, // Search prices for goods %s
    msgTotalLinksFound // 'Total links found: %d'
  );

type
  TGoods = record
    Name: string;
    Question: string;
    Price: Cardinal;
  end;

type
  TGoodsList = TArray<TGoods>;

type
  TSearchSettings = record
    ResultCount: Integer;
    LowestPricesCount: Cardinal;
  end;

type
  TSearchEngineData = record
    Url: string;
    Regex: string;
  end;

type
  TPriceUrl = record
    Price: Cardinal;
    Url: string;
  end;

type
  TPriceUrlList = TArray<TPriceUrl>;

type
  TReport = record
    GoodsName: string;
    PriceUrlList: TPriceUrlList;
  end;

type
  TSearchEngineList = TList<TSearchEngineData>;

type
  TOnAddReport = procedure(Sender: TObject; const Report: TReport) of object;
  TOnLowestPriceFound = procedure(Sender: TObject; const Report: TReport) of object;
  TOnMessage = procedure(Sender: TObject; const Msg: TMessage; const Args: array of const) of object;
  TOnProgress = procedure(Sender: TObject; const Complete, Total: Integer) of object;
  TOnStatus = procedure(Sender: TObject; const Status: TMainStatus) of object;

type
  TReports = class(TList<TReport>)
  private
    FOnAdd: TOnAddReport;
    procedure Notify(Sender: TObject; const Report: TReport; Action: TCollectionNotification);
    procedure DoAdd(Sender: TObject; const Report: TReport);
  public
    property OnAdd: TOnAddReport read FOnAdd write FOnAdd;
    constructor Create;
  end;

type
  TPriceSearcher = class
  private
    FOnMessage: TOnMessage;
    FOnGoodsProgress: TOnProgress;
    FOnLinksProgress: TOnProgress;
    FOnLowestPriceFound: TOnLowestPriceFound;
    FOnStart: TNotifyEvent;
    FOnComplete: TNotifyEvent;
    FOnStatus: TOnStatus;
    FBlackList: TStringList;
    FFoundLinks: TStringList;
    FPriceList: TList<TPriceUrl>;
    FPriceSearchRegexList: TStringList;
    FReports: TReports;
    FSearchEngineList: TSearchEngineList;
    FSearchSettings: TSearchSettings;
    FBrowser: TSyncCEFBrowser;
    FTask: ITask;
    FStatus: TMainStatus;
    FStop: Boolean;
    function BuildReport(const GoodName: string): TReport;
    function CanStart: Boolean;
    function GetStatus: TMainStatus;
    procedure CollectLinks(const Good: TGoods);
    function FindPriceInText(const Text: string): Single;
    function InBlackList(const Url: string): Boolean;
    procedure DoStart;
    procedure DoComplete;
    procedure DoMessage(const Value: TMessage);
    procedure DoMessageF(const Value: TMessage; const Args: array of const);
    procedure DoLowestPriceFound(const Report: TReport);
    procedure DoGoodsProgress(const Completed, Total: Integer);
    procedure DoLinksProgress(const Completed, Total: Integer);
    procedure DoStatus(Value: TMainStatus);
  public
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnMessage: TOnMessage read FOnMessage write FOnMessage;
    property OnLinksProgress: TOnProgress read FOnLinksProgress write FOnLinksProgress;
    property OnLowestPriceFound: TOnLowestPriceFound read FOnLowestPriceFound write FOnLowestPriceFound;
    property OnGoodsProgress: TOnProgress read FOnGoodsProgress write FOnGoodsProgress;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnStatus: TOnStatus read FOnStatus write FOnStatus;
    property PriceSearchRegexList: TStringList read FPriceSearchRegexList;
    property SearchEngineList: TSearchEngineList read FSearchEngineList;
    property SearchSettings: TSearchSettings read FSearchSettings write FSearchSettings;
    property Status: TMainStatus read GetStatus;
    property Browser: TSyncCEFBrowser read FBrowser write FBrowser;
    property BlackList: TStringList read FBlackList;
    property Reports: TReports read FReports;
    constructor Create;
    destructor Destroy; override;
    function FindGoodsPrice(const Url: string): Cardinal;
    procedure FindGoodsPrices(const Value: TGoods);
    procedure Start(const GoodsList: TGoodsList);
    procedure Stop;
  end;

function IsUrl(const Url: string): Boolean;
function GetHostUrl(const Url: string): string;

implementation

function IsUrl(const Url: string): Boolean;
begin
  Result := (Url.StartsWith('http://') or Url.StartsWith('https://')) and Url.Contains('.');
end;

function GetHostUrl(const Url: string): string;
var
  SlashPos: Integer;
begin
  Result := Url;

  if IsUrl(Result) then
  begin
    SlashPos := Pos('/', Result, 9);
    if SlashPos > 0 then
      SetLength(Result, SlashPos - 1);
  end;
end;

function ComparePrices(const Price1, Price2: TPriceUrl): Integer;
begin
   Result := Price1.Price - Price2.Price;
end;

{ TBrowserTask }

function TPriceSearcher.BuildReport(const GoodName: string): TReport;
var
  i: Integer;
begin
  FPriceList.Sort(
  TComparer<TPriceUrl>.Construct(
    function(const Left, Right: TPriceUrl): Integer
    begin
      Result := Left.Price - Right.Price;
    end
  )
  );

  Result.GoodsName := GoodName;
  Result.PriceUrlList := [];

  if FPriceList.Count > 0 then
    for i := 0 to FPriceList.Count - 1 do
    begin
      Result.PriceUrlList := Result.PriceUrlList + [FPriceList[i]];

      if i >= SearchSettings.LowestPricesCount - 1 then
        Break;
    end;
end;

function TPriceSearcher.CanStart: Boolean;
begin
  Result := FStatus <> Running;
end;

procedure TPriceSearcher.CollectLinks(const Good: TGoods);

  function SearchEngineRequest(const Value: string; const SearchEngineData: TSearchEngineData): Integer;
  var
    Regex: TRegEx;
    Matches: TMatchCollection;
    Url: string;
    i: Integer;
  begin
    Result := 0;

    if FBrowser.LoadUrl(Value) then
    begin
      Regex := TRegEx.Create(SearchEngineData.Regex, [roIgnoreCase, roMultiLine]);
      Matches := RegEx.Matches(FBrowser.ResponseBody);

      Result := Matches.Count;

      if Result > 0 then
        for i := 0 to Result - 1 do
        begin
          Url := Matches[i].Value;

          if IsUrl(Url) then
            if not InBlackList(Url) then
            begin
              FFoundLinks.Add(Url);
            end;
        end;

    end;
  end;

  function CreateUrlRequest(const Template: string; Question: string; ResultsCount: Integer; out Url: string): Boolean;
  begin
    Result := False;
    Url := '';

    Question := TNetEncoding.URL.Encode(Good.Question);

    if Question <> '' then
    begin
      Url := Template.Replace(URL_TEMPLATE_REQUEST, Question, [rfIgnoreCase]);
      Url := Url.Replace(URL_TEMPLATE_COUNT, ResultsCount.ToString, [rfIgnoreCase]);

      Result := Url.Length > 0;
    end;
  end;

var
  SearchEngineData: TSearchEngineData;
  Url: string;
  FoundCount: Integer;
begin
  FFoundLinks.Clear;

  for SearchEngineData in FSearchEngineList do
    if CreateUrlRequest(SearchEngineData.Url, Good.Question, SearchSettings.ResultCount, Url) then
    begin
      FoundCount := SearchEngineRequest(Url, SearchEngineData);

      DoMessageF(msgFoundLinks, [FoundCount]);
    end
      else
    begin
      DoMessageF(msgParseUrlTemplateError, [SearchEngineData.Url]);
    end;

  DoMessageF(msgTotalLinksFound, [FFoundLinks.Count]);
end;

constructor TPriceSearcher.Create;
begin
  inherited;
  FOnStart := nil;
  FOnComplete := nil;
  FOnLowestPriceFound := nil;
  FTask := nil;
  FOnMessage := nil;
  FOnGoodsProgress := nil;
  FOnLinksProgress := nil;
  FOnStatus := nil;

  FBrowser := TSyncCEFBrowser.Create;

  FStop := False;

  FReports := TReports.Create;
  FBlackList := TStringList.Create;
  FFoundLinks := TStringList.Create;
  FFoundLinks.Sorted := True;
  FFoundLinks.Duplicates := dupIgnore;

  FPriceList := TList<TPriceUrl>.Create;
  FPriceSearchRegexList := TStringList.Create;

  FSearchEngineList := TSearchEngineList.Create;

  FSearchSettings.ResultCount := 20;
  FSearchSettings.LowestPricesCount := 5;

  DoStatus(TMainStatus.Free);
end;

destructor TPriceSearcher.Destroy;
begin
  FBrowser.Free;

  FPriceSearchRegexList.Free;
  FSearchEngineList.Free;

  FReports.Free;
  FBlackList.Free;
  FFoundLinks.Free;
  FPriceList.Free;

  inherited;
end;

procedure TPriceSearcher.DoComplete;
begin
  if FStop then
    DoStatus(Stopped)
  else
    DoStatus(Completed);

  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

procedure TPriceSearcher.DoGoodsProgress(const Completed, Total: Integer);
begin
  if Assigned(FOnGoodsProgress) then
    FOnGoodsProgress(Self, Completed, Total);
end;

procedure TPriceSearcher.DoLinksProgress(const Completed, Total: Integer);
begin
  if Assigned(FOnLinksProgress) then
    FOnLinksProgress(Self, Completed, Total);
end;

procedure TPriceSearcher.DoLowestPriceFound(const Report: TReport);
begin
  if Assigned(FOnLowestPriceFound) then
    FOnLowestPriceFound(Self, Report);
end;

procedure TPriceSearcher.DoMessage(const Value: TMessage);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, Value, []);
end;

procedure TPriceSearcher.DoMessageF(const Value: TMessage; const Args: array of const);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, Value, Args);
end;

procedure TPriceSearcher.DoStart;
begin
  FStop := False;

  DoStatus(Running);

  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TPriceSearcher.DoStatus(Value: TMainStatus);
begin
  FStatus := Value;

  if Assigned(FOnStatus) then
    FOnStatus(Self, Value);
end;

function TPriceSearcher.FindGoodsPrice(const Url: string): Cardinal;
begin
  Result := 0;

  if FBrowser.LoadUrl(Url) then
  begin
    Result := Round(FindPriceInText(FBrowser.ResponseBody));

    if Result > 0 then
    begin
      DoMessageF(msgPriceFound, [Result]);
    end
      else
    begin
      DoMessageF(msgPriceNotFound, [Url]);
    end;
  end;
end;

procedure TPriceSearcher.FindGoodsPrices(const Value: TGoods);
var
  Url: string;
  i: Integer;
  Price: Integer;
  Report: TReport;
  IsLowestPriceFound: Boolean;
  PriceUrl: TPriceUrl;
begin
  IsLowestPriceFound := False;
  FPriceList.Clear;

  DoMessageF(msgPriceSearching, [Value.Name]);

  CollectLinks(Value);

  i := 0;
  for Url in FFoundLinks do
  begin
    Inc(i);
    DoLinksProgress(i, FFoundLinks.Count);

    Price := FindGoodsPrice(Url);

    if Price > 0 then
    begin
      PriceUrl.Price := Price;
      PriceUrl.Url := Url;
      FPriceList.Add(PriceUrl);

      if Price < Value.Price then
        IsLowestPriceFound := True;
    end;

    if FStop then
      Break;
  end;

  if FPriceList.Count > 0 then
  begin
    DoMessageF(msgGoodsPriceFoundCount, [Value.Name, FPriceList.Count]);

    Report := BuildReport(Value.Name);
    FReports.Add(Report);

    if IsLowestPriceFound then
      DoLowestPriceFound(Report);
  end
    else
  begin
    DoMessageF(msgGoodsPriceNotFound, [Value.Name]);
  end;

end;

function TPriceSearcher.FindPriceInText(const Text: string): Single;

  function ContainPrice(const Source: string; out Price: Single): Boolean;
  var
    RegEx: TRegEx;
    Match: TMatch;
  begin
    Result := False;

    RegEx := TRegEx.Create('(\d*\.)?\d+');
    Match := RegEx.Match(Source.Replace(' ', ''));

    if Match.Success then
      Result := TryStrToFloat(Match.Value.Replace('.', ','), Price);
  end;

var
  Regex: TRegEx;
  PriceSearchPattern: string;
  Match: TMatch;
begin
  Result := 0;

  for PriceSearchPattern in FPriceSearchRegexList do
  begin
    RegEx := TRegEx.Create(PriceSearchPattern);
    Match := RegEx.Match(Text);

    if Match.Success then
      if ContainPrice(Match.Value, Result) then
      begin
        Exit;
      end;
  end;

end;

function TPriceSearcher.GetStatus: TMainStatus;
begin
  Result := FStatus;
end;

function TPriceSearcher.InBlackList(const Url: string): Boolean;
var
  HostUrl: string;
  s: string;
begin
  Result := False;

  for s in FBlackList do
    if Url.Contains(s) then
      Exit(True);
end;

procedure TPriceSearcher.Start(const GoodsList: TGoodsList);
begin
  if CanStart then
  begin
    FTask := TTask.Create
   (procedure()
    var
      i: Integer;
      Good: TGoods;
    begin
      DoStart;

      i := 0;
      for Good in GoodsList do
      begin
        Inc(i);
        DoGoodsProgress(i, Length(GoodsList));

        FindGoodsPrices(Good);

        if FStop then
          Break;
      end;

      DoComplete;
    end
    );
    FTask.Start;
  end
    else
  DoMessage(msgCantStart);
end;

procedure TPriceSearcher.Stop;
begin
  if Assigned(FTask) then
    if FTask.Status = TTaskStatus.Running then
    begin
      FStop := True;
      FTask.Cancel;
      FBrowser.Stop;
    end;
end;

{ TReports }

constructor TReports.Create;
begin
  inherited Create;
  FOnAdd := nil;
  OnNotify:= Notify;
end;

procedure TReports.DoAdd(Sender: TObject; const Report: TReport);
begin
  if Assigned(FOnAdd) then
    FOnAdd(Self, Report);
end;

procedure TReports.Notify(Sender: TObject; const Report: TReport; Action: TCollectionNotification);
begin
  if Action = cnAdded then
    DoAdd(Self, Report);
end;

end.
