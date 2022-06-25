unit uSyncCEFBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.SyncObjs,
  uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFConstants, uCEFChromiumCore, uCEFMiscFunctions,
  uCEFApplication, uCEFBufferPanel;

type
  TOnLoad = procedure(Sender: TObject; const Url: string) of object;
  TOnLoadEnd = procedure(Sender: TObject; const Url, ResponseBody: string; const StatusCode: Integer) of object;
  TOnLoadError = procedure(Sender: TObject; const Url: string; const ErrorCode: Integer) of object;

type
  TSyncCEFBrowser = class
  private
    FOnLoad: TOnLoad;
    FOnLoadEnd: TOnLoadEnd;
    FOnLoadError: TOnLoadError;
    FOnEndRequest: TSimpleEvent;
    FAfterLoadTimeout: Cardinal;
    FResponseBody: string;
    FStatusCode: Integer;
    FBrowser: TChromium;
    procedure DoLoad(const Url: string);
    procedure DoLoadEnd(const Url, ResponseBody: string; const StatusCode: Integer);
    procedure DoLoadError(const Url: string; const ErrorCode: Integer);
    procedure Wait(const Timeout: Integer);
    procedure BrowserLoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
    procedure BrowserTextResultAvailable(Sender: TObject; const Text: ustring);
    procedure BrowserLoadingStateChange(Sender: TObject; const Browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
    procedure BrowserLoadStart(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; transitionType: Cardinal);
    procedure BrowserLoadError(Sender: TObject; const Browser: ICefBrowser; const Frame: ICefFrame; ErrorCode: Integer; const ErrorText, FailedUrl: ustring);
  public
    property OnLoad: TOnLoad read FOnLoad write FOnLoad;
    property OnLoadEnd: TOnLoadEnd read FOnLoadEnd write FOnLoadEnd;
    property OnLoadError: TOnLoadError read FOnLoadError write FOnLoadError;
    property AfterLoadTimeout: Cardinal read FAfterLoadTimeout write FAfterLoadTimeout;
    property ResponseBody: string read FResponseBody;
    property StatusCode: Integer read FStatusCode;
    constructor Create;
    destructor Destroy; override;
    function LoadUrl(const Url: string): Boolean;
    procedure Stop;
  end;

procedure CreateGlobalCEFApp;

implementation

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp := TCefApplication.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.TouchEvents := STATE_DISABLED;
  GlobalCEFApp.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);
  GlobalCEFApp.LogSeverity := LOGSEVERITY_DISABLE;
  GlobalCEFApp.DisableImageLoading := True;
  GlobalCEFApp.ShowMessageDlg := False;
  GlobalCEFApp.EnableHighDPISupport := False;
  GlobalCEFApp.ShowMessageDlg := False;
  GlobalCEFApp.BlinkSettings := 'hideScrollbars=true,scrollAnimatorEnabled=false';
  GlobalCEFApp.EnableGPU := False;
  GlobalCEFApp.SmoothScrolling := STATE_DISABLED;
  GlobalCEFApp.EnableSpeechInput := False;
  GlobalCEFApp.EnableUsermediaScreenCapturing := False;
  GlobalCEFApp.EnablePrintPreview := False;
  GlobalCEFApp.DisableJavascriptAccessClipboard := True;
  GlobalCEFApp.DisableJavascriptDomPaste := True;
  GlobalCEFApp.DisableSpellChecking := True;
  GlobalCEFApp.MuteAudio := True;
  GlobalCEFApp.AllowFileAccessFromFiles := True;
  GlobalCEFApp.EnableMediaStream := False;
  GlobalCEFApp.IgnoreCertificateErrors := True;
  GlobalCEFApp.NoSandbox := True;
  GlobalCEFApp.DisableBackForwardCache := True;
  GlobalCEFApp.DeleteCache := True;
  GlobalCEFApp.DeleteCookies := True;
  GlobalCEFApp.PersistSessionCookies := False;
  GlobalCEFApp.PersistUserPreferences := False;
end;

{ TPriceSearcher }

procedure TSyncCEFBrowser.BrowserLoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
  httpStatusCode: Integer);
begin
  if Frame.IsMain then
  begin
    FStatusCode := httpStatusCode;
    FBrowser.RetrieveHTML;
    FOnEndRequest.SetEvent;
  end;
end;

procedure TSyncCEFBrowser.BrowserLoadError(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; errorCode: Integer;
  const errorText, failedUrl: ustring);
begin
  if Frame.IsMain then
  begin
    FStatusCode := 0;
    FOnEndRequest.SetEvent;
    DoLoadError(failedUrl, errorCode);
  end;
end;

procedure TSyncCEFBrowser.BrowserLoadingStateChange(Sender: TObject; const browser: ICefBrowser;
  isLoading, canGoBack, canGoForward: Boolean);
begin
  //
end;

procedure TSyncCEFBrowser.BrowserLoadStart(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
  transitionType: Cardinal);
begin
//  if Frame.IsMain then
//  begin
//    FResponseBody := '';
//    FStatusCode := 0;
//  end;
end;

procedure TSyncCEFBrowser.BrowserTextResultAvailable(Sender: TObject; const Text: ustring);
begin
  FResponseBody := Text;
end;

constructor TSyncCEFBrowser.Create;
begin
  FOnLoad := nil;
  FOnLoadEnd := nil;
  FOnLoadError := nil;

  FStatusCode := 0;
  FResponseBody := '';

  FBrowser := TChromium.Create(nil);
  FBrowser.Options.BackgroundColor := CefColorSetARGB($FF, $FF, $FF, $FF);
  FBrowser.OnTextResultAvailable := BrowserTextResultAvailable;
  FBrowser.OnLoadingStateChange := BrowserLoadingStateChange;
  FBrowser.OnLoadStart := BrowserLoadStart;
  FBrowser.OnLoadEnd := BrowserLoadEnd;
  FBrowser.OnLoadError := BrowserLoadError;
  FBrowser.CreateBrowser;
  FBrowser.Options.ImageLoading := STATE_DISABLED;

  FOnEndRequest := TSimpleEvent.Create;

  FAfterLoadTimeout := 1000;
end;

destructor TSyncCEFBrowser.Destroy;
begin
  FOnEndRequest.Free;
  FBrowser.Free;

  inherited;
end;

procedure TSyncCEFBrowser.DoLoad(const Url: string);
begin
  if Assigned(FOnLoad) then
    FOnLoad(Self, Url);
end;

procedure TSyncCEFBrowser.DoLoadEnd(const Url, ResponseBody: string; const StatusCode: Integer);
begin
  if Assigned(FOnLoadEnd) then
    FOnLoadEnd(Self, Url, ResponseBody, StatusCode);
end;

procedure TSyncCEFBrowser.DoLoadError(const Url: string; const ErrorCode: Integer);
begin
  if Assigned(FOnLoadError) then
    FOnLoadError(Self, Url, ErrorCode);
end;

function TSyncCEFBrowser.LoadUrl(const Url: string): Boolean;
begin
  DoLoad(Url);

  FResponseBody := '';
  FStatusCode := 0;
  FOnEndRequest.ResetEvent;

  FBrowser.LoadURL(Url);
  Wait(10000);

  Sleep(FAfterLoadTimeout);

  FBrowser.StopLoad;

  Result := FStatusCode = 200;

  DoLoadEnd(Url, FResponseBody, FStatusCode);
end;

procedure TSyncCEFBrowser.Stop;
begin
  FOnEndRequest.SetEvent;
end;

procedure TSyncCEFBrowser.Wait(const Timeout: Integer);
begin
  FOnEndRequest.WaitFor(Timeout);
end;

end.
