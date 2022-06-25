unit uSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  Vcl.ComCtrls, smtpsend, uPriceSearch, uTranslator;

type
  TFormSettings = class(TForm)
    ButtonSave: TButton;
    LabelSearchRequestCount: TLabel;
    SpinEdit: TSpinEdit;
    LabelTelegBotToken: TLabel;
    EditBotToken: TEdit;
    LabelEmailRecipient: TLabel;
    EditMailTo: TEdit;
    EditMailHost: TEdit;
    EditMailPort: TEdit;
    EditMailLogin: TEdit;
    ButtonCheckSMTPConnection: TButton;
    LabelSMTPPort: TLabel;
    LabelEmailLogin: TLabel;
    LabelPageLoadWaitTime: TLabel;
    LabelSMTPServer: TLabel;
    EditUserID: TEdit;
    EditMailPassword: TEdit;
    LabelTelegUserId: TLabel;
    LabelEmailPass: TLabel;
    ButtonTelegTest: TButton;
    SpinEditPageLoadWaitTime: TSpinEdit;
    LabelLowPriceCount: TLabel;
    SpinEditLowestPricesCount: TSpinEdit;
    CheckBoxNotifToTelegram: TCheckBox;
    CheckBoxNotifToEmail: TCheckBox;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    MemoRules: TMemo;
    EditTestRulesUrl: TEdit;
    ButtonTestRules: TButton;
    LabelUrl: TLabel;
    CheckBoxSavePageToFile: TCheckBox;
    TabSheet5: TTabSheet;
    MemoSearchEngines: TMemo;
    LabelLang: TLabel;
    ComboBoxLangList: TComboBox;
    procedure Load;
    procedure Save;
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonCheckSMTPConnectionClick(Sender: TObject);
    procedure ButtonTelegTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonTestRulesClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.dfm}

uses
  uMain;

procedure TFormSettings.ButtonTelegTestClick(Sender: TObject);
begin
  if (EditBotToken.Text <> '') and (EditUserID.Text <> '') then
    FormMain.SendTelegramMessage(EditUserID.Text, EditBotToken.Text, Translate('Messages.ThisIsATestMessage'));
end;

procedure TFormSettings.ButtonTestRulesClick(Sender: TObject);
var
  Url: string;
  List: TStringList;
  Path: string;
begin
  Url := EditTestRulesUrl.Text;
  if IsUrl(Url) then
  begin
    PriceSearcher.FindGoodsPrice(Url);

    if CheckBoxSavePageToFile.Checked then
      if PriceSearcher.Browser.StatusCode = 200 then
      begin
        if not DirectoryExists(AppPath + 'Output') then
          CreateDir(AppPath + 'Output');

        Path := AppPath + 'Output\SiteBody.txt';

        List := TStringList.Create;
        List.Text := PriceSearcher.Browser.ResponseBody;
        List.SaveToFile(Path);
        List.Free;

        FormMain.LogF(Translate('Messages.TheWebPageHasBeenSavedTo'), [Path]);
      end;
  end;
end;

procedure TFormSettings.FormCreate(Sender: TObject);
var
  Lang: string;
begin
  PageControl.TabIndex := 0;

  for Lang in Translator.GetLangList do
  begin
    ComboBoxLangList.Items.Add(Lang);
  end;

  Caption := Translate('Labels.Settings');

  PageControl.Pages[0].Caption := Translate('Labels.Common');
  PageControl.Pages[1].Caption := Translate('Labels.Telegram');
  PageControl.Pages[2].Caption := Translate('Labels.Email');
  PageControl.Pages[3].Caption := Translate('Labels.PriceSearch');
  PageControl.Pages[4].Caption := Translate('Labels.SearchEngines');

  ButtonSave.Caption := Translate('Buttons.Save');
  ButtonTelegTest.Caption := Translate('Buttons.Test');
  ButtonCheckSMTPConnection.Caption := Translate('Buttons.CheckConnection');
  ButtonTestRules.Caption := Translate('Buttons.Test');

  LabelSearchRequestCount.Caption := Translate('Labels.SearchResultsCount');
  LabelPageLoadWaitTime.Caption := Translate('Labels.WebPageLoadWaitTime');
  LabelLowPriceCount.Caption := Translate('Labels.LowPriceCount');
  LabelLang.Caption := Translate('Labels.Lang');
  CheckBoxNotifToTelegram.Caption := Translate('Labels.NotifToTelegram');
  CheckBoxNotifToEmail.Caption := Translate('Labels.NotifToEmail');
  LabelTelegBotToken.Caption := Translate('Labels.TelegBotToken');
  LabelTelegUserId.Caption := Translate('Labels.TelegUserId');
  LabelSMTPServer.Caption := Translate('Labels.SMTPServer');
  LabelSMTPPort.Caption := Translate('Labels.SMTPPort');
  LabelEmailRecipient.Caption := Translate('Labels.EmailRecipient');
  LabelEmailLogin.Caption := Translate('Labels.EmailLogin');
  LabelEmailPass.Caption := Translate('Labels.EmailPass');
  LabelUrl.Caption := Translate('Labels.Url');
  CheckBoxSavePageToFile.Caption := Translate('Labels.SaveWebPage');

end;

procedure TFormSettings.ButtonCheckSMTPConnectionClick(Sender: TObject);
var
  SMTPClient: TSMTPSend;
begin
  SMTPClient := TSMTPSend.Create;
  SMTPClient.Timeout := 3000;
  SMTPClient.AutoTLS := False;
  SMTPClient.FullSSL := true;

  SMTPClient.TargetHost := EditMailHost.Text;
  SMTPClient.TargetPort := EditMailPort.Text;
  SMTPClient.UserName := EditMailLogin.Text;
  SMTPClient.Password := EditMailPassword.Text;

  if SMTPClient.Login then
    if SMTPClient.AuthDone then
    begin
      FormMain.Log(Translate('Messages.MailConnectionTestWasSuccessful'));
      SMTPClient.Logout;
      SMTPClient.Free;
      Exit;
    end;

  FormMain.Log(Translate('Messages.MailServerConnectionError'));

  SMTPClient.Free;
end;

procedure TFormSettings.ButtonSaveClick(Sender: TObject);
begin
  Save;
  Close;
end;

procedure TFormSettings.Load;
begin
  ComboBoxLangList.ItemIndex := ComboBoxLangList.Items.IndexOf(Translator.LocaleInfo.Lang);

  SpinEdit.Value := Settings.Data.SearchRequestCount;
  SpinEditPageLoadWaitTime.Value := Settings.Data.PageLoadWaitTime;
  SpinEditLowestPricesCount.Value := Settings.Data.LowestPricesCount;
  CheckBoxNotifToTelegram.Checked := Settings.Data.SendNotifToTelegram;
  CheckBoxNotifToEmail.Checked := Settings.Data.SendNotifToEmail;

  EditMailTo.Text := Settings.Data.MailTo;
  EditMailHost.Text := Settings.Data.MailHost;
  EditMailPort.Text := Settings.Data.MailPort;
  EditMailLogin.Text := Settings.Data.MailLogin;
  EditMailPassword.Text := Settings.Data.MailPass;

  EditBotToken.Text := Settings.Data.TelegBotToken;
  EditUserID.Text := Settings.Data.TelegUserID;

  MemoRules.Text := PriceSearcher.PriceSearchRegexList.Text;

  MemoSearchEngines.Text := SearchEngineListToText(PriceSearcher.SearchEngineList);

end;

procedure TFormSettings.Save;
begin
  Settings.Data.LangFile := Translator.GetFileNameByLang(ComboBoxLangList.Text);
  Translator.TryLoadLocale(Settings.Data.LangFile);

  Settings.Data.SearchRequestCount := SpinEdit.Value;
  Settings.Data.PageLoadWaitTime := SpinEditPageLoadWaitTime.Value;
  Settings.Data.LowestPricesCount := SpinEditLowestPricesCount.Value;
  Settings.Data.SendNotifToTelegram := CheckBoxNotifToTelegram.Checked;
  Settings.Data.SendNotifToEmail := CheckBoxNotifToEmail.Checked;

  Settings.Data.MailTo := EditMailTo.Text;
  Settings.Data.MailHost := EditMailHost.Text;
  Settings.Data.MailPort := EditMailPort.Text;
  Settings.Data.MailLogin := EditMailLogin.Text;
  Settings.Data.MailPass := EditMailPassword.Text;

  Settings.Data.TelegBotToken := EditBotToken.Text;
  Settings.Data.TelegUserID := EditUserID.Text;

  PriceSearcher.PriceSearchRegexList.Text := MemoRules.Text;

  TextToSearchEngineList(MemoSearchEngines.Text, PriceSearcher.SearchEngineList);

  FormMain.SetMailClientSettings(Settings.Data.MailHost, Settings.Data.MailPort,
    Settings.Data.MailLogin, Settings.Data.MailPass);
end;

end.
