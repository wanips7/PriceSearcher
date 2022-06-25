program PriceSearcher;

{$I cef.inc}

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FormMain},
  uCEFApplication,
  uEditGood in 'uEditGood.pas' {FormEditGood},
  uBlackList in 'uBlackList.pas' {FormBlackList},
  uSettings in 'uSettings.pas' {FormSettings},
  Vcl.Themes,
  Vcl.Styles,
  uPriceSearch in 'uPriceSearch.pas',
  uTranslator in 'uTranslator.pas',
  uSyncCEFBrowser in 'uSyncCEFBrowser.pas';

{$R *.res}

begin
  CreateGlobalCEFApp;

  if GlobalCEFApp.StartMainProcess then
  begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormEditGood, FormEditGood);
  Application.CreateForm(TFormBlackList, FormBlackList);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.Run;
  end;

  DestroyGlobalCEFApp;
end.
