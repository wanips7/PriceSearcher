unit uBlackList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uTranslator;

type
  TFormBlackList = class(TForm)
    ButtonSave: TButton;
    Memo: TMemo;
    Edit: TEdit;
    ButtonAdd: TButton;
    procedure Load(Value: TStringList);
    procedure Save;
    procedure Clear;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
  private
    FStringList: TStringList;
  public
    { Public declarations }
  end;

var
  FormBlackList: TFormBlackList;

implementation

{$R *.dfm}

uses
  uMain, uPriceSearch;

{ TFormBlackList }

procedure TFormBlackList.ButtonAddClick(Sender: TObject);
var
  Url: string;
begin
  Url := GetHostUrl(Edit.Text);

  if Url <> '' then
  begin
    if FStringList.IndexOf(Url) = -1 then
      Memo.Lines.Add(Url);
  end;
end;

procedure TFormBlackList.ButtonSaveClick(Sender: TObject);
begin
  Save;
  Close;
end;

procedure TFormBlackList.Clear;
begin
  Memo.Clear;
  Edit.Clear;
end;

procedure TFormBlackList.FormCreate(Sender: TObject);
begin
  FStringList := nil;

  Caption := Translate('Labels.BlackList');

  ButtonAdd.Caption := Translate('Buttons.Add');
  ButtonSave.Caption := Translate('Buttons.Save');


end;

procedure TFormBlackList.Load(Value: TStringList);
begin
  Clear;

  FStringList := Value;

  if Assigned(FStringList) then
    Memo.Text := FStringList.Text;
end;

procedure TFormBlackList.Save;
begin
  if Assigned(FStringList) then
    FStringList.Text := Memo.Text;
end;

end.
