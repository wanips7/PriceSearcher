unit uEditGood;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uMain, uPriceSearch, uTranslator,
  Vcl.Samples.Spin;

type
  TFormEditGood = class(TForm)
    ButtonSaveGood: TButton;
    EditGoodName: TEdit;
    EditRequest: TEdit;
    LabelGoodName: TLabel;
    LabelSearchRequest: TLabel;
    LabelLowerLimitPrice: TLabel;
    SpinEditPrice: TSpinEdit;
    procedure Clear;
    function Save: Boolean;
    procedure Load(const Good: TGoods);
    procedure ButtonSaveGoodClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEditGood: TFormEditGood;

implementation

{$R *.dfm}

{ TFormEditGood }

procedure TFormEditGood.ButtonSaveGoodClick(Sender: TObject);
begin
  if Save then
    Close;
end;

procedure TFormEditGood.Clear;
begin
  EditGoodName.Clear;
  EditRequest.Clear;
  SpinEditPrice.Value := 0;
end;

procedure TFormEditGood.FormCreate(Sender: TObject);
begin
  Caption := Translate('Labels.GoodName');

  ButtonSaveGood.Caption := Translate('Buttons.Save');

  LabelGoodName.Caption := Translate('Labels.GoodName');
  LabelSearchRequest.Caption := Translate('Labels.SearchRequest');
  LabelLowerLimitPrice.Caption := Translate('Labels.LowerPriceLimit');

end;

procedure TFormEditGood.Load(const Good: TGoods);
begin
  EditGoodName.Text := Good.Name;
  EditRequest.Text := Good.Question;
  SpinEditPrice.Text := Good.Price.ToString;
end;

function TFormEditGood.Save: Boolean;
var
  Good: TGoods;
begin
  Result := (EditGoodName.Text <> '') and (EditRequest.Text <> '');
  if Result then
  begin
    Good.Name := EditGoodName.Text;
    Good.Question := EditRequest.Text;
    Good.Price := StrToInt(SpinEditPrice.Text);

    FormMain.AddGoods(Good);
  end;
end;

end.
