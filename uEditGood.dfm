object FormEditGood: TFormEditGood
  Left = 0
  Top = 0
  Caption = 'Edit good'
  ClientHeight = 235
  ClientWidth = 780
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object LabelGoodName: TLabel
    Left = 24
    Top = 26
    Width = 65
    Height = 16
    Caption = 'Good name'
  end
  object LabelSearchRequest: TLabel
    Left = 24
    Top = 106
    Width = 87
    Height = 16
    Caption = 'Search request'
  end
  object LabelLowerLimitPrice: TLabel
    Left = 368
    Top = 26
    Width = 95
    Height = 16
    Caption = 'Lower price limit'
  end
  object ButtonSaveGood: TButton
    Left = 336
    Top = 184
    Width = 105
    Height = 25
    Caption = 'Save'
    TabOrder = 0
    OnClick = ButtonSaveGoodClick
  end
  object EditGoodName: TEdit
    Left = 24
    Top = 48
    Width = 321
    Height = 24
    TabOrder = 1
  end
  object EditRequest: TEdit
    Left = 24
    Top = 128
    Width = 729
    Height = 24
    TabOrder = 2
  end
  object SpinEditPrice: TSpinEdit
    Left = 368
    Top = 48
    Width = 121
    Height = 26
    MaxValue = 999999999
    MinValue = 1
    TabOrder = 3
    Value = 1
  end
end
