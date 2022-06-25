object FormBlackList: TFormBlackList
  Left = 0
  Top = 0
  Caption = 'Black list'
  ClientHeight = 434
  ClientWidth = 791
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
  object ButtonSave: TButton
    Left = 344
    Top = 401
    Width = 105
    Height = 25
    Caption = 'Save'
    TabOrder = 0
    OnClick = ButtonSaveClick
  end
  object Memo: TMemo
    Left = 8
    Top = 8
    Width = 776
    Height = 357
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Edit: TEdit
    Left = 8
    Top = 371
    Width = 689
    Height = 24
    TabOrder = 2
  end
  object ButtonAdd: TButton
    Left = 703
    Top = 371
    Width = 81
    Height = 25
    Caption = 'Add'
    TabOrder = 3
    OnClick = ButtonAddClick
  end
end
