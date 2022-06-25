object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Price Searcher'
  ClientHeight = 730
  ClientWidth = 1358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1358
    730)
  PixelsPerInch = 120
  TextHeight = 16
  object LabelAutoInterval: TLabel
    Left = 855
    Top = 12
    Width = 83
    Height = 16
    Caption = 'Interval (min):'
  end
  object ButtonAddGoods: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Add'
    TabOrder = 0
    OnClick = ButtonAddGoodsClick
  end
  object ButtonRemoveGoods: TButton
    Left = 230
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Remove'
    TabOrder = 1
    OnClick = ButtonRemoveGoodsClick
  end
  object ButtonStart: TButton
    Left = 375
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = ButtonStartClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 512
    Width = 1312
    Height = 193
    Anchors = [akLeft, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object ButtonStartAuto: TButton
    Left = 727
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Start auto'
    TabOrder = 4
    OnClick = ButtonStartAutoClick
  end
  object ButtonBlackList: TButton
    Left = 1118
    Top = 8
    Width = 113
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Black list'
    TabOrder = 5
    OnClick = ButtonBlackListClick
  end
  object ButtonSettings: TButton
    Left = 1237
    Top = 8
    Width = 113
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Settings'
    TabOrder = 6
    OnClick = ButtonSettingsClick
  end
  object ButtonStop: TButton
    Left = 606
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Stop'
    TabOrder = 7
    OnClick = ButtonStopClick
  end
  object ButtonStartSelected: TButton
    Left = 471
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Start selected'
    TabOrder = 8
    OnClick = ButtonStartSelectedClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 711
    Width = 1358
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 250
      end
      item
        Width = 250
      end
      item
        Width = 50
      end>
  end
  object MemoReports: TMemo
    Left = 623
    Top = 40
    Width = 697
    Height = 466
    Anchors = [akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 10
  end
  object SpinEditAutoInterval: TSpinEdit
    Left = 976
    Top = 8
    Width = 81
    Height = 26
    MaxValue = 5000
    MinValue = 1
    TabOrder = 11
    Value = 10
    OnChange = SpinEditAutoIntervalChange
  end
  object ButtonClearLog: TButton
    Left = 1326
    Top = 680
    Width = 32
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'o'
    TabOrder = 12
    OnClick = ButtonClearLogClick
  end
  object ButtonClearReports: TButton
    Left = 1326
    Top = 480
    Width = 32
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'o'
    TabOrder = 13
    OnClick = ButtonClearReportsClick
  end
  object ButtonEditGoods: TButton
    Left = 119
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Edit'
    TabOrder = 14
    OnClick = ButtonEditGoodsClick
  end
  object StringGridGoods: TStringGrid
    Left = 8
    Top = 39
    Width = 608
    Height = 467
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 3
    DefaultColWidth = 184
    DefaultRowHeight = 18
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect, goFixedRowDefAlign]
    TabOrder = 15
  end
end
