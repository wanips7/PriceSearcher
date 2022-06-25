object FormSettings: TFormSettings
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 460
  ClientWidth = 720
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
    Left = 316
    Top = 427
    Width = 97
    Height = 25
    Caption = 'Save'
    TabOrder = 0
    OnClick = ButtonSaveClick
  end
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 704
    Height = 413
    ActivePage = TabSheet4
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Common'
      object LabelSearchRequestCount: TLabel
        Left = 17
        Top = 8
        Width = 169
        Height = 16
        Caption = 'The number of search results'
      end
      object LabelPageLoadWaitTime: TLabel
        Left = 234
        Top = 8
        Width = 144
        Height = 16
        Caption = 'Page load wait time (ms)'
      end
      object LabelLowPriceCount: TLabel
        Left = 459
        Top = 8
        Width = 90
        Height = 16
        Caption = 'Low price count'
      end
      object LabelLang: TLabel
        Left = 18
        Top = 88
        Width = 55
        Height = 16
        Caption = 'Language'
      end
      object SpinEdit: TSpinEdit
        Left = 18
        Top = 30
        Width = 65
        Height = 26
        MaxValue = 300
        MinValue = 10
        TabOrder = 0
        Value = 10
      end
      object SpinEditPageLoadWaitTime: TSpinEdit
        Left = 234
        Top = 30
        Width = 65
        Height = 26
        Increment = 100
        MaxValue = 10000
        MinValue = 100
        TabOrder = 1
        Value = 1000
      end
      object SpinEditLowestPricesCount: TSpinEdit
        Left = 462
        Top = 30
        Width = 65
        Height = 26
        MaxValue = 20
        MinValue = 1
        TabOrder = 2
        Value = 3
      end
      object ComboBoxLangList: TComboBox
        Left = 17
        Top = 110
        Width = 204
        Height = 24
        Style = csDropDownList
        TabOrder = 3
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Telegram'
      ImageIndex = 1
      object LabelTelegBotToken: TLabel
        Left = 16
        Top = 56
        Width = 53
        Height = 16
        Caption = 'Bot token'
      end
      object LabelTelegUserId: TLabel
        Left = 16
        Top = 121
        Width = 41
        Height = 16
        Caption = 'User Id'
      end
      object CheckBoxNotifToTelegram: TCheckBox
        Left = 16
        Top = 17
        Width = 258
        Height = 17
        Caption = 'Send notification to Telegram'
        TabOrder = 0
      end
      object EditBotToken: TEdit
        Left = 16
        Top = 78
        Width = 433
        Height = 24
        TabOrder = 1
      end
      object EditUserID: TEdit
        Left = 16
        Top = 143
        Width = 297
        Height = 24
        TabOrder = 2
      end
      object ButtonTelegTest: TButton
        Left = 374
        Top = 143
        Width = 75
        Height = 25
        Caption = 'Test'
        TabOrder = 3
        OnClick = ButtonTelegTestClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Email'
      ImageIndex = 2
      object LabelSMTPServer: TLabel
        Left = 16
        Top = 56
        Width = 73
        Height = 16
        Caption = 'SMTP server'
      end
      object LabelEmailLogin: TLabel
        Left = 16
        Top = 128
        Width = 30
        Height = 16
        Caption = 'Login'
      end
      object LabelEmailPass: TLabel
        Left = 246
        Top = 128
        Width = 55
        Height = 16
        Caption = 'Password'
      end
      object LabelSMTPPort: TLabel
        Left = 246
        Top = 56
        Width = 23
        Height = 16
        Caption = 'Port'
      end
      object LabelEmailRecipient: TLabel
        Left = 351
        Top = 56
        Width = 84
        Height = 16
        Caption = 'Email recipient'
      end
      object CheckBoxNotifToEmail: TCheckBox
        Left = 16
        Top = 17
        Width = 258
        Height = 17
        Caption = 'Send notification to Email'
        TabOrder = 0
      end
      object EditMailHost: TEdit
        Left = 16
        Top = 78
        Width = 204
        Height = 24
        TabOrder = 1
      end
      object EditMailLogin: TEdit
        Left = 16
        Top = 150
        Width = 204
        Height = 24
        TabOrder = 2
      end
      object EditMailPassword: TEdit
        Left = 246
        Top = 150
        Width = 204
        Height = 24
        TabOrder = 3
      end
      object EditMailPort: TEdit
        Left = 246
        Top = 78
        Width = 89
        Height = 24
        NumbersOnly = True
        TabOrder = 4
      end
      object EditMailTo: TEdit
        Left = 351
        Top = 78
        Width = 153
        Height = 24
        TabOrder = 5
      end
      object ButtonCheckSMTPConnection: TButton
        Left = 464
        Top = 150
        Width = 185
        Height = 25
        Caption = 'Check connection'
        TabOrder = 6
        OnClick = ButtonCheckSMTPConnectionClick
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Price search'
      ImageIndex = 3
      object LabelUrl: TLabel
        Left = 16
        Top = 287
        Width = 16
        Height = 16
        Caption = 'Url'
      end
      object MemoRules: TMemo
        Left = 16
        Top = 16
        Width = 657
        Height = 265
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object EditTestRulesUrl: TEdit
        Left = 16
        Top = 309
        Width = 657
        Height = 24
        TabOrder = 1
      end
      object ButtonTestRules: TButton
        Left = 598
        Top = 339
        Width = 75
        Height = 25
        Caption = 'Test'
        TabOrder = 2
        OnClick = ButtonTestRulesClick
      end
      object CheckBoxSavePageToFile: TCheckBox
        Left = 16
        Top = 339
        Width = 217
        Height = 17
        Caption = 'Save page to file'
        TabOrder = 3
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Search engines'
      ImageIndex = 4
      object MemoSearchEngines: TMemo
        Left = 16
        Top = 16
        Width = 665
        Height = 313
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
