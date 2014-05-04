object Form29: TForm29
  Left = 0
  Top = 0
  Caption = 'MSTimer (MSCadencer) test'
  ClientHeight = 496
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 41
    Align = alTop
    TabOrder = 0
    object chkbxEnabled: TCheckBox
      Left = 16
      Top = 10
      Width = 97
      Height = 17
      Caption = 'Enabled'
      TabOrder = 0
      OnClick = chkbxEnabledClick
    end
    object btnReset: TButton
      Left = 96
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Reset'
      TabOrder = 1
      OnClick = btnResetClick
    end
  end
  object MemoLog: TMemo
    Left = 0
    Top = 41
    Width = 635
    Height = 455
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
