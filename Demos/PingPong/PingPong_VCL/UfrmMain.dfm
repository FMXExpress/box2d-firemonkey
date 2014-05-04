object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'box2D PingPong'
  ClientHeight = 547
  ClientWidth = 611
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  DesignSize = (
    611
    547)
  PixelsPerInch = 96
  TextHeight = 13
  object imgDisplay: TImage
    Left = 6
    Top = 6
    Width = 600
    Height = 500
    OnMouseDown = imgDisplayMouseDown
  end
  object Bevel1: TBevel
    Left = -4
    Top = 512
    Width = 619
    Height = 2
  end
  object btnNewGame: TButton
    Left = 6
    Top = 517
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'New Game'
    TabOrder = 0
    OnClick = btnNewGameClick
  end
  object clrBlockColor: TColorBox
    Left = 364
    Top = 519
    Width = 73
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    OnChange = clrBlockColorChange
  end
  object cboBlockType: TComboBox
    Left = 441
    Top = 519
    Width = 88
    Height = 21
    AutoDropDown = True
    AutoCloseUp = True
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    OnChange = cboBlockTypeChange
    Items.Strings = (
      'Unbreakable'
      'Hit Once'
      'Hit Twice'
      'Hit Thrice')
  end
  object btnPauseResume: TButton
    Left = 83
    Top = 517
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Pause'
    TabOrder = 3
    OnClick = btnPauseResumeClick
  end
  object btnLoadMap: TButton
    Left = 160
    Top = 517
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Load Map'
    TabOrder = 4
    OnClick = btnLoadMapClick
  end
  object btnSaveMap: TButton
    Left = 532
    Top = 517
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save Map'
    TabOrder = 5
    OnClick = btnSaveMapClick
  end
  object chkEditMode: TCheckBox
    Left = 280
    Top = 522
    Width = 80
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Editing Mode'
    TabOrder = 6
    OnClick = chkEditModeClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.map'
    Filter = 'Map File(*.map)|*.map'
    Options = [ofHideReadOnly, ofNoChangeDir, ofFileMustExist, ofEnableSizing]
    Left = 360
    Top = 256
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.map'
    Filter = 'Map File(*.map)|*.map'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 416
    Top = 256
  end
end
