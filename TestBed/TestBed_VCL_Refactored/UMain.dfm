object frmMain: TfrmMain
  Left = 282
  Top = 108
  Caption = 'TestBed - Delphi VCL OpenGL - Refactored "Box2D-Delphi" test bed'
  ClientHeight = 606
  ClientWidth = 784
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 621
    Top = 0
    Width = 163
    Height = 606
    Align = alRight
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      163
      606)
    object Label1: TLabel
      Left = 8
      Top = 5
      Width = 26
      Height = 13
      Caption = 'Tests'
    end
    object Label2: TLabel
      Left = 12
      Top = 224
      Width = 37
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Visibility'
    end
    object Bevel1: TBevel
      Left = 5
      Top = 156
      Width = 154
      Height = 2
      Anchors = [akLeft, akBottom]
      ExplicitTop = 112
    end
    object chkWarmStarting: TCheckBox
      Left = 8
      Top = 175
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Warm Starting'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = SimulationOptionsChanged
    end
    object chkContinuousPhysics: TCheckBox
      Left = 8
      Top = 190
      Width = 129
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Continuous Physics'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = SimulationOptionsChanged
    end
    object chklstVisibility: TCheckListBox
      Left = 8
      Top = 238
      Width = 148
      Height = 149
      OnClickCheck = chklstVisibilityClickCheck
      Anchors = [akLeft, akBottom]
      ImeName = 'Chinese (Simplified) - US Keyboard'
      ItemHeight = 13
      Items.Strings = (
        'Shapes'
        'Joints'
        'AABBs'
        'Pairs'
        'Contact Points'
        'Contact Normals'
        'Contact Impulse'
        'Friction Impulse'
        'Center of Masses'
        'Statistics'
        'Key Information')
      TabOrder = 2
    end
    object btnPause: TButton
      Left = 7
      Top = 528
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Pause'
      TabOrder = 3
      OnClick = btnPauseClick
    end
    object btnSingleStep: TButton
      Left = 82
      Top = 528
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Single Step'
      TabOrder = 4
      OnClick = btnSingleStepClick
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 393
      Width = 148
      Height = 70
      Anchors = [akLeft, akBottom]
      Caption = 'Gravity'
      TabOrder = 5
      object Label3: TLabel
        Left = 11
        Top = 21
        Width = 6
        Height = 13
        Caption = 'X'
      end
      object Label4: TLabel
        Left = 11
        Top = 45
        Width = 6
        Height = 13
        Caption = 'Y'
      end
      object editGravityX: TEdit
        Left = 20
        Top = 18
        Width = 51
        Height = 21
        ImeName = 'Chinese (Simplified) - US Keyboard'
        TabOrder = 0
      end
      object editGravityY: TEdit
        Left = 20
        Top = 42
        Width = 51
        Height = 21
        ImeName = 'Chinese (Simplified) - US Keyboard'
        TabOrder = 1
      end
      object btnConfirmGravity: TButton
        Left = 80
        Top = 16
        Width = 63
        Height = 25
        Caption = 'Confirm'
        TabOrder = 2
        OnClick = btnConfirmGravityClick
      end
    end
    object btnReset: TButton
      Left = 7
      Top = 553
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Reset'
      TabOrder = 6
      OnClick = btnResetClick
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 465
      Width = 148
      Height = 57
      Anchors = [akLeft, akBottom]
      Caption = 'Mode'
      TabOrder = 7
      object rdoRealTime: TRadioButton
        Left = 8
        Top = 16
        Width = 113
        Height = 17
        Caption = 'Real Time'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rdoRealTimeClick
      end
      object rdoFixedStep: TRadioButton
        Left = 8
        Top = 34
        Width = 113
        Height = 17
        Caption = 'Fixed Step(1/60s)'
        TabOrder = 1
        OnClick = rdoFixedStepClick
      end
    end
    object chkAntialiasing: TCheckBox
      Left = 8
      Top = 584
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Antialiasing'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = chkAntialiasingClick
    end
    object chkSubStepping: TCheckBox
      Left = 8
      Top = 205
      Width = 105
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Sub-Stepping'
      TabOrder = 9
      OnClick = SimulationOptionsChanged
    end
    object btnDumpWorld: TButton
      Left = 82
      Top = 553
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Dump World'
      TabOrder = 10
      OnClick = btnDumpWorldClick
    end
    object listTestEntries: TListBox
      Left = 8
      Top = 20
      Width = 148
      Height = 130
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      TabOrder = 11
      OnKeyDown = listTestEntriesKeyDown
      OnKeyUp = listTestEntriesKeyUp
      OnMouseDown = listTestEntriesMouseDown
    end
    object chkEnableSleep: TCheckBox
      Left = 8
      Top = 160
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Enable Sleep'
      Checked = True
      State = cbChecked
      TabOrder = 12
      OnClick = SimulationOptionsChanged
    end
  end
end
