object FrmJsonArrayEditor: TFrmJsonArrayEditor
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Json Array Editor'
  ClientHeight = 369
  ClientWidth = 224
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
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 224
    Height = 328
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 185
    ExplicitHeight = 361
  end
  object Panel1: TPanel
    Left = 0
    Top = 328
    Width = 224
    Height = 41
    Align = alBottom
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    ExplicitLeft = 88
    ExplicitTop = 344
    ExplicitWidth = 185
    object BtnOk: TButton
      Left = 19
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ok'
      TabOrder = 0
      OnClick = BtnOkClick
    end
    object btnCancel: TButton
      Left = 131
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
