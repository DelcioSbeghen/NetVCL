object FrmJsonArrayEditor: TFrmJsonArrayEditor
  Left = 0
  Height = 369
  Top = 0
  Width = 224
  BorderIcons = []
  Caption = 'Json Array Editor'
  ClientHeight = 369
  ClientWidth = 224
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  LCLVersion = '3.2.0.0'
  object Memo1: TMemo
    Left = 0
    Height = 328
    Top = 0
    Width = 224
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Height = 41
    Top = 328
    Width = 224
    Align = alBottom
    Caption = 'Panel1'
    ClientHeight = 41
    ClientWidth = 224
    ParentBackground = False
    TabOrder = 1
    object BtnOk: TButton
      Left = 19
      Height = 25
      Top = 8
      Width = 75
      Caption = 'Ok'
      TabOrder = 0
      OnClick = BtnOkClick
    end
    object btnCancel: TButton
      Left = 131
      Height = 25
      Top = 8
      Width = 75
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
