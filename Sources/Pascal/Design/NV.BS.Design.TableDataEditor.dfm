object FrmNvBsTableDataEditor: TFrmNvBsTableDataEditor
  Left = 0
  Top = 0
  Caption = 'NetVCL Table Data Editor'
  ClientHeight = 466
  ClientWidth = 550
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object dbgrd1: TDBGrid
    Left = 0
    Top = 0
    Width = 550
    Height = 416
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 416
    Width = 550
    Height = 50
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      550
      50)
    object BtnOk: TButton
      Left = 180
      Top = 13
      Width = 75
      Height = 25
      Anchors = []
      Caption = 'Ok'
      TabOrder = 0
      OnClick = BtnOkClick
    end
    object btnCancel: TButton
      Left = 295
      Top = 13
      Width = 75
      Height = 25
      Anchors = []
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object MemTable: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 200
    Top = 24
  end
  object DataSource1: TDataSource
    DataSet = MemTable
    Left = 344
    Top = 88
  end
end
