object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 299
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 353
    Height = 299
    Align = alLeft
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object btn1: TButton
    Left = 424
    Top = 40
    Width = 177
    Height = 25
    Caption = 'Dynamic Load Bpl App'
    TabOrder = 1
    OnClick = btn1Click
  end
  object dlgOpen1: TOpenDialog
    DefaultExt = '.bpl'
    FileName = 'C:\Delcio\Projetos\NetVCL\Demos\DevTests\App1.bpl'
    Filter = 'packages|*.bpl'
    Left = 528
    Top = 128
  end
  object NVServer1: TNVServer
    Port = '888'
    LogMemo = Memo1
    Left = 416
    Top = 96
  end
end
