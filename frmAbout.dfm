object FormAbout: TFormAbout
  Left = 0
  Top = 0
  ActiveControl = btnOk
  BorderStyle = bsDialog
  Caption = 'About PyJedi...'
  ClientHeight = 181
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    341
    181)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 131
    Top = 140
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    ExplicitTop = 134
  end
  object mmoAbout: TMemo
    Left = 8
    Top = 8
    Width = 325
    Height = 126
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 1
    ExplicitHeight = 120
  end
end
