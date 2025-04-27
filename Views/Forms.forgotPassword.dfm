object FormForgotPassword: TFormForgotPassword
  Left = 0
  Top = 0
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object txtEmail: TWebEdit
    Left = 101
    Top = 0
    Width = 96
    Height = 25
    ChildOrder = 1
    ElementID = 'txtEmail'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object btnForgot: TWebButton
    Left = 202
    Top = 0
    Width = 96
    Height = 25
    ChildOrder = 2
    ElementID = 'btnForgot'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object TWebHTMLDiv
    Left = 303
    Top = 0
    Width = 96
    Height = 25
    ElementID = 'alertMsg'
    HeightStyle = ssAuto
    WidthStyle = ssAuto
    ChildOrder = 3
    ElementPosition = epIgnore
    ElementFont = efCSS
    Role = ''
  end
end
