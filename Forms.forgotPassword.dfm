object FormForgotPassword: TFormForgotPassword
  Width = 640
  Height = 480
  object lnkLogin: TWebLinkLabel
    Left = 68
    Top = 55
    Width = 3
    Height = 15
    ElementID = 'lnkLogin'
    ElementFont = efCSS
    HeightStyle = ssAuto
    WidthStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
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
  object WebButton1: TWebButton
    Left = 202
    Top = 80
    Width = 96
    Height = 25
    ChildOrder = 2
    ElementID = 'lnkLogins'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
end
