object FormLogin: TFormLogin
  Width = 640
  Height = 480
  object loginPassword: TWebEdit
    Left = 271
    Top = 256
    Width = 96
    Height = 25
    ElementID = 'loginPassword'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object rememberMe: TWebCheckBox
    Left = 271
    Top = 384
    Width = 96
    Height = 25
    ChildOrder = 1
    ElementID = 'rememberMe'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object submitLogin: TWebButton
    Left = 271
    Top = 328
    Width = 96
    Height = 25
    ChildOrder = 2
    ElementID = 'submitLogin'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    OnClick = submitLoginClick
  end
  object loginEmail: TWebEdit
    Left = 271
    Top = 168
    Width = 96
    Height = 25
    ChildOrder = 3
    ElementID = 'loginEmail'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object WebEdit1: TWebEdit
    Left = 112
    Top = 128
    Width = 121
    Height = 22
    ChildOrder = 4
    HeightPercent = 100.000000000000000000
    Text = 'WebEdit1'
    WidthPercent = 100.000000000000000000
  end
  object WebButton1: TWebButton
    Left = 112
    Top = 167
    Width = 96
    Height = 25
    Caption = 'WebButton1'
    ChildOrder = 5
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = WebButton1Click
  end
  object RequestLogin: TWebHttpRequest
    Left = 192
    Top = 248
  end
end
