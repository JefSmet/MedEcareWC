inherited FormHome: TFormHome
  Color = clBtnFace
  object lblUserInfo: TWebLabel
    Left = 20
    Top = 60
    Width = 57
    Height = 15
    Caption = 'lblUserInfo'
    Color = clBtnFace
    ElementID = 'lblUserInfo'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object lblWelcome: TWebLabel
    Left = 20
    Top = 20
    Width = 63
    Height = 15
    Caption = 'lblWelcome'
    Color = clBtnFace
    ElementID = 'lblWelcome'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object btnLogout: TWebButton
    Left = 20
    Top = 100
    Width = 96
    Height = 25
    Caption = 'btnLogout'
    ElementID = 'btnLogout'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    OnClick = btnLogoutClick
  end
end
