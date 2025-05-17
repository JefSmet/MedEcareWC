inherited FormRegisterUser: TFormRegisterUser
  object firstName: TWebEdit
    Left = 20
    Top = 20
    Width = 96
    Height = 25
    ElementID = 'firstName'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    Text = 'firstName'
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object email: TWebEdit
    Left = 20
    Top = 100
    Width = 96
    Height = 25
    ChildOrder = 1
    ElementID = 'email'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    Text = 'email'
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object password: TWebEdit
    Left = 20
    Top = 140
    Width = 96
    Height = 25
    ChildOrder = 2
    ElementID = 'password'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    Text = 'password'
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object role: TWebComboBox
    Left = 20
    Top = 180
    Width = 96
    Height = 23
    ElementID = 'role'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    Text = 'role'
    TextHint = 'Selecteer een rol'
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    ItemIndex = -1
    Items.Strings = (
      'Admin'
      'User')
  end
  object lastName: TWebEdit
    Left = 20
    Top = 60
    Width = 96
    Height = 25
    ChildOrder = 4
    ElementID = 'lastName'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    Text = 'lastName'
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object dateOfBirth: TWebDateTimePicker
    Left = 20
    Top = 220
    Width = 96
    Height = 25
    ElementID = 'dateOfBirth'
    HeightStyle = ssAuto
    WidthStyle = ssAuto
    BorderStyle = bsSingle
    ChildOrder = 5
    Color = clWhite
    Date = 45782.646525486110000000
    ElementFont = efCSS
    ElementPosition = epIgnore
    Role = ''
    Text = ''
  end
  object register: TWebButton
    Left = 20
    Top = 260
    Width = 96
    Height = 25
    Caption = 'register'
    ChildOrder = 6
    ElementID = 'register'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    OnClick = registerClick
  end
end
