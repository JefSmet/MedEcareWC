inherited FormRegisterUser: TFormRegisterUser
  object firstName: TWebEdit
    Left = 0
    Top = 0
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
    Left = 101
    Top = 0
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
    Left = 202
    Top = 0
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
    Left = 303
    Top = 0
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
    Left = 404
    Top = 0
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
    Left = 505
    Top = 0
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
    Left = 0
    Top = 30
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
