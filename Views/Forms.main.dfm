object FormMain: TFormMain
  Width = 640
  Height = 520
  OnCreate = WebFormCreate
  object WebPanel1: TWebPanel
    Left = 0
    Top = 0
    Width = 81
    Height = 520
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      81
      520)
    object btnRegister: TWebButton
      Left = 3
      Top = 3
      Width = 72
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Registreren'
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
      OnClick = btnRegisterClick
    end
    object WebButton2: TWebButton
      Left = 3
      Top = 40
      Width = 72
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'btn'
      ChildOrder = 1
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
      OnClick = WebButton2Click
    end
    object WebButton3: TWebButton
      Left = 3
      Top = 77
      Width = 72
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'btn'
      ChildOrder = 2
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
    end
    object btnLogout: TWebButton
      Left = 3
      Top = 482
      Width = 72
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Log out'
      ChildOrder = 3
      HeightPercent = 100.000000000000000000
      WidthPercent = 100.000000000000000000
      OnClick = btnLogoutClick
    end
  end
  object FormContainer: TWebPanel
    Left = 81
    Top = 0
    Width = 559
    Height = 520
    Align = alClient
    ChildOrder = 1
    TabOrder = 1
  end
end
