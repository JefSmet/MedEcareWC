inherited FormPlanning: TFormPlanning
  Width = 895
  Height = 650
  OnCreate = WebFormCreate
  OnDestroy = WebFormDestroy
  object WebHTMLDiv1: TWebHTMLDiv
    Left = 8
    Top = 0
    Width = 577
    Height = 465
    ElementID = 'left-panel'
    HeightStyle = ssPercent
    WidthStyle = ssPercent
    ChildOrder = 1
    ElementPosition = epIgnore
    Role = ''
  end
  object WebHTMLDiv2: TWebHTMLDiv
    Left = 608
    Top = 240
    Width = 177
    Height = 97
    ElementID = 'right-panel'
    ChildOrder = 1
    Role = ''
    object WebButton1: TWebButton
      Left = 32
      Top = 32
      Width = 96
      Height = 25
      Caption = 'WebButton1'
      ElementFont = efCSS
      ElementPosition = epIgnore
      HeightStyle = ssAuto
      HeightPercent = 100.000000000000000000
      WidthStyle = ssAuto
      WidthPercent = 100.000000000000000000
      OnClick = WebButton1Click
    end
  end
end
