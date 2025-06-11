object FormMain: TFormMain
  Width = 640
  Height = 437
  OnCreate = WebFormCreate
  object userName: TWebLabel
    Left = 136
    Top = 192
    Width = 3
    Height = 15
    ElementID = 'userName'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
  end
  object acl: TWebElementActionList
    Actions = <
      item
        ID = 'sidebar-brand'
        Name = 'ocBrand'
        OnExecute = aclocBrandExecute
      end
      item
        ID = 'nav-home'
        Name = 'ocHome'
        OnExecute = acShowHomeExecute
      end
      item
        ID = 'nav-verlof'
        Name = 'ocVerlof'
        OnExecute = acShowVerlofUserExecute
      end
      item
        ID = 'dropdown-signout'
        Name = 'ocLogout'
        OnExecute = aclocLogoutExecute
      end
      item
        ID = ''
        Name = 'Action4'
      end>
    Left = 217
    Top = 96
  end
end
