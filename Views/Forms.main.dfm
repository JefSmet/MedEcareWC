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
        TargetAction = actAddRemoveClass
        TargetSelector = '.nav-link.active'
        TargetClassAdd = 'link-dark'
        TargetClassRemove = 'active'
        OnExecute = acShowHomeExecute
        OnUpdate = aclocBrandUpdate
      end
      item
        ID = 'nav-home'
        Name = 'ocHome'
        TargetAction = actAddRemoveClass
        TargetSelector = '.nav-link.active'
        TargetClassAdd = 'link-dark'
        TargetClassRemove = 'active'
        OnExecute = acShowHomeExecute
      end
      item
        ID = 'nav-verlof'
        Name = 'ocVerlof'
        TargetAction = actAddRemoveClass
        TargetSelector = '.nav-link.active'
        TargetClassAdd = 'link-dark'
        TargetClassRemove = 'active'
        OnExecute = acShowVerlofUserExecute
        OnUpdate = aclocVerlofUpdate
      end
      item
        ID = 'dropdown-signout'
        Name = 'ocLogout'
        OnExecute = aclocLogoutExecute
      end>
    Left = 217
    Top = 96
  end
end
