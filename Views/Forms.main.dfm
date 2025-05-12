object FormMain: TFormMain
  Width = 640
  Height = 437
  OnCreate = WebFormCreate
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
      end>
    Left = 217
    Top = 96
  end
end
