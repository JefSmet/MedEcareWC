object FormMain: TFormMain
  Width = 640
  Height = 437
  OnCreate = WebFormCreate
  object acl: TWebElementActionList
    Actions = <
      item
        ID = 'sidebar-brand'
        Name = 'ocBrand'
        OnExecute = acShowHomeExecute
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
      end>
    Left = 217
    Top = 96
  end
end
