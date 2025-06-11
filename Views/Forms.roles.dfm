inherited FormRoles: TFormRoles
  Width = 730
  Height = 585
  OnCreate = WebFormCreate
  OnDestroy = WebFormDestroy
  object edtName: TWebEdit
    Left = 8
    Top = 200
    Width = 96
    Height = 25
    ChildOrder = 1
    ElementID = 'edtName'
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
        ID = 'btnNew'
        Name = 'acShowNewRole'
        OnExecute = showNewRole
      end
      item
        ID = 'btnCancel'
        Name = 'acHideNewRole'
        OnExecute = aclacHideNewRoleExecute
      end
      item
        ID = 'btnSave'
        Name = 'acSave'
        OnExecute = aclacSaveExecute
      end
      item
        ID = ''
        Name = 'acEdit'
        Selector = '.edit-button'
        OnExecute = aclacEditExecute
      end
      item
        ID = ''
        Name = 'acDelete'
        Selector = '.delete-button'
        OnExecute = aclacDeleteExecute
      end>
    Left = 376
    Top = 352
  end
end
