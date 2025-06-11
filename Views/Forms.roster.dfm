inherited FormRoster: TFormRoster
  Width = 730
  Height = 585
  OnCreate = WebFormCreate
  OnDestroy = WebFormDestroy
  object acl: TWebElementActionList
    Actions = <
      item
        ID = ''
        Name = 'acSetActiveCell'
        Selector = '.shift-cell'
        OnExecute = aclacSetActiveCellExecute
      end
      item
        ID = 'buttonAdd'
        Name = 'acAdd'
        OnExecute = aclacAddExecute
      end
      item
        ID = ''
        Name = 'acSelectCell'
        Selector = '.cell'
        OnExecute = aclacSelectCellExecute
      end
      item
        ID = 'moveUp'
        Name = 'acMoveUp'
        OnExecute = aclacMoveUpExecute
      end
      item
        ID = 'moveDown'
        Name = 'acMoveDown'
        OnExecute = aclacMoveDownExecute
      end
      item
        ID = 'delete'
        Name = 'acDelete'
        OnExecute = aclacDeleteExecute
      end
      item
        ID = 'save'
        Name = 'acSave'
        OnExecute = aclacSaveExecute
      end>
    Left = 376
    Top = 352
  end
end
