inherited FormShiftTypes: TFormShiftTypes
  Width = 931
  Height = 625
  OnCreate = WebFormCreate
  OnDestroy = WebFormDestroy
  object edtStartHour: TWebEdit
    Left = 8
    Top = 384
    Width = 121
    Height = 22
    ChildOrder = 2
    ElementID = 'edtStartHour'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object edtName: TWebEdit
    Left = 248
    Top = 384
    Width = 121
    Height = 22
    ChildOrder = 2
    ElementID = 'edtName'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object edtDurationMinutes: TWebEdit
    Left = 8
    Top = 512
    Width = 121
    Height = 22
    ChildOrder = 2
    ElementID = 'edtDurationMinutes'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object edtStartMinute: TWebEdit
    Left = 8
    Top = 472
    Width = 121
    Height = 22
    ChildOrder = 2
    ElementID = 'edtStartMinute'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object dpActiveUntil: TWebDateTimePicker
    Left = 376
    Top = 248
    Width = 170
    Height = 22
    ElementID = 'edtActiveUntil'
    BorderStyle = bsSingle
    ChildOrder = 5
    Color = clWhite
    Date = 45814.706769108790000000
    Role = ''
    Text = ''
  end
  object dpActiveFrom: TWebDateTimePicker
    Left = 112
    Top = 248
    Width = 170
    Height = 22
    ElementID = 'edtActiveFrom'
    BorderStyle = bsSingle
    ChildOrder = 5
    Color = clWhite
    Date = 45814.706769108790000000
    Role = ''
    Text = ''
  end
  object acl: TWebElementActionList
    Actions = <
      item
        ID = 'btnNew'
        Name = 'acShowNewShiftType'
        OnExecute = showNewShiftType
      end
      item
        ID = 'btnCancel'
        Name = 'acHideNewShiftType'
        OnExecute = aclacHideNewShiftTypeExecute
      end
      item
        ID = 'btnSave'
        Name = 'acSave'
        OnExecute = aclacSaveExecute
      end
      item
        ID = ''
        Name = 'acShowUpdateShiftType'
        OnExecute = aclacShowUpdateShiftTypeExecute
      end
      item
        ID = ''
        Name = 'acEdit'
        Selector = '.update-button'
        OnExecute = aclacEditExecute
      end
      item
        ID = ''
        Name = 'acDelete'
        Selector = '.btn-danger'
      end>
    Left = 816
    Top = 488
  end
end
