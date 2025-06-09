inherited FormUser: TFormUser
  OnCreate = WebFormCreate
  OnDestroy = WebFormDestroy
  object edtEmail: TWebEdit
    Left = 56
    Top = 120
    Width = 121
    Height = 22
    ElementID = 'edtEmail'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object edtPassword: TWebEdit
    Left = 56
    Top = 148
    Width = 121
    Height = 22
    ChildOrder = 1
    ElementID = 'edtPassword'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object edtVoornaam: TWebEdit
    Left = 56
    Top = 176
    Width = 121
    Height = 22
    ChildOrder = 1
    ElementID = 'edtVoornaam'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object edtAchternaam: TWebEdit
    Left = 56
    Top = 204
    Width = 121
    Height = 22
    ChildOrder = 1
    ElementID = 'edtAchternaam'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object edtRiziv: TWebEdit
    Left = 56
    Top = 232
    Width = 121
    Height = 22
    ChildOrder = 1
    ElementID = 'edtRiziv'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
  end
  object dpGeboorteDatum: TWebDateTimePicker
    Left = 56
    Top = 260
    Width = 170
    Height = 22
    ElementID = 'edtGeboorteDatum'
    BorderStyle = bsSingle
    ChildOrder = 5
    Color = clWhite
    Date = 45817.549140069440000000
    Role = ''
    Text = ''
  end
  object cbRole: TWebComboBox
    Left = 56
    Top = 288
    Width = 145
    Height = 23
    ElementID = 'cbRole'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    ItemIndex = -1
  end
  object cbIsActiefStafflid: TWebComboBox
    Left = 56
    Top = 317
    Width = 145
    Height = 23
    ElementID = 'cbIsActiefStafflid'
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    ItemIndex = -1
  end
  object acl: TWebElementActionList
    Actions = <
      item
        ID = 'btnNew'
        Name = 'acShowNewUserForm'
        OnExecute = showNewUser
      end
      item
        ID = 'btnCancel'
        Name = 'acHIdeForm'
        OnExecute = aclacHideNewUserExecute
      end
      item
        ID = 'btnSave'
        Name = 'acSave'
        OnExecute = aclacSaveExecute
      end
      item
        ID = ''
        Name = 'acUpdateClick'
        Selector = '.edit-button'
        OnExecute = aclacEditExecute
      end
      item
        ID = ''
        Name = 'acDeleteClick'
        Selector = '.delete-button'
        OnExecute = aclacDeleteClickExecute
      end>
    Left = 264
    Top = 296
  end
end
