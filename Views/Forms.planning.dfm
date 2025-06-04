inherited FormPlanning: TFormPlanning
  Width = 895
  Height = 650
  OnCreate = WebFormCreate
  OnDestroy = WebFormDestroy
  object Calendar: TWebHTMLDiv
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
  object RightPanel: TWebHTMLDiv
    Left = 584
    Top = 0
    Width = 303
    Height = 465
    ElementID = 'right-panel'
    ChildOrder = 1
    Role = ''
    object AvailableDoctors: TWebHTMLDiv
      Left = 16
      Top = 23
      Width = 265
      Height = 42
      ElementID = 'availableDoctors'
      Role = ''
    end
  end
  object acl: TWebElementActionList
    Actions = <
      item
        ID = ''
        Name = 'acSetActiveCell'
        Selector = '.shift-cell'
        OnExecute = aclacSetActiveCellExecute
      end
      item
        ID = ''
        Name = 'acCalendarButton'
        Selector = '#calendar-prev, #calendar-next'
        OnExecute = aclacCalendarButtonExecute
      end
      item
        ID = ''
        Name = 'acDoctorButton'
        Selector = '.available-doctor-button'
        OnExecute = aclacDoctorButtonExecute
      end
      item
        ID = 'deleteActivity'
        Name = 'acDeleteActivity'
        OnExecute = aclacDeleteActivityExecute
      end>
    Left = 584
    Top = 544
  end
end
