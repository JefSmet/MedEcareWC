object FormVerlofUser: TFormVerlofUser
  Width = 887
  Height = 647
  OnCreate = WebFormCreate
  OnDestroy = WebFormDestroy
  object filtertype: TWebComboBox
    Left = 76
    Top = 358
    Width = 96
    Height = 23
    ElementID = 'filter-type'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    TextHint = 'Filter by type'
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    OnChange = onSearchInputChange
    ItemIndex = -1
    Items.Strings = (
      ''
      'Verlof'
      'Congres'
      'Ziek'
      'Andere')
  end
  object calendarnext: TWebButton
    Left = 404
    Top = 388
    Width = 96
    Height = 25
    ChildOrder = 4
    ElementID = 'calendar-next'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    OnClick = calendarButtonClick
  end
  object enddate: TWebDateTimePicker
    Left = 191
    Top = 214
    Width = 96
    Height = 25
    ElementID = 'end-date'
    HeightStyle = ssAuto
    WidthStyle = ssAuto
    BorderStyle = bsSingle
    ChildOrder = 6
    Color = clWhite
    Date = 45800.545343437500000000
    ElementFont = efCSS
    ElementPosition = epIgnore
    Role = ''
    Text = ''
    OnChange = enddateChange
  end
  object submitrequest: TWebButton
    Left = 469
    Top = 215
    Width = 96
    Height = 25
    ChildOrder = 8
    ElementID = 'submit-request'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    OnClick = submitrequestClick
  end
  object startdate: TWebDateTimePicker
    Left = 48
    Top = 214
    Width = 96
    Height = 25
    ElementID = 'start-date'
    HeightStyle = ssAuto
    WidthStyle = ssAuto
    BorderStyle = bsSingle
    ChildOrder = 9
    Color = clWhite
    Date = 45800.545343437500000000
    ElementFont = efCSS
    ElementPosition = epIgnore
    Role = ''
    Text = ''
    OnChange = startdateChange
  end
  object calendarprev: TWebButton
    Left = 293
    Top = 388
    Width = 96
    Height = 25
    ChildOrder = 11
    ElementID = 'calendar-prev'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    OnClick = calendarButtonClick
  end
  object leavetype: TWebComboBox
    Left = 316
    Top = 216
    Width = 96
    Height = 23
    ElementID = 'leave-type'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    Text = 'Verlof'
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    ItemIndex = 0
    Items.Strings = (
      'Verlof'
      'Congres'
      'Ziek'
      'Andere')
  end
  object searchinput: TWebEdit
    Left = 191
    Top = 358
    Width = 96
    Height = 25
    ChildOrder = 16
    ElementID = 'search-input'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    TextHint = 'Search by name'
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    OnChange = onSearchInputChange
  end
  object calendarmonth: TWebHTMLSpan
    Left = 191
    Top = 312
    Width = 96
    Height = 25
    ElementID = 'calendar-month'
    HeightStyle = ssAuto
    WidthStyle = ssAuto
    ChildOrder = 18
    ElementPosition = epIgnore
    ElementFont = efCSS
    Role = ''
  end
  object filterstatus: TWebComboBox
    Left = 191
    Top = 389
    Width = 96
    Height = 23
    ElementID = 'filter-status'
    ElementFont = efCSS
    ElementPosition = epIgnore
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    TextHint = 'Filter by status'
    WidthStyle = ssAuto
    WidthPercent = 100.000000000000000000
    OnChange = onSearchInputChange
    ItemIndex = -1
    Items.Strings = (
      ''
      'Scheduled'
      'Approved'
      'Rejected')
  end
  object webElementAL: TWebElementActionList
    Actions = <
      item
        ID = ''
        Name = 'acApproved'
        Selector = '[data-action="approve"]'
        OnExecute = webElementALacApprovedExecute
      end
      item
        ID = ''
        Name = 'acRejected'
        Selector = '[data-action="reject"]'
        OnExecute = webElementALacRejectedExecute
      end
      item
        ID = ''
        Name = 'acDelete'
        Selector = '[data-action="delete"]'
        OnExecute = webElementALacDeleteExecute
      end>
    Left = 576
    Top = 504
  end
end
