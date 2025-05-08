object estForm: TestForm
  Width = 1200
  Height = 800
  CSSLibrary = cssBootstrap
  ElementFont = efCSS
  ElementPosition = epRelative
  object pnlMain: TWebPanel
    Left = 0
    Top = 0
    Width = 1200
    Height = 800
    Align = alClient
    ChildOrder = 1
    TabOrder = 0
    object tabControl: TWebPageControl
      Left = 0
      Top = 0
      Width = 1200
      Height = 800
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object tabCalendar: TWebTabSheet
        Left = 0
        Top = 20
        Width = 1200
        Height = 780
        Caption = 'Calendar View'
        object calLeave: TWebCalendar
          Left = 0
          Top = 0
          Width = 1200
          Height = 780
          Align = alClient
          TabOrder = 0
          SelectedDate = 45785.000000000000000000
          Year = 2025
          Month = 5
          Day = 8
          NameOfMonths.January = 'January'
          NameOfMonths.February = 'February'
          NameOfMonths.March = 'March'
          NameOfMonths.April = 'April'
          NameOfMonths.May = 'May'
          NameOfMonths.June = 'June'
          NameOfMonths.July = 'July'
          NameOfMonths.August = 'August'
          NameOfMonths.September = 'September'
          NameOfMonths.October = 'October'
          NameOfMonths.November = 'November'
          NameOfMonths.December = 'December'
          NameOfDays.Sunday = 'Sun'
          NameOfDays.Monday = 'Mon'
          NameOfDays.Tuesday = 'Tue'
          NameOfDays.Wednesday = 'Wed'
          NameOfDays.Thursday = 'Thu'
          NameOfDays.Friday = 'Fri'
          NameOfDays.Saturday = 'Sat'
        end
      end
      object tabList: TWebTabSheet
        Left = 0
        Top = 20
        Width = 1200
        Height = 780
        Caption = 'List View'
        object gridLeaves: TWebStringGrid
          Left = 0
          Top = 0
          Width = 1200
          Height = 780
          Align = alClient
          BorderStyle = bsNone
          ColCount = 6
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
          TabOrder = 0
          FixedFont.Charset = DEFAULT_CHARSET
          FixedFont.Color = clWindowText
          FixedFont.Height = -12
          FixedFont.Name = 'Segoe UI'
          FixedFont.Style = []
          RangeEdit.Max = 100.000000000000000000
          RangeEdit.Step = 1.000000000000000000
          HeightPercent = 100.000000000000000000
          WidthPercent = 100.000000000000000000
          ColWidths = (
            150
            100
            100
            100
            500
            100)
        end
      end
      object tabRequest: TWebTabSheet
        Left = 0
        Top = 20
        Width = 1200
        Height = 780
        Caption = 'Request Leave'
        object pnlRequest: TWebPanel
          Left = 248
          Top = 40
          Width = 680
          Height = 673
          TabOrder = 0
          object lblEmployee: TWebLabel
            Left = 16
            Top = 16
            Width = 52
            Height = 15
            Caption = 'Employee'
            HeightPercent = 100.000000000000000000
            WidthPercent = 100.000000000000000000
          end
          object lblStartDate: TWebLabel
            Left = 16
            Top = 80
            Width = 51
            Height = 15
            Caption = 'Start Date'
            HeightPercent = 100.000000000000000000
            WidthPercent = 100.000000000000000000
          end
          object lblEndDate: TWebLabel
            Left = 352
            Top = 80
            Width = 47
            Height = 15
            Caption = 'End Date'
            HeightPercent = 100.000000000000000000
            WidthPercent = 100.000000000000000000
          end
          object lblType: TWebLabel
            Left = 16
            Top = 144
            Width = 24
            Height = 15
            Caption = 'Type'
            HeightPercent = 100.000000000000000000
            WidthPercent = 100.000000000000000000
          end
          object lblReason: TWebLabel
            Left = 16
            Top = 208
            Width = 38
            Height = 15
            Caption = 'Reason'
            HeightPercent = 100.000000000000000000
            WidthPercent = 100.000000000000000000
          end
          object cmbEmployee: TWebComboBox
            Left = 16
            Top = 38
            Width = 648
            Height = 23
            HeightPercent = 100.000000000000000000
            WidthPercent = 100.000000000000000000
            ItemIndex = -1
          end
          object cmbType: TWebComboBox
            Left = 16
            Top = 166
            Width = 648
            Height = 23
            HeightPercent = 100.000000000000000000
            TabOrder = 3
            WidthPercent = 100.000000000000000000
            ItemIndex = -1
          end
          object memoReason: TWebMemo
            Left = 16
            Top = 230
            Width = 648
            Height = 89
            HeightPercent = 100.000000000000000000
            Lines.Strings = (
              '')
            Required = True
            SelLength = 0
            SelStart = 2
            TabOrder = 4
            WidthPercent = 100.000000000000000000
          end
          object btnSubmit: TWebButton
            Left = 16
            Top = 337
            Width = 648
            Height = 40
            Caption = 'Submit Request'
            ElementClassName = 'btn btn-primary'
            ElementFont = efCSS
            HeightStyle = ssAuto
            HeightPercent = 100.000000000000000000
            TabOrder = 5
            WidthPercent = 100.000000000000000000
            OnClick = btnSubmitClick
          end
        end
      end
    end
  end
end
