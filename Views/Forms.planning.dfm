inherited FormPlanning: TFormPlanning
  Width = 895
  Height = 650
  OnCreate = WebFormCreate
  OnDestroy = WebFormDestroy
  object WebHTMLDiv1: TWebHTMLDiv
    Left = 8
    Top = 0
    Width = 577
    Height = 465
    ElementID = 'grid'
    HeightStyle = ssPercent
    WidthStyle = ssAuto
    ChildOrder = 1
    ElementPosition = epIgnore
    Role = ''
    object dgDataGrid: TWebDataGrid
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 575
      Height = 463
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Banding.Enabled = False
      Banding.OddRowsColor = 16777215
      Banding.EvenRowsColor = 16777215
      MaxBlocksInCache = 0
      TabOrder = 0
      RowMultiSelectWithClick = False
      EnableClickSelection = True
      BidiMode = bdLeftToRight
      SuppressMoveWhenColumnDragging = False
      Align = alClient
      MultilevelHeaders = <>
      ColumnDefs = <
        item
          Field = 'column1'
          CellDataType = cdtDateString
          CheckboxSelection = False
          Width = 0
          SelectOptions = <>
        end
        item
          Field = 'column2'
          CellDataType = cdtText
          CheckboxSelection = False
          Width = 0
          SelectOptions = <>
        end>
    end
  end
end
