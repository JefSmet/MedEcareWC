inherited FormPlanning: TFormPlanning
  Width = 895
  Height = 650
  OnCreate = WebFormCreate
  OnDestroy = WebFormDestroy
  object WebHTMLDiv1: TWebHTMLDiv
    Left = 8
    Top = 0
    Width = 761
    Height = 650
    ElementID = 'grid'
    HeightStyle = ssPercent
    WidthStyle = ssAuto
    ChildOrder = 1
    ElementPosition = epIgnore
    Role = ''
    DesignSize = (
      761
      650)
    object dgDataGrid: TWebDataGrid
      Left = 16
      Top = 20
      Width = 729
      Height = 590
      Banding.Enabled = False
      Banding.OddRowsColor = 16777215
      Banding.EvenRowsColor = 16777215
      MaxBlocksInCache = 0
      TabOrder = 0
      RowMultiSelectWithClick = False
      EnableClickSelection = True
      BidiMode = bdLeftToRight
      SuppressMoveWhenColumnDragging = False
      Anchors = [akLeft, akTop, akRight, akBottom]
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
      ExplicitHeight = 565
    end
  end
end
