inherited FormHome: TFormHome
  Width = 1012
  Height = 755
  Color = clBtnFace
  object WebHTMLDiv1: TWebHTMLDiv
    Left = 24
    Top = 24
    Width = 561
    Height = 409
    ElementID = 'testhome'
    HeightStyle = ssAuto
    WidthStyle = ssAuto
    ChildOrder = 1
    ElementPosition = epIgnore
    Role = ''
    DesignSize = (
      561
      409)
    object WebDataGrid1: TWebDataGrid
      Left = 3
      Top = 3
      Width = 537
      Height = 385
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
          CellDataType = cdtText
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
  object DGCustomDataAdapter1: TDGCustomDataAdapter
    Left = 608
    Top = 584
  end
end
