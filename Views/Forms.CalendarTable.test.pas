unit Forms.CalendarTable.test;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls, System.DateUtils,
  WEBLib.Forms, WEBLib.Dialogs, VCL.TMSFNCTypes, VCL.TMSFNCUtils, VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes,
  System.Rtti, VCL.TMSFNCDataGridCell, VCL.TMSFNCDataGridData, VCL.TMSFNCDataGridBase, VCL.TMSFNCDataGridCore,
  VCL.TMSFNCDataGridRenderer, Vcl.Controls, VCL.TMSFNCCustomControl, VCL.TMSFNCDataGrid, VCL.TMSFNCCalendar, VCL.TMSFNCGridCell,
  VCL.TMSFNCGridOptions, VCL.TMSFNCCustomScrollControl, VCL.TMSFNCGridData, VCL.TMSFNCCustomGrid, VCL.TMSFNCGrid, view.base;

type
  TFormCalendarTable = class(TWebForm)
    TMSFNCDataGrid1: TTMSFNCDataGrid;
    TMSFNCCalendar1: TTMSFNCCalendar;
    procedure WebFormCreate(Sender: TObject);
    procedure TMSFNCDataGrid1CellClick(Sender: TObject; AColumn, ARow: Integer);
    procedure TMSFNCDataGrid1Click(Sender: TObject);
    procedure TMSFNCGrid1Click(Sender: TObject);
  private
    { Private declarations }
    procedure GridCellClick(Sender: TObject; ACol, ARow: Integer);
  public
    procedure VulMaandLijst(Grid: TTMSFNCDataGrid; AJaar, AMaand: Word);
    procedure VulHuidigeMaand;
  end;

var
  FormCalendarTable: TFormCalendarTable;

implementation

{$R *.dfm}

{ TFormCalendarTable }

procedure TFormCalendarTable.GridCellClick(Sender: TObject; ACol, ARow: Integer);
begin
  // Set the clicked cell to current date/time (Now)
  if (Sender is TTMSFNCDataGrid) and (ARow > 0) then // Skip the header row (row 0)
  begin
    ShowMessage('clicked');
  end;
end;

procedure TFormCalendarTable.VulMaandLijst(Grid: TTMSFNCDataGrid; AJaar, AMaand: Word);
var
  FS: TFormatSettings;
  AantalDagen, i: Integer;
  D: TDateTime;
begin
  FS := TFormatSettings.Create('nl-BE');

  Grid.BeginUpdate;
  try
    Grid.Clear;
    // Single column with combined date and day
    Grid.ColumnCount := 1;
    Grid.Columns[0].Header := 'Datum';
    
    // Formatting for the date column
    Grid.Columns[0].Formatting.&Type := gdftDate;
    Grid.Columns[0].Formatting.Format := 'dd-mm-yyyy ddd';
    Grid.Columns[0].AddSetting(gcsFormatting);

    Grid.Columns[0].Width := 100;

    // Set row count: number of days in month plus 1 for the header
    Grid.RowCount := DaysInAMonth(AJaar, AMaand) + 1;
    
    // Fixed header row is at index 0, skip it when populating days
    for i := 1 to Grid.RowCount - 1 do
    begin
      D := EncodeDate(AJaar, AMaand, i);
      
      // Populate data starting from row 1 (row 0 is the header)
      Grid.Cells[0, i] := D;
    end;
    
    // Assign the OnCellClick event to handle setting current date on click
    Grid.OnCellClick := GridCellClick;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TFormCalendarTable.WebFormCreate(Sender: TObject);
begin
  inherited;
  //VulHuidigeMaand;
end;

procedure TFormCalendarTable.TMSFNCDataGrid1CellClick(Sender: TObject; AColumn, ARow: Integer);
begin
  GridCellClick(Sender,AColumn,ARow);
end;

procedure TFormCalendarTable.TMSFNCDataGrid1Click(Sender: TObject);
begin
ShowMessage('clicked');
end;

procedure TFormCalendarTable.TMSFNCGrid1Click(Sender: TObject);
begin
ShowMessage('grid');
end;

procedure TFormCalendarTable.VulHuidigeMaand;
var
  CurrentYear, CurrentMonth, CurrentDay: Word;
begin
  // Get the current year and month
  DecodeDate(Now, CurrentYear, CurrentMonth, CurrentDay);
  
  // Fill the grid with the current month/year
 // VulMaandLijst(TMSFNCDataGrid1, CurrentYear, CurrentMonth);
end;

end.