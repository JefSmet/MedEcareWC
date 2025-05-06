unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, VCL.TMSFNCTypes, VCL.TMSFNCUtils, VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes, System.Rtti,
  VCL.TMSFNCDataGridCell, VCL.TMSFNCDataGridData, VCL.TMSFNCDataGridBase, VCL.TMSFNCDataGridCore, VCL.TMSFNCDataGridRenderer,
  Vcl.Controls, VCL.TMSFNCCustomControl, VCL.TMSFNCDataGrid, view.base, Vcl.StdCtrls, WEBLib.StdCtrls;

type
  TForm1 = class(TViewBase)
    TMSFNCDataGrid1: TTMSFNCDataGrid;
    WebButton1: TWebButton;
    procedure TMSFNCDataGrid1CellClick(Sender: TObject; AColumn, ARow: Integer);
    procedure WebButton1Click(Sender: TObject);
  private
    procedure VulMaandLijst(Grid: TTMSFNCDataGrid; AJaar, AMaand: Word);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses DateUtils;

{$R *.dfm}

procedure TForm1.TMSFNCDataGrid1CellClick(Sender: TObject; AColumn, ARow: Integer);
var
  adt: TDateTime;
begin
  if ARow>0 then
  begin
    adt:=toNumber(TMSFNCDataGrid1.Cells[acolumn,arow]);
    ShowMessage(FormatDateTime('dddd dd-mm-yyyy',adt));
  end;
end;

procedure TForm1.VulMaandLijst(Grid: TTMSFNCDataGrid; AJaar, AMaand: Word);
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
    //Grid.OnCellClick := GridCellClick;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TForm1.WebButton1Click(Sender: TObject);
begin
TMSFNCDataGrid1.Clear;
TMSFNCDataGrid1.ClearColumns;
TMSFNCDataGrid1.RowCount:=0;
VulMaandLijst(TMSFNCDataGrid1,YearOf(now()),monthof(now()));
end;

end.