unit Forms.planning;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, System.Generics.Collections,
  System.Generics.Defaults,
  orm.Person, orm.Activity, model.AppManager,
  WEBLib.REST, WEBLib.Actions, System.DateUtils, WEBLib.WebTools,
  WEBLib.DataGrid.Common, Vcl.Controls, Vcl.Grids, WEBLib.DataGrid,
  WEBLib.DataGrid.Options, WEBLib.WebCtrls, orm.Doctor, Vcl.StdCtrls,
  WEBLib.StdCtrls, libdatagrid, WEBLib.DataGrid.DataAdapter.Base, WEBLib.DataGrid.DataAdapter.Custom, Types, WEBLib.Grids;

type
  TGridHeader = record
    Id: string;
    Caption: string;
    constructor Create(const AId, ACaption: string);
  end;

  TFormPlanning = class(TViewBase)
    WebHTMLDiv1: TWebHTMLDiv;

    WebHTMLDiv2: TWebHTMLDiv;
    WebButton1: TWebButton;
     [async]
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    function DateColumnFormatter(Value: string): string;
//    procedure dgDataGridCellEditingStarted(Params: TJSCellEditingEvent);
    procedure WebButton1Click(Sender: TObject);

  private
    FShiftList: TList<TActivity>;
    FVerlofList: TList<TActivity>;
    FDoctorsList: TList<TDoctor>;
    FGridHeaders: TList<TGridHeader>;
    FYear: word;
    FMonth: word;
    FDaysBefore: word;
    FDaysAfter: word;
    FGrid: TStringList;
    FAppManager: TAppManager;
    procedure getHeaders;
    procedure initHeaders;
    [async]
    procedure getDoctors;
    [async]
    procedure getShiftList;
    [async]
    procedure getVerlofList;

    procedure buildGrid;
  public
    { Public declarations }
  end;

var
  FormPlanning: TFormPlanning;

implementation

{$R *.dfm}

procedure TFormPlanning.buildGrid;
var
  headers, row: string;
  I, D: integer;
  daycount: word;
  date: TDateTime;
begin
  FGrid.Clear;
  headers := 'Datum,';
  for I := 0 to FGridHeaders.Count - 1 do
  begin
    headers := headers + FGridHeaders.Items[I].Caption + ',';
  end;
  headers := copy(headers, 0, headers.Length - 1);
  // FGrid.Add(headers);
  daycount := DaysInAMonth(FYear, FMonth) + FDaysBefore + FDaysAfter;
  date := EncodeDate(FYear, FMonth, 1);
  date := IncDay(date, -FDaysBefore);
  for D := 0 to daycount - 1 do
  begin
    row := IntToStr(trunc(date)) + ',' + ',' + ',' + ',' + ',';
    FGrid.Add(row);
    date := IncDay(date, 1);
  end;
end;

{ TForm2 }

function TFormPlanning.DateColumnFormatter(Value: string): string;
var
  dt: TDateTime;
begin
  dt := StrToFloat(Value);
  result := FormatDateTime('dd-mm-yyy ddd', dt);
end;


procedure TFormPlanning.getDoctors;
begin
  await(FAppManager.DB.getDoctors(FDoctorsList));
  FDoctorsList.Sort(TComparer<TDoctor>.Construct(
    function(const Left, Right: TDoctor): integer
    begin
      result := CompareText(Left.Person.LastName, Right.Person.LastName);
    end));
end;

procedure TFormPlanning.getHeaders;
var
  mockGridHeader: TGridHeader;
begin
  FGridHeaders.Clear;
  mockGridHeader := TGridHeader.Create(TGUID.NewGuid.ToString, 'Dag 1');
  FGridHeaders.Add(mockGridHeader);
  mockGridHeader := TGridHeader.Create(TGUID.NewGuid.ToString, 'Nacht 1');
  FGridHeaders.Add(mockGridHeader);
  mockGridHeader := TGridHeader.Create(TGUID.NewGuid.ToString, 'Dag 2');
  FGridHeaders.Add(mockGridHeader);
  mockGridHeader := TGridHeader.Create(TGUID.NewGuid.ToString, 'Nacht 2');
  FGridHeaders.Add(mockGridHeader);
  mockGridHeader := TGridHeader.Create(TGUID.NewGuid.ToString, 'Arts3');
  FGridHeaders.Add(mockGridHeader);
end;

procedure TFormPlanning.getShiftList;
var
  startdate, endDate, firstDayOfNowMonth, lastDayOfNowMonth: TDateTime;
begin
  firstDayOfNowMonth := EncodeDate(FYear, FMonth, 1);

  lastDayOfNowMonth := EncodeDate(FYear, FMonth, DaysInAMonth(FYear, FMonth));
  startdate := IncDay(firstDayOfNowMonth, -FDaysBefore);
  endDate := IncDay(lastDayOfNowMonth, FDaysAfter);
  await(FAppManager.DB.getShifts(startdate, endDate, FShiftList));
end;

procedure TFormPlanning.getVerlofList;
var
  startdate, endDate, firstDayOfNowMonth, lastDayOfNowMonth: TDateTime;
begin
  firstDayOfNowMonth := EncodeDate(FYear, FMonth, 1);

  lastDayOfNowMonth := EncodeDate(FYear, FMonth, DaysInAMonth(FYear, FMonth));
  startdate := IncDay(firstDayOfNowMonth, -FDaysBefore);
  endDate := IncDay(lastDayOfNowMonth, FDaysAfter);
  await(FAppManager.DB.getVerlof(startdate, endDate, FVerlofList));
end;

procedure TFormPlanning.initHeaders;
var
  colDef: TDGColumnDefsCollectionItem;
  gridHeader: TGridHeader;
  selOpt: TDGSelectCellEditorCollectionItem;
  // doctor: TDoctor;
begin
  try
//    // op deze manier de kolommen definiëren anders heb je geen controle over de cellen
//    // *********************************************
//    dgDataGrid.ColumnDefs.Clear;
//    colDef := dgDataGrid.ColumnDefs.Add;
//    colDef.CellDataType := cdtDate;
//    colDef.Field := 'Datum';
//    colDef.ValueFormatter := DateColumnFormatter;
//    // *************************************************
//
//    for gridHeader in FGridHeaders do
//    begin
//      colDef := dgDataGrid.ColumnDefs.Add;
//      colDef.CellDataType := cdtText;
//      colDef.EditModeType := cetCombobox;
//      colDef.Editable := true;
//      colDef.Sortable := false;
//      colDef.Field := gridHeader.Caption;
//      // for doctor in FDoctorsList do
//      // begin
//      // selopt := colDef.SelectOptions.Add();
//      // selopt.Text := doctor.Person.LastName;
//      // selopt.Value := doctor.PersonId;
//      // end;
//    end;
//    // fix for empty combobox in last column
//    colDef := dgDataGrid.ColumnDefs.Add;
//    colDef.Visible := false;
  finally
  end;

end;

procedure TFormPlanning.WebButton1Click(Sender: TObject);
var curSelected : TJSRowNode;
i: integer;
begin
  inherited;
//  curselected := dgDataGrid.FindFirstSelectedRowNode;
//  i := dgDataGrid.FindFirstSelectedRowIndex;
//  dgDataGrid.id
//ShowMessage(curSelected.Data.toString);
//showMessage(dgDataGrid.AGGrid.getFocusedCell.Column.ColId );
end;

procedure TFormPlanning.WebFormCreate(Sender: TObject);
begin
  FAppManager := TAppManager.GetInstance;
  FGridHeaders := TList<TGridHeader>.Create;
  FDoctorsList := TList<TDoctor>.Create;
  FShiftList := TActivityList.Create;
  FVerlofList := TActivityList.Create;
  FGrid := TStringList.Create;
  FYear := YearOf(now);
  FMonth := MonthOf(now);
  FDaysBefore := 5;
  FDaysAfter := 5;
  await(getDoctors);
  await(getShiftList);
  await(getVerlofList);
  getHeaders;
  initHeaders;
  buildGrid;
end;

procedure TFormPlanning.WebFormDestroy(Sender: TObject);
begin
  FGridHeaders.Free;
  FDoctorsList.Free;
  FShiftList.Free;
  FVerlofList.Free;
  FGrid.Free;
end;

{ TGridHeader }

constructor TGridHeader.Create(const AId, ACaption: string);
begin
  Id := AId;
  Caption := ACaption;
end;

end.
