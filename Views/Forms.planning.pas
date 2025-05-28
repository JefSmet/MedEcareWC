unit Forms.planning;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, System.Generics.Collections,
  orm.Person, orm.Activity, model.AppManager,
  WEBLib.REST, WEBLib.Actions, System.DateUtils, WEBLib.WebTools,
  WEBLib.DataGrid.Common, Vcl.Controls, Vcl.Grids, WEBLib.DataGrid,
  WEBLib.DataGrid.Options, WEBLib.WebCtrls, orm.Doctor, Vcl.StdCtrls,
  WEBLib.StdCtrls;

type
  TGridHeader = record
    Id: string;
    Caption: string;
    HeaderID: integer;
    constructor Create(const AId, ACaption: string; AHeaderID: integer = -1);
  end;

  TFormPlanning = class(TViewBase)
    dgDataGrid: TWebDataGrid;
    WebHTMLDiv1: TWebHTMLDiv;
    [async]
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    function DateColumnFormatter(Value: string): string;

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
    procedure RenderCalendar;
    procedure getHeaders;
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
  dgDataGrid.LoadFromStrings(FGrid, ',', '"', false);
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
end;

procedure TFormPlanning.getHeaders;
var
  cd: TDGColumnDefsCollectionItem;
begin
  dgDataGrid.ColumnDefs.Clear;
  cd := dgDataGrid.ColumnDefs.Add;
  cd.CellDataType := cdtDate;
  cd.Field := 'Datum';
  cd.ValueFormatter := DateColumnFormatter;
  dgDataGrid.ColumnDefs.Add.Field := 'Array Potter';
  dgDataGrid.ColumnDefs.Add.Field := 'Bananakin Skywalker';
  dgDataGrid.ColumnDefs.Add.Field := 'Obi Juan Kenobi';
  dgDataGrid.ColumnDefs.Add.Field := 'Count Broekoe';
  dgDataGrid.ColumnDefs.Add.Field := 'Darth Veester';
  dgDataGrid.ColumnDefs.Add.Field := 'General Tyfus';
  FGridHeaders.Add(TGridHeader.Create('1', '"Array Potter"'));
  FGridHeaders.Add(TGridHeader.Create('2', '"Bananakin Skywalker"'));
  FGridHeaders.Add(TGridHeader.Create('3', '"Obi Juan Kenobi"'));
  FGridHeaders.Add(TGridHeader.Create('4', '"Count Broekoe"'));
  FGridHeaders.Add(TGridHeader.Create('5', '"Darth Veester"'));
  FGridHeaders.Add(TGridHeader.Create('6', '"General Tyfus"'));
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

procedure TFormPlanning.RenderCalendar;
begin

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
  getHeaders;
  await(getDoctors);
  await(getShiftList);
  await(getVerlofList);
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

constructor TGridHeader.Create(const AId, ACaption: string; AHeaderID: integer);
begin
  Id := AId;
  Caption := ACaption;
  HeaderID := AHeaderID;
end;

end.
