unit Forms.planning;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, System.Generics.Collections,
  orm.Person, orm.Activity, model.AppManager,
  WEBLib.REST, WEBLib.Actions, System.DateUtils, WEBLib.WebTools,
  WEBLib.DataGrid.Common, Vcl.Controls, Vcl.Grids, WEBLib.DataGrid, WEBLib.DataGrid.Options, WEBLib.WebCtrls;

type
  TGridHeader = record
    Id: string;
    Caption: string;

    constructor Create(const AId, ACaption: string);
  end;

  TFormPlanning = class(TViewBase)
    dgDataGrid: TWebDataGrid;
    WebHTMLDiv1: TWebHTMLDiv;
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    function ColumnDateFormatter(Value: string): string;

  private

    FAllShiftList: TList<TActivity>;
    FShiftList: TList<TActivity>;
    FYear: word;
    FMonth: word;
    FDaysBefore: word;
    FDaysAfter: word;
    FGrid: TStringList;
    FGridHeaders: TList<TGridHeader>;
    procedure RenderCalendar;
    procedure filterVerlofList(AFilteredList: TActivityList; AName: string = '';
      AStatus: string = ''; AActivityType: string = ''; APersonId: string = '');
    procedure getHeaders;
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
  I, D: Integer;
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
  //FGrid.Add(headers);
  daycount := DaysInAMonth(FYear, FMonth) + FDaysBefore + FDaysAfter;
  date := EncodeDate(FYear, FMonth, 1);
  date := IncDay(date, -FDaysBefore);
  for D := 0 to daycount - 1 do
  begin
    row := IntToStr(trunc(date))+','+','+','+','+',';
    FGrid.Add(row);
    date := IncDay(date,1);
  end;
  dgDataGrid.LoadFromStrings(FGrid,',','"',false);
end;

{ TForm2 }

function TFormPlanning.ColumnDateFormatter(Value: string): string;
var
  dt: TDateTime;
begin
dt :=  StrToFloat(Value);
result := FormatDateTime('dd-mm-yyy ddd',dt);
end;

procedure TFormPlanning.filterVerlofList(AFilteredList: TActivityList;
  AName, AStatus, AActivityType, APersonId: string);
var
  Activity: TActivity;
  matchesFilter: Boolean;
begin
  AFilteredList.Clear;
  // Loop door alle activiteiten en pas de filter toe
  for Activity in FAllShiftList do
  begin
    matchesFilter := True;

    if (APersonId <> '') then
    begin
      matchesFilter := (Activity.PersonId = APersonId);
    end;

    // Filter op naam (als er een naam is opgegeven)
    if (AName <> '') and
      (Pos(LowerCase(AName), LowerCase(Activity.Person.LastName)) = 0) and
      (Pos(LowerCase(AName), LowerCase(Activity.Person.FirstName)) = 0) then
      matchesFilter := False;

    // Filter op status (als er een status is opgegeven)
    if (AStatus <> '') and (LowerCase(Activity.Status) <> LowerCase(AStatus))
    then
      matchesFilter := False;

    // Filter op status (als er een status is opgegeven)
    if (AActivityType <> '') and
      (LowerCase(Activity.ActivityType) <> LowerCase(AActivityType)) then
      matchesFilter := False;

    // Als deze activiteit door alle filters komt, voeg toe aan de tijdelijke lijst
    if matchesFilter then
      AFilteredList.Add(Activity);
  end;
end;

procedure TFormPlanning.getHeaders;
var
  cd: TDGColumnDefsCollectionItem;
begin
  dgDataGrid.ColumnDefs.Clear;
  cd := dgDataGrid.ColumnDefs.Add;
  cd.CellDataType:=cdtDate;
  cd.Field:='Datum';
  cd.ValueFormatter:=ColumnDateFormatter;
  dgDataGrid.ColumnDefs.Add.Field:='Array Potter';
  dgDataGrid.ColumnDefs.Add.Field:='Bananakin Skywalker';
  dgDataGrid.ColumnDefs.Add.Field:='Obi Juan Kenobi';
  dgDataGrid.ColumnDefs.Add.Field:='Count Broekoe';
  dgDataGrid.ColumnDefs.Add.Field:='Darth Veester';
  dgDataGrid.ColumnDefs.Add.Field:='General Tyfus';
  FGridHeaders.Add(TGridHeader.Create('1', '"Array Potter"'));
  FGridHeaders.Add(TGridHeader.Create('2', '"Bananakin Skywalker"'));
  FGridHeaders.Add(TGridHeader.Create('3', '"Obi Juan Kenobi"'));
  FGridHeaders.Add(TGridHeader.Create('4', '"Count Broekoe"'));
  FGridHeaders.Add(TGridHeader.Create('5', '"Darth Veester"'));
  FGridHeaders.Add(TGridHeader.Create('6', '"General Tyfus"'));
end;

procedure TFormPlanning.RenderCalendar;
const
  DaysPerWeek = 7;
var
  sb: TStringBuilder;
  FirstOfMonth, CellDate, TodayDate: TDateTime;
  StartDow, DaysInMonth, DayCounter: Integer;
  WeekIdx, WeekDayIdx, PascalDow: Integer;
  DayName, ClassAttr: string;
  I: Integer;
begin
  filterVerlofList(FShiftList, '', 'Approved');
  // 1) Bereken eerste dag en aantal dagen
  FirstOfMonth := EncodeDate(FYear, FMonth, 1);
  StartDow := DayOfWeek(FirstOfMonth); // 1=zo � 7=za
  DaysInMonth := DaysInAMonth(FYear, FMonth);
  TodayDate := date;

  // 2) Bepaal in welke kolom dag 1 valt (1..7)
  // kolom = ((StartDow - AStartDow) mod 7) + 1
  DayCounter := 1;

  sb := TStringBuilder.Create;
  try
    sb.AppendLine
      ('<table id="calendar-table" class="table calendar-table mb-0">');
    sb.AppendLine('  <thead>');
    sb.AppendLine('    <tr>');
    // 2.1) Header: rotatie beginnend bij AStartDow
    for WeekDayIdx := 1 to DaysPerWeek do
    begin
      PascalDow := ((2 - 1 + WeekDayIdx - 1) mod DaysPerWeek) + 1;
      DayName := GetLocaleShortDayName(PascalDow, GetBrowserLocale);
      sb.AppendFormat('      <th>%s</th>', [DayName]).AppendLine;
    end;
    sb.AppendLine('    </tr>');
    sb.AppendLine('  </thead>');
    sb.AppendLine('  <tbody>');

    // 2.2) Vul de rijen met dagen
    for WeekIdx := 0 to 5 do
    begin
      sb.AppendLine('    <tr>');
      for WeekDayIdx := 1 to DaysPerWeek do
      begin
        // Eerste week: voor de 1e dag, of na het einde van de maand?
        if ((WeekIdx = 0) and (WeekDayIdx < ((StartDow - 2 + DaysPerWeek)
          mod DaysPerWeek) + 1)) or (DayCounter > DaysInMonth) then
        begin
          sb.AppendLine('      <td>&nbsp;</td>')
        end
        else
        begin
          CellDate := EncodeDate(FYear, FMonth, DayCounter);
          ClassAttr := '';

          // Weekend?
          if DayOfWeek(CellDate) in [1, 7] then
            ClassAttr := 'weekend';

          // Vandaag?
          if SameDate(CellDate, TodayDate) then
            if ClassAttr <> '' then
              ClassAttr := ClassAttr + ' today'
            else
              ClassAttr := 'today';

          // Open <td> met id en eventueel class
          if ClassAttr <> '' then
            sb.AppendFormat('      <td id="day-%d-%.2d-%.2d" class="%s">',
              [FYear, FMonth, DayCounter, ClassAttr]).AppendLine
          else
            sb.AppendFormat('      <td id="day-%d-%.2d-%.2d">',
              [FYear, FMonth, DayCounter]).AppendLine;

          sb.AppendFormat('        %d', [DayCounter]).AppendLine;

          for I := FShiftList.Count - 1 downto 0 do
          begin
            if SameDate(DateOf(FShiftList[I].Start), CellDate) then
            begin
              sb.AppendFormat('        <div class="event">%s</div>',
                [FShiftList[I].Person.LastName]).AppendLine;
              // FVerlofLijst.Delete(I);
            end;

          end;
          sb.AppendLine('      </td>');
          Inc(DayCounter);
        end;
      end;
      sb.AppendLine('    </tr>');
      if DayCounter > DaysInMonth then
        Break;
    end;

    sb.AppendLine('  </tbody>');
    sb.AppendLine('</table>');

    Document.getElementById('calendar-table').innerHTML := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TFormPlanning.WebFormCreate(Sender: TObject);
begin
  FGridHeaders := TList<TGridHeader>.Create;
  FGrid := TStringList.Create;
  FYear := YearOf(now);
  FMonth := MonthOf(now);
  FDaysBefore := 7;
  FDaysAfter := 7;
  getHeaders;
  buildGrid;
end;

procedure TFormPlanning.WebFormDestroy(Sender: TObject);
begin
  FGridHeaders.Free;
  FGrid.Free;
end;

{ TGridHeader }

constructor TGridHeader.Create(const AId, ACaption: string);
begin
  Id := AId;
  Caption := ACaption;
end;

end.
