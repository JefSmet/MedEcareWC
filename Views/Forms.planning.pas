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
  WEBLib.StdCtrls, libdatagrid, WEBLib.DataGrid.DataAdapter.base,
  WEBLib.DataGrid.DataAdapter.Custom, Types, WEBLib.Grids, orm.Roster,
  orm.ShiftType;

type
  TGridHeader = record
    Id: string;
    Caption: string;
    Roster: TRoster;
    constructor Create(const AId, ACaption: string);
  end;

  TFormPlanning = class(TViewBase)
    Calendar: TWebHTMLDiv;
    RightPanel: TWebHTMLDiv;

    acl: TWebElementActionList;

    AvailableDoctors: TWebHTMLDiv;

    [async]
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    function DateColumnFormatter(Value: string): string;
    // procedure dgDataGridCellEditingStarted(Params: TJSCellEditingEvent);
    procedure WebButton1Click(Sender: TObject);
    procedure aclacSetActiveCellExecute(Sender: TObject;
      Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    procedure aclacCalendarButtonExecute(Sender: TObject;
      Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]
    procedure aclacDoctorButtonExecute(Sender: TObject;
      Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]procedure aclacDeleteActivityExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    procedure filterDoctors;

  private
    FShiftList: TList<TActivity>;
    FVerlofList: TList<TActivity>;
    FDoctorsList: TList<TDoctor>;
    FRosters: TList<TRoster>;
    FYear: word;
    FMonth: word;
    FDaysBefore: word;
    FDaysAfter: word;
    FGrid: TStringList;
    FAppManager: TAppManager;
    FSelectedDate: Tdate;
    FSelectedColumnIndex: integer;
    FSelectedCell: TJSHTMLElement;
    [async]
    procedure getRosters;
    [async]
    procedure getDoctors;
    [async]
    procedure getShiftList;
    [async]
    procedure getVerlofList;
    [async]
    procedure doUpdateShiftList;

    procedure buildGrid;
    procedure SortShiftList;
    procedure GroupShiftsPerDate(out MapPerDatePerShift: TDictionary < Tdate,
      TDictionary < string, TArray<TActivity> >> );
    procedure PopFront(var Arr: TArray<TActivity>);
    procedure renderAvailableDoctors;
    [async]
    procedure fillCell(AActivity: TActivity);
  public
    { Public declarations }
  end;

var
  FormPlanning: TFormPlanning;

implementation

{$R *.dfm}
{ TFormPlanning }

procedure TFormPlanning.aclacCalendarButtonExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  encodatedDate: TDateTime;
  day: word;
  toInc: integer;
begin
  if Element.Element = document.getElementById('calendar-prev') then
    toInc := -1
  else
    toInc := 1;
  encodatedDate := EncodeDate(FYear, FMonth, 1);

  encodatedDate := IncMonth(encodatedDate, toInc);
  DecodeDate(encodatedDate, FYear, FMonth, day);
  document.getElementById('calendar-month').innerHTML :=
    FormatDateTime('mmmm yyyy', encodatedDate);
  FSelectedDate := encodatedDate;
  FSelectedColumnIndex := 0;
  doUpdateShiftList;
end;

procedure TFormPlanning.aclacDeleteActivityExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  id: string;
begin
  inherited;
if FSelectedCell.classList.contains('empty') then
exit;
id:=FSelectedCell.getAttribute('activityId');
await(Boolean,FAppManager.DB.DeleteActivity(id));
await(getShiftList);
buildGrid;
FSelectedCell := TJSHTMLElement(document.querySelector('.active-cell'));
end;

procedure TFormPlanning.aclacDoctorButtonExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  Activity: TActivity;
  Roster: TRoster;
begin
  Roster := FRosters[strToInt(FSelectedCell.getAttribute('colindex'))];

  Activity.ActivityType := 'SHIFT';
  Activity.Start := strToInt(FSelectedCell.getAttribute('dateTime')) +
    EncodeTime(Roster.Starthour, Roster.StartMinute, 0, 0);
  Activity.EndTime := IncMinute(Activity.Start, Roster.DurationMinutes);
  Activity.PersonId := Element.Element.getAttribute('doctor-id');
  Activity.ShiftTypeId := Roster.ShiftTypeId;
  Activity.Status := 'SCHEDULED';

  if FSelectedCell.classList.contains('empty') then
    await(FAppManager.DB.PostActivity(Activity.Status, Activity.ActivityType,
      Activity.Start, Activity.EndTime, Activity.PersonId,
      Activity.ShiftTypeId))

  else
  begin
    Activity.Id := FSelectedCell.getAttribute('activityId');
    await(FAppManager.DB.PutActivity(Activity.Id, Activity.Status,
      Activity.ActivityType, Activity.Start, Activity.EndTime,
      Activity.PersonId, Activity.ShiftTypeId));
  end;
  FSelectedColumnIndex := (FSelectedColumnIndex + 1) mod FRosters.Count;
  if FSelectedColumnIndex = 0 then
    FSelectedDate := IncDay(FSelectedDate);

  await(getShiftList);
  buildGrid;
  FSelectedCell := TJSHTMLElement(document.querySelector('.active-cell'));
end;

procedure TFormPlanning.aclacSetActiveCellExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  activeCellList: TJSNodeList;
  i: integer;
  elementInList: TJSHTMLElement;
begin
  FSelectedCell := Element.Element;
  activeCellList := document.querySelectorAll('.active-cell');
  for i := 0 to activeCellList.length - 1 do
  begin
    elementInList := TJSHTMLElement(activeCellList[i]);
    elementInList.classList.remove('active-cell');
  end;
  FSelectedDate := strToInt(Element.Element.getAttribute('dateTime'));
  FSelectedColumnIndex := strToInt(Element.Element.getAttribute('colindex'));
  Element.Element.classList.add('active-cell');
  renderAvailableDoctors;
end;

procedure TFormPlanning.buildGrid;
var
  sb: TStringBuilder;
  FirstOfMonth, LastOfMonth, StartDate, EndDate: TDateTime;
  TotalDays, dagOffset, colIndex: integer;
  CurrentDate: TDateTime;
  DisplayDatum, RawDatumISO, ClassAttr: string;
  MapPerDatePerShift
    : TDictionary<Tdate, TDictionary<string, TArray<TActivity>>>;
  innerDict, RowMap: TDictionary<string, TArray<TActivity>>;
  ShiftTypeId, displayLastName, activityId: string;
  actsArray: TArray<TActivity>;
  chosenAct: TActivity;
  d: Tdate;
  selectedCell: string;
begin
  // 1) Sorteer en groepeer de shift-lijst
  SortShiftList;
  GroupShiftsPerDate(MapPerDatePerShift);

  // 2) Bepaal de kalenderperiode
  FirstOfMonth := EncodeDate(FYear, FMonth, 1);
  LastOfMonth := EncodeDate(FYear, FMonth, DaysInAMonth(FYear, FMonth));
  StartDate := IncDay(FirstOfMonth, -FDaysBefore);
  EndDate := IncDay(LastOfMonth, FDaysAfter);
  TotalDays := Trunc(EndDate) - Trunc(StartDate) + 1;

  // 3) Begin HTML-opbouw
  sb := TStringBuilder.Create;
  try
    sb.AppendLine
      ('<table id="shift-table" class="shift-table" style="border-collapse:collapse; width:100%;">');

    // 3a) thead met kolomtitels
    sb.AppendLine('  <thead>');
    sb.AppendLine('    <tr>');
    sb.AppendLine
      ('      <th style="border:1px solid #ccc; padding:4px 6px;">Datum</th>');
    for colIndex := 0 to FRosters.Count - 1 do
      sb.AppendFormat
        ('      <th style="border:1px solid #ccc; padding:4px 6px;">%s</th>',
        [FRosters[colIndex].ShiftTypeName]).AppendLine;
    sb.AppendLine('    </tr>');
    sb.AppendLine('  </thead>');

    // 3b) tbody met per dag een rij
    sb.AppendLine('  <tbody>');
    for dagOffset := 0 to TotalDays - 1 do
    begin
      CurrentDate := IncDay(StartDate, dagOffset);
      d := DateOf(CurrentDate);

      // 3b.1) Maak per dag een kopie‐map RowMap (ShiftTypeId → array<TActivity>)
      if MapPerDatePerShift.TryGetValue(d, innerDict) then
      begin
        RowMap := TDictionary < string, TArray < TActivity >>.Create;
        try
          for ShiftTypeId in innerDict.Keys do
            RowMap.add(ShiftTypeId, Copy(innerDict[ShiftTypeId]));
        except
          RowMap.Free;
          raise;
        end;
      end
      else
        RowMap := TDictionary < string, TArray < TActivity >>.Create;

      // 3b.2) Start rijdag
      sb.AppendLine('    <tr>');

      // 3b.3) Eerste kolom: datum
      DisplayDatum := FormatDateTime('dd-mm-yyyy ddd', CurrentDate);
      RawDatumISO := FormatDateTime('yyyy-mm-dd', CurrentDate);
      ClassAttr := '';
      if DayOfWeek(CurrentDate) in [1, 7] then
        ClassAttr := 'weekend';
      if SameDate(CurrentDate, Date) then
        ClassAttr := Trim(ClassAttr + ' today');

      sb.AppendFormat
        ('      <td data-date="%s" class="%s" style="border:1px solid #ccc; padding:4px 6px;">%s</td>',
        [RawDatumISO, ClassAttr, DisplayDatum]).AppendLine;

      // 3b.4) Voor elke kolom in FGridHeaders de bijbehorende activity of leeg
      for colIndex := 0 to FRosters.Count - 1 do
      begin
        ShiftTypeId := FRosters[colIndex].ShiftTypeId;
        if RowMap.TryGetValue(ShiftTypeId, actsArray) then
        begin
          if length(actsArray) > 0 then
          begin
            chosenAct := actsArray[0];
            displayLastName := chosenAct.Person.LastName;
            activityId := chosenAct.Id;
            if (CurrentDate = FSelectedDate) and
              (colIndex = FSelectedColumnIndex) then
              selectedCell := 'active-cell'
            else
              selectedCell := '';
            sb.AppendFormat
              ('      <td activityId="%s" dateTime="%d" colindex="%d" data-shift-id="%s" data-activity-id="%s" class="shift-cell %s" title="Bijgewerkt: %s" '
              + 'style="border:1px solid #ccc; padding:4px 6px;">%s</td>',
              [chosenAct.Id, Trunc(CurrentDate), colIndex, ShiftTypeId,
              activityId, selectedCell, FormatDateTime('dd-mm-yyyy hh:nn',
              chosenAct.UpdatedAt), displayLastName]).AppendLine;

            PopFront(actsArray);
            RowMap[ShiftTypeId] := actsArray;
          end
          else
          begin
            if (CurrentDate = FSelectedDate) and
              (colIndex = FSelectedColumnIndex) then
              selectedCell := 'active-cell'
            else
              selectedCell := '';
            sb.AppendFormat
              ('      <td dateTime="%d" colindex="%d" class="empty shift-cell %s" style="border:1px solid #ccc; padding:4px 6px;"></td>',
              [Trunc(CurrentDate), colIndex, selectedCell]).AppendLine;
          end;
        end
        else
        begin
          if (CurrentDate = FSelectedDate) and (colIndex = FSelectedColumnIndex)
          then
            selectedCell := 'active-cell'
          else
            selectedCell := '';
          sb.AppendFormat
            ('      <td dateTime="%d" colindex="%d" class="empty shift-cell %s" style="border:1px solid #ccc; padding:4px 6px;"></td>',
            [Trunc(CurrentDate), colIndex, selectedCell]).AppendLine;
        end;
      end;

      // 3b.5) Sluit rijdag af
      sb.AppendLine('    </tr>');
      RowMap.Free;
    end;
    sb.AppendLine('  </tbody>');

    // 3c) Sluit tabel
    sb.AppendLine('</table>');

    // 4) Schrijf in je DIV
    Calendar.HTML.Text := sb.ToString;
  finally
    acl.BindActions;
    sb.Free;
    for innerDict in MapPerDatePerShift.Values do
      innerDict.Free;
    MapPerDatePerShift.Free;
  end;
end;

function TFormPlanning.DateColumnFormatter(Value: string): string;
var
  dt: TDateTime;
begin
  dt := StrToFloat(Value);
  result := FormatDateTime('dd-mm-yyy ddd', dt);
end;

procedure TFormPlanning.doUpdateShiftList;
begin
  await(getShiftList);
  buildGrid;
end;

procedure TFormPlanning.fillCell(AActivity: TActivity);
begin

end;

procedure TFormPlanning.filterDoctors;
var
  AvailableDoctors: TList<TDoctor>;
  sb: TStringBuilder;
  Doctor: TDoctor;
begin
  AvailableDoctors := TList<TDoctor>.Create;
  sb := TStringBuilder.Create;
  try
    // filter
    AvailableDoctors.AddRange(FDoctorsList);
    for Doctor in AvailableDoctors do
    begin
      sb.AppendLine
        (Format('<button doctor-id="%s" type="button" class ="available-doctor-button btn btn-success">%s</button>',
        [Doctor.PersonId, Doctor.Person.LastName]));
    end;
    document.getElementById('availableDoctors').innerHTML := sb.ToString;
    acl.BindActions;
  finally
    AvailableDoctors.Free;
    sb.Free;
  end;
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

procedure TFormPlanning.getRosters;
begin

  FRosters.Clear;

  await(FAppManager.DB.getRosters(FRosters));
end;

procedure TFormPlanning.getShiftList;
var
  StartDate, EndDate, firstDayOfNowMonth, lastDayOfNowMonth: TDateTime;
begin
  firstDayOfNowMonth := EncodeDate(FYear, FMonth, 1);

  lastDayOfNowMonth := EncodeDate(FYear, FMonth, DaysInAMonth(FYear, FMonth));
  StartDate := IncDay(firstDayOfNowMonth, -FDaysBefore);
  EndDate := IncDay(lastDayOfNowMonth, FDaysAfter);
  await(FAppManager.DB.getShifts(StartDate, EndDate, FShiftList));
end;

procedure TFormPlanning.getVerlofList;
var
  StartDate, EndDate, firstDayOfNowMonth, lastDayOfNowMonth: TDateTime;
begin
  firstDayOfNowMonth := EncodeDate(FYear, FMonth, 1);

  lastDayOfNowMonth := EncodeDate(FYear, FMonth, DaysInAMonth(FYear, FMonth));
  StartDate := IncDay(firstDayOfNowMonth, -FDaysBefore);
  EndDate := IncDay(lastDayOfNowMonth, FDaysAfter);
  await(FAppManager.DB.getVerlof(StartDate, EndDate, FVerlofList));
end;

procedure TFormPlanning.GroupShiftsPerDate(out MapPerDatePerShift: TDictionary <
  Tdate, TDictionary < string, TArray<TActivity> >> );
var
  act: TActivity;
  d: Tdate;
  innerDict: TDictionary<string, TArray<TActivity>>;
  actsArray: TArray<TActivity>;
begin
  // Maakt een dictionary: Key = datum (TDate), Value = dictionary (ShiftTypeId → array<TActivity>)
  MapPerDatePerShift := TDictionary < Tdate, TDictionary < string,
    TArray<TActivity> >>.Create;
  try
    for act in FShiftList do
    begin
      d := DateOf(act.Start);

      // Haal (of maak) per-datum dictionary op
      if not MapPerDatePerShift.TryGetValue(d, innerDict) then
      begin
        innerDict := TDictionary < string, TArray < TActivity >>.Create;
        MapPerDatePerShift.add(d, innerDict);
      end;

      // Haal (of default) de array op voor deze ShiftTypeId
      if not innerDict.TryGetValue(act.ShiftTypeId, actsArray) then
        actsArray := nil;

      // Voeg nu dit act toe achteraan (FShiftList is al in de juiste prioriteitsvolgorde)
      SetLength(actsArray, length(actsArray) + 1);
      actsArray[High(actsArray)] := act;

      innerDict.AddOrSetValue(act.ShiftTypeId, actsArray);
    end;
    // Let op: innerDict en arrays blijven bestaan; we freed MapPerDatePerShift later in BuildGrid
  except
    MapPerDatePerShift.Free;
    raise;
  end;
end;

procedure TFormPlanning.PopFront(var Arr: TArray<TActivity>);
var
  i: integer;
begin
  // Verwijder het element op index 0 en schuif de rest naar voren
  if length(Arr) <= 1 then
    SetLength(Arr, 0)
  else
  begin
    for i := 1 to High(Arr) do
      Arr[i - 1] := Arr[i];
    SetLength(Arr, length(Arr) - 1);
  end;
end;

procedure TFormPlanning.renderAvailableDoctors;
var
  sb: TStringBuilder;
  Doctor: TDoctor;
  Act: TActivity;
  IsOnLeave: Boolean;
begin
  sb := TStringBuilder.Create;
  try
    for Doctor in FDoctorsList do
    begin
      // 1) Check of deze dokter verlof heeft op FSelectedDate
      IsOnLeave := False;
      for Act in FVerlofList do
      begin
        if (Act.PersonId = Doctor.PersonId) and
           // let op: Act.Start en Act.EndTime bevatten mogelijk
           // begin- en eindtijd van de verlof-periode
           (DateOf(Act.Start) <= FSelectedDate) and
           (FSelectedDate <= DateOf(Act.EndTime)) then
        begin
          IsOnLeave := True;
          Break;
        end;
      end;

      // 2) Alleen als hij/zij niet op verlof is, maak je een knop
      if not IsOnLeave then
      begin
        sb.AppendLine(
          Format(
            '<button doctor-id="%s" type="button" class="available-doctor-button btn btn-success">%s</button>',
            [ Doctor.PersonId,
              Doctor.Person.LastName
            ]
          )
        );
      end;
    end;

    // 3) Render de buttons
    document.getElementById('availableDoctors').innerHTML := sb.ToString;
    acl.BindActions;
  finally
    sb.Free;
  end;
end;

procedure TFormPlanning.SortShiftList;
begin
  // Sorteer FShiftList op:
  // 1) Start-datum oplopend
  // 2) ShiftTypeId (alfabetisch)
  // 3) UpdatedAt desc (nieuwste eerst)
  // 4) CreatedAt desc
  FShiftList.Sort(TComparer<TActivity>.Construct(
    function(const A, B: TActivity): integer
    begin
      // Vergelijk eerst op datum (zonder tijd)
      result := CompareDateTime(DateOf(A.Start), DateOf(B.Start));
      if result <> 0 then
        Exit;

      // Daarna op ShiftTypeId
      result := CompareText(A.ShiftTypeId, B.ShiftTypeId);
      if result <> 0 then
        Exit;

      // Daarna UpdatedAt DESC
      result := CompareDateTime(B.UpdatedAt, A.UpdatedAt);
      if result <> 0 then
        Exit;

      // Tenslotte CreatedAt DESC
      result := CompareDateTime(B.CreatedAt, A.CreatedAt);
    end));
end;

procedure TFormPlanning.WebButton1Click(Sender: TObject);
var
  curSelected: TJSRowNode;
  nl: TJSNodeList;
  i: integer;
begin
  inherited;
  document.querySelectorAll('')
  // curselected := dgDataGrid.FindFirstSelectedRowNode;
  // i := dgDataGrid.FindFirstSelectedRowIndex;
  // dgDataGrid.id
  // ShowMessage(curSelected.Data.toString);
  // showMessage(dgDataGrid.AGGrid.getFocusedCell.Column.ColId );
end;

procedure TFormPlanning.WebFormCreate(Sender: TObject);
var
  day: word;
begin
  FAppManager := TAppManager.GetInstance;
  FRosters := TList<TRoster>.Create;
  FDoctorsList := TList<TDoctor>.Create;
  FShiftList := TActivityList.Create;
  FVerlofList := TActivityList.Create;
  FGrid := TStringList.Create;
  DecodeDate(now, FYear, FMonth, day);
  document.getElementById('calendar-month').innerHTML :=
    FormatDateTime('mmmm yyyy', now);
  FDaysBefore := 5;
  FDaysAfter := 5;
  FSelectedDate := EncodeDate(FYear, FMonth, 1);
  FSelectedColumnIndex := 0;
  await(getDoctors);
  await(getShiftList);
  await(getVerlofList);
  await(getRosters);
  buildGrid;
  FSelectedCell := TJSHTMLElement(document.querySelector('.active-cell'));
  renderAvailableDoctors;
end;

procedure TFormPlanning.WebFormDestroy(Sender: TObject);
begin
  FRosters.Free;
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
