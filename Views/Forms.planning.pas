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
  WEBLib.StdCtrls, libdatagrid, WEBLib.DataGrid.DataAdapter.base, WEBLib.DataGrid.DataAdapter.Custom, Types, WEBLib.Grids;

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

    acl: TWebElementActionList;
    //seperator
    [async]
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    function DateColumnFormatter(Value: string): string;
    // procedure dgDataGridCellEditingStarted(Params: TJSCellEditingEvent);
    procedure WebButton1Click(Sender: TObject);
    procedure aclacSetActiveCellExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);

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
    [async]
    procedure getDoctors;
    [async]
    procedure getShiftList;
    [async]
    procedure getVerlofList;

    procedure buildGrid;
    procedure SortShiftList;
    procedure GroupShiftsPerDate(out MapPerDatePerShift: TDictionary < TDate, TDictionary < string, TArray<TActivity> >> );
    procedure PopFront(var Arr: TArray<TActivity>);
  public
    { Public declarations }
  end;

var
  FormPlanning: TFormPlanning;

implementation

{$R *.dfm}
{ TFormPlanning }

procedure TFormPlanning.aclacSetActiveCellExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  activeCellList : TJSNodeList;
  i : integer;
  elementInList: TJSHTMLElement;
begin
  inherited;
  activeCellList := document.querySelectorAll('.active-cell');
  for i := 0 to activeCellList.length-1 do
    begin
    elementInList := TJSHTMLElement(activeCellList[i]);
    elementInList.classList.remove('active-cell');
    end;
Element.element.classList.add('active-cell');
end;

procedure TFormPlanning.BuildGrid;
var
  sb: TStringBuilder;
  FirstOfMonth, LastOfMonth, StartDate, EndDate: TDateTime;
  TotalDays, dagOffset, colIndex: Integer;
  CurrentDate: TDateTime;
  DisplayDatum, RawDatumISO, ClassAttr: string;
  MapPerDatePerShift: TDictionary<TDate, TDictionary<string, TArray<TActivity>>>;
  innerDict, RowMap: TDictionary<string, TArray<TActivity>>;
  shiftTypeId, displayLastName, activityId: string;
  actsArray: TArray<TActivity>;
  chosenAct: TActivity;
  d: TDate;
begin
  // 1) Sorteer en groepeer de shift-lijst
  SortShiftList;
  GroupShiftsPerDate(MapPerDatePerShift);

  // 2) Bepaal de kalenderperiode
  FirstOfMonth := EncodeDate(FYear, FMonth, 1);
  LastOfMonth  := EncodeDate(FYear, FMonth, DaysInAMonth(FYear, FMonth));
  StartDate    := IncDay(FirstOfMonth, -FDaysBefore);
  EndDate      := IncDay(LastOfMonth,  FDaysAfter);
  TotalDays    := Trunc(EndDate) - Trunc(StartDate) + 1;

  // 3) Begin HTML-opbouw
  sb := TStringBuilder.Create;
  try
    sb.AppendLine('<table id="shift-table" class="shift-table" style="border-collapse:collapse; width:100%;">');

    // 3a) thead met kolomtitels
    sb.AppendLine('  <thead>');
    sb.AppendLine('    <tr>');
    sb.AppendLine('      <th style="border:1px solid #ccc; padding:4px 6px;">Datum</th>');
    for colIndex := 0 to FGridHeaders.Count - 1 do
      sb.AppendFormat('      <th style="border:1px solid #ccc; padding:4px 6px;">%s</th>',
        [FGridHeaders[colIndex].Caption]).AppendLine;
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
        RowMap := TDictionary<string, TArray<TActivity>>.Create;
        try
          for shiftTypeId in innerDict.Keys do
            RowMap.Add(shiftTypeId, Copy(innerDict[shiftTypeId]));
        except
          RowMap.Free;
          raise;
        end;
      end
      else
        RowMap := TDictionary<string, TArray<TActivity>>.Create;

      // 3b.2) Start rijdag
      sb.AppendLine('    <tr>');

      // 3b.3) Eerste kolom: datum
      DisplayDatum := FormatDateTime('dd-mm-yyyy ddd', CurrentDate);
      RawDatumISO   := FormatDateTime('yyyy-mm-dd', CurrentDate);
      ClassAttr := '';
      if DayOfWeek(CurrentDate) in [1, 7] then
        ClassAttr := 'weekend';
      if SameDate(CurrentDate, Date) then
        ClassAttr := Trim(ClassAttr + ' today');

      sb.AppendFormat(
        '      <td data-date="%s" class="%s" style="border:1px solid #ccc; padding:4px 6px;">%s</td>',
        [RawDatumISO, ClassAttr, DisplayDatum]
      ).AppendLine;

      // 3b.4) Voor elke kolom in FGridHeaders de bijbehorende activity of leeg
      for colIndex := 0 to FGridHeaders.Count - 1 do
      begin
        shiftTypeId := FGridHeaders[colIndex].Id;
        if RowMap.TryGetValue(shiftTypeId, actsArray) then
        begin
          if Length(actsArray) > 0 then
          begin
            chosenAct := actsArray[0];
            displayLastName := chosenAct.Person.LastName;
            activityId := chosenAct.Id;

            sb.AppendFormat(
              '      <td data-shift-id="%s" data-activity-id="%s" class="shift-cell" title="Bijgewerkt: %s" ' +
              'style="border:1px solid #ccc; padding:4px 6px;">%s</td>',
              [ shiftTypeId,
                activityId,
                FormatDateTime('dd-mm-yyyy hh:nn', chosenAct.UpdatedAt),
                displayLastName ]
            ).AppendLine;

            PopFront(actsArray);
            RowMap[shiftTypeId] := actsArray;
          end
          else
            sb.AppendLine('      <td class="empty shift-cell" style="border:1px solid #ccc; padding:4px 6px;"></td>');
        end
        else
          sb.AppendLine('      <td class="empty shift-cell" style="border:1px solid #ccc; padding:4px 6px;"></td>');
      end;

      // 3b.5) Sluit rijdag af
      sb.AppendLine('    </tr>');
      RowMap.Free;
    end;
    sb.AppendLine('  </tbody>');

    // 3c) Sluit tabel
    sb.AppendLine('</table>');

    // 4) Schrijf in je DIV
    WebHTMLDiv1.HTML.Text := sb.ToString;
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

procedure TFormPlanning.getDoctors;
begin
  await(FAppManager.DB.getDoctors(FDoctorsList));
  FDoctorsList.Sort(TComparer<TDoctor>.Construct(
    function(const Left, Right: TDoctor): Integer
    begin
      result := CompareText(Left.Person.LastName, Right.Person.LastName);
    end));
end;

procedure TFormPlanning.getHeaders;
var
  mockGridHeader: TGridHeader;
begin
  FGridHeaders.Clear;
  mockGridHeader := TGridHeader.Create('C39ED775-592E-47BA-BCAD-87A08D9D554E', 'Dag 1');
  FGridHeaders.Add(mockGridHeader);
  mockGridHeader := TGridHeader.Create('5192843E-DA72-4A5D-BB66-664716351EA9', 'Nacht 1');
  FGridHeaders.Add(mockGridHeader);
  mockGridHeader := TGridHeader.Create('C39ED775-592E-47BA-BCAD-87A08D9D554E', 'Dag 2');
  FGridHeaders.Add(mockGridHeader);
  mockGridHeader := TGridHeader.Create('5192843E-DA72-4A5D-BB66-664716351EA9', 'Nacht 2');
  FGridHeaders.Add(mockGridHeader);
  mockGridHeader := TGridHeader.Create('2A86B3C0-BA35-487C-B07B-EB3F72C5AD85', 'Arts3');
  FGridHeaders.Add(mockGridHeader);
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

procedure TFormPlanning.GroupShiftsPerDate(out MapPerDatePerShift: TDictionary < TDate, TDictionary < string,
  TArray<TActivity> >> );
var
  act: TActivity;
  d: TDate;
  innerDict: TDictionary<string, TArray<TActivity>>;
  actsArray: TArray<TActivity>;
begin
  // Maakt een dictionary: Key = datum (TDate), Value = dictionary (ShiftTypeId → array<TActivity>)
  MapPerDatePerShift := TDictionary < TDate, TDictionary < string, TArray<TActivity> >>.Create;
  try
    for act in FShiftList do
    begin
      d := DateOf(act.Start);

      // Haal (of maak) per-datum dictionary op
      if not MapPerDatePerShift.TryGetValue(d, innerDict) then
      begin
        innerDict := TDictionary < string, TArray < TActivity >>.Create;
        MapPerDatePerShift.Add(d, innerDict);
      end;

      // Haal (of default) de array op voor deze ShiftTypeId
      if not innerDict.TryGetValue(act.shiftTypeId, actsArray) then
        actsArray := nil;

      // Voeg nu dit act toe achteraan (FShiftList is al in de juiste prioriteitsvolgorde)
      SetLength(actsArray, Length(actsArray) + 1);
      actsArray[High(actsArray)] := act;

      innerDict.AddOrSetValue(act.shiftTypeId, actsArray);
    end;
    // Let op: innerDict en arrays blijven bestaan; we freed MapPerDatePerShift later in BuildGrid
  except
    MapPerDatePerShift.Free;
    raise;
  end;
end;

procedure TFormPlanning.PopFront(var Arr: TArray<TActivity>);
var
  i: Integer;
begin
  // Verwijder het element op index 0 en schuif de rest naar voren
  if Length(Arr) <= 1 then
    SetLength(Arr, 0)
  else
  begin
    for i := 1 to High(Arr) do
      Arr[i - 1] := Arr[i];
    SetLength(Arr, Length(Arr) - 1);
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
    function(const A, B: TActivity): Integer
    begin
      // Vergelijk eerst op datum (zonder tijd)
      result := CompareDateTime(DateOf(A.Start), DateOf(B.Start));
      if result <> 0 then
        Exit;

      // Daarna op ShiftTypeId
      result := CompareText(A.shiftTypeId, B.shiftTypeId);
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
  curSelected: TJSRowNode; nl: tjsnodelist;
  i: Integer;
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
