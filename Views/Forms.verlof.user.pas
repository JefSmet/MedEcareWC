unit Forms.verlof.user;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.StdCtrls, WEBLib.StdCtrls,
  Vcl.Controls, WEBLib.WebCtrls, WEBLib.Lists, WEBLib.WebTools,
  System.Generics.Collections, orm.Person, orm.Activity, model.AppManager,
  WEBLib.REST, WEBLib.Actions, System.DateUtils;

type
  TFormVerlofUser = class(TWebForm)
    reject1: TWebButton;
    tabrequestbtn: TWebButton;
    filtertype: TWebComboBox;
    list: TWebHTMLDiv;
    calendarnext: TWebButton;
    tabcalendarbtn: TWebButton;
    enddate: TWebDateTimePicker;
    request: TWebHTMLDiv;
    submitrequest: TWebButton;
    startdate: TWebDateTimePicker;
    leaveTabsContent: TWebHTMLDiv;
    calendarprev: TWebButton;
    calendartable: TWebHTMLDiv;
    calendarlegend: TWebHTMLDiv;
    leavetype: TWebComboBox;
    tablistbtn: TWebButton;
    searchinput: TWebEdit;
    calendarmonth: TWebHTMLSpan;
    calendar: TWebHTMLDiv;
    filterstatus: TWebComboBox;
    approve1: TWebButton;
    btnTest: TWebButton;
    webElementAL: TWebElementActionList;
    [async]
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure calendarButtonClick(Sender: TObject);
  private
    FAppmanager: TAppManager;
    FYear: Word;
    FMonth: Word;
    FVerlofLijst: TActivityList;
    FAdminVerlofLijst: TActivityList;
    FAllVerlofLijst: TActivityList;
    [async]
    procedure DoTest;
    [async]
    function GetVerlofList: Boolean;
    [async]
    procedure DoDateChanged;
    procedure checkInitAdminUi;
    procedure renderCalendar;
    procedure renderList;
    procedure filterVerlofList(AFilteredList: TActivityList; AName: string = '';
      AStatus: string = '');
  public
    { Public declarations }
  end;

var
  FormVerlofUser: TFormVerlofUser;

implementation

{$R *.dfm}

procedure TFormVerlofUser.btnTestClick(Sender: TObject);
begin
  DoTest;
end;

procedure TFormVerlofUser.DoDateChanged;
begin

  await(Boolean,GetVerlofList);
  renderCalendar;
//  renderList;
end;

procedure TFormVerlofUser.DoTest;
var
  verlofList: TList<TActivity>;
begin
  verlofList := TList<TActivity>.Create;
  try
    await(Boolean, FAppmanager.DB.GetActivities('Verlof', 2025, 05,
      verlofList));
    ShowMessage(verlofList.First.Person.FirstName);
  finally
    verlofList.Free;
  end;
end;

procedure TFormVerlofUser.filterVerlofList(AFilteredList: TActivityList;
  AName, AStatus: string);
var
  Activity: TActivity;
  matchesFilter: Boolean;
begin
  AFilteredList.Clear;
  // Loop door alle activiteiten en pas de filter toe
  for Activity in FAllVerlofLijst do
  begin
    matchesFilter := True;

    // Filter op naam (als er een naam is opgegeven)
    if (AName <> '') and
      (Pos(LowerCase(AName), LowerCase(Activity.Person.LastName)) = 0) and
      (Pos(LowerCase(AName), LowerCase(Activity.Person.FirstName)) = 0) then
      matchesFilter := False;

    // Filter op status (als er een status is opgegeven)
    if (AStatus <> '') and (LowerCase(Activity.Status) <> LowerCase(AStatus))
    then
      matchesFilter := False;

    // Als deze activiteit door alle filters komt, voeg toe aan de tijdelijke lijst
    if matchesFilter then
      AFilteredList.Add(Activity);
  end;
end;

function TFormVerlofUser.GetVerlofList: Boolean;
begin
  try
    await(Boolean, FAppmanager.DB.GetActivities('Verlof', FYear, FMonth,
      FAllVerlofLijst));
    Exit(True);
  except
    result := False;
    raise;
  end;
end;

procedure TFormVerlofUser.renderCalendar;
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
  filterVerlofList(FVerlofLijst, '', 'Approved');
  // 1) Bereken eerste dag en aantal dagen
  FirstOfMonth := EncodeDate(FYear, FMonth, 1);
  StartDow := DayOfWeek(FirstOfMonth); // 1=zo … 7=za
  DaysInMonth := DaysInAMonth(FYear, FMonth);
  TodayDate := Date;

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
        if ((WeekIdx = 0) and
          (WeekDayIdx < ((StartDow - 2 + DaysPerWeek) mod DaysPerWeek) +
          1)) or (DayCounter > DaysInMonth) then
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

          for I := FVerlofLijst.Count - 1 downto 0 do
          begin
            if SameDate(DateOf(FVerlofLijst[I].Start), CellDate) then
            begin
              sb.AppendFormat('        <div class="event">%s</div>',
                [FVerlofLijst[I].Person.LastName]).AppendLine;
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

procedure TFormVerlofUser.renderList;
var
  searchQuery, Status: string;
  sb: TStringBuilder;
  act: TActivity;
  dateRange: string;
  days: Integer;
begin
  if not FAppmanager.Auth.currentPerson.Roles.Contains('admin') then
    Exit;

  searchQuery := searchinput.Text;
  Status := filterstatus.Items[filterstatus.ItemIndex];
  filterVerlofList(FAdminVerlofLijst, searchQuery, Status);

  sb := TStringBuilder.Create;
  try
    for act in FAdminVerlofLijst do
    begin
      dateRange := FormatDateTime('mmm dd, yyyy', act.Start) + ' - ' +
        FormatDateTime('mmm dd, yyyy', act.EndTime);
      days := DaysBetween(DateOf(act.EndTime), DateOf(act.Start)) + 1;
      sb.AppendFormat('<tr data-request-id="%s">', [act.Id]).AppendLine;
      sb.AppendFormat('  <td>%s %s</td>', [act.Person.FirstName,
        act.Person.LastName]).AppendLine;
      sb.AppendFormat('  <td>%s</td>', [dateRange]).AppendLine;
      sb.AppendFormat
        ('  <td><span class="badge bg-secondary">%s</span> %d days</td>',
        [act.ActivityType, days]).AppendLine;
      sb.AppendLine('  <td></td>');
      sb.AppendFormat('  <td><span class="badge bg-secondary">%s</span></td>',
        [act.Status]).AppendLine;
      sb.AppendFormat
        ('  <td><button type="button" class="btn btn-sm btn-success" data-action="approve" data-id="%s"><i class="bi bi-check-lg"></i></button>',
        [act.Id]).AppendLine;
      sb.AppendFormat
        ('      <button type="button" class="btn btn-sm btn-danger" data-action="reject" data-id="%s"><i class="bi bi-x-lg"></i></button></td>',
        [act.Id]).AppendLine;
      sb.AppendLine('</tr>');
    end;
    Document.querySelector('#requests-table tbody').innerHTML := sb.ToString;
  finally
    sb.Free;
  end;

end;

procedure TFormVerlofUser.calendarButtonClick(Sender: TObject);
var
  encodatedDate: TDateTime;
  day: Word;
  toInc: integer;
begin
  if Sender = calendarprev then
    toInc := -1
  else
    toInc := 1;
  encodatedDate := EncodeDate(FYear, FMonth, 1);
  encodatedDate := IncMonth(encodatedDate,toInc);
  DecodeDate(encodatedDate, FYear, FMonth, day);
  DoDateChanged;
end;

procedure TFormVerlofUser.checkInitAdminUi;
begin
  if FAppmanager.Auth.currentPerson.Roles.Contains('admin') then
    Document.getElementById('listViewTab').classList.remove('d-none');
end;

procedure TFormVerlofUser.WebFormCreate(Sender: TObject);
var
  day: Word;
begin
  FAppmanager := TAppManager.GetInstance;
  FVerlofLijst := TActivityList.Create;
  FAdminVerlofLijst := TActivityList.Create;
  FAllVerlofLijst := TActivityList.Create;
  DecodeDate(Now, FYear, FMonth, day);
  checkInitAdminUi;

  DoDateChanged;
end;

procedure TFormVerlofUser.WebFormDestroy(Sender: TObject);
begin
  FVerlofLijst.Free;
  FAdminVerlofLijst.Free;
  FAllVerlofLijst.Free;
end;

end.
