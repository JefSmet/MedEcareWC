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
    filtertype: TWebComboBox;
    calendarnext: TWebButton;
    enddate: TWebDateTimePicker;
    submitrequest: TWebButton;
    startdate: TWebDateTimePicker;
    calendarprev: TWebButton;
    leavetype: TWebComboBox;
    searchinput: TWebEdit;
    calendarmonth: TWebHTMLSpan;
    filterstatus: TWebComboBox;
    webElementAL: TWebElementActionList;
    [async]
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    procedure calendarButtonClick(Sender: TObject);
    procedure onSearchInputChange(Sender: TObject);
    [async] procedure submitrequestClick(Sender: TObject);
    [async]
    procedure webElementALacApprovedExecute(Sender: TObject;
      Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]
    procedure startdateChange(Sender: TObject);
    procedure enddateChange(Sender: TObject);
    [async]
    procedure webElementALacDeleteExecute(Sender: TObject;
      Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]procedure webElementALacRejectedExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
  private
    FAppmanager: TAppManager;
    FYear: Word;
    FMonth: Word;
    FVerlofLijst: TActivityList;
    FAdminVerlofLijst: TActivityList;
    FAllVerlofLijst: TActivityList;
    [async]
    function GetVerlofList: Boolean;
    [async]
    procedure DoUpdateLists;
    procedure renderCalendar;
    procedure renderList;
    procedure filterVerlofList(AFilteredList: TActivityList; AName: string = '';
      AStatus: string = ''; AActivityType: string = ''; APersonId: string = '');
  public
    { Public declarations }
  end;

var
  FormVerlofUser: TFormVerlofUser;

implementation

{$R *.dfm}

procedure TFormVerlofUser.DoUpdateLists;
begin

  await(Boolean, GetVerlofList);
  renderCalendar;
  renderList;
end;

procedure TFormVerlofUser.enddateChange(Sender: TObject);
begin
  if startdate.Date > enddate.Date then
  begin
    startdate.Date := enddate.Date;
  end;
end;

procedure TFormVerlofUser.filterVerlofList(AFilteredList: TActivityList;
  AName, AStatus, AActivityType, APersonId: string);
var
  Activity: TActivity;
  matchesFilter: Boolean;
begin
  AFilteredList.Clear;
  // Loop door alle activiteiten en pas de filter toe
  for Activity in FAllVerlofLijst do
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

function TFormVerlofUser.GetVerlofList: Boolean;
var
  I: integer;
begin
  try
    await(Boolean, FAppmanager.DB.GetActivities('', FYear, FMonth,
      FAllVerlofLijst));
    for I := FAllVerlofLijst.Count - 1 downto 0 do
    begin
      if LowerCase(FAllVerlofLijst[I].ActivityType) = 'shift' then
      begin
        FAllVerlofLijst.Delete(I);
      end;
    end;
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
  StartDow, DaysInMonth, DayCounter: integer;
  WeekIdx, WeekDayIdx, PascalDow: integer;
  DayName, ClassAttr: string;
  I: integer;
begin
  filterVerlofList(FVerlofLijst, '', 'Approved');
  // 1) Bereken eerste dag en aantal dagen
  FirstOfMonth := EncodeDate(FYear, FMonth, 1);
  StartDow := DayOfWeek(FirstOfMonth); // 1=zo � 7=za
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
  searchQuery, Status, ActivityType: string;
  sb: TStringBuilder;
  act: TActivity;
  dateRange: string;
  days: integer;
begin

  searchQuery := searchinput.Text;
  if filterstatus.ItemIndex < 0 then
    Status := ''
  else
    Status := filterstatus.Items[filterstatus.ItemIndex];
  ActivityType := filtertype.Text;

  if not FAppmanager.Auth.currentPerson.Roles.Contains('admin') then
  begin
    filterVerlofList(FAdminVerlofLijst, searchQuery, Status, ActivityType,
      FAppmanager.Auth.currentPerson.PersonId);
  end
  else
  begin
    filterVerlofList(FAdminVerlofLijst, searchQuery, Status, ActivityType);
  end;

  sb := TStringBuilder.Create;
  try
    for act in FAdminVerlofLijst do
    begin
      dateRange := FormatDateTime('mmm dd, yyyy', act.Start) + ' - ' +
        FormatDateTime('mmm dd, yyyy', act.EndTime);
      days := DaysBetween(DateOf(act.EndTime), DateOf(act.Start)) + 1;
      sb.AppendFormat('<tr data-request-id="%s">', [act.Id]).AppendLine;
      sb.AppendFormat('  <td style="vertical-align: middle;">%s %s</td>',
        [act.Person.FirstName, act.Person.LastName]).AppendLine;
      sb.AppendFormat('  <td style="vertical-align: middle;">%s</td>',
        [dateRange]).AppendLine;
      sb.AppendFormat
        ('  <td style="vertical-align: middle;"><span class="badge bg-secondary">%s</span> %d days</td>',
        [act.ActivityType, days]).AppendLine;
      sb.AppendFormat
        ('  <td style="vertical-align: middle;"><span class="badge bg-secondary">%s</span></td>',
        [act.Status]).AppendLine;
      if FAppmanager.Auth.currentPerson.Roles.Contains('admin') then
      begin
        sb.AppendFormat
          ('  <td style="vertical-align: middle;"><button type="button" class="btn btn-sm btn-success" data-action="approve" data-id="%s"><i class="bi bi-check-lg"></i></button>',
          [act.Id]).AppendLine;
        sb.AppendFormat
          ('      <button type="button" class="btn btn-sm btn-danger" data-action="reject" data-id="%s"><i class="bi bi-x-lg"></i></button>',
          [act.Id]).AppendLine;
          if (act.PersonId = FAppmanager.Auth.currentPerson.personId ) then
          begin
            sb.AppendFormat
          ('      <button type="button" class="btn btn-sm btn-danger" data-action="delete" data-id="%s"><i class="bi bi-trash-fill"></i></button></td>',
          [act.Id]).AppendLine;
          end
          else
          begin
            sb.AppendLine('</td>');
          end;
      end
      else
      begin
        sb.AppendFormat
          ('      <td><button type="button" class="btn btn-sm btn-danger" data-action="delete" data-id="%s"><i class="bi bi-trash-fill"></i></button></td>',
          [act.Id]).AppendLine;
      end;
      sb.AppendLine('</tr>');

    end;
    Document.querySelector('#requests-table tbody').innerHTML := sb.ToString;
    webElementAL.BindActions;
  finally
    sb.Free;
  end;

end;

procedure TFormVerlofUser.startdateChange(Sender: TObject);
begin
  if enddate.Date < startdate.Date then
    enddate.Date := startdate.Date;
end;

procedure TFormVerlofUser.submitrequestClick(Sender: TObject);
begin
  await(FAppmanager.DB.PostActivity('pending', leavetype.Items[leavetype.ItemIndex],
    startdate.Date, enddate.Date, FAppmanager.Auth.currentPerson.PersonId, ''));
    await(DoUpdateLists);
    startdate.Date := Now;
    enddate.date := Now;
    FAppmanager.ShowToast('Uw aanvraag is succesvol verzonden!');

end;

procedure TFormVerlofUser.webElementALacApprovedExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  await(Boolean, FAppmanager.DB.PutActivity(Element.Element.Attrs['data-id'],
    'approved', '', 0, 0, '', ''));
  await(DoUpdateLists);
end;

procedure TFormVerlofUser.webElementALacDeleteExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  await(Boolean, FAppmanager.DB.DeleteActivity(Element.Element.Attrs
    ['data-id']));
  await(DoUpdateLists);
end;

procedure TFormVerlofUser.webElementALacRejectedExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
await(Boolean, FAppmanager.DB.PutActivity(Element.Element.Attrs['data-id'],
    'rejected', '', 0, 0, '', ''));
  await(DoUpdateLists);
end;

procedure TFormVerlofUser.onSearchInputChange(Sender: TObject);
begin
  renderList;
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
  encodatedDate := IncMonth(encodatedDate, toInc);
  DecodeDate(encodatedDate, FYear, FMonth, day);
  calendarmonth.HTML.Text := FormatDateTime('mmmm yyyy', encodatedDate);
  DoUpdateLists;

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
  calendarmonth.HTML.Text := FormatDateTime('mmmm yyyy', Now);
  startdate.Date := Now;
  endDate.Date := Now;
  startdate.Min := Now;
  enddate.Min := Now;
  DoUpdateLists;
end;

procedure TFormVerlofUser.WebFormDestroy(Sender: TObject);
begin
  FVerlofLijst.Free;
  FAdminVerlofLijst.Free;
  FAllVerlofLijst.Free;
end;

end.
