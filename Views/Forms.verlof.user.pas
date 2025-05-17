unit Forms.verlof.user;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.StdCtrls, WEBLib.StdCtrls,
  Vcl.Controls, WEBLib.WebCtrls, WEBLib.Lists, WEBLib.WebTools,
  System.Generics.Collections, orm.Person, orm.Activity, model.AppManager,
  WEBLib.REST, WEBLib.Actions;

type
  TFormVerlofUser = class(TViewBase)
    reject1: TWebButton;
    tabrequestbtn: TWebButton;
    filtertype: TWebComboBox;
    list: TWebHTMLDiv;
    calendarnext: TWebButton;
    tabcalendarbtn: TWebButton;
    enddate: TWebDateTimePicker;
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
    WebHttpRequest1: TWebHttpRequest;
    request: TWebHTMLDiv;
    WebElementActionList1: TWebElementActionList;
    procedure WebFormCreate(Sender: TObject);
    procedure calendarnextClick(Sender: TObject);
    procedure calendarprevClick(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    procedure submitrequestClick(Sender: TObject);
    procedure startdateChange(Sender: TObject);
    procedure enddateChange(Sender: TObject);
    [async]
    procedure WebElementActionList1Action0Execute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]
    procedure WebElementActionList1Action1Execute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
  private
    FCurrentDate: TDateTime;
    FActivityList: TList<TActivity>;
    procedure renderList;
    procedure renderCalendar;
    [async]
    procedure SetActivityList;
  public
    function GenerateCalendarHTML(AYear, AMonth: Word;
      AActivityList: TList<TActivity>; AStartDow: Integer = 2): string;
      function GenerateListHTML(AActivityList: TList<TActivity>): string;
  end;

var
  FormVerlofUser: TFormVerlofUser;

implementation

uses
  System.DateUtils, middleware.httponly;

const
  baseUrl: string = 'http://localhost:3000/admin/activities/';
  verlofEndpoint: string = 'filter?year=%d&month=%d&activityType=%s';

{$R *.dfm}
  { TFormVerlofUser }

procedure TFormVerlofUser.calendarnextClick(Sender: TObject);
begin
  inherited;
  FCurrentDate := IncMonth(FCurrentDate);
  SetActivityList;
end;

procedure TFormVerlofUser.calendarprevClick(Sender: TObject);
begin
  inherited;
  FCurrentDate := IncMonth(FCurrentDate, -1);
  SetActivityList;
end;

procedure TFormVerlofUser.enddateChange(Sender: TObject);
begin
  inherited;
  if startdate.Date > enddate.Date then
  begin
    startdate.Date := enddate.Date;
  end;
end;

function TFormVerlofUser.GenerateCalendarHTML(AYear, AMonth: Word;
  AActivityList: TList<TActivity>; AStartDow: Integer): string;
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
  // 1) Bereken eerste dag en aantal dagen
  FirstOfMonth := EncodeDate(AYear, AMonth, 1);
  StartDow := DayOfWeek(FirstOfMonth); // 1=zo … 7=za
  DaysInMonth := DaysInAMonth(AYear, AMonth);
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
      PascalDow := ((AStartDow - 1 + WeekDayIdx - 1) mod DaysPerWeek) + 1;
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
          (WeekDayIdx < ((StartDow - AStartDow + DaysPerWeek) mod DaysPerWeek) +
          1)) or (DayCounter > DaysInMonth) then
        begin
          sb.AppendLine('      <td>&nbsp;</td>')
        end
        else
        begin
          CellDate := EncodeDate(AYear, AMonth, DayCounter);
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
              [AYear, AMonth, DayCounter, ClassAttr]).AppendLine
          else
            sb.AppendFormat('      <td id="day-%d-%.2d-%.2d">',
              [AYear, AMonth, DayCounter]).AppendLine;

          sb.AppendFormat('        %d', [DayCounter]).AppendLine;

          for I := AActivityList.Count - 1 downto 0 do
          begin
            if SameDate(DateOf(AActivityList[I].Start), CellDate) then
            begin
              sb.AppendFormat('        <div class="event">%s</div>',
                [AActivityList[I].Person.LastName]).AppendLine;
//              AActivityList.Delete(I);
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

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TFormVerlofUser.GenerateListHTML(AActivityList: TList<TActivity>): string;
var
  sb: TStringBuilder;
  act: TActivity;
  dateRange: string;
  days: Integer;
begin
  sb := TStringBuilder.Create;
  try
    for act in AActivityList do
    begin
      dateRange := FormatDateTime('mmm dd, yyyy', act.Start) + ' - ' +
                   FormatDateTime('mmm dd, yyyy', act.EndTime);
      days := DaysBetween(DateOf(act.EndTime), DateOf(act.Start)) + 1;
      sb.AppendFormat('<tr data-request-id="%s">', [act.Id]).AppendLine;
      sb.AppendFormat('  <td>%s %s</td>',
        [act.Person.FirstName, act.Person.LastName]).AppendLine;
      sb.AppendFormat('  <td>%s</td>', [dateRange]).AppendLine;
      sb.AppendFormat('  <td><span class="badge bg-secondary">%s</span> %d days</td>',
        [act.ActivityType, days]).AppendLine;
      sb.AppendLine('  <td></td>');
      sb.AppendFormat('  <td><span class="badge bg-secondary">%s</span></td>',[act.status]).AppendLine;
      sb.AppendFormat('  <td><button type="button" class="btn btn-sm btn-success" data-action="approve" data-id="%s"><i class="bi bi-check-lg"></i></button>', [act.Id]).AppendLine;
      sb.AppendFormat('      <button type="button" class="btn btn-sm btn-danger" data-action="reject" data-id="%s"><i class="bi bi-x-lg"></i></button></td>', [act.Id]).AppendLine;
      sb.AppendLine('</tr>');
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;


procedure TFormVerlofUser.renderCalendar;
var
  Date: string;
  tblString: string;
  y, m, d: Word;
begin
  DecodeDate(FCurrentDate, y, m, d);
  tblString := GenerateCalendarHTML(y, m, FActivityList);
  document.getElementById('calendar-table').innerHTML := tblString;
  Date := FormatDateTime('mmmm yyyy', FCurrentDate);
  Date[1] := UpCase(Date[1]);
  calendarmonth.HTML.Text := Date;
end;

procedure TFormVerlofUser.renderList;
var
  tblString: string;
begin
  tblString := GenerateListHTML(FActivityList);
  document.querySelector('#requests-table tbody').innerHTML := tblString;
  WebElementActionList1.BindActions;
end;

procedure TFormVerlofUser.SetActivityList;
var
  y, m, d: Word;
  formattedVerlofEndpoint: string;
  activityType: string;
  xhr: TJSXMLHttpRequest;
  response: string;
begin
  DecodeDate(FCurrentDate, y, m, d);
  activityType := 'shift';
  formattedVerlofEndpoint := Format(verlofEndpoint, [y, m, activityType]);
  WebHttpRequest1.URL := baseUrl + formattedVerlofEndpoint;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(WebHttpRequest1));
    if (xhr.Status = 200) or (xhr.Status = 304) then
    begin
      response := xhr.responseText;
      if Assigned(FActivityList) then
      begin
        FActivityList.Free;
      end;
      FActivityList := TActivity.ToList(response, true);
      renderCalendar;
      renderList;
    end;
  except
    on e: exception do
      AppManager.ShowToast('Er ging iets mis: ' + e.Message);
  end;
end;

procedure TFormVerlofUser.startdateChange(Sender: TObject);
begin
  inherited;
  if enddate.Date < startdate.Date then
    enddate.Date := startdate.Date;
end;

procedure TFormVerlofUser.submitrequestClick(Sender: TObject);
begin
  inherited;
  AppManager.DB.PostActivity('pending',leavetype.Items[leavetype.ItemIndex],
    startdate.Date, enddate.Date, AppManager.Auth.currentPerson.personId, '');
end;

procedure TFormVerlofUser.WebElementActionList1Action0Execute(Sender: TObject; Element: TJSHTMLElementRecord;
  Event: TJSEventParameter);
begin
  inherited;
  try
  await(Boolean,AppManager.DB.PutActivity(Element.element.Attrs['data-id'],'approved','',0,0,'',''));
  except

  end;
  SetActivityList;
end;

procedure TFormVerlofUser.WebElementActionList1Action1Execute(Sender: TObject; Element: TJSHTMLElementRecord;
  Event: TJSEventParameter);
begin
  inherited;
   try
  await(Boolean,AppManager.DB.PutActivity(Element.element.Attrs['data-id'],'rejected','',0,0,'',''));
  except

  end;
  SetActivityList;
end;

procedure TFormVerlofUser.WebFormCreate(Sender: TObject);
begin
  inherited;
  FCurrentDate := now;
  FActivityList := TList<TActivity>.Create;
  SetActivityList;
  startdate.Min := now;
  enddate.Min := now;
end;

procedure TFormVerlofUser.WebFormDestroy(Sender: TObject);
begin
  inherited;
  if Assigned(FActivityList) then
    FActivityList.Free;
end;

end.
