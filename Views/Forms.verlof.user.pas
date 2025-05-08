unit Forms.verlof.user;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.StdCtrls, WEBLib.StdCtrls,
  Vcl.Controls, WEBLib.WebCtrls, WEBLib.Lists;

type
  TFormVerlofUser = class(TViewBase)
    reject1: TWebButton;
    tabrequestbtn: TWebButton;
    filtertype: TWebComboBox;
    reasontext: TWebMemo;
    list: TWebHTMLDiv;
    calendarnext: TWebButton;
    tabcalendarbtn: TWebButton;
    enddate: TWebDateTimePicker;
    request: TWebHTMLDiv;
    employeeinput: TWebEdit;
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
    procedure WebFormCreate(Sender: TObject);
    procedure calendarnextClick(Sender: TObject);
    procedure calendarprevClick(Sender: TObject);
  private
    FCurrentDate: TDateTime;
    procedure renderCalendar;
    procedure initControls;
  public
    function GenerateCalendarHTML(AYear, AMonth: Word): string;
  end;

var
  FormVerlofUser: TFormVerlofUser;

implementation

uses
  System.DateUtils;

{$R *.dfm}
{ TFormVerlofUser }

procedure TFormVerlofUser.calendarnextClick(Sender: TObject);
begin
  inherited;
  FCurrentDate := IncMonth(FCurrentDate);
  renderCalendar;
end;

procedure TFormVerlofUser.calendarprevClick(Sender: TObject);
begin
  inherited;
  FCurrentDate := IncMonth(FCurrentDate, -1);
  renderCalendar;
end;

function TFormVerlofUser.GenerateCalendarHTML(AYear, AMonth: Word): string;
var
  sb: TStringBuilder;
  FirstOfMonth: TDateTime;
  StartDow, DaysInMonth, DayCounter, WeekDayIdx, WeekIdx: Integer;
begin
  sb := TStringBuilder.Create;
  try
    // Bereken eerste dag en aantal dagen
    FirstOfMonth := EncodeDate(AYear, AMonth, 1);
    StartDow := DayOfWeek(FirstOfMonth); // 1=Sunday � 7=Saturday
    DaysInMonth := DaysInAMonth(AYear, AMonth);

    // Table header
    sb.AppendLine
      ('<table id="calendar-table" class="table calendar-table mb-0">');
    sb.AppendLine('  <thead>');
    sb.AppendLine('    <tr>');
    sb.AppendLine('      <th>Sun</th><th>Mon</th><th>Tue</th><th>Wed</th><th>Thu</th><th>Fri</th><th>Sat</th>');
    sb.AppendLine('    </tr>');
    sb.AppendLine('  </thead>');
    sb.AppendLine('  <tbody>');

    DayCounter := 1;
    // Maximaal 6 weken (42 cellen)
    for WeekIdx := 0 to 5 do
    begin
      sb.AppendLine('    <tr>');
      for WeekDayIdx := 1 to 7 do
      begin
        // Bepaal of we in de eerste week v��r de maand zijn, of na de maand
        if ((WeekIdx = 0) and (WeekDayIdx < StartDow)) or
          (DayCounter > DaysInMonth) then
        begin
          // lege cel
          sb.AppendLine('      <td>&nbsp;</td>');
        end
        else
        begin
          // echte dag
          sb.AppendFormat('      <td id="day-%d-%.2d-%.2d">',
            [AYear, AMonth, DayCounter]).AppendLine;
          sb.AppendFormat('        %d', [DayCounter]).AppendLine;
          sb.AppendLine('      </td>');
          Inc(DayCounter);
        end;
      end;
      sb.AppendLine('    </tr>');

      // Stoppen als we alle dagen hebben geplaatst
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

procedure TFormVerlofUser.initControls;
var
  i: Integer;
  Component: TComponent;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    Component := Components[i];
    if Component is TWebButton then
      TWebButton(Component).Caption := '';
  end;
end;

procedure TFormVerlofUser.renderCalendar;
var
  date: string;
begin
  document.getElementById('calendar-table').innerHTML :=
    GenerateCalendarHTML(YearOf(FCurrentDate), MonthOf(FCurrentDate));
  date := FormatDateTime('mmmm yyyy', FCurrentDate);
  date[1] := UpCase(date[1]);
  calendarmonth.HTML.Text := date;
end;

procedure TFormVerlofUser.WebFormCreate(Sender: TObject);
begin
  inherited;
  initControls;
  FCurrentDate := now;
  renderCalendar;
end;

end.
