unit Forms.verlof.user;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.StdCtrls, WEBLib.StdCtrls,
  Vcl.Controls, WEBLib.WebCtrls, WEBLib.Lists, WEBLib.WebTools,
  System.Generics.Collections, orm.Person, orm.Activity, model.AppManager;

type
  TFormVerlofUser = class(TWebForm)
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
    WebButton1: TWebButton;
    procedure WebFormCreate(Sender: TObject);
    procedure calendarnextClick(Sender: TObject);
    procedure calendarprevClick(Sender: TObject);
    procedure WebButton1Click(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
  private
    FCurrentDate: TDateTime;
    FActivityList: TList<TActivity>;
    FAppManager: TAppManager;
    procedure renderCalendar;
    procedure SetActivityList;
  public
    function GenerateCalendarHTML(AYear, AMonth: Word;
      AStartDow: Integer): string;
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
  SetActivityList;
  renderCalendar();
end;

procedure TFormVerlofUser.calendarprevClick(Sender: TObject);
begin
  inherited;
  FCurrentDate := IncMonth(FCurrentDate, -1);
  SetActivityList;
  renderCalendar;
end;

function TFormVerlofUser.GenerateCalendarHTML(AYear, AMonth: Word;
  AStartDow: Integer // 1 = zondag … 7 = zaterdag
  ): string;
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

          for I := FActivityList.Count - 1 downto 0 do
          begin
            if SameDate(DateOf(FActivityList[I].Start), CellDate) then
            begin
              sb.AppendFormat('        <div class="event">%s</div>',
                [FActivityList[I].Person.LastName]).AppendLine;
               FActivityList.Delete(I);
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

procedure TFormVerlofUser.renderCalendar;
var
  Date: string;
begin
  document.getElementById('calendar-table').innerHTML :=
    GenerateCalendarHTML(YearOf(FCurrentDate), MonthOf(FCurrentDate), 2);
  Date := FormatDateTime('mmmm yyyy', FCurrentDate);
  Date[1] := UpCase(Date[1]);
  calendarmonth.HTML.Text := Date;
end;

procedure TFormVerlofUser.SetActivityList;
var
  y,m,d: word;
begin
//exit;
DecodeDate(FCurrentDate,y,m,d);
FAppManager.db.getActivities('shift',y,m, FActivityList);
end;

procedure TFormVerlofUser.WebButton1Click(Sender: TObject);
begin
  inherited;
  FAppManager.db.getActivities('shift',2025,4,FActivityList);
  renderCalendar;
end;

procedure TFormVerlofUser.WebFormCreate(Sender: TObject);
begin
  inherited;
  FCurrentDate := now;
  FActivityList:=TList<TActivity>.Create;
  FAppManager := TAppManager.GetInstance;
  SetActivityList;
  renderCalendar;
end;

procedure TFormVerlofUser.WebFormDestroy(Sender: TObject);
begin
  inherited;
  if Assigned(FActivityList) then
    FActivityList.Free;
end;

end.
