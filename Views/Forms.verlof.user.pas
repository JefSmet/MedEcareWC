unit Forms.verlof.user;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, VCL.TMSFNCTypes, VCL.TMSFNCUtils,
  VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes,
  VCL.Controls, VCL.TMSFNCCustomControl, VCL.TMSFNCCalendar, DateUtils, VCL.StdCtrls, WEBLib.StdCtrls;

type
  TFormVerlofUser = class(TViewBase)
    CalStart: TTMSFNCCalendar;
    CalEnd: TTMSFNCCalendar;
    WebButton1: TWebButton;
    WebLabel1: TWebLabel;
    WebLabel2: TWebLabel;
    procedure WebButton1Click(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
    procedure CalEndCustomNavigation(Sender: TObject; ADate, AFocusedDate: TDate; ADirection: Boolean);
  private
    procedure InitCalendar(ACal: TTMSFNCCalendar);
  public
    { Public declarations }
    procedure SetDate(AYear, AMonth: Word);
  end;

var
  FormVerlofUser: TFormVerlofUser;

implementation

{$R *.dfm}
{ TFormVerlofUser }

procedure TFormVerlofUser.CalEndCustomNavigation(Sender: TObject; ADate, AFocusedDate: TDate; ADirection: Boolean);
var
  date: TDateTime;
begin
  date := ADate;
  if ADirection then
  begin
    date := IncMonth(date, -1);
  end;
  SetDate(YearOf(date), MonthOf(date));
end;

procedure TFormVerlofUser.InitCalendar(ACal: TTMSFNCCalendar);
begin
  ACal.BeginUpdate;
  try
    ACal.FirstDay := 2;     // eerste kolom dag van de week: 1 = zondag... 7 = zaterdag  (zoals SysUtils.DayOfWeek)
    ACal.Footer.Caption := 'Vandaag:';
    ACal.Footer.Visible := False;
    ACal.Interaction.MultiSelect := True;
    ACal.SelectedDates.UnselectAll;
  finally
    ACal.EndUpdate;
  end;
end;

procedure TFormVerlofUser.SetDate(AYear, AMonth: Word);
begin
  CalStart.BeginUpdate;
  CalEnd.BeginUpdate;
  try
    CalStart.date := EncodeDate(AYear, AMonth, 1);
    CalEnd.date := IncMonth(CalStart.date, 1);;
  finally
    CalStart.EndUpdate;
    CalEnd.EndUpdate;
  end;
end;

procedure TFormVerlofUser.WebButton1Click(Sender: TObject);
begin
  inherited;
  SetDate(2024, 12);
end;

procedure TFormVerlofUser.WebFormCreate(Sender: TObject);
begin
  inherited;
  InitCalendar(CalStart);
  InitCalendar(CalEnd);
  SetDate(YearOf(Now), MonthOf(Now));
end;

end.
