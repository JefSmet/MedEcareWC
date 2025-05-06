unit Forms.verlof.user;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, VCL.TMSFNCTypes, VCL.TMSFNCUtils,
  VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes,
  VCL.Controls, VCL.TMSFNCCustomControl, VCL.TMSFNCCalendar, DateUtils, Vcl.StdCtrls, WEBLib.StdCtrls;

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
    { Private declarations }
  public
    { Public declarations }
    procedure SetDate(AMonth, AYear: word);
  end;

var
  FormVerlofUser: TFormVerlofUser;

implementation

{$R *.dfm}
{ TFormVerlofUser }

procedure TFormVerlofUser.CalEndCustomNavigation(Sender: TObject; ADate, AFocusedDate: TDate; ADirection: Boolean);
var date: TDateTime;
begin
date := ADate;
if ADirection then
begin
  date := IncMonth(date,-1);
end;
setDate(MonthOf(date),YearOf(date));
end;

procedure TFormVerlofUser.SetDate(AMonth, AYear: word);
var
  date: TDateTime;
begin
  CalStart.BeginUpdate;
  CalEnd.BeginUpdate;
  date := encodeDate(AYear, AMonth, 1);
  CalStart.date := date;
  date := IncMonth(date, 1);
  CalEnd.date := date;
  CalStart.EndUpdate;
  CalEnd.EndUpdate;
end;

procedure TFormVerlofUser.WebButton1Click(Sender: TObject);
begin
  inherited;
setDate(12,2024);
end;

procedure TFormVerlofUser.WebFormCreate(Sender: TObject);
begin
  inherited;
setDate(MonthOf(Now),YearOf(Now));
end;

end.
