unit Forms.verlof.user;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.StdCtrls, WEBLib.StdCtrls,
  Vcl.Controls, WEBLib.WebCtrls, WEBLib.Lists, WEBLib.WebTools,
  System.Generics.Collections, orm.Person, orm.Activity, model.AppManager,
  WEBLib.REST, WEBLib.Actions;

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
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
  private
    FAppmanager: TAppManager;
    FYear : Word;
    FMonth : Word;
    FVerlofLijst : TList<TActivity>;
    [async] procedure DoTest;
    [async] function GetVerlofList : Boolean;
    [async] procedure DoDateChanged;
  public
    { Public declarations }
  end;

var
  FormVerlofUser: TFormVerlofUser;

implementation

{$R *.dfm}


procedure TFormVerlofUser.DoDateChanged;
begin
await(Boolean,GetVerlofList);
end;

procedure TFormVerlofUser.DoTest;
var
  verlofList: TList<TActivity>;
begin
  verlofList := TList<TActivity>.Create;
  try
    await(Boolean,FAppmanager.DB.GetActivities('Verlof', 2025, 05, verlofList));
    ShowMessage(verlofList.First.Person.FirstName);
  finally
    verlofList.Free;
  end;

end;

function TFormVerlofUser.GetVerlofList : Boolean;
begin
    try
    await(Boolean,FAppmanager.DB.GetActivities('Verlof',FYear,FMonth, FVerlofLijst);  
  except on e :Exception do
  begin
    FAppmanager.ShowToast('Er ging iets mis: '+e.Message);
  end;
  end;
end;

procedure TFormVerlofUser.WebFormCreate(Sender: TObject);
var
  day : Word;
begin
  FAppmanager:=TAppManager.GetInstance;
  DecodeDate(Now,FYear,FMonth,day);
end;

procedure TFormVerlofUser.WebFormDestroy(Sender: TObject);
begin
if assigned(FVerlofLijst) then
  FVerlofLijst.Free;
end;

end.
