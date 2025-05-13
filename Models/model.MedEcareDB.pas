unit model.MedEcareDB;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.REST,
  orm.Activity, System.Generics.Collections;

type
  TMedEcareDB = class(TWebDataModule)
    reqGetActivities: TWebHttpRequest;
    reqPostVerlof: TWebHttpRequest;
  private
    { Private declarations }
  public
    { Public declarations }
    [async]
    function GetActivities(const AType: string; const AYear, AMonth: word;
      AList: TList<TActivity>): Boolean;
    function Test: string;
    [async]
    procedure PostActivity(AActivityType: string;
      AStartDate, AEndDate: TDateTime; APersonId, AShifttypeId: string);
  end;

var
  MedEcareDB: TMedEcareDB;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses middleware.httponly, DateUtils, model.AppManager;
{$R *.dfm}

const
  baseUrl = 'http://localhost:3000/';

  { TMedEcareDB }

function TMedEcareDB.GetActivities(const AType: string;
  const AYear, AMonth: word; AList: TList<TActivity>): Boolean;
var
  endpoint: string;
  xhr: TJSXMLHttpRequest;
  list: TList<TActivity>;
begin
  endpoint := Format('admin/activities/filter?year=%d&month=%d&activityType=%s',
    [AYear, AMonth, AType]);
  reqGetActivities.URL := baseUrl + endpoint;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetActivities));
    if xhr.Status = 200 then
    begin
      list := TActivity.ToList(TJSJSON.stringify(xhr.response), true);
      try
        AList.Clear;
        AList.AddRange(list);
        Result := true;
      finally
        list.Free;
      end;
    end;
  except
    exit(false);
  end;

end;

procedure TMedEcareDB.PostActivity(AActivityType: string;
  AStartDate, AEndDate: TDateTime; APersonId, AShifttypeId: string);
var
  postData: string;
  startDate, endDate: string;
  xhr: TJSXMLHttpRequest;
begin
  reqPostVerlof.URL := baseUrl + 'admin/activities';
  startDate := FormatDateime('yyyy-mm-dd"T"hh:mm:ss"Z"',AStartDate);
  endDate := TWebRESTClient.DateTimeToWL(AEndDate);
  postData :=
    Format('{"activityType": "%s","start": "%s","end": "%s","personId": "%s","shiftTypeId": "%s"}',
    [AActivityType, startDate, endDate, APersonId, AShifttypeId]);
  reqPostVerlof.postData := postData;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPostVerlof));
  except on e : exception do
  TAppManager.GetInstance.ShowToast(e.Message);
  end;
end;

function TMedEcareDB.Test: string;
begin
  Result := 'medecaredb';
end;

end.
