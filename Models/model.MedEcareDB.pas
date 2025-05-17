unit model.MedEcareDB;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.REST,
  orm.Activity, System.Generics.Collections;

type
  TMedEcareDB = class(TWebDataModule)
    reqGetActivities: TWebHttpRequest;
    reqPostVerlof: TWebHttpRequest;
    reqPutVerlof: TWebHttpRequest;
  private
    { Private declarations }
  public
    { Public declarations }
    [async]
    function GetActivities(const AType: string; const AYear, AMonth: word;
      AList: TList<TActivity>): Boolean;
    function Test: string;
    [async]
    procedure PostActivity(AActivityStatus, AActivityType: string;
      AStartDate, AEndDate: TDateTime; APersonId, AShifttypeId: string);
    [async]
    function PutActivity(AActivityID, AActivityStatus, AActivityType: string;
      AStartDate, AEndDate: TDateTime; APersonId, AShifttypeId: string) : Boolean;
  end;

var
  MedEcareDB: TMedEcareDB;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses middleware.httponly, DateUtils, model.AppManager, WEBLib.JSON;
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

procedure TMedEcareDB.PostActivity(AActivityStatus, AActivityType: string;
  AStartDate, AEndDate: TDateTime; APersonId, AShifttypeId: string);
var
  postData: string;
  startDate, endDate, shifttypeid: string;
  xhr: TJSXMLHttpRequest;
begin
  reqPostVerlof.URL := baseUrl + 'admin/activities';
  startDate := FormatDateTime('yyyy-mm-dd"T"hh:mm:ss".000Z"', AStartDate);
  endDate := FormatDateTime('yyyy-mm-dd"T"hh:mm:ss".000Z"', AEndDate);
  if AShifttypeId = '' then
    shifttypeid := 'null'
  else
    shifttypeid := '"' + AShifttypeId + '"';
  postData :=
    Format('{"activityType": "%s","start": "%s","end": "%s","personId": "%s","shiftTypeId": %s}',
    [AActivityType, startDate, endDate, APersonId, shifttypeid]);
  reqPostVerlof.postData := postData;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPostVerlof));
  except
    on e: exception do
      TAppManager.GetInstance.ShowToast(e.Message);
  end;
end;

function TMedEcareDB.PutActivity(AActivityID, AActivityStatus,
  AActivityType: string; AStartDate, AEndDate: TDateTime;
  APersonId, AShifttypeId: string) : Boolean;
var
  postData: string;
  startDate, endDate: string;
  xhr: TJSXMLHttpRequest;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
    try
    reqPutVerlof.URL := baseUrl + 'admin/activities/' + AActivityID;
      if (AStartDate > 0) then
      begin
        startDate := FormatDateTime('yyyy-mm-dd"T"hh:mm:ss".000Z"', AStartDate);
        JSON.AddPair('start', startDate);
      end;
      if (AEndDate > 0) then
      begin
        endDate := FormatDateTime('yyyy-mm-dd"T"hh:mm:ss".000Z"', AEndDate);
        JSON.AddPair('end', endDate);
      end;

      if AShifttypeId <> '' then
      JSON.AddPair('shiftTypeId', AShifttypeId);
      if APersonId <> '' then
      begin
        JSON.AddPair('personId', APersonId);
      end;

      if AActivityStatus <> '' then
      begin
        JSON.AddPair('status',AActivityStatus);
      end;

      postData := JSON.ToString;
      reqPutVerlof.postData := postData;
  finally
  json.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPutVerlof));
  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast(e.Message);
      Result := false;
    end;
  end;
  result := true;
end;

function TMedEcareDB.Test: string;
begin
  Result := 'medecaredb';
end;

end.
