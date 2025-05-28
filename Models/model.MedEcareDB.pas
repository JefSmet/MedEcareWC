unit model.MedEcareDB;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.REST,
  orm.Activity, System.Generics.Collections, orm.Person, orm.Doctor;

type
  TMedEcareDB = class(TWebDataModule)
    reqGetActivities: TWebHttpRequest;
    reqPostVerlof: TWebHttpRequest;
    reqPutVerlof: TWebHttpRequest;
    reqDeleteVerlof: TWebHttpRequest;
    reqGetStaff: TWebHttpRequest;
    reqGetShiftsByPeriod: TWebHttpRequest;
    reqGetVerlofByPeriod: TWebHttpRequest;
  private
    { Private declarations }
  public
    { Public declarations }
    [async]
    function GetActivities(const AType: string; const AYear, AMonth: word;
      AList: TActivityList): Boolean;
    function Test: string;
    [async]
    procedure PostActivity(AActivityStatus, AActivityType: string;
      AStartDate, AEndDate: TDateTime; APersonId, AShifttypeId: string);
    [async]
    function PutActivity(AActivityID, AActivityStatus, AActivityType: string;
      AStartDate, AEndDate: TDateTime; APersonId, AShifttypeId: string)
      : Boolean;
    [async]
    function DeleteActivity(AActivityID: string): Boolean;
    [async]
    function getDoctors(AList: TList<TDoctor>): Boolean;
    [async]
    function getShifts(startDate, endDate: TDateTime;
      AList: TActivityList): Boolean;
    [async]
    function getVerlof(startDate, endDate: TDateTime;
      AList: TActivityList): Boolean;
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

function TMedEcareDB.DeleteActivity(AActivityID: string): Boolean;
var
  xhr: TJSXMLHttpRequest;
begin
  reqDeleteVerlof.URL := baseUrl + 'admin/activities/' + AActivityID;

  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqDeleteVerlof));
    if (xhr.Status = 200) then
    begin
      Exit(true);
    end;
  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
  end;
end;

function TMedEcareDB.GetActivities(const AType: string;
  const AYear, AMonth: word; AList: TActivityList): Boolean;
var
  formattedVerlofEndpoint: string;
  ActivityType: string;
  xhr: TJSXMLHttpRequest;
  response: string;
  activityList: TList<TActivity>;
begin
  ActivityType := AType;
  formattedVerlofEndpoint :=
    Format('admin/activities/filter?year=%d&month=%d&activityType=%s',
    [AYear, AMonth, ActivityType]);
  reqGetActivities.URL := baseUrl + formattedVerlofEndpoint;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetActivities));
    if (xhr.Status = 200) or (xhr.Status = 304) then
    begin
      response := xhr.responseText;
      activityList := TActivity.ToList(response, true);
      try
        if Assigned(AList) then
        begin
          AList.Clear;
          AList.AddRange(activityList);
        end;
      finally
        activityList.Free;
      end;

      Exit(true);
    end;
  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
  end;
end;

function TMedEcareDB.getDoctors(AList: TList<TDoctor>): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
  doctorList: TList<TDoctor>;
begin

  reqGetStaff.URL := baseUrl + 'admin/persons/staff';
  try

    xhr := await(TJSXMLHttpRequest, PerformRequestWithCredentials(reqGetStaff));

    doctorList := TDoctor.ToList(xhr.responseText, true);

    try
      if Assigned(AList) then
      begin
        AList.Clear;
        AList.AddRange(doctorList);
      end;
    finally
      doctorList.Free;
    end;
    Exit(true);

  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
  end;
end;

function TMedEcareDB.getShifts(startDate, endDate: TDateTime;
  AList: TActivityList): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response,startString,endString: string;
  shiftList: TActivityList;
begin
  startString := FormatDateTime('yyyy-mm-dd',startDate);
  endString := FormatDateTime('yyyy-mm-dd',endDate);
  reqGetShiftsByPeriod.URL := baseUrl +
    Format('admin/activities/period/shifts?startDate=%s&endDate=%s',
    [startString, endString]);
  try

    xhr := await(TJSXMLHttpRequest, PerformRequestWithCredentials(reqGetShiftsByPeriod));

    shiftList := TActivity.ToList(xhr.responseText, true);

    try
      if Assigned(AList) then
      begin
        AList.Clear;
        AList.AddRange(shiftList);
      end;
    finally
      shiftList.Free;
    end;
    Exit(true);

  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
  end;
end;

function TMedEcareDB.getVerlof(startDate, endDate: TDateTime;
  AList: TActivityList): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response,startString,endString: string;
  verlofList: TActivityList;
begin
  startString := FormatDateTime('yyyy-mm-dd',startDate);
  endString := FormatDateTime('yyyy-mm-dd',endDate);
  reqGetVerlofByPeriod.URL := baseUrl +
    Format('admin/activities/period/verlof?startDate=%s&endDate=%s',
    [startString, endString]);
  try

    xhr := await(TJSXMLHttpRequest, PerformRequestWithCredentials(reqGetVerlofByPeriod));

    verlofList := TActivity.ToList(xhr.responseText, true);

    try
      if Assigned(AList) then
      begin
        AList.Clear;
        AList.AddRange(verlofList);
      end;
    finally
      verlofList.Free;
    end;
    Exit(true);

  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
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
  APersonId, AShifttypeId: string): Boolean;
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
      JSON.AddPair('status', AActivityStatus);
    end;

    postData := JSON.ToString;
    reqPutVerlof.postData := postData;
  finally
    JSON.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPutVerlof));
  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast(e.Message);
      Result := False;
    end;
  end;
  Result := true;
end;

function TMedEcareDB.Test: string;
begin
  Result := 'medecaredb';
end;

end.
