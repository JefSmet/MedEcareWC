unit model.MedEcareDB;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.REST,
  orm.Activity, System.Generics.Collections, orm.Person, orm.Doctor,
  orm.ShiftType, orm.Role;

type
  TMedEcareDB = class(TWebDataModule)
    reqGetActivities: TWebHttpRequest;
    reqPostVerlof: TWebHttpRequest;
    reqPutVerlof: TWebHttpRequest;
    reqDeleteVerlof: TWebHttpRequest;
    reqGetStaff: TWebHttpRequest;
    reqGetShiftsByPeriod: TWebHttpRequest;
    reqGetVerlofByPeriod: TWebHttpRequest;
    reqGetShiftTypes: TWebHttpRequest;
    reqPostShiftTypes: TWebHttpRequest;
    reqPutShiftTypes: TWebHttpRequest;
    reqDeleteShiftTypes: TWebHttpRequest;
    reqGetRoles: TWebHttpRequest;
    reqPostRoles: TWebHttpRequest;
    reqPutRoles: TWebHttpRequest;
    reqDeleteRoles: TWebHttpRequest;
    reqPostActivites: TWebHttpRequest;
    reqPutActivities: TWebHttpRequest;
    reqDeleteActivities: TWebHttpRequest;
    reqDeletePersons: TWebHttpRequest;
    reqPutPersons: TWebHttpRequest;
    reqPostPersons: TWebHttpRequest;
    reqGetPersons: TWebHttpRequest;
    reqPutUser: TWebHttpRequest;
    reqPutChangePassword: TWebHttpRequest;
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
    function PostShiftType(AStartHour, AStartMinute, ADurationMinutes: integer;
      ActiveFrom, ActiveUntil: TDateTime; AName: string): Boolean;
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
    [async]
    function getShiftTypes(AList: TList<TShiftType>): Boolean;
    [async]
    function getRoles(AList: TList<TRole>): Boolean;
    [async]
    function getRoleById(ARoleId: string; out ARole: TRole): Boolean;
    [async]
    function PostRole(AName: string): Boolean;
    [async]
    function PutRole(ARoleId, AName: string): Boolean;
    [async]
    function DeleteRole(ARoleId: string): Boolean;
    [async]
    function getPersons(AList: TList<TPerson>): Boolean;
    [async]
    function getPersonById(APersonId: string; out APerson: TPerson): Boolean;
    [async]
    function PostPerson(AFirstName, ALastName: string; ADateOfBirth: TDateTime): Boolean;
    [async]
    function PutPerson(APersonId, AFirstName, ALastName: string;ADateOfBirth : TDateTime): Boolean;
    [async]
    function DeletePerson(APersonId: string): Boolean;
    [async]
    function getAllActivities(AList: TActivityList): Boolean;
    [async]
    function getActivityById(AActivityId: string; out AActivity: TActivity): Boolean;
    [async]
    function filterActivitiesPeriod(startDate, endDate: TDateTime; AActivityType: string; AList: TActivityList): Boolean;
    [async]
    function PutUser(AUserId, AEmail, APassword, ARole: string): Boolean;
    [async]
    function ChangePassword(AOldPassword, ANewPassword: string): Boolean;
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
  response, startString, endString: string;
  shiftList: TActivityList;
begin
  startString := FormatDateTime('yyyy-mm-dd', startDate);
  endString := FormatDateTime('yyyy-mm-dd', endDate);
  reqGetShiftsByPeriod.URL := baseUrl +
    Format('admin/activities/period/shifts?startDate=%s&endDate=%s',
    [startString, endString]);
  try

    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetShiftsByPeriod));

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

function TMedEcareDB.getShiftTypes(AList: TList<TShiftType>): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response, startString, endString: string;
  shiftTypeList: TList<TShiftType>;
begin
  reqGetShiftsByPeriod.URL := baseUrl + Format('admin/shift-types',
    [startString, endString]);
  try

    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetShiftsByPeriod));

    shiftTypeList := TShiftType.ToList(xhr.responseText, true);

    try
      if Assigned(AList) then
      begin
        AList.Clear;
        AList.AddRange(shiftTypeList);
      end;
    finally
      shiftTypeList.Free;
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
  response, startString, endString: string;
  verlofList: TActivityList;
begin
  startString := FormatDateTime('yyyy-mm-dd', startDate);
  endString := FormatDateTime('yyyy-mm-dd', endDate);
  reqGetVerlofByPeriod.URL := baseUrl +
    Format('admin/activities/period/verlof?startDate=%s&endDate=%s',
    [startString, endString]);
  try

    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetVerlofByPeriod));

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

function TMedEcareDB.PostShiftType(AStartHour, AStartMinute, ADurationMinutes
  : integer; ActiveFrom, ActiveUntil: TDateTime; AName: string): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
begin
  reqPostVerlof.URL := baseUrl + 'admin/shift-types';
  postData :=
    Format('{"name": "%s","startHour": %i,"startMinute": %i,"durationMinutes": %i,"activeFrom" : %i"activeUntil" : %i }',
    [AName, AStartHour, AStartMinute, ADurationMinutes,ActiveFrom,ActiveUntil]);
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

function TMedEcareDB.getRoles(AList: TList<TRole>): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
  roleList: TList<TRole>;
begin
  reqGetRoles.URL := baseUrl + 'admin/roles';
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetRoles));

    roleList := TRole.ToList(xhr.responseText, true);

    try
      if Assigned(AList) then
      begin
        AList.Clear;
        AList.AddRange(roleList);
      end;
    finally
      roleList.Free;
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

function TMedEcareDB.getRoleById(ARoleId: string; out ARole: TRole): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
begin
  reqGetRoles.URL := baseUrl + 'admin/roles/' + ARoleId;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetRoles));
    
    if (xhr.Status = 200) then
    begin
      response := xhr.responseText;
      ARole := TRole.FromJSON(response,true);
      Exit(true);
    end
    else if (xhr.Status = 404) then
    begin
      TAppManager.GetInstance.ShowToast('Role niet gevonden');
      Exit(False);
    end;
  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
  end;
end;

function TMedEcareDB.PostRole(AName: string): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
begin
  reqPostRoles.URL := baseUrl + 'admin/roles';
  postData := Format('{"name": "%s"}', [AName.ToUpper]);
  reqPostRoles.postData := postData;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPostRoles));
    if (xhr.Status = 201) then
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

function TMedEcareDB.PutRole(ARoleId, AName: string): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    reqPutRoles.URL := baseUrl + 'admin/roles/' + ARoleId;
    
    if AName <> '' then
      JSON.AddPair('name', AName.ToUpper);

    postData := JSON.ToString;
    reqPutRoles.postData := postData;
  finally
    JSON.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPutRoles));
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

function TMedEcareDB.DeleteRole(ARoleId: string): Boolean;
var
  xhr: TJSXMLHttpRequest;
begin
  reqDeleteRoles.URL := baseUrl + 'admin/roles/' + ARoleId;

  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqDeleteRoles));
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

function TMedEcareDB.getPersons(AList: TList<TPerson>): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
  personList: TList<TPerson>;
begin
  reqGetPersons.URL := baseUrl + 'admin/persons';
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetPersons));

    personList := TPerson.ToList(xhr.responseText, true);

    try
      if Assigned(AList) then
      begin
        AList.Clear;
        AList.AddRange(personList);
      end;
    finally
      personList.Free;
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

function TMedEcareDB.getPersonById(APersonId: string; out APerson: TPerson): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
begin
  reqGetPersons.URL := baseUrl + 'admin/persons/' + APersonId;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetPersons));
    
    if (xhr.Status = 200) then
    begin
      response := xhr.responseText;
      APerson := TPerson.ToObject(response,true);
      Exit(true);
    end
    else if (xhr.Status = 404) then
    begin
      TAppManager.GetInstance.ShowToast('Persoon niet gevonden');
      Exit(False);
    end;
  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
  end;
end;

function TMedEcareDB.PostPerson(AFirstName, ALastName: string; ADateOfBirth: TDateTime): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
  dateOfBirth: string;
begin
  reqPostPersons.URL := baseUrl + 'admin/persons';
  dateOfBirth := FormatDateTime('yyyy-mm-dd"T"hh:mm:ss".000Z"', ADateOfBirth);
  postData := Format('{"firstName": "%s", "lastName": "%s", "dateOfBirth": "%s"}', 
    [AFirstName, ALastName, dateOfBirth]);
  reqPostPersons.postData := postData;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPostPersons));
    if (xhr.Status = 201) then
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

function TMedEcareDB.PutPerson(APersonId, AFirstName, ALastName: string;ADateOfBirth : TDateTime): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    reqPutPersons.URL := baseUrl + 'admin/persons/' + APersonId;
    
    if AFirstName <> '' then
      JSON.AddPair('firstName', AFirstName);
    
    if ALastName <> '' then
      JSON.AddPair('lastName', ALastName);

    if ADateOfBirth > 0 then
      JSON.AddPair('dateOfBirth', ADateOfBirth);

    postData := JSON.ToString;
    reqPutPersons.postData := postData;
  finally
    JSON.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPutPersons));
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

function TMedEcareDB.DeletePerson(APersonId: string): Boolean;
var
  xhr: TJSXMLHttpRequest;
begin
  reqDeletePersons.URL := baseUrl + 'admin/persons/' + APersonId;

  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqDeletePersons));
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

function TMedEcareDB.getAllActivities(AList: TActivityList): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
  activityList: TActivityList;
begin
  reqGetActivities.URL := baseUrl + 'admin/activities';
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetActivities));

    activityList := TActivity.ToList(xhr.responseText, true);

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

  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
  end;
end;

function TMedEcareDB.getActivityById(AActivityId: string; out AActivity: TActivity): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
begin
  reqGetActivities.URL := baseUrl + 'admin/activities/' + AActivityId;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetActivities));
    
    if (xhr.Status = 200) then
    begin
      response := xhr.responseText;
      AActivity := TActivity.FromJSON(response,true);
      Exit(true);
    end
    else if (xhr.Status = 404) then
    begin
      TAppManager.GetInstance.ShowToast('Activiteit niet gevonden');
      Exit(False);
    end;
  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
  end;
end;

function TMedEcareDB.filterActivitiesPeriod(startDate, endDate: TDateTime; AActivityType: string; AList: TActivityList): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response, startString, endString, endpoint: string;
  activityList: TActivityList;
begin
  startString := FormatDateTime('yyyy-mm-dd', startDate);
  endString := FormatDateTime('yyyy-mm-dd', endDate);
  
  if AActivityType <> '' then
    endpoint := Format('admin/activities/period?startDate=%s&endDate=%s&activityType=%s', [startString, endString, AActivityType])
  else
    endpoint := Format('admin/activities/period?startDate=%s&endDate=%s', [startString, endString]);
    
  reqGetActivities.URL := baseUrl + endpoint;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetActivities));

    activityList := TActivity.ToList(xhr.responseText, true);

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

  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
  end;
end;

function TMedEcareDB.PutUser(AUserId, AEmail, APassword, ARole: string): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    reqPutUser.URL := baseUrl + 'admin/users/' + AUserId;
    
    if AEmail <> '' then
      JSON.AddPair('email', AEmail);
    
    if APassword <> '' then
      JSON.AddPair('password', APassword);
      
    if ARole <> '' then
      JSON.AddPair('role', ARole);

    postData := JSON.ToString;
    reqPutUser.postData := postData;
  finally
    JSON.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPutUser));
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

function TMedEcareDB.ChangePassword(AOldPassword, ANewPassword: string): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    reqPutChangePassword.URL := baseUrl + 'change-password';
    
    JSON.AddPair('oldPassword', AOldPassword);
    JSON.AddPair('newPassword', ANewPassword);

    postData := JSON.ToString;
    reqPutChangePassword.postData := postData;
  finally
    JSON.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPutChangePassword));
    if (xhr.Status = 200) then
    begin
      TAppManager.GetInstance.ShowToast('Wachtwoord succesvol gewijzigd');
      Exit(true);
    end
    else if (xhr.Status = 401) then
    begin
      TAppManager.GetInstance.ShowToast('Oud wachtwoord is incorrect');
      Exit(False);
    end
    else if (xhr.Status = 400) then
    begin
      TAppManager.GetInstance.ShowToast('Beide wachtwoorden zijn vereist of nieuw wachtwoord voldoet niet aan de vereisten');
      Exit(False);
    end;
  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit(False);
    end;
  end;
end;

end.
