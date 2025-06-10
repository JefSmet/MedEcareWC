unit model.MedEcareDB;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.REST,
  orm.Activity, System.Generics.Collections, orm.Person, orm.Doctor,
  orm.ShiftType, orm.Role, orm.Roster, orm.User, orm.UserRole;

type
  TShiftTypeNameAndId = record
    id: string;
    name: string;
  end;

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
    reqGetRoster: TWebHttpRequest;
    reqGetShiftTypeById: TWebHttpRequest;
    reqGetUsers: TWebHttpRequest;
    reqGetDoctorById: TWebHttpRequest;
    reqGetUserRole: TWebHttpRequest;
    reqPostUser: TWebHttpRequest;
    reqPostDoctor: TWebHttpRequest;
    reqPutDoctor: TWebHttpRequest;
    reqDeleteDoctor: TWebHttpRequest;
    reqDeleteUser: TWebHttpRequest;
    reqPostRegister: TWebHttpRequest;
    reqPostRoster: TWebHttpRequest;
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
    function getShiftTypeById(AShiftTypeId: string; out AShiftType: TShiftType): Boolean;
    [async]
    function PutShiftType(AShiftTypeId, AName: string; AStartHour, AStartMinute, ADurationMinutes: integer; AActiveFrom, AActiveUntil: TDateTime): Boolean;
    [async]
    function DeleteShiftType(AShiftTypeId: string): Boolean;
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
    function PostPerson(AFirstName, ALastName: string; ADateOfBirth: TDateTime): TPerson;
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
    [async]
    function getRosters(AList: TList<TRoster>): Boolean;
    [async]
    function getUsers(AList: TList<TUser>): Boolean;
    [async]
    function getDoctorById(ADoctorId: string; out ADoctor: TDoctor): Boolean;
    [async]
    function getUserRoles(AList: TList<TUserRole>): Boolean;
    [async]
    function PostDoctor(APersonId, ARizivNumber: string; AIsEnabledInShifts: Boolean): Boolean;
    [async]
    function PutDoctor(ADoctorId, ARizivNumber: string; AIsEnabledInShifts: Boolean): Boolean;
    [async]
    function DeleteDoctor(ADoctorId: string): Boolean;
//    [async]
//    function PostUser(AEmail, APassword, ARole: string): Boolean;
//    [async]
//    function getUserById(AUserId: string; out AUser: TUser): Boolean;
    [async]
    function DeleteUser(AUserId: string): Boolean;
    [async]
    function PostRegister(AEmail, APassword, ARole, AFirstName, ALastName: string; ADateOfBirth: TDateTime; ARizivNumber: string; AIsEnabledInShifts: Boolean = true): Boolean;
    [async]
    function PostRoster(ARosters: string): Boolean;
  end;

var
  MedEcareDB: TMedEcareDB;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses middleware.httponly, DateUtils, model.AppManager, WEBLib.JSON, Forms.roster;
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

  reqGetStaff.URL := baseUrl + 'admin/doctors';
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
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    reqPostShiftTypes.URL := baseUrl + 'admin/shift-types';
    
    if AName <> '' then
      JSON.AddPair('name', AName);
      
    if AStartHour >= 0 then
      JSON.AddPair('startHour', AStartHour);
      
    if AStartMinute >= 0 then
      JSON.AddPair('startMinute', AStartMinute);
      
    if ADurationMinutes > 0 then
      JSON.AddPair('durationMinutes', ADurationMinutes);
      
    if ActiveFrom > 0 then
      JSON.AddPair('activeFrom', FormatDateTime('yyyy-mm-dd"T"hh:mm:ss".000Z"', ActiveFrom));
      
    if ActiveUntil > 0 then
      JSON.AddPair('activeUntil', FormatDateTime('yyyy-mm-dd"T"hh:mm:ss".000Z"', ActiveUntil));

    postData := JSON.ToString;
    reqPostShiftTypes.postData := postData;
  finally
    JSON.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPostShiftTypes));
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

function TMedEcareDB.PostPerson(AFirstName, ALastName: string; ADateOfBirth: TDateTime): TPerson;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
  dateOfBirth: string;
  returnPerson: TPerson;
  jo: TJSONObject;
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
      jo := TJSONObject(xhr.response);
      returnPerson := TPerson.ToObject(jo,true);
      Exit(returnPerson);
    end;
  except
    on e: exception do
    begin
      TAppManager.GetInstance.ShowToast('Er ging iets mis: ' + e.Message);
      Exit;
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
      JSON.AddPair('dateOfBirth', WEBLib.REST.TWebRESTClient.DateTimeToWL(ADateOfBirth));

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

function TMedEcareDB.getRosters(AList: TList<TRoster>): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
  rosterList: TList<TRoster>;
begin
  reqGetRoster.URL := baseUrl + 'admin/rosters';
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetRoster));

    if (xhr.Status = 200) then
    begin
      rosterList := TRoster.ToList(xhr.responseText, true);
      
      try
        if Assigned(AList) then
        begin
          AList.Clear;
          AList.AddRange(rosterList);
        end;
      finally
        rosterList.Free;
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

function TMedEcareDB.getShiftTypeById(AShiftTypeId: string; out AShiftType: TShiftType): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
begin
  reqGetShiftTypeById.URL := baseUrl + 'admin/shift-types/' + AShiftTypeId;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetShiftTypeById));
    
    if (xhr.Status = 200) then
    begin
      response := xhr.responseText;
      AShiftType := TShiftType.ToObject(response, true);
      Exit(true);
    end
    else if (xhr.Status = 404) then
    begin
      TAppManager.GetInstance.ShowToast('ShiftType niet gevonden');
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

function TMedEcareDB.PutShiftType(AShiftTypeId, AName: string; AStartHour, AStartMinute, ADurationMinutes: integer; AActiveFrom, AActiveUntil: TDateTime): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    reqPutShiftTypes.URL := baseUrl + 'admin/shift-types/' + AShiftTypeId;
    
    if AName <> '' then
      JSON.AddPair('name', AName);
      
    if AStartHour >= 0 then
      JSON.AddPair('startHour', AStartHour);
      
    if AStartMinute >= 0 then
      JSON.AddPair('startMinute', AStartMinute);
      
    if ADurationMinutes > 0 then
      JSON.AddPair('durationMinutes', ADurationMinutes);
      
    if AActiveFrom > 0 then
      JSON.AddPair('activeFrom', FormatDateTime('yyyy-mm-dd"T"hh:mm:ss".000Z"', AActiveFrom));
      
    if AActiveUntil > 0 then
      JSON.AddPair('activeUntil', FormatDateTime('yyyy-mm-dd"T"hh:mm:ss".000Z"', AActiveUntil));

    postData := JSON.ToString;
    reqPutShiftTypes.postData := postData;
  finally
    JSON.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPutShiftTypes));
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

function TMedEcareDB.DeleteShiftType(AShiftTypeId: string): Boolean;
var
  xhr: TJSXMLHttpRequest;
begin
  reqDeleteShiftTypes.URL := baseUrl + 'admin/shift-types/' + AShiftTypeId;

  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqDeleteShiftTypes));
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

function TMedEcareDB.getUsers(AList: TList<TUser>): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
  userList: TList<TUser>;
begin
  reqGetUsers.URL := baseUrl + 'admin/users';
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetUsers));

    if (xhr.Status = 200) then
    begin
      userList := TUser.ToList(xhr.responseText, true);
      
      try
        if Assigned(AList) then
        begin
          AList.Clear;
          AList.AddRange(userList);
        end;
      finally
        userList.Free;
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

function TMedEcareDB.getDoctorById(ADoctorId: string; out ADoctor: TDoctor): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
begin
  reqGetDoctorById.URL := baseUrl + 'admin/doctors/' + ADoctorId;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetDoctorById));
    
    if (xhr.Status = 200) then
    begin
      response := xhr.responseText;
      ADoctor := TDoctor.ToObject(response, true);
      Exit(true);
    end
    else if (xhr.Status = 404) then
    begin
      TAppManager.GetInstance.ShowToast('Doctor niet gevonden');
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

function TMedEcareDB.getUserRoles(AList: TList<TUserRole>): Boolean;
var
  xhr: TJSXMLHttpRequest;
  response: string;
  userRoleList: TList<TUserRole>;
begin
  reqGetUserRole.URL := baseUrl + 'admin/user-roles';
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqGetUserRole));

    if (xhr.Status = 200) then
    begin
      userRoleList := TUserRole.ToList(xhr.responseText, true);
      
      try
        if Assigned(AList) then
        begin
          AList.Clear;
          AList.AddRange(userRoleList);
        end;
      finally
        userRoleList.Free;
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

function TMedEcareDB.PostDoctor(APersonId, ARizivNumber: string; AIsEnabledInShifts: Boolean): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    reqPostDoctor.URL := baseUrl + 'admin/doctors';
    
    JSON.AddPair('personId', APersonId);
    
    if ARizivNumber <> '' then
      JSON.AddPair('rizivNumber', ARizivNumber);
      
    JSON.AddPair('isEnabledInShifts', AIsEnabledInShifts.ToInteger);

    postData := JSON.ToString;
    reqPostDoctor.postData := postData;
  finally
    JSON.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPostDoctor));
    if (xhr.Status = 201) then
    begin
      Exit(true);
    end
    else if (xhr.Status = 400) then
    begin
      TAppManager.GetInstance.ShowToast('Persoon niet gevonden of validatiefout');
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

function TMedEcareDB.PutDoctor(ADoctorId, ARizivNumber: string; AIsEnabledInShifts: Boolean): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    reqPutDoctor.URL := baseUrl + 'admin/doctors/' + ADoctorId;
    
    if ARizivNumber <> '' then
      JSON.AddPair('rizivNumber', ARizivNumber);
      
    JSON.AddPair('isEnabledInShifts', TJSONBool.Create(AIsEnabledInShifts));

    postData := JSON.ToString;
    reqPutDoctor.postData := postData;
  finally
    JSON.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPutDoctor));
    if (xhr.Status = 200) then
    begin
      Exit(true);
    end
    else if (xhr.Status = 404) then
    begin
      TAppManager.GetInstance.ShowToast('Doctor niet gevonden');
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

function TMedEcareDB.DeleteDoctor(ADoctorId: string): Boolean;
var
  xhr: TJSXMLHttpRequest;
begin
  reqDeleteDoctor.URL := baseUrl + 'admin/doctors/' + ADoctorId;

  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqDeleteDoctor));
    if (xhr.Status = 200) then
    begin
      Exit(true);
    end
    else if (xhr.Status = 404) then
    begin
      TAppManager.GetInstance.ShowToast('Doctor niet gevonden');
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

function TMedEcareDB.DeleteUser(AUserId: string): Boolean;
var
  xhr: TJSXMLHttpRequest;
begin
  reqDeleteUser.URL := baseUrl + 'admin/users/' + AUserId;

  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqDeleteUser));
    if (xhr.Status = 200) then
    begin
      Exit(true);
    end
    else if (xhr.Status = 404) then
    begin
      TAppManager.GetInstance.ShowToast('Gebruiker niet gevonden');
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

function TMedEcareDB.PostRegister(AEmail, APassword, ARole, AFirstName, ALastName: string; ADateOfBirth: TDateTime; ARizivNumber: string; AIsEnabledInShifts: Boolean = true): Boolean;
var
  postData: string;
  xhr: TJSXMLHttpRequest;
  JSON: TJSONObject;
  dateOfBirth: string;
begin
  JSON := TJSONObject.Create;
  try
    reqPostRegister.URL := baseUrl + 'auth/register';
    
    JSON.AddPair('email', AEmail);
    JSON.AddPair('password', APassword);
    
    if ARole <> '' then
      JSON.AddPair('role', ARole);
      
    JSON.AddPair('firstName', AFirstName);
    JSON.AddPair('lastName', ALastName);
    
    dateOfBirth := FormatDateTime('yyyy-mm-dd"T"hh:mm:ss".000Z"', ADateOfBirth);
    JSON.AddPair('dateOfBirth', dateOfBirth);
    
    JSON.AddPair('rizivNumber', ARizivNumber);
    JSON.AddPair('isEnabledInShifts',TJSONBool.Create(AIsEnabledInShifts));
//    if AIsEnabledInShifts then
//    JSON.AddPair('isEnabledInShifts', 'true')
//    else
//    JSON.AddPair('isEnabledInShifts', 'false');

    postData := JSON.ToString;
    reqPostRegister.postData := postData;
  finally
    JSON.Free;
  end;
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPostRegister));
    if (xhr.Status = 201) then
    begin
      TAppManager.GetInstance.ShowToast('Registratie succesvol voltooid');
      Exit(true);
    end
    else if (xhr.Status = 400) then
    begin
      TAppManager.GetInstance.ShowToast('Validatiefout: controleer alle velden en wachtwoordvereisten');
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

function TMedEcareDB.PostRoster(ARosters: string): Boolean;
var
  xhr: TJSXMLHttpRequest;
begin
  reqPostRoster.URL := baseUrl + 'admin/rosters';
  reqPostRoster.postData := ARosters;
  
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(reqPostRoster));
    if (xhr.Status = 201) then
    begin
      TAppManager.GetInstance.ShowToast('Roster succesvol aangemaakt');
      Exit(true);
    end
    else if (xhr.Status = 400) then
    begin
      TAppManager.GetInstance.ShowToast('Validatiefout: controleer roster data structuur');
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
