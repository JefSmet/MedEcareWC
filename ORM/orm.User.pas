unit orm.User;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON, orm.Person, orm.Doctor, orm.UserRole;

type
//{
//    "personId": "414756FA-F4A9-4908-B8AB-42982697585E",
//    "email": "filip.smet@medecare.be",
//    "createdAt": "2025-04-28T13:58:32.696Z",
//    "updatedAt": "2025-04-28T13:58:32.696Z",
//    "person": {
//      "id": "414756FA-F4A9-4908-B8AB-42982697585E",
//      "firstName": "Filip",
//      "lastName": "Smet",
//      "dateOfBirth": "1969-04-08T00:00:00.000Z",
//      "createdAt": "2025-04-28T13:58:32.696Z",
//      "updatedAt": "2025-04-28T13:58:32.696Z"
//    }
// }

  TUser = record
    Id: string;
    Email: string;
    Password: string;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;
    ResetToken: string;
    ResetExpire: TDateTime;

    Person: TPerson;
    Doctor : TDoctor;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TUser; overload; static;
    class function FromJSON(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean)
      : TUser; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TUser>; static;
  end;

implementation

class function TUser.FromJSON(const AJson: string;
  ADateTimeIsUTC: Boolean): TUser;
var
  JS: TJSON;
  jsonObject: TJSONObject;
begin
  JS := TJSON.Create;
  try
    jsonObject := TJSONObject(JS.Parse(AJson));
    Result := FromJSON(jsonObject, ADateTimeIsUTC);
  finally
    JS.Free;
  end;
end;

class function TUser.FromJSON(const AJsonObj: TJSONObject;
  ADateTimeIsUTC: Boolean): TUser;
  var
    jsonPerson : TJSONObject;
begin
  Result := Default(TUser);
  Result.Id := AJsonObj.GetJSONValue('personId');
  Result.Email := AJsonObj.GetJSONValue('email');
  Result.Password := AJsonObj.GetJSONValue('password');
  Result.CreatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('createdAt'), ADateTimeIsUTC);
  Result.UpdatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('updatedAt'), ADateTimeIsUTC);
  Result.ResetToken := AJsonObj.GetJSONValue('resetToken');
  Result.ResetExpire := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('resetExpire'), ADateTimeIsUTC);

   // Parse embedded Person object if present
  if AJsonObj.GetJSONValue('person') <> '' then
  begin
  jsonPerson := TJSONObject(AJsonObj.GetValue('person'));
    Result.Person := TPerson.ToObject(jsonPerson, ADateTimeIsUTC);
    if jsonPerson.GetJSONValue('doctor') <> '' then
  begin
    Result.Doctor := TDoctor.ToObject(TJSONObject(jsonPerson.GetValue('doctor')), ADateTimeIsUTC);
  end;
  end;

end;

class function TUser.ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TUser>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  User: TUser;
  I: Integer;
begin
  Result := TList<TUser>.Create;
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        User := TUser.FromJSON(jsonObject, ADateTimeIsUTC);
        Result.Add(User);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

end.
