unit orm.User;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON;

type

  TUser = record
    Id: string;
    Email: string;
    Password: string;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;
    ResetToken: string;
    ResetExpire: TDateTime;

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
begin
  Result := Default(TUser);
  Result.Id := AJsonObj.GetJSONValue('personId');
  Result.Email := AJsonObj.GetJSONValue('email');
  Result.Password := AJsonObj.GetJSONValue('password');
  Result.CreatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('createdAt'), ADateTimeIsUTC);
  Result.UpdatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('updatedAt'), ADateTimeIsUTC);
  Result.ResetToken := AJsonObj.GetJSONValue('resetToken');
  Result.ResetExpire := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('resetExpire'), ADateTimeIsUTC);
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
