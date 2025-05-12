unit orm.UserRole;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON;

type

  TUserRole = record
    UserId: string;
    RoleId: string;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TUserRole; overload; static;
    class function FromJSON(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean)
      : TUserRole; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TUserRole>; static;
  end;

implementation

class function TUserRole.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
  : TUserRole;
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

class function TUserRole.FromJSON(const AJsonObj: TJSONObject;
  ADateTimeIsUTC: Boolean): TUserRole;
begin
  Result := Default(TUserRole);
  Result.UserId := AJsonObj.GetJSONValue('userId');
  Result.RoleId := AJsonObj.GetJSONValue('roleId');
end;

class function TUserRole.ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TUserRole>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  UR: TUserRole;
  I: Integer;
begin
  Result := TList<TUserRole>.Create;
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        UR := TUserRole.FromJSON(jsonObject, ADateTimeIsUTC);
        Result.Add(UR);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

end.
