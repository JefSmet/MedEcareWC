unit orm.Role;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON;

type

  TRole = record
    Id: string;
    Name: string;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TRole; overload; static;
    class function FromJSON(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean)
      : TRole; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TRole>; static;
  end;

implementation

class function TRole.FromJSON(const AJson: string;
  ADateTimeIsUTC: Boolean): TRole;
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

class function TRole.FromJSON(const AJsonObj: TJSONObject;
  ADateTimeIsUTC: Boolean): TRole;
begin
  Result := Default(TRole);
  Result.Id := AJsonObj.GetJSONValue('id');
  Result.Name := AJsonObj.GetJSONValue('name');
end;

class function TRole.ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TRole>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  Role: TRole;
  I: Integer;
begin
  Result := TList<TRole>.Create;
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        Role := TRole.FromJSON(jsonObject, ADateTimeIsUTC);
        Result.Add(Role);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

end.
