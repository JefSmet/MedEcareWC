unit orm.UserConstraint;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON, System.SysUtils;

type

  TUserConstraint = record
    Id: string;
    PersonId: string;
    MaxNightShiftsPerWeek: Integer;
    MaxConsecutiveNightShifts: Integer;
    MinRestHoursBetweenShifts: Integer;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TUserConstraint; overload; static;
    class function FromJSON(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean)
      : TUserConstraint; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TUserConstraint>; static;
  end;

implementation

class function TUserConstraint.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TUserConstraint;
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

class function TUserConstraint.FromJSON(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean): TUserConstraint;
begin
  Result := Default(TUserConstraint);
  Result.Id := AJsonObj.GetJSONValue('id');
  Result.PersonId := AJsonObj.GetJSONValue('personId');
  Result.MaxNightShiftsPerWeek := StrToInt(AJsonObj.GetJSONValue('maxNightShiftsPerWeek'));
  Result.MaxConsecutiveNightShifts := StrToInt(AJsonObj.GetJSONValue('maxConsecutiveNightShifts'));
  Result.MinRestHoursBetweenShifts := StrToInt(AJsonObj.GetJSONValue('minRestHoursBetweenShifts'));
  Result.CreatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('createdAt'), ADateTimeIsUTC);
  Result.UpdatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('updatedAt'), ADateTimeIsUTC);
end;

class function TUserConstraint.ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TUserConstraint>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  Constraint: TUserConstraint;
  I: Integer;
begin
  Result := TList<TUserConstraint>.Create;
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        Constraint := TUserConstraint.FromJSON(jsonObject, ADateTimeIsUTC);
        Result.Add(Constraint);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

end.
