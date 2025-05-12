unit orm.ShiftType;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON, System.SysUtils;

type

  TShiftType = record
    Id: string;
    Name: string;
    StartHour: Integer;
    StartMinute: Integer;
    DurationMinutes: Integer;
    ActiveFrom: TDateTime;
    ActiveUntil: TDateTime;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;

    class function ToObject(const AJson: string; ADateTimeIsUTC: Boolean)
      : TShiftType; overload; static;
    class function ToObject(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean)
      : TShiftType; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TShiftType>; static;
  end;

implementation

class function TShiftType.ToObject(const AJson: string; ADateTimeIsUTC: Boolean): TShiftType;
var
  JS: TJSON;
  jsonObject: TJSONObject;
begin
  JS := TJSON.Create;
  try
    jsonObject := TJSONObject(JS.Parse(AJson));
    Result := ToObject(jsonObject, ADateTimeIsUTC);
  finally
    JS.Free;
  end;
end;

class function TShiftType.ToObject(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean): TShiftType;
begin
  Result := Default(TShiftType);
  Result.Id := AJsonObj.GetJSONValue('id');
  Result.Name := AJsonObj.GetJSONValue('name');
  Result.StartHour := StrToInt(AJsonObj.GetJSONValue('startHour'));
  Result.StartMinute := StrToInt(AJsonObj.GetJSONValue('startMinute'));
  Result.DurationMinutes := StrToInt(AJsonObj.GetJSONValue('durationMinutes'));
  Result.ActiveFrom := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('activeFrom'), ADateTimeIsUTC);
  Result.ActiveUntil := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('activeUntil'), ADateTimeIsUTC);
  Result.CreatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('createdAt'), ADateTimeIsUTC);
  Result.UpdatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('updatedAt'), ADateTimeIsUTC);
end;

class function TShiftType.ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TShiftType>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  ShiftType: TShiftType;
  I: Integer;
begin
  Result := TList<TShiftType>.Create;
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        ShiftType := TShiftType.ToObject(jsonObject, ADateTimeIsUTC);
        Result.Add(ShiftType);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

end.
