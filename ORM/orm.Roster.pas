unit orm.Roster;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON,
  System.SysUtils, orm.ShiftType;

type

  TRoster = record
    Id: Integer;
    ShiftTypeId: string;
    ShiftTypeName: string;
    Starthour: Integer;
    StartMinute: Integer;
    DurationMinutes: Integer;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;

    class function ToObject(const AJson: string; ADateTimeIsUTC: Boolean)
      : TRoster; overload; static;
    class function ToObject(const AJsonObj: TJSONObject;
      ADateTimeIsUTC: Boolean): TRoster; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean)
      : TList<TRoster>; static;
  end;

implementation

class function TRoster.ToObject(const AJson: string;
  ADateTimeIsUTC: Boolean): TRoster;
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

class function TRoster.ToList(const AJson: string; ADateTimeIsUTC: Boolean)
  : TList<TRoster>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  Roster: TRoster;
  I: Integer;
begin
  Result := TList<TRoster>.Create();
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        Roster := ToObject(jsonObject, ADateTimeIsUTC);
        Result.Add(Roster);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

class function TRoster.ToObject(const AJsonObj: TJSONObject;
  ADateTimeIsUTC: Boolean): TRoster;
var
  ShiftType: TJSONObject;
begin
  Result := Default (TRoster);
  Result.Id := StrToInt(AJsonObj.GetJSONValue('id'));
  Result.ShiftTypeId := AJsonObj.GetJSONValue('shiftTypeId');
  ShiftType := TJSONObject(AJsonObj.GetValue('shiftType'));
  Result.ShiftTypeName := ShiftType.GetJSONValue('name');

  Result.Starthour := StrToInt(ShiftType.GetJSONValue('startHour'));
  Result.StartMinute := StrToInt(ShiftType.GetJSONValue('startMinute'));
  Result.DurationMinutes := StrToInt(ShiftType.GetJSONValue('durationMinutes'));

  Result.CreatedAt := TWebRESTClient.IsoToDateTime
    (AJsonObj.GetJSONValue('createdAt'), ADateTimeIsUTC);
  Result.UpdatedAt := TWebRESTClient.IsoToDateTime
    (AJsonObj.GetJSONValue('updatedAt'), ADateTimeIsUTC);
end;

end.
