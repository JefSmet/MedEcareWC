unit orm.Activity;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON, orm.Person,
  orm.ShiftType;

type

  TActivity = record
    Id: string;
    ActivityType: string;
    Start: TDateTime;
    EndTime: TDateTime;
    PersonId: string;
    ShiftTypeId: string;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;
    Person: TPerson;
    ShiftType: TShiftType;
    Status: string;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TActivity; overload; static;
    class function FromJSON(const AJsonObj: TJSONObject;
      ADateTimeIsUTC: Boolean): TActivity; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean)
      : TList<TActivity>; static;
  end;

  TActivityList = TList<TActivity>;

implementation

class function TActivity.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
  : TActivity;
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

class function TActivity.FromJSON(const AJsonObj: TJSONObject;
  ADateTimeIsUTC: Boolean): TActivity;
begin
  Result := Default (TActivity);
  Result.Id := AJsonObj.GetJSONValue('id');
  Result.ActivityType := AJsonObj.GetJSONValue('activityType');
  Result.Start := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('start'),
    ADateTimeIsUTC);
  Result.EndTime := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('end'),
    ADateTimeIsUTC);
  Result.PersonId := AJsonObj.GetJSONValue('personId');
  Result.ShiftTypeId := AJsonObj.GetJSONValue('shiftTypeId');
  Result.CreatedAt := TWebRESTClient.IsoToDateTime
    (AJsonObj.GetJSONValue('createdAt'), ADateTimeIsUTC);
  Result.UpdatedAt := TWebRESTClient.IsoToDateTime
    (AJsonObj.GetJSONValue('updatedAt'), ADateTimeIsUTC);
  Result.Person := TPerson.ToObject(TJSONObject(AJsonObj.GetValue('person')),
    ADateTimeIsUTC);
  if Result.ShiftTypeId <> 'null' then
    Result.ShiftType := TShiftType.ToObject
      (TJSONObject(AJsonObj.GetValue('shiftType')), ADateTimeIsUTC);
  Result.Status := AJsonObj.GetJSONValue('status');
end;

class function TActivity.ToList(const AJson: string; ADateTimeIsUTC: Boolean)
  : TList<TActivity>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  Activity: TActivity;
  I: Integer;
begin
  Result := TList<TActivity>.Create;
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        Activity := TActivity.FromJSON(jsonObject, ADateTimeIsUTC);
        Result.Add(Activity);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

end.
