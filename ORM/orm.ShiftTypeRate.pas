unit orm.ShiftTypeRate;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON, System.SysUtils;

type

  TShiftTypeRate = record
    Id: string;
    ShiftTypeId: string;
    Rate: Double;
    ValidFrom: TDateTime;
    ValidUntil: TDateTime;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TShiftTypeRate; overload; static;
    class function FromJSON(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean)
      : TShiftTypeRate; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TShiftTypeRate>; static;
  end;

implementation

class function TShiftTypeRate.FromJSON(const AJson: string;
  ADateTimeIsUTC: Boolean): TShiftTypeRate;
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

class function TShiftTypeRate.FromJSON(const AJsonObj: TJSONObject;
  ADateTimeIsUTC: Boolean): TShiftTypeRate;
begin
  Result := Default(TShiftTypeRate);
  Result.Id := AJsonObj.GetJSONValue('id');
  Result.ShiftTypeId := AJsonObj.GetJSONValue('shiftTypeId');
  Result.Rate := StrToFloat(AJsonObj.GetJSONValue('rate'));
  Result.ValidFrom := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('validFrom'), ADateTimeIsUTC);
  Result.ValidUntil := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('validUntil'), ADateTimeIsUTC);
  Result.CreatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('createdAt'), ADateTimeIsUTC);
  Result.UpdatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('updatedAt'), ADateTimeIsUTC);
end;

class function TShiftTypeRate.ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TShiftTypeRate>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  Rate: TShiftTypeRate;
  I: Integer;
begin
  Result := TList<TShiftTypeRate>.Create;
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        Rate := TShiftTypeRate.FromJSON(jsonObject, ADateTimeIsUTC);
        Result.Add(Rate);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

end.
