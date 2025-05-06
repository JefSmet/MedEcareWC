unit orm.ShiftTypeRate;

interface

uses
  JS, Web, WEBLib.REST;

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
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean)
      : TShiftTypeRate; overload; static;
  end;

implementation

class function TShiftTypeRate.FromJSON(const AJson: string;
  ADateTimeIsUTC: Boolean): TShiftTypeRate;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TShiftTypeRate.FromJSON(const AJsonObj: TJSObject;
  ADateTimeIsUTC: Boolean): TShiftTypeRate;
begin
  Result := Default (TShiftTypeRate);
  if AJsonObj.hasOwnProperty('id') then
  begin
    Result.Id := JS.toString(AJsonObj['id']);
  end;
  if AJsonObj.hasOwnProperty('shiftTypeId') then
  begin
    Result.ShiftTypeId := JS.toString(AJsonObj['shiftTypeId']);
  end;
  if AJsonObj.hasOwnProperty('rate') then
  begin
    Result.Rate := JS.toNumber(AJsonObj['rate']);
  end;
  if AJsonObj.hasOwnProperty('validFrom') then
  begin
    Result.ValidFrom := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['validFrom']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('validUntil') then
  begin
    Result.ValidUntil := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['validUntil']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('createdAt') then
  begin
    Result.CreatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('updatedAt') then
  begin
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['updatedAt']), ADateTimeIsUTC);
  end;
end;

end.
