unit Model.ShiftTypeRate;

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
    Result.Id := JS.toString(AJsonObj['id']);
  if AJsonObj.hasOwnProperty('shiftTypeId') then
    Result.ShiftTypeId := JS.toString(AJsonObj['shiftTypeId']);
  if AJsonObj.hasOwnProperty('rate') then
    Result.Rate := JS.toNumber(AJsonObj['rate']);
  if AJsonObj.hasOwnProperty('validFrom') then
    Result.ValidFrom := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['validFrom']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('validUntil') then
    Result.ValidUntil := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['validUntil']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('createdAt') then
    Result.CreatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('updatedAt') then
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['updatedAt']), ADateTimeIsUTC);
end;

end.
