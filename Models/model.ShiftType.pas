unit Model.ShiftType;

interface

uses
  JS, Web, WEBLib.REST;

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

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TShiftType; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean)
      : TShiftType; overload; static;
  end;

implementation

class function TShiftType.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
  : TShiftType;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TShiftType.FromJSON(const AJsonObj: TJSObject;
  ADateTimeIsUTC: Boolean): TShiftType;
begin
  Result := Default (TShiftType);
  if AJsonObj.hasOwnProperty('id') then
    Result.Id := JS.toString(AJsonObj['id']);
  if AJsonObj.hasOwnProperty('name') then
    Result.Name := JS.toString(AJsonObj['name']);
  if AJsonObj.hasOwnProperty('startHour') then
    Result.StartHour := JS.toInteger(AJsonObj['startHour']);
  if AJsonObj.hasOwnProperty('startMinute') then
    Result.StartMinute := JS.toInteger(AJsonObj['startMinute']);
  if AJsonObj.hasOwnProperty('durationMinutes') then
    Result.DurationMinutes := JS.toInteger(AJsonObj['durationMinutes']);
  if AJsonObj.hasOwnProperty('activeFrom') then
    Result.ActiveFrom := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['activeFrom']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('activeUntil') then
    Result.ActiveUntil := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['activeUntil']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('createdAt') then
    Result.CreatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('updatedAt') then
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['updatedAt']), ADateTimeIsUTC);
end;

end.
