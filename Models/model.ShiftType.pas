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

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TShiftType; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TShiftType; overload; static;
  end;

implementation

class function TShiftType.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TShiftType;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TShiftType.FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TShiftType;
begin
  Result := Default (TShiftType);
  if AJsonObj.hasOwnProperty('id') then
  begin
    Result.Id := JS.toString(AJsonObj['id']);
  end;
  if AJsonObj.hasOwnProperty('name') then
  begin
    Result.Name := JS.toString(AJsonObj['name']);
  end;
  if AJsonObj.hasOwnProperty('startHour') then
  begin
    Result.StartHour := JS.toInteger(AJsonObj['startHour']);
  end;
  if AJsonObj.hasOwnProperty('startMinute') then
  begin
    Result.StartMinute := JS.toInteger(AJsonObj['startMinute']);
  end;
  if AJsonObj.hasOwnProperty('durationMinutes') then
  begin
    Result.DurationMinutes := JS.toInteger(AJsonObj['durationMinutes']);
  end;
  if AJsonObj.hasOwnProperty('activeFrom') then
  begin
    Result.ActiveFrom := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['activeFrom']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('activeUntil') then
  begin
    Result.ActiveUntil := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['activeUntil']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('createdAt') then
  begin
    Result.CreatedAt := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('updatedAt') then
  begin
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['updatedAt']), ADateTimeIsUTC);
  end;
end;

end.
