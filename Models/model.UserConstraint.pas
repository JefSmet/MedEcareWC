unit Model.UserConstraint;

interface

uses
  JS, Web, WEBLib.REST;

type

  TUserConstraint = record
    Id: string;
    PersonId: string;
    MaxNightShiftsPerWeek: Integer;
    MaxConsecutiveNightShifts: Integer;
    MinRestHoursBetweenShifts: Integer;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TUserConstraint; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TUserConstraint; overload; static;
  end;

implementation

class function TUserConstraint.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TUserConstraint;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TUserConstraint.FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TUserConstraint;
begin
  Result := Default (TUserConstraint);
  if AJsonObj.hasOwnProperty('id') then
  begin
    Result.Id := JS.toString(AJsonObj['id']);
  end;
  if AJsonObj.hasOwnProperty('personId') then
  begin
    Result.PersonId := JS.toString(AJsonObj['personId']);
  end;
  if AJsonObj.hasOwnProperty('maxNightShiftsPerWeek') then
  begin
    Result.MaxNightShiftsPerWeek := JS.toInteger(AJsonObj['maxNightShiftsPerWeek']);
  end;
  if AJsonObj.hasOwnProperty('maxConsecutiveNightShifts') then
  begin
    Result.MaxConsecutiveNightShifts := JS.toInteger(AJsonObj['maxConsecutiveNightShifts']);
  end;
  if AJsonObj.hasOwnProperty('minRestHoursBetweenShifts') then
  begin
    Result.MinRestHoursBetweenShifts := JS.toInteger(AJsonObj['minRestHoursBetweenShifts']);
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
