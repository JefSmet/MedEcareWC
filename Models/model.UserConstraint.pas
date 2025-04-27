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

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TUserConstraint; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean)
      : TUserConstraint; overload; static;
  end;

implementation

class function TUserConstraint.FromJSON(const AJson: string;
  ADateTimeIsUTC: Boolean): TUserConstraint;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TUserConstraint.FromJSON(const AJsonObj: TJSObject;
  ADateTimeIsUTC: Boolean): TUserConstraint;
begin
  Result := Default (TUserConstraint);
  if AJsonObj.hasOwnProperty('id') then
    Result.Id := JS.toString(AJsonObj['id']);
  if AJsonObj.hasOwnProperty('personId') then
    Result.PersonId := JS.toString(AJsonObj['personId']);
  if AJsonObj.hasOwnProperty('maxNightShiftsPerWeek') then
    Result.MaxNightShiftsPerWeek :=
      JS.toInteger(AJsonObj['maxNightShiftsPerWeek']);
  if AJsonObj.hasOwnProperty('maxConsecutiveNightShifts') then
    Result.MaxConsecutiveNightShifts :=
      JS.toInteger(AJsonObj['maxConsecutiveNightShifts']);
  if AJsonObj.hasOwnProperty('minRestHoursBetweenShifts') then
    Result.MinRestHoursBetweenShifts :=
      JS.toInteger(AJsonObj['minRestHoursBetweenShifts']);
  if AJsonObj.hasOwnProperty('createdAt') then
    Result.CreatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('updatedAt') then
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['updatedAt']), ADateTimeIsUTC);
end;

end.
