unit Model.Activity;

interface

uses
  JS, Web, WEBLib.REST;

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

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TActivity; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean)
      : TActivity; overload; static;
  end;

implementation

class function TActivity.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
  : TActivity;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TActivity.FromJSON(const AJsonObj: TJSObject;
  ADateTimeIsUTC: Boolean): TActivity;
begin
  Result := Default (TActivity);
  if AJsonObj.hasOwnProperty('id') then
    Result.Id := JS.toString(AJsonObj['id']);
  if AJsonObj.hasOwnProperty('activityType') then
    Result.ActivityType := JS.toString(AJsonObj['activityType']);
  if AJsonObj.hasOwnProperty('start') then
    Result.Start := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['start']),
      ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('end') then
    Result.EndTime := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['end']),
      ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('personId') then
    Result.PersonId := JS.toString(AJsonObj['personId']);
  if AJsonObj.hasOwnProperty('shiftTypeId') then
    Result.ShiftTypeId := JS.toString(AJsonObj['shiftTypeId']);
  if AJsonObj.hasOwnProperty('createdAt') then
    Result.CreatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('updatedAt') then
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['updatedAt']), ADateTimeIsUTC);
end;

end.
