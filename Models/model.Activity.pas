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

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TActivity; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TActivity; overload; static;
  end;

implementation

class function TActivity.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TActivity;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TActivity.FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TActivity;
begin
  Result := Default (TActivity);
  if AJsonObj.hasOwnProperty('id') then
  begin
    Result.Id := JS.toString(AJsonObj['id']);
  end;
  if AJsonObj.hasOwnProperty('activityType') then
  begin
    Result.ActivityType := JS.toString(AJsonObj['activityType']);
  end;
  if AJsonObj.hasOwnProperty('start') then
  begin
    Result.Start := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['start']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('end') then
  begin
    Result.EndTime := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['end']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('personId') then
  begin
    Result.PersonId := JS.toString(AJsonObj['personId']);
  end;
  if AJsonObj.hasOwnProperty('shiftTypeId') then
  begin
    Result.ShiftTypeId := JS.toString(AJsonObj['shiftTypeId']);
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
