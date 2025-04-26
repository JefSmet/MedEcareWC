unit Model.Activity;

interface

uses
  JS, Web, WEBLib.REST;

type
  TActivity = record
    Id           : string;
    ActivityType : string;
    Start        : TDateTime;
    EndTime      : TDateTime;
    PersonId     : string;
    ShiftTypeId  : string;
    CreatedAt    : TDateTime;
    UpdatedAt    : TDateTime;
    class function FromJSON(const S: string): TActivity; static;
  end;

implementation

class function TActivity.FromJSON(const S: string): TActivity;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(S);
  if o.hasOwnProperty('id') then Result.Id := string(o['id']);
  if o.hasOwnProperty('activityType') then Result.ActivityType := string(o['activityType']);
  if o.hasOwnProperty('start') then
    Result.Start := TWebRESTClient.IsoToDateTime(string(o['start']));
  if o.hasOwnProperty('end') then
    Result.EndTime := TWebRESTClient.IsoToDateTime(string(o['end']));
  if o.hasOwnProperty('personId') then Result.PersonId := string(o['personId']);
  if o.hasOwnProperty('shiftTypeId') then Result.ShiftTypeId := string(o['shiftTypeId']);
  if o.hasOwnProperty('createdAt') then
    Result.CreatedAt := TWebRESTClient.IsoToDateTime(string(o['createdAt']));
  if o.hasOwnProperty('updatedAt') then
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime(string(o['updatedAt']));
end;

end.
