unit Model.ShiftType;

interface

uses
  JS, Web, WEBLib.REST;

type
  TShiftType = record
    Id              : string;
    Name            : string;
    StartHour       : Integer;
    StartMinute     : Integer;
    DurationMinutes : Integer;
    ActiveFrom      : TDateTime;
    ActiveUntil     : TDateTime;
    CreatedAt       : TDateTime;
    UpdatedAt       : TDateTime;
    class function FromJSON(const S: string): TShiftType; static;
  end;

implementation

class function TShiftType.FromJSON(const S: string): TShiftType;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(S);
  if o.hasOwnProperty('id') then Result.Id := string(o['id']);
  if o.hasOwnProperty('name') then Result.Name := string(o['name']);
  if o.hasOwnProperty('startHour') then Result.StartHour := Integer(o['startHour']);
  if o.hasOwnProperty('startMinute') then Result.StartMinute := Integer(o['startMinute']);
  if o.hasOwnProperty('durationMinutes') then Result.DurationMinutes := Integer(o['durationMinutes']);
  if o.hasOwnProperty('activeFrom') then
    Result.ActiveFrom := TWebRESTClient.IsoToDateTime(string(o['activeFrom']));
  if o.hasOwnProperty('activeUntil') then
    Result.ActiveUntil := TWebRESTClient.IsoToDateTime(string(o['activeUntil']));
  if o.hasOwnProperty('createdAt') then
    Result.CreatedAt := TWebRESTClient.IsoToDateTime(string(o['createdAt']));
  if o.hasOwnProperty('updatedAt') then
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime(string(o['updatedAt']));
end;

end.
