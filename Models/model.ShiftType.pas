unit Model.ShiftType;

interface

uses
  JS, Web;

type
  TShiftType = record
    Id: string;
    Name: string;
    StartHour: Integer;
    StartMinute: Integer;
    DurationMinutes: Integer;
    ActiveFrom: string;
    ActiveUntil: string;
    CreatedAt: string;
    UpdatedAt: string;
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
  if o.hasOwnProperty('activeFrom') then Result.ActiveFrom := string(o['activeFrom']);
  if o.hasOwnProperty('activeUntil') then Result.ActiveUntil := string(o['activeUntil']);
  if o.hasOwnProperty('createdAt') then Result.CreatedAt := string(o['createdAt']);
  if o.hasOwnProperty('updatedAt') then Result.UpdatedAt := string(o['updatedAt']);
end;

end.
