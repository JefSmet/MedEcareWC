unit Model.UserConstraint;

interface

uses
  JS, Web;

type
  TUserConstraint = record
    Id: string;
    PersonId: string;
    MaxNightShiftsPerWeek: Integer;
    MaxConsecutiveNightShifts: Integer;
    MinRestHoursBetweenShifts: Integer;
    CreatedAt: string;
    UpdatedAt: string;
    class function FromJSON(const S: string): TUserConstraint; static;
  end;

implementation

class function TUserConstraint.FromJSON(const S: string): TUserConstraint;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(S);
  if o.hasOwnProperty('id') then Result.Id := string(o['id']);
  if o.hasOwnProperty('personId') then Result.PersonId := string(o['personId']);
  if o.hasOwnProperty('maxNightShiftsPerWeek') then Result.MaxNightShiftsPerWeek := Integer(o['maxNightShiftsPerWeek']);
  if o.hasOwnProperty('maxConsecutiveNightShifts') then Result.MaxConsecutiveNightShifts := Integer(o['maxConsecutiveNightShifts']);
  if o.hasOwnProperty('minRestHoursBetweenShifts') then Result.MinRestHoursBetweenShifts := Integer(o['minRestHoursBetweenShifts']);
  if o.hasOwnProperty('createdAt') then Result.CreatedAt := string(o['createdAt']);
  if o.hasOwnProperty('updatedAt') then Result.UpdatedAt := string(o['updatedAt']);
end;

end.
