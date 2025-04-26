unit Model.ShiftTypeRate;

interface

uses
  JS, Web;

type
  TShiftTypeRate = record
    Id: string;
    ShiftTypeId: string;
    Rate: Double;
    ValidFrom: string;
    ValidUntil: string;
    CreatedAt: string;
    UpdatedAt: string;
    class function FromJSON(const S: string): TShiftTypeRate; static;
  end;

implementation

class function TShiftTypeRate.FromJSON(const S: string): TShiftTypeRate;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(S);
  if o.hasOwnProperty('id') then Result.Id := string(o['id']);
  if o.hasOwnProperty('shiftTypeId') then Result.ShiftTypeId := string(o['shiftTypeId']);
  if o.hasOwnProperty('rate') then Result.Rate := Double(o['rate']);
  if o.hasOwnProperty('validFrom') then Result.ValidFrom := string(o['validFrom']);
  if o.hasOwnProperty('validUntil') then Result.ValidUntil := string(o['validUntil']);
  if o.hasOwnProperty('createdAt') then Result.CreatedAt := string(o['createdAt']);
  if o.hasOwnProperty('updatedAt') then Result.UpdatedAt := string(o['updatedAt']);
end;

end.
