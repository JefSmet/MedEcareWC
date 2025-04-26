unit Model.ShiftTypeRate;

interface

uses
  JS, Web, WEBLib.REST;

type
  TShiftTypeRate = record
    Id          : string;
    ShiftTypeId : string;
    Rate        : Double;
    ValidFrom   : TDateTime;
    ValidUntil  : TDateTime;
    CreatedAt   : TDateTime;
    UpdatedAt   : TDateTime;
    class function FromJSON(const AJson: string): TShiftTypeRate; static;
  end;

implementation

class function TShiftTypeRate.FromJSON(const AJson: string): TShiftTypeRate;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(AJson);

  if o.hasOwnProperty('id')           then Result.Id          := string(o['id']);
  if o.hasOwnProperty('shiftTypeId')  then Result.ShiftTypeId := string(o['shiftTypeId']);
  if o.hasOwnProperty('rate')         then Result.Rate        := Double(o['rate']);
  if o.hasOwnProperty('validFrom')    then Result.ValidFrom   := TWebRESTClient.IsoToDateTime(string(o['validFrom']));
  if o.hasOwnProperty('validUntil')   then Result.ValidUntil  := TWebRESTClient.IsoToDateTime(string(o['validUntil']));
  if o.hasOwnProperty('createdAt')    then Result.CreatedAt   := TWebRESTClient.IsoToDateTime(string(o['createdAt']));
  if o.hasOwnProperty('updatedAt')    then Result.UpdatedAt   := TWebRESTClient.IsoToDateTime(string(o['updatedAt']));
end;

end.
