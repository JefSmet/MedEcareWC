unit Model.Person;

interface

uses
  JS, Web, WEBLib.REST;

type
  TPerson = record
    Id          : string;
    FirstName   : string;
    LastName    : string;
    DateOfBirth : TDateTime;
    CreatedAt   : TDateTime;
    UpdatedAt   : TDateTime;
    class function FromJSON(const S: string): TPerson; static;
  end;

implementation

class function TPerson.FromJSON(const S: string): TPerson;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(S);
  if o.hasOwnProperty('id') then Result.Id := string(o['id']);
  if o.hasOwnProperty('firstName') then Result.FirstName := string(o['firstName']);
  if o.hasOwnProperty('lastName') then Result.LastName := string(o['lastName']);
  if o.hasOwnProperty('dateOfBirth') then
    Result.DateOfBirth := TWebRESTClient.IsoToDateTime(string(o['dateOfBirth']));
  if o.hasOwnProperty('createdAt') then
    Result.CreatedAt := TWebRESTClient.IsoToDateTime(string(o['createdAt']));
  if o.hasOwnProperty('updatedAt') then
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime(string(o['updatedAt']));
end;

end.
