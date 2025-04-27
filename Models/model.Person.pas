unit Model.Person;

interface

uses
  JS, Web, WEBLib.REST;

type

  TPerson = record
    Id: string;
    FirstName: string;
    LastName: string;
    DateOfBirth: TDateTime;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TPerson; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean)
      : TPerson; overload; static;
  end;

implementation

class function TPerson.FromJSON(const AJson: string;
  ADateTimeIsUTC: Boolean): TPerson;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TPerson.FromJSON(const AJsonObj: TJSObject;
  ADateTimeIsUTC: Boolean): TPerson;
begin
  Result := Default (TPerson);
  if AJsonObj.hasOwnProperty('id') then
    Result.Id := JS.toString(AJsonObj['id']);
  if AJsonObj.hasOwnProperty('firstName') then
    Result.FirstName := JS.toString(AJsonObj['firstName']);
  if AJsonObj.hasOwnProperty('lastName') then
    Result.LastName := JS.toString(AJsonObj['lastName']);
  if AJsonObj.hasOwnProperty('dateOfBirth') then
    Result.DateOfBirth := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['dateOfBirth']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('createdAt') then
    Result.CreatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('updatedAt') then
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['updatedAt']), ADateTimeIsUTC);
end;

end.
