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

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TPerson; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TPerson; overload; static;
  end;

implementation

class function TPerson.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TPerson;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TPerson.FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TPerson;
begin
  Result := Default (TPerson);
  if AJsonObj.hasOwnProperty('id') then
  begin
    Result.Id := JS.toString(AJsonObj['id']);
  end;
  if AJsonObj.hasOwnProperty('firstName') then
  begin
    Result.FirstName := JS.toString(AJsonObj['firstName']);
  end;
  if AJsonObj.hasOwnProperty('lastName') then
  begin
    Result.LastName := JS.toString(AJsonObj['lastName']);
  end;
  if AJsonObj.hasOwnProperty('dateOfBirth') then
  begin
    Result.DateOfBirth := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['dateOfBirth']), ADateTimeIsUTC);
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
