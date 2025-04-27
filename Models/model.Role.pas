unit Model.Role;

interface

uses
  JS, Web, WEBLib.REST;

type

  TRole = record
    Id: string;
    Name: string;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TRole; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TRole; overload; static;
  end;

implementation

class function TRole.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TRole;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TRole.FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TRole;
begin
  Result := Default (TRole);
  if AJsonObj.hasOwnProperty('id') then
  begin
    Result.Id := JS.toString(AJsonObj['id']);
  end;
  if AJsonObj.hasOwnProperty('name') then
  begin
    Result.Name := JS.toString(AJsonObj['name']);
  end;
end;

end.
