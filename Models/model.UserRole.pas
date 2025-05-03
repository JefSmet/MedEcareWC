unit Model.UserRole;

interface

uses
  JS, Web, WEBLib.REST;

type

  TUserRole = record
    UserId: string;
    RoleId: string;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TUserRole; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TUserRole; overload; static;
  end;

implementation

class function TUserRole.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TUserRole;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TUserRole.FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TUserRole;
begin
  Result := Default (TUserRole);
  if AJsonObj.hasOwnProperty('userId') then
  begin
    Result.UserId := JS.toString(AJsonObj['userId']);
  end;
  if AJsonObj.hasOwnProperty('roleId') then
  begin
    Result.RoleId := JS.toString(AJsonObj['roleId']);
  end;
end;

end.
