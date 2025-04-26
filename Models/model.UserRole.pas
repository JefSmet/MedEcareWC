unit Model.UserRole;

interface

uses
  JS, Web, WEBLib.REST;

type
  TUserRole = record
    UserId : string;
    RoleId : string;
    class function FromJSON(const AJson: string): TUserRole; static;
  end;

implementation

class function TUserRole.FromJSON(const AJson: string): TUserRole;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(AJson);

  if o.hasOwnProperty('userId') then Result.UserId := string(o['userId']);
  if o.hasOwnProperty('roleId') then Result.RoleId := string(o['roleId']);
end;

end.
