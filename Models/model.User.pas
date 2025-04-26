unit Model.User;

interface

uses
  JS, Web;

type
  TUser = record
    Id: string;
    Email: string;
    Password: string;
    CreatedAt: string;
    UpdatedAt: string;
    ResetToken: string;
    ResetExpire: string;
    class function FromJSON(const S: string): TUser; static;
  end;

implementation

class function TUser.FromJSON(const S: string): TUser;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(S);
  if o.hasOwnProperty('id') then Result.Id := string(o['id']);
  if o.hasOwnProperty('email') then Result.Email := string(o['email']);
  if o.hasOwnProperty('password') then Result.Password := string(o['password']);
  if o.hasOwnProperty('createdAt') then Result.CreatedAt := string(o['createdAt']);
  if o.hasOwnProperty('updatedAt') then Result.UpdatedAt := string(o['updatedAt']);
  if o.hasOwnProperty('resetToken') then Result.ResetToken := string(o['resetToken']);
  if o.hasOwnProperty('resetExpire') then Result.ResetExpire := string(o['resetExpire']);
end;

end.
