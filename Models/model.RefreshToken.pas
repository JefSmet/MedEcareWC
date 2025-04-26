unit Model.RefreshToken;

interface

uses
  JS, Web;

type
  TRefreshToken = record
    Id: string;
    Token: string;
    UserId: string;
    ExpiresAt: string;
    CreatedAt: string;
    class function FromJSON(const S: string): TRefreshToken; static;
  end;

implementation

class function TRefreshToken.FromJSON(const S: string): TRefreshToken;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(S);
  if o.hasOwnProperty('id') then Result.Id := string(o['id']);
  if o.hasOwnProperty('token') then Result.Token := string(o['token']);
  if o.hasOwnProperty('userId') then Result.UserId := string(o['userId']);
  if o.hasOwnProperty('expiresAt') then Result.ExpiresAt := string(o['expiresAt']);
  if o.hasOwnProperty('createdAt') then Result.CreatedAt := string(o['createdAt']);
end;

end.
