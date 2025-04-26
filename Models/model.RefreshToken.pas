unit Model.RefreshToken;

interface

uses
  JS, Web, WEBLib.REST;

type
  TRefreshToken = record
    Id        : string;
    Token     : string;
    UserId    : string;
    ExpiresAt : TDateTime;
    CreatedAt : TDateTime;
    class function FromJSON(const AJson: string): TRefreshToken; static;
  end;

implementation

class function TRefreshToken.FromJSON(const AJson: string): TRefreshToken;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(AJson);

  if o.hasOwnProperty('id')        then Result.Id        := string(o['id']);
  if o.hasOwnProperty('token')     then Result.Token     := string(o['token']);
  if o.hasOwnProperty('userId')    then Result.UserId    := string(o['userId']);
  if o.hasOwnProperty('expiresAt') then Result.ExpiresAt := TWebRESTClient.IsoToDateTime(string(o['expiresAt']));
  if o.hasOwnProperty('createdAt') then Result.CreatedAt := TWebRESTClient.IsoToDateTime(string(o['createdAt']));
end;

end.
