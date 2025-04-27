unit Model.RefreshToken;

interface

uses
  JS, Web, WEBLib.REST;

type

  TRefreshToken = record
    Id: string;
    Token: string;
    UserId: string;
    ExpiresAt: TDateTime;
    CreatedAt: TDateTime;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TRefreshToken; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean)
      : TRefreshToken; overload; static;
  end;

implementation

class function TRefreshToken.FromJSON(const AJson: string;
  ADateTimeIsUTC: Boolean): TRefreshToken;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TRefreshToken.FromJSON(const AJsonObj: TJSObject;
  ADateTimeIsUTC: Boolean): TRefreshToken;
begin
  Result := Default (TRefreshToken);
  if AJsonObj.hasOwnProperty('id') then
    Result.Id := JS.toString(AJsonObj['id']);
  if AJsonObj.hasOwnProperty('token') then
    Result.Token := JS.toString(AJsonObj['token']);
  if AJsonObj.hasOwnProperty('userId') then
    Result.UserId := JS.toString(AJsonObj['userId']);
  if AJsonObj.hasOwnProperty('expiresAt') then
    Result.ExpiresAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['expiresAt']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('createdAt') then
    Result.CreatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
end;

end.
