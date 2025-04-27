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

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TRefreshToken; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TRefreshToken; overload; static;
  end;

implementation

class function TRefreshToken.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TRefreshToken;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TRefreshToken.FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean): TRefreshToken;
begin
  Result := Default (TRefreshToken);
  if AJsonObj.hasOwnProperty('id') then
  begin
    Result.Id := JS.toString(AJsonObj['id']);
  end;
  if AJsonObj.hasOwnProperty('token') then
  begin
    Result.Token := JS.toString(AJsonObj['token']);
  end;
  if AJsonObj.hasOwnProperty('userId') then
  begin
    Result.UserId := JS.toString(AJsonObj['userId']);
  end;
  if AJsonObj.hasOwnProperty('expiresAt') then
  begin
    Result.ExpiresAt := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['expiresAt']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('createdAt') then
  begin
    Result.CreatedAt := TWebRESTClient.IsoToDateTime(JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
  end;
end;

end.
