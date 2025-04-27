unit Model.User;

interface

uses
  JS, Web, WEBLib.REST;

type

  TUser = record
    Id: string;
    Email: string;
    Password: string;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;
    ResetToken: string;
    ResetExpire: TDateTime;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TUser; overload; static;
    class function FromJSON(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean)
      : TUser; overload; static;
  end;

implementation

class function TUser.FromJSON(const AJson: string;
  ADateTimeIsUTC: Boolean): TUser;
begin
  Result := FromJSON(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
end;

class function TUser.FromJSON(const AJsonObj: TJSObject;
  ADateTimeIsUTC: Boolean): TUser;
begin
  Result := Default (TUser);
  if AJsonObj.hasOwnProperty('id') then
    Result.Id := JS.toString(AJsonObj['id']);
  if AJsonObj.hasOwnProperty('email') then
    Result.Email := JS.toString(AJsonObj['email']);
  if AJsonObj.hasOwnProperty('password') then
    Result.Password := JS.toString(AJsonObj['password']);
  if AJsonObj.hasOwnProperty('createdAt') then
    Result.CreatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('updatedAt') then
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['updatedAt']), ADateTimeIsUTC);
  if AJsonObj.hasOwnProperty('resetToken') then
    Result.ResetToken := JS.toString(AJsonObj['resetToken']);
  if AJsonObj.hasOwnProperty('resetExpire') then
    Result.ResetExpire := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['resetExpire']), ADateTimeIsUTC);
end;

end.
