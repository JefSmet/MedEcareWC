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
  begin
    Result.Id := JS.toString(AJsonObj['id']);
  end;
  if AJsonObj.hasOwnProperty('email') then
  begin
    Result.Email := JS.toString(AJsonObj['email']);
  end;
  if AJsonObj.hasOwnProperty('password') then
  begin
    Result.Password := JS.toString(AJsonObj['password']);
  end;
  if AJsonObj.hasOwnProperty('createdAt') then
  begin
    Result.CreatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['createdAt']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('updatedAt') then
  begin
    Result.UpdatedAt := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['updatedAt']), ADateTimeIsUTC);
  end;
  if AJsonObj.hasOwnProperty('resetToken') then
  begin
    Result.ResetToken := JS.toString(AJsonObj['resetToken']);
  end;
  if AJsonObj.hasOwnProperty('resetExpire') then
  begin
    Result.ResetExpire := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['resetExpire']), ADateTimeIsUTC);
  end;
end;

end.
