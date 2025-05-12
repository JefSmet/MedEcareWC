unit orm.RefreshToken;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON;

type

  TRefreshToken = record
    Id: string;
    Token: string;
    UserId: string;
    ExpiresAt: TDateTime;
    CreatedAt: TDateTime;

    class function FromJSON(const AJson: string; ADateTimeIsUTC: Boolean)
      : TRefreshToken; overload; static;
    class function FromJSON(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean)
      : TRefreshToken; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TRefreshToken>; static;
  end;

implementation

class function TRefreshToken.FromJSON(const AJson: string; ADateTimeIsUTC: Boolean): TRefreshToken;
var
  JS: TJSON;
  jsonObject: TJSONObject;
begin
  JS := TJSON.Create;
  try
    jsonObject := TJSONObject(JS.Parse(AJson));
    Result := FromJSON(jsonObject, ADateTimeIsUTC);
  finally
    JS.Free;
  end;
end;

class function TRefreshToken.FromJSON(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean): TRefreshToken;
begin
  Result := Default(TRefreshToken);
  Result.Id := AJsonObj.GetJSONValue('id');
  Result.Token := AJsonObj.GetJSONValue('token');
  Result.UserId := AJsonObj.GetJSONValue('userId');
  Result.ExpiresAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('expiresAt'), ADateTimeIsUTC);
  Result.CreatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('createdAt'), ADateTimeIsUTC);
end;

class function TRefreshToken.ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TRefreshToken>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  Token: TRefreshToken;
  I: Integer;
begin
  Result := TList<TRefreshToken>.Create;
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        Token := TRefreshToken.FromJSON(jsonObject, ADateTimeIsUTC);
        Result.Add(Token);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

end.
