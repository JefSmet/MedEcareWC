unit orm.Person;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON;

type

  TPerson = record
    Id: string;
    FirstName: string;
    LastName: string;
    DateOfBirth: TDateTime;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;

    class function ToObject(const AJson: string; ADateTimeIsUTC: Boolean)
      : TPerson; overload; static;
    class function ToObject(const AJsonObj: TJSONObject;
      ADateTimeIsUTC: Boolean): TPerson; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean)
      : TList<TPerson>; static;
  end;

implementation

class function TPerson.ToObject(const AJson: string;
  ADateTimeIsUTC: Boolean): TPerson;
var
  JS: TJSON;
  jsonObject: TJSONObject;
begin
  JS := TJSON.Create;
  try
    jsonObject := TJSONObject(JS.Parse(AJson));
    Result := ToObject(jsonObject, ADateTimeIsUTC);
  finally
    JS.Free;
  end;
end;

class function TPerson.ToList(const AJson: string; ADateTimeIsUTC: Boolean)
  : TList<TPerson>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  Person: TPerson;
  I: Integer;
begin
  Result := TList<TPerson>.Create();
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        Person := ToObject(jsonObject, ADateTimeIsUTC);
        Result.Add(Person);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

class function TPerson.ToObject(const AJsonObj: TJSONObject;
  ADateTimeIsUTC: Boolean): TPerson;
begin
  Result := Default (TPerson);
  Result.Id := AJsonObj.GetJSONValue('id');
  Result.FirstName := AJsonObj.GetJSONValue('firstName');
  Result.LastName := AJsonObj.GetJSONValue('lastName');
  Result.DateOfBirth := TWebRESTClient.IsoToDateTime
    (AJsonObj.GetJSONValue('dateOfBirth'), ADateTimeIsUTC);
  Result.CreatedAt := TWebRESTClient.IsoToDateTime
    (AJsonObj.GetJSONValue('createdAt'), ADateTimeIsUTC);
  Result.UpdatedAt := TWebRESTClient.IsoToDateTime
    (AJsonObj.GetJSONValue('updatedAt'), ADateTimeIsUTC);
end;

end.
