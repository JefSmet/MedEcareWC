unit orm.Doctor;

interface

uses
  JS, Web, WEBLib.REST, System.Generics.Collections, WEBLib.JSON,
  orm.Person;

type
  TDoctor = record
    PersonId: string;
    RizivNumber: string;
    IsEnabledInShifts: Boolean;
    CreatedAt: TDateTime;
    UpdatedAt: TDateTime;

    // Relatie met Person
    Person: TPerson;

    class function ToObject(const AJson: string; ADateTimeIsUTC: Boolean): TDoctor; overload; static;
    class function ToObject(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean): TDoctor; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TDoctor>; static;
  end;

implementation

class function TDoctor.ToObject(const AJson: string; ADateTimeIsUTC: Boolean): TDoctor;
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

class function TDoctor.ToList(const AJson: string; ADateTimeIsUTC: Boolean): TList<TDoctor>;
var
  JS: TJSON;
  jsonArr: TJSONArray;
  jsonObject: TJSONObject;
  Doc: TDoctor;
  I: Integer;
begin
  Result := TList<TDoctor>.Create;
  try
    JS := TJSON.Create;
    try
      jsonArr := TJSONArray(JS.Parse(AJson));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        Doc := ToObject(jsonObject, ADateTimeIsUTC);
        Result.Add(Doc);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TDoctor.ToObject(const AJsonObj: TJSONObject; ADateTimeIsUTC: Boolean): TDoctor;
begin
  Result := Default(TDoctor);
  Result.PersonId := AJsonObj.GetJSONValue('personId');
  Result.RizivNumber := AJsonObj.GetJSONValue('rizivNumber');
  Result.IsEnabledInShifts := AJsonObj.GetJSONValue('isEnabledInShifts') = 'true';
  Result.CreatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('createdAt'), ADateTimeIsUTC);
  Result.UpdatedAt := TWebRESTClient.IsoToDateTime(AJsonObj.GetJSONValue('updatedAt'), ADateTimeIsUTC);

  // Parse embedded Person object if present
  if AJsonObj.GetJSONValue('person') <> '' then
    Result.Person := TPerson.ToObject(TJSONObject(AJsonObj.GetValue('person')), ADateTimeIsUTC);
end;

end.

