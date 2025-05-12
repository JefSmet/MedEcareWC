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
    class function ToObject(const AJsonObj: TJSObject; ADateTimeIsUTC: Boolean)
      : TPerson; overload; static;
    class function ToList(const AJson: string; ADateTimeIsUTC: Boolean)
      : TList<TPerson>; static;
  end;

implementation

const
  PERSONS_JSON: string = '[' +
    '{"id":"c1335314-abde-47e9-a6e3-1e1d120d7b04","firstName":"Isabeau","lastName":"Verbelen","dateOfBirth":"1990-07-04T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"},'
    + '{"id":"05b2122d-009f-4b8f-b010-2c8e84f2651b","firstName":"Annemie","lastName":"Van Ingelgem","dateOfBirth":"1972-08-17T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"},'
    + '{"id":"414756fa-f4a9-4908-b8ab-42982697585e","firstName":"Filip","lastName":"Smet","dateOfBirth":"1969-04-08T00:00:00.000Z","createdAt":"2025-04-28T13:58:32.696Z","updatedAt":"2025-04-28T13:58:32.696Z"},'
    + '{"id":"37cfacdb-dd29-4125-bcd8-4e8f519d433e","firstName":"Bert","lastName":"Peeters","dateOfBirth":"1990-10-11T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"},'
    + '{"id":"d7fc6fd4-093a-46a6-94d2-520497754145","firstName":"Tania","lastName":"Decoster","dateOfBirth":"1968-02-15T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"},'
    + '{"id":"d722ab12-7d5b-4a12-989b-7dc23046822f","firstName":"Koen","lastName":"Hezemans","dateOfBirth":"1990-06-19T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"},'
    + '{"id":"c9e423e0-ce00-4cfa-8dca-836010be0eea","firstName":"Nathan","lastName":"Van Hoeck","dateOfBirth":"1991-06-20T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"},'
    + '{"id":"c0255e85-86fa-478f-816e-c50c364d367f","firstName":"Daphnée","lastName":"Demaeght","dateOfBirth":"1988-05-17T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"},'
    + '{"id":"234a95ac-713d-4b84-9702-d68d3e6d6e2b","firstName":"Lennert","lastName":"Poppeliers","dateOfBirth":"1987-02-17T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"},'
    + '{"id":"3be470d8-74b3-4c01-9e4f-eae069f26b0a","firstName":"Jef","lastName":"Smet","dateOfBirth":"2000-08-04T00:00:00.000Z","createdAt":"2025-04-28T13:31:02.392Z","updatedAt":"2025-04-28T13:31:02.392Z"},'
    + '{"id":"6e7431ea-3439-408d-a514-ec9723204771","firstName":"Goswin","lastName":"Onsia","dateOfBirth":"1989-08-03T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"},'
    + '{"id":"c947b64e-e08d-4b0b-8827-f12c09c2469e","firstName":"Evi","lastName":"Van Den Kerckhove","dateOfBirth":"1987-04-27T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"},'
    + '{"id":"2dd622e3-8dc0-4252-82d9-f39b5f93d0c2","firstName":"Mark","lastName":"Timmermans","dateOfBirth":"1972-04-25T00:00:00.000Z","createdAt":"2025-04-28T20:18:07.040Z","updatedAt":"2025-04-28T20:18:07.040Z"}'
    + ']';

class function TPerson.ToObject(const AJson: string;
  ADateTimeIsUTC: Boolean): TPerson;
begin
  Result := ToObject(TJSJSON.parseObject(AJson), ADateTimeIsUTC);
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
      jsonArr := TJSONArray(JS.Parse(PERSONS_JSON));
      for I := 0 to jsonArr.Count - 1 do
      begin
        jsonObject := TJSONObject(jsonArr.Items[I]);
        Person := Default (TPerson);
        Person.Id := jsonObject.GetJSONValue('id');
        Person.FirstName := jsonObject.GetJSONValue('firstName');
        Person.LastName := jsonObject.GetJSONValue('lastName');
        Person.DateOfBirth := TWebRESTClient.IsoToDateTime
          (jsonObject.GetJSONValue('dateOfBirth'), ADateTimeIsUTC);
        Person.CreatedAt := TWebRESTClient.IsoToDateTime
          (jsonObject.GetJSONValue('createdAt'), ADateTimeIsUTC);
        Person.UpdatedAt := TWebRESTClient.IsoToDateTime
          (jsonObject.GetJSONValue('updatedAt'), ADateTimeIsUTC);
        Result.Add(Person);
      end;
    finally
      JS.Free;
    end;
  except
    Result.Free;
  end;
end;

class function TPerson.ToObject(const AJsonObj: TJSObject;
  ADateTimeIsUTC: Boolean): TPerson;
begin
  Result := Default (TPerson);
  if AJsonObj.hasOwnProperty('id') then
  begin
    Result.Id := JS.toString(AJsonObj['id']);
  end;
  if AJsonObj.hasOwnProperty('firstName') then
  begin
    Result.FirstName := JS.toString(AJsonObj['firstName']);
  end;
  if AJsonObj.hasOwnProperty('lastName') then
  begin
    Result.LastName := JS.toString(AJsonObj['lastName']);
  end;
  if AJsonObj.hasOwnProperty('dateOfBirth') then
  begin
    Result.DateOfBirth := TWebRESTClient.IsoToDateTime
      (JS.toString(AJsonObj['dateOfBirth']), ADateTimeIsUTC);
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
end;

end.
