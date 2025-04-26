unit Model.Role;

interface

uses
  JS, Web;

type
  TRole = record
    Id: string;
    Name: string;
    class function FromJSON(const S: string): TRole; static;
  end;

implementation

class function TRole.FromJSON(const S: string): TRole;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(S);
  if o.hasOwnProperty('id') then Result.Id := string(o['id']);
  if o.hasOwnProperty('name') then Result.Name := string(o['name']);
end;

end.
