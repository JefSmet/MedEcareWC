unit Model.Role;

interface

uses
  JS, Web, WEBLib.REST;

type
  TRole = record
    Id   : string;
    Name : string;
    class function FromJSON(const AJson: string): TRole; static;
  end;

implementation

class function TRole.FromJSON(const AJson: string): TRole;
var
  o: TJSObject;
begin
  o := TJSJSON.parseObject(AJson);

  if o.hasOwnProperty('id')   then Result.Id   := string(o['id']);
  if o.hasOwnProperty('name') then Result.Name := string(o['name']);
end;

end.
