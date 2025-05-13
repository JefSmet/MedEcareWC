unit model.MedEcareDB;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.REST, orm.Activity, System.Generics.Collections;

type
  TMedEcareDB = class(TWebDataModule)
    reqGetActivities: TWebHttpRequest;
  private
    { Private declarations }
  public
    { Public declarations }
    [async]
    function getActivities(const AType: string; const AYear, AMonth: word; AList: TList<TActivity>): Boolean;
    function Test: string;
  end;

var
  MedEcareDB: TMedEcareDB;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses middleware.httponly;
{$R *.dfm}

const
  baseUrl = 'http://localhost:3000';

  { TMedEcareDB }

function TMedEcareDB.getActivities(const AType: string; const AYear, AMonth: word; AList: TList<TActivity>): Boolean;
var
  endpoint: string;
  xhr: TJSXMLHttpRequest;
  list: TList<TActivity>;
begin
  endpoint := Format('/admin/activities/filter?year=%d&month=%d&activityType=%s', [AYear, AMonth, AType]);
  reqGetActivities.URL := baseUrl + endpoint;
  try
    xhr := await(TJSXMLHttpRequest, PerformRequestWithCredentials(reqGetActivities));
    if xhr.Status = 200 then
    begin
      list := TActivity.ToList(TJSJSON.stringify(xhr.response), true);
      try
        AList.Clear;
        AList.AddRange(list);
        Result := true;
      finally
        list.Free;
      end;
    end;
  except
    exit(false);
  end;

end;

function TMedEcareDB.Test: string;
begin
  Result := 'medecaredb';
end;

end.
