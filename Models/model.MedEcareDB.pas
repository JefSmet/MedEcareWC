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
    function getActivities(AType: string; AYear, AMonth: word): TList<TActivity>;
  end;

var
  MedEcareDB: TMedEcareDB;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses middleware.httponly;
{$R *.dfm}

const
  baseUrl = 'http://localhost:3000/';

{ TMedEcareDB }


function TMedEcareDB.getActivities(AType: string; AYear, AMonth: word): TList<TActivity>;
var endpoint : string;
xhr : TJSXMLHttpRequest;
begin
  endpoint := Format('/admin/activities/type/%s/year/%d/month/%d',[AType,AYear,AMonth]);
  reqGetActivities.URL := baseUrl + endpoint;
  try
  xhr := await(TJSXMLHttpRequest,PerformRequestWithCredentials(reqGetActivities));
  except
  end;
end;

end.
