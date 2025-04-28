unit model.Authorisation;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.Storage,
  WEBLib.REST, jsdelphisystem, WEBLib.JSON;

type
  TAuthorisation = class(TWebDataModule)
    WebSessionStorage1: TWebSessionStorage;
    WebLocalStorage1: TWebLocalStorage;
    WebHttpRequest1: TWebHttpRequest;
  private
    function PerformRequestWithCredentials(ARequest: TWebHttpRequest): TJSPromise;
    procedure setLoginRequest(AEmail: string; APassword: string; APlatform: string);
  public
    [async]
    function DoLogin(AEmail: string; APassword: string; APlatform: string): TJSXMLHttpRequest;
    function ListUsers: string;
    [async]
    function TryAutoLogin: Boolean;
  end;

var
  Authorisation: TAuthorisation;

implementation

uses
  WEBLib.WebTools, vcl.dialogs;

const
  url = 'http://localhost:3000';

function HttpCommandToString(ACommand: THTTPCommand; const ACustom: string): string;
begin
  case ACommand of
    httpGET:
      Result := 'GET';
    httpPOST:
      Result := 'POST';
    httpPUT:
      Result := 'PUT';
    httpDELETE:
      Result := 'DELETE';
    httpPATCH:
      Result := 'PATCH';
    httpHEAD:
      Result := 'HEAD';
    httpCustom:
      if ACustom.Trim <> '' then
        Result := ACustom
      else
        Result := 'GET';
  else
    Result := 'GET'; // fallback
  end;
end;

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}
{ TAuthorisation }

function TAuthorisation.DoLogin(AEmail, APassword, APlatform: string): TJSXMLHttpRequest;
var
  req: TJSXMLHttpRequest;
  jsObj, userObj: TJSObject;
  userID: string;
begin
  // Eventueel je headers of body instellen (implementatie van setRequest)
  setLoginRequest(AEmail, APassword, APlatform);

  // Voer de asynchrone request uit en wacht tot deze klaar is
  req := await(TJSXMLHttpRequest, PerformRequestWithCredentials(WebHttpRequest1));

  // // Omdat ResponseType = rtJSON is, is req.response direct een JS-object
  // jsObj := TJSObject(req.response);
  //
  // // Haal het 'user'-object op binnenin 'jsObj'
  // userObj := TJSObject(jsObj['user']);
  //
  // // Haal de 'id'‐waarde eruit
  // userID := string(userObj['id']);
  //
  // // Sla op in SessionStorage
  // WebSessionStorage1.SetValue('userID', userID);

  // Geef het hele request (JS-object) terug
  Result := req;
end;

function TAuthorisation.ListUsers: string;
begin

end;

function TAuthorisation.PerformRequestWithCredentials(ARequest: TWebHttpRequest): TJSPromise;
var
  req: TJSXMLHttpRequest;
  method: string;
  i: Integer;
  headerKey, headerValue: string;
begin
  // 1) Nieuw XMLHttpRequest-object
  req := TJSXMLHttpRequest.New;

  // 2) Bepaal de HTTP-methode (GET, POST, etc.) uit TWebHttpRequest
  method := HttpCommandToString(ARequest.Command, ARequest.CustomCommand);

  // 3) open(...) met de URL
  req.open(method, ARequest.url);

  // 4) Zorg dat HttpOnly cookies worden meegestuurd
  req.withCredentials := True;

  // 5) (Optioneel) Stel responseType in
  // THTTPRequestResponseType = (rtDefault, rtText, rtBlob, rtJSON, rtDocument, rtArrayBuffer);
  case ARequest.ResponseType of
    rtDefault:
      req.ResponseType := '';
    rtText:
      req.ResponseType := 'text';
    rtBlob:
      req.ResponseType := 'blob';
    rtJSON:
      req.ResponseType := 'json';
    rtDocument:
      req.ResponseType := 'document';
    rtArrayBuffer:
      req.ResponseType := 'arraybuffer';
  end;

  // 6) Headers overnemen van TWebHttpRequest
  for i := 0 to ARequest.Headers.Count - 1 do
  begin
    headerKey := ARequest.Headers.Names[i];
    headerValue := ARequest.Headers.ValueFromIndex[i];
    req.setRequestHeader(headerKey, headerValue);
  end;

  // 7) Promise maken die “afgaat” bij readyState = DONE (4)
  // en dan op basis van req.Status wel/niet ‘resolve’
  Result := TJSPromise.New(
    procedure(Resolve, Reject: TJSPromiseResolver)
    begin
      // A) Luister naar onreadystatechange
      req.onreadystatechange := procedure
        begin
          // DONE?
          if req.readyState = TJSXMLHttpRequest.DONE then
          begin
            // HTTP status 2xx => success
            if (req.Status >= 200) and (req.Status < 300) then
              Resolve(req) // geef xhr object terug
            else
              Reject(Exception.CreateFmt('{"status": %d,"message": "%s"}', [req.Status, req.StatusText]));
          end;
        end;

      // B) Verstuur bij POST/PUT de PostData, anders geen body
      if (method = 'GET') or (method = 'DELETE') or (ARequest.PostData.Trim = '') then
        req.send
      else
        req.send(ARequest.PostData);
    end);
end;

procedure TAuthorisation.setLoginRequest(AEmail: string; APassword: string; APlatform: string);
begin
  WebHttpRequest1.url := url + '/auth/login';
  WebHttpRequest1.Command := THTTPCommand.httpPOST;
  WebHttpRequest1.ResponseType := rtJSON;
  WebHttpRequest1.PostData := Format('{"email": "%s","password": "%s", "platform": "%s"}', [AEmail, APassword, APlatform]);
end;

function TAuthorisation.TryAutoLogin: Boolean;
var
  xhr: TJSXMLHttpRequest;
  jsObj, userObj: TJSObject;
begin
  // 1) call /auth/refresh (refreshToken zit in de HttpOnly cookie)
  WebHttpRequest1.url := url + '/auth/refresh';
  WebHttpRequest1.Command := httpPOST;
  WebHttpRequest1.ResponseType := rtJSON;
  WebHttpRequest1.PostData := '{"platform": "web-persist"}';

  try
    xhr := await(TJSXMLHttpRequest, PerformRequestWithCredentials(WebHttpRequest1));
  except
    exit(False);

  end;

  if xhr.Status = 200 then
  begin
//     jsObj  := TJSObject(xhr.response);
    // userObj:= TJSObject(jsObj['user']);        // ← laat backend evt. user terugsturen
    // WebSessionStorage1.SetValue('userID', string(userObj['id']));
    exit(True);
  end;

  exit(False);
end;

end.
