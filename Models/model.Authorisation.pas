unit model.Authorisation;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.Storage, WEBLib.REST;

type
  TAuthorisation = class(TWebDataModule)
    WebSessionStorage1: TWebSessionStorage;
    WebLocalStorage1: TWebLocalStorage;
    WebHttpRequest1: TWebHttpRequest;
  private
    function PerformRequestWithCredentials(ARequest: TWebHttpRequest) : TJSPromise;
  public
    [async] procedure DoLogin;
    function ListUsers : string;
  end;

var
  Authorisation: TAuthorisation;

implementation

uses
  WEBLib.WebTools, vcl.dialogs;

function HttpCommandToString(ACommand: THTTPCommand; const ACustom: string): string;
begin
  case ACommand of
    httpGET:     Result := 'GET';
    httpPOST:    Result := 'POST';
    httpPUT:     Result := 'PUT';
    httpDELETE:  Result := 'DELETE';
    httpPATCH:   Result := 'PATCH';
    httpHEAD:    Result := 'HEAD';
    httpCustom:  if ACustom.Trim <> '' then
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



{ TAuthorisation }

procedure TAuthorisation.DoLogin;
var
    req: TJSXMLHttpRequest;
begin
WebHttpRequest1.URL := WebHttpRequest1.URL + '/auth/login';
WebHttpRequest1.Command := THTTPCommand.httpPOST;
WebHttpRequest1.PostData := '{'+
  '"email": "jef.smet@telenet.be",'+
  '"password": "0QxpWjS2dvsa.", '+
  '"platform": "web" '+
'}'  ;
req := await (TJSXMLHttpRequest , PerformRequestWithCredentials(WebHttpRequest1));
ShowMessage(string (req.response));
end;

function TAuthorisation.ListUsers: string;
begin

end;

function TAuthorisation.PerformRequestWithCredentials(
  ARequest: TWebHttpRequest): TJSPromise;
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
  req.open(method, ARequest.URL);

  // 4) Zorg dat HttpOnly cookies worden meegestuurd
  req.withCredentials := True;

  // 5) (Optioneel) Stel responseType in
  //    THTTPRequestResponseType = (rtDefault, rtText, rtBlob, rtJSON, rtDocument, rtArrayBuffer);
  case ARequest.ResponseType of
    rtDefault:    req.responseType := '';
    rtText:       req.responseType := 'text';
    rtBlob:       req.responseType := 'blob';
    rtJSON:       req.responseType := 'json';
    rtDocument:   req.responseType := 'document';
    rtArrayBuffer:req.responseType := 'arraybuffer';
  end;

  // 6) Headers overnemen van TWebHttpRequest
  for i := 0 to ARequest.Headers.Count - 1 do
  begin
    headerKey := ARequest.Headers.Names[i];
    headerValue := ARequest.Headers.ValueFromIndex[i];
    req.setRequestHeader(headerKey, headerValue);
  end;

  // 7) Promise maken die “afgaat” bij readyState = DONE (4)
  //    en dan op basis van req.Status wel/niet ‘resolve’
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
            Resolve(req)  // geef xhr object terug
          else
            Reject('HTTP Error: ' + IntToStr(req.Status) + ' ' + req.StatusText);
        end;
      end;

      // B) Verstuur bij POST/PUT de PostData, anders geen body
      if (method = 'GET') or (method = 'DELETE') or (ARequest.PostData.Trim = '') then
        req.send
      else
        req.send(ARequest.PostData);
    end
  );
end;



end.
