unit middleware.httponly;

interface

uses WebLib.REST, System.SysUtils, JS, Web;

function PerformRequestWithCredentials(ARequest: TWebHttpRequest): TJSPromise;
function PerformHttpOnly(ARequest: TWebHttpRequest): TJSPromise;

implementation

function HttpCommandToString(ACommand: THTTPCommand;
  const ACustom: string): string;
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

function IsSuccessStatus(AStatus: Integer): Boolean;
begin
  // Succes als 2xx of 304
  Result := ((AStatus >= 200) and (AStatus < 300)) or (AStatus = 304);
end;

function PerformHttpOnly(ARequest: TWebHttpRequest): TJSPromise;
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

  // 3) open(...) met de URL (asynchroon = True)
  req.open(method, ARequest.URL, True);

  // 4) Zorg dat HttpOnly cookies worden meegestuurd
  req.withCredentials := True;

  // 5) (Optioneel) Stel responseType in
  case ARequest.ResponseType of
    rtDefault:     req.ResponseType := '';
    rtText:        req.ResponseType := 'text';
    rtBlob:        req.ResponseType := 'blob';
    rtJSON:        req.ResponseType := 'json';
    rtDocument:    req.ResponseType := 'document';
    rtArrayBuffer: req.ResponseType := 'arraybuffer';
  end;

  // 6) Headers overnemen van TWebHttpRequest
  for i := 0 to ARequest.Headers.Count - 1 do
  begin
    headerKey := ARequest.Headers.Names[i];
    headerValue := ARequest.Headers.ValueFromIndex[i];
    req.setRequestHeader(headerKey, headerValue);
  end;

  // 7) Promise maken die “afgaat” bij readyState = DONE (4)
  Result := TJSPromise.New(
    procedure(Resolve, Reject: TJSPromiseResolver)
    begin
      // A) Luister naar onreadystatechange
      req.onreadystatechange := procedure
        begin
          // Klaar?
          if req.readyState = TJSXMLHttpRequest.DONE then
          begin
            // Als je hier niks krijgt, is de request (bijna altijd) mislukt
            // of (bij file://) status=0. Bij echte HTTP-verzoeken betekent status=0 vaak netwerkfout.

            // Succes?
            if IsSuccessStatus(req.Status) then
              Resolve(req)
            else
              // Geef ook de status en statusText mee
              Reject(
                req
//                Exception.CreateFmt(
//                  '{"status": %d,"message": "%s"}',
//                  [req.Status, req.StatusText]
//                )
              );
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

function PerformRequestWithCredentials(ARequest: TWebHttpRequest): TJSPromise;
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
  req.withCredentials := true;

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
              //Reject(req);
              Reject(Exception.CreateFmt('status: %d, message %s',
                [req.Status, req.StatusText]));
          end;
        end;

      // B) Verstuur bij POST/PUT de PostData, anders geen body
      if (method = 'GET') or (method = 'DELETE') or (ARequest.PostData.Trim = '')
      then
        req.send
      else
        req.send(ARequest.PostData);
    end);
end;

end.
