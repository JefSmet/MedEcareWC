unit model.Authorisation;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.Storage,
  WEBLib.REST, jsdelphisystem, WEBLib.JSON, DateUtils, orm.Person,
  System.Generics.Collections;

type
  TAuthorisation = class(TWebDataModule)
    WebSessionStorage1: TWebSessionStorage;
    WebLocalStorage1: TWebLocalStorage;
    WebHttpRequest1: TWebHttpRequest;
    procedure WebDataModuleCreate(Sender: TObject);
    procedure WebDataModuleDestroy(Sender: TObject);
  private
    FcurrentPerson: TPerson;
    FcurrentUserRoles: TList<string>;
    [async]
    procedure SetRequest(AEndpoint: string; ACommand: THTTPCommand;
      APostData: string = ''; AResponsetype: THTTPRequestResponseType = rtJSON);
    procedure SetCurrentUser(AJSValue: TJSValue);

  public
    property currentPerson: TPerson read FcurrentPerson write FcurrentPerson;
    property currentUserRoles: TList<string> read FcurrentUserRoles;
    procedure ClearStorage;
    [async]
    function DoLogin(AEmail: string; APassword: string; APlatform: string)
      : TJSXMLHttpRequest;
    [async]
    function TryAutoLogin: Boolean;
    [async]
    function DoLogout: Boolean;
    [async]
    function forgotPassword(AEmail: string): Boolean;
    [async]
    function ResetPassword(AToken: string; APassword: string): Boolean;
    [async]
    function RegisterNewUser(AEmail, APassword, ARole, AFirstName,
      ALastname: string; ADateOfBirth: TDateTime): Boolean;
  end;

implementation

uses
  WEBLib.WebTools, vcl.dialogs, middleware.httponly;

const
  url = 'http://localhost:3000/auth';

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}
{ TAuthorisation }

procedure TAuthorisation.ClearStorage;
begin
  WebLocalStorage1.Clear;
  WebSessionStorage1.Clear;
end;

function TAuthorisation.DoLogin(AEmail, APassword, APlatform: string)
  : TJSXMLHttpRequest;
var
  req: TJSXMLHttpRequest;
  jsObj, userObj: TJSObject;
  userID: string;
begin
  SetRequest('/login', httpPOST,
    Format('{"email": "%s","password": "%s", "platform": "%s"}',
    [AEmail, APassword, APlatform]));

  // Voer de asynchrone request uit en wacht tot deze klaar is
  req := await(TJSXMLHttpRequest,
    PerformRequestWithCredentials(WebHttpRequest1));

  SetCurrentUser(req.response);

  // Geef het hele request (JS-object) terug
  Result := req;
end;

function TAuthorisation.DoLogout: Boolean;
var
  xhr: TJSXMLHttpRequest;
begin
  SetRequest('/logout', httpPOST);

  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(WebHttpRequest1));
  except
    Exit(false);

  end;

  if xhr.Status = 200 then
  begin
    Exit(true);
  end;

  Exit(false);

end;

procedure TAuthorisation.SetCurrentUser(AJSValue: TJSValue);
var
  jsObj: TJSObject;
  roles: TJSArray;
  i: Integer;
begin
  jsObj := TJSObject(AJSValue);
  jsObj := TJSObject(jsObj['user']);
  FcurrentPerson := default (TPerson);
  FcurrentPerson.FirstName := string(jsObj['firstName']);
  FcurrentPerson.LastName := string(jsObj['lastName']);
  FcurrentPerson.Id := string(jsObj['personId']);
  FcurrentPerson.CreatedAt := now;
  FcurrentPerson.UpdatedAt := now;
  FcurrentUserRoles.Clear;
  roles := toArray(jsObj['roles']);
  for i := 0 to roles.Length - 1 do
  begin
    FcurrentUserRoles.Add(string(roles[i]));
  end;
  showMessage(FcurrentPerson.FirstName + FcurrentUserRoles[0]);
end;

procedure TAuthorisation.SetRequest(AEndpoint: string; ACommand: THTTPCommand;
APostData: string; AResponsetype: THTTPRequestResponseType);
begin
  WebHttpRequest1.url := url + AEndpoint;
  WebHttpRequest1.Command := ACommand;
  WebHttpRequest1.ResponseType := AResponsetype;
  WebHttpRequest1.PostData := APostData;
end;

function TAuthorisation.TryAutoLogin: Boolean;
var
  xhr: TJSXMLHttpRequest;
  jsObj, userObj: TJSObject;
begin
  // 1) call /auth/refresh (refreshToken zit in de HttpOnly cookie)
  SetRequest('/refresh', httpPOST, '{"platform": "web-persist"}');

  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(WebHttpRequest1));
  except
    Exit(false);

  end;

  if xhr.Status = 200 then
  begin
    SetCurrentUser(xhr.response);
    Exit(true);
  end;

  Exit(false);
end;

procedure TAuthorisation.WebDataModuleCreate(Sender: TObject);
begin
  FcurrentUserRoles := TList<string>.Create;
end;

procedure TAuthorisation.WebDataModuleDestroy(Sender: TObject);
begin
  FcurrentUserRoles.Free;
end;

// reset password
function TAuthorisation.forgotPassword(AEmail: string): Boolean;
var
  xhr: TJSXMLHttpRequest;
begin
  SetRequest('/forgot-password', httpPOST, Format('{"email" : "%s"}',
    [AEmail]));
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(WebHttpRequest1));
    Exit(true);
  except
    Exit(false);
  end;

end;

function TAuthorisation.RegisterNewUser(AEmail, APassword, ARole, AFirstName,
  ALastname: string; ADateOfBirth: TDateTime): Boolean;
var
  JsonStr: string;
  xhr: TJSXMLHttpRequest;
  dob: string;
begin
  dob := FormatDateTime('yyyy-mm-dd', ADateOfBirth);
  JsonStr :=
    Format('{  "email": "%s","password": "%s", "role": "%s", "firstName": "%s", "lastName": "%s", "dateOfBirth": "%s"}',
    [AEmail, APassword, ARole, AFirstName, ALastname, dob]);
  SetRequest('/register', httpPOST, JsonStr);
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(WebHttpRequest1));
    Exit(true);
  except
    Exit(false);
  end;

end;

function TAuthorisation.ResetPassword(AToken: string;
APassword: string): Boolean;
var
  PostData: string;
  xhr: TJSXMLHttpRequest;
begin
  PostData := Format('{"token" : "%s", "newPassword" : "%s"}',
    [AToken, APassword]);
  SetRequest('/reset-password', httpPOST, PostData);
  try
    xhr := await(TJSXMLHttpRequest,
      PerformRequestWithCredentials(WebHttpRequest1));
    Exit(true);
  except
    Exit(false);
  end;

end;



// register

end.
