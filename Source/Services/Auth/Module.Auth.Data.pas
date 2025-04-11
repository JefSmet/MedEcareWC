unit Module.Auth.Data;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.Storage;

type
TAuthStatus = (asLoggedOut, asLoggedIn, asError);

  TAuthData = class(TWebDataModule)
    WebLocalStorage: TWebLocalStorage;
    WebSessionStorage: TWebSessionStorage;
  private
    FToken: string;
    FUserEmail: string;
    FAuthStatus: TAuthStatus;
    FOnStatusChanged: TNotifyEvent;
    procedure SetAuthStatus(const Value: TAuthStatus);
  public
    function Login(const Email, Password: string; Remember: Boolean): Boolean;
    procedure Logout;
    function IsLoggedIn: Boolean;
    function GetSavedEmail: string;
    procedure SaveLoginData(const Email: string; Remember: Boolean);
    property Token: string read FToken;
    property UserEmail: string read FUserEmail;
    property AuthStatus: TAuthStatus read FAuthStatus write SetAuthStatus;
    property OnStatusChanged: TNotifyEvent read FOnStatusChanged write FOnStatusChanged;
  end;

var
  AuthData: TAuthData;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function TAuthData.GetSavedEmail: string;
begin
  Result := WebLocalStorage.Values['saved_email'];
end;

function TAuthData.IsLoggedIn: Boolean;
begin
  Result := (WebSessionStorage.Values['auth_token'] <> '') and (AuthStatus = asLoggedIn);
end;

function TAuthData.Login(const Email, Password: string; Remember: Boolean): Boolean;
begin
  // Dit wordt later door de presenter aangeroepen met de API response
  Result := False;
end;

procedure TAuthData.Logout;
begin
  WebSessionStorage.Values['auth_token'] := '';
  WebSessionStorage.Values['user_email'] := '';
  FToken := '';
  FUserEmail := '';
  AuthStatus := asLoggedOut;
end;

procedure TAuthData.SaveLoginData(const Email: string; Remember: Boolean);
begin
  if Remember then
  begin
    WebLocalStorage.Values['remember_me'] := 'true';
    WebLocalStorage.Values['saved_email'] := Email;
  end
  else
  begin
    WebLocalStorage.Values['remember_me'] := '';
    WebLocalStorage.Values['saved_email'] := '';
  end;
end;

procedure TAuthData.SetAuthStatus(const Value: TAuthStatus);
begin
  if FAuthStatus <> Value then
  begin
    FAuthStatus := Value;
    if Assigned(FOnStatusChanged) then
      FOnStatusChanged(Self);
  end;
end;

end.
