unit Forms.login;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, Vcl.Controls,
  WEBLib.REST;

type
  TFormLogin = class(TWebForm)
    loginPassword: TWebEdit;
    rememberMe: TWebCheckBox;
    submitLogin: TWebButton;
    loginEmail: TWebEdit;
    RequestLogin: TWebHttpRequest;
    forgotPassword: TWebButton;
    procedure submitLoginClick(Sender: TObject);
    procedure RequestLoginRequestResponse(Sender: TObject;
      ARequest: TJSXMLHttpRequestRecord; AResponse: string);
    procedure forgotPasswordClick(Sender: TObject);
  private
    { Private declarations }
    femail : string;
  public
    { Public declarations }
  end;

var
  FormLogin: TFormLogin;

implementation

{$R *.dfm}

procedure TFormLogin.forgotPasswordClick(Sender: TObject);
begin
//
end;

procedure TFormLogin.RequestLoginRequestResponse(Sender: TObject;
  ARequest: TJSXMLHttpRequestRecord; AResponse: string);
begin
  if ARequest.req.Status = 200 then
    ShowMessage('Login gelukt: ' + AResponse)
  else
    ShowMessage('Login mislukt (' + IntToStr(ARequest.req.Status) + '): ' + AResponse);
end;


procedure TFormLogin.submitLoginClick(Sender: TObject);
var
  password, email, jsonBody: string;
begin
  email := loginEmail.Text;
  password := loginPassword.Text;

  jsonBody := Format('{"email":"%s","password":"%s","platform":"web"}', [email, password]);

  // voorbeeld van gebruik:
  requestLogin.URL := 'http://localhost:3000/auth/login';
  requestLogin.Command := THTTPCommand.httpPOST;
  requestLogin.Headers.Clear;
  requestLogin.Headers.Add('Content-Type=application/json');
  requestLogin.PostData := jsonBody;
  requestLogin.Execute(procedure(AResponse: string; ARequest: TJSXMLHttpRequest)
  begin
  femail:= aresponse;
    if ARequest.Status = 200 then
    ShowMessage('Login gelukt: ' + AResponse)
  else
    ShowMessage('Login mislukt (' + IntToStr(ARequest.Status) + '): ' + AResponse);
  end
);

end;


end.
