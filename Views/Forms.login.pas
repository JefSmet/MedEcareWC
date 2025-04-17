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

uses model.AppManager;


procedure TFormLogin.forgotPasswordClick(Sender: TObject);
begin
 TAppManager.GetInstance.Auth.DoLogin;
end;

procedure TFormLogin.RequestLoginRequestResponse(Sender: TObject;
  ARequest: TJSXMLHttpRequestRecord; AResponse: string);
begin
  if ARequest.req.Status = 200 then
    ShowMessage('Login gelukt: ' + AResponse)
  else
    ShowMessage('Login mislukt (' + IntToStr(ARequest.req.Status) + '): ' + AResponse);
end;


//procedure TFormLogin.submitLoginClick(Sender: TObject);
//var
//  password, email, jsonBody: string;
//begin
//  email := loginEmail.Text;
//  password := loginPassword.Text;
//
//  jsonBody := Format('{"email":"%s","password":"%s","platform":"web"}', [email, password]);
//
//  // voorbeeld van gebruik:
//  requestLogin.URL := 'http://localhost:3000/auth/login';
//  requestLogin.Command := THTTPCommand.httpPOST;
//  requestLogin.Headers.Clear;
//  requestLogin.Headers.Add('Content-Type=application/json');
//  requestLogin.PostData := jsonBody;
//  requestLogin.Execute(procedure(AResponse: string; ARequest: TJSXMLHttpRequest)
//  begin
//  femail:= aresponse;
//    if ARequest.Status = 200 then
//    ShowMessage('Login gelukt: ' + AResponse)
//  else
//    ShowMessage('Login mislukt (' + IntToStr(ARequest.Status) + '): ' + AResponse);
//  end
//);
//
//end;

procedure TFormLogin.submitLoginClick(Sender: TObject);
var
  password, email, jsonBody: string;
begin
  email := Trim(loginEmail.Text);
  password := loginPassword.Text;

  // Basisvalidatie
  if email = '' then
  begin
    ShowMessage('Vul een e-mailadres in.');
    Exit;
  end;

  if password = '' then
  begin
    ShowMessage('Vul een wachtwoord in.');
    Exit;
  end;

  // Toon laadanimatie of zet login knop op disabled
  submitLogin.Enabled := False;
  submitLogin.Caption := 'Bezig met inloggen...';

  jsonBody := Format('{"email":"%s","password":"%s","platform":"web","remember":%s}',
    [email, password, LowerCase(BoolToStr(rememberMe.Checked, True))]);

  requestLogin.URL := 'http://localhost:3000/auth/login';
  requestLogin.Command := THTTPCommand.httpPOST;
  requestLogin.Headers.Clear;
  requestLogin.Headers.Add('Content-Type=application/json');
  requestLogin.PostData := jsonBody;
  requestLogin.Execute(procedure(AResponse: string; ARequest: TJSXMLHttpRequest)
  begin
    // Reset login knop
    submitLogin.Enabled := True;
    submitLogin.Caption := 'Inloggen';

    if ARequest.Status = 200 then
    begin
      // Sla token/gebruikersgegevens op in sessie/localStorage
      window.localStorage.setItem('auth_token', AResponse);
      window.localStorage.setItem('user_email', email);

      if rememberMe.Checked then
      begin
        // Sla gegevens op in localStorage voor "Onthoud mij"
        window.localStorage.setItem('remember_me', 'true');
        window.localStorage.setItem('saved_email', email);
      end
      else
      begin
        window.localStorage.removeItem('remember_me');
        window.localStorage.removeItem('saved_email');
      end;

      // Navigeer naar homepage
//      if assigned(FormMain.loadedForm) then
//        FormMain.loadedForm.free;
//      Application.CreateForm(TFormHome, FormMain.FormContainer.ElementID, FormMain.loadedForm);
    end
    else
      ShowMessage('Login mislukt (' + IntToStr(ARequest.Status) + '): ' + AResponse);
  end);
end;


end.
