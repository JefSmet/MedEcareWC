unit Forms.login;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, Vcl.Controls,
  WEBLib.REST, view.base;

type
  TFormLogin = class(TViewBase)
    loginPassword: TWebEdit;
    rememberMe: TWebCheckBox;
    submitLogin: TWebButton;
    loginEmail: TWebEdit;
    RequestLogin: TWebHttpRequest;
    forgotPassword: TWebButton;
   [async]
   procedure submitLoginClick(Sender: TObject);
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


procedure TFormLogin.submitLoginClick(Sender: TObject);
var
  password, email, jsonBody: string;
  response : TJSXMLHttpRequest;
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

response := await(AppManager.Auth.DoLogin(loginEmail.Text,loginPassword.Text));
if response.Status = 200 then
  begin
    AppManager.ShowHome();
  end;
end;


end.
