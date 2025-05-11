unit Forms.login;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, Vcl.Controls,
  WEBLib.REST, view.base, WEBLib.JSON, jsdelphisystem;

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
    femail: string;
    function IsValidEmail(const Email: string): Boolean;
  public
    { Public declarations }
  end;

var
  FormLogin: TFormLogin;

implementation

uses System.StrUtils, WEBLib.RegularExpressions;

{$R *.dfm}

function TFormLogin.IsValidEmail(const Email: string): Boolean;
const
  EmailRegex = '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$';
begin
  Result := TRegEx.IsMatch(Email, EmailRegex);
end;

procedure TFormLogin.forgotPasswordClick(Sender: TObject);
begin
  AppManager.showForgotPassword();
end;

procedure TFormLogin.submitLoginClick(Sender: TObject);
var
  password, Email, jsonBody: string;
  response: TJSXMLHttpRequest;
  remember: Boolean;
  emailErrorElement, passwordErrorElement: TJSHTMLElement;
  errorMessage: string;
  errorStatus: integer;
  errorObject: TJSObject;
  errorValue: TJSValue;
begin
  // Get references to error message elements
  emailErrorElement := document.getElementById('emailError') as TJSHTMLElement;
  passwordErrorElement := document.getElementById('passwordError')
    as TJSHTMLElement;

  // Hide error messages initially
  emailErrorElement.classList.add('d-none');
  passwordErrorElement.classList.add('d-none');

  Email := Trim(loginEmail.Text);
  password := loginPassword.Text;
  remember := rememberMe.Checked;

  // Validate email

  if not IsValidEmail(Email) then
  begin
    emailErrorElement.textContent := 'Vul een geldig e-mailadres in.';
    emailErrorElement.classList.remove('d-none');
    Exit;
  end;

  if IsValidEmail(Email) then
  begin
    emailErrorElement.classList.add('d-none');
  end;
  // Validate password
  if password = '' then
  begin
    passwordErrorElement.textContent := 'Vul een wachtwoord in.';
    passwordErrorElement.classList.remove('d-none');
    Exit;
  end;
  try
    response := await(AppManager.Auth.DoLogin(loginEmail.Text,
      loginPassword.Text, ifthen(remember, 'web-persist', 'web')));
    if response.Status = 200 then
    begin
       AppManager.ShowHome();
    end;
  except
    on e: Exception do
    begin
      errorObject := TJSJSON.parseObject(e.Message);
      errorMessage := JS.toString(errorObject['message']);
      errorStatus := JS.toInteger(errorObject['status']);
      AppManager.ShowToast(errorMessage);
    end;
  end;
end;

end.
