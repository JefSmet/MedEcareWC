unit Forms.forgotPassword;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls, Vcl.StdCtrls,
  WEBLib.StdCtrls, WEBLib.ExtCtrls, view.base, WEBLib.RegularExpressions;

type
  TFormForgotPassword = class(TViewBase)
    txtEmail: TWebEdit;
    btnForgot: TWebButton;
    btnBack: TWebButton;
    alertMsg: TWebHTMLDiv;
    procedure btnBackClick(Sender: TObject);
    [async]
    procedure btnForgotClick(Sender: TObject);
  private
    function IsValidEmail(const Email: string): Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormForgotPassword: TFormForgotPassword;

implementation

{$R *.dfm}

procedure TFormForgotPassword.btnBackClick(Sender: TObject);
begin
  AppManager.ShowLogin();
end;

procedure TFormForgotPassword.btnForgotClick(Sender: TObject);
var
  alertMsgElement: TJSHTMLElement;
begin
  // eerst client-side validatie
  if not IsValidEmail(txtEmail.Text) then
  begin
    alertMsgElement := document.getElementById('alertMsg') as TJSHTMLElement;
    alertMsgElement.classList.remove('d-none');
    alertMsgElement.innerText := 'Vul een geldig emailadres in';
    Exit;
  end;

  if await(Boolean,AppManager.Auth.forgotPassword(txtEmail.Text)) then
  begin
    // velden uitschakelen
    txtEmail.Enabled := False;
    btnForgot.Enabled := False;

    // toon de Bootstrap-toast (via een asm-blok)
    asm
      var toastEl = document.getElementById('myToast');
      var bsToast = new bootstrap.Toast(toastEl, { delay: 5000 });
      bsToast.show();
    end;
  end
else
  ShowMessage
    ('Je wachtwoord kon niet gereset worden. Probeer het later opnieuw of neem contact op met de ondersteuning.');
end;

function TFormForgotPassword.IsValidEmail(const Email: string): Boolean;
const
  EmailRegex = '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$';
begin
  Result := TRegEx.IsMatch(Email, EmailRegex);
end;

end.
