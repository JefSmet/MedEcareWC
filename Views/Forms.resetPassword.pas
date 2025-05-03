unit Forms.resetPassword;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls, Vcl.StdCtrls,
  WEBLib.StdCtrls, view.base;

type
  TFormResetPassword = class(TViewBase)
    txtConfirmPassword: TWebEdit;
    btnReset: TWebButton;
    alertMsg: TWebHTMLDiv;
    lnkLogin: TWebButton;
    txtNewPassword: TWebEdit;
    procedure WebFormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure lnkLoginClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormResetPassword: TFormResetPassword;

implementation

{$R *.dfm}

procedure TFormResetPassword.btnResetClick(Sender: TObject);
var
  newPassword: string;
  confirmPassword: string;
  userId: string;
begin
  inherited;
  newPassword := txtNewPassword.Text;
  confirmPassword := txtConfirmPassword.Text;

  if newPassword.Equals(confirmPassword) then
  begin
    // maak call
    userId := AppManager.Auth.WebSessionStorage1.GetValue('userId');

  end
  else
  begin
    // show alert
  end;

end;

procedure TFormResetPassword.lnkLoginClick(Sender: TObject);
begin
  inherited;
  AppManager.ShowLogin;
end;

procedure TFormResetPassword.WebFormCreate(Sender: TObject);
begin
  txtConfirmPassword.Text := '';
  txtNewPassword.Text := '';
  btnReset.Caption := '';
  lnkLogin.Caption := '';
  alertMsg.HTML.Text := '';
end;

end.
