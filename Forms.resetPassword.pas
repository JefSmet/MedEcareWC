unit Forms.resetPassword;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls, Vcl.StdCtrls,
  WEBLib.StdCtrls;

type
  TFormResetPassword = class(TWebForm)
    txtConfirmPassword: TWebEdit;
    btnReset: TWebButton;
    alertMsg: TWebHTMLDiv;
    lnkLogin: TWebButton;
    txtNewPassword: TWebEdit;
    procedure WebFormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormResetPassword: TFormResetPassword;

implementation

{$R *.dfm}


procedure TFormResetPassword.WebFormCreate(Sender: TObject);
begin
  txtConfirmPassword.Text := '';
  txtNewPassword.Text := '';
  btnReset.Caption := '';
  lnkLogin.Caption := '';
  alertMsg.HTML.Text := '';
end;

end.