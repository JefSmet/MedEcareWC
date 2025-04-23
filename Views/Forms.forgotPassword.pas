unit Forms.forgotPassword;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls, Vcl.StdCtrls,
  WEBLib.StdCtrls, WEBLib.ExtCtrls, view.base;

type
  TFormForgotPassword = class(TViewBase)
    txtEmail: TWebEdit;
    btnForgot: TWebButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormForgotPassword: TFormForgotPassword;

implementation

{$R *.dfm}

end.