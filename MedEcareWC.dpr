program MedEcareWC;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  Forms.main in 'Source\Forms\Common\Forms.main.pas' {FormMain: TWebForm} {*.html},
  Forms.login in 'Source\Forms\Auth\Forms.login.pas' {FormLogin: TWebForm} {*.html},
  Forms.forgotPassword in 'Source\Forms\Auth\Forms.forgotPassword.pas' {FormForgotPassword: TWebForm} {*.html},
  Forms.resetPassword in 'Source\Forms\Auth\Forms.resetPassword.pas' {FormResetPassword: TWebForm} {*.html},
  Forms.home in 'Source\Forms\Common\Forms.home.pas' {FormHome: TWebForm} {*.html},
  Module.Auth.Data in 'Source\Services\Auth\Module.Auth.Data.pas' {AuthData: TWebDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormHome, FormHome);
  Application.CreateForm(TAuthData, AuthData);
  Application.Run;
end.
