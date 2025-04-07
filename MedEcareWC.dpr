program MedEcareWC;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  Forms.main in 'Forms.main.pas' {FormMain: TWebForm} {*.html},
  Forms.login in 'Forms.login.pas' {FormLogin: TWebForm} {*.html},
  Forms.forgotPassword in 'Forms.forgotPassword.pas' {FormForgotPassword: TWebForm} {*.html},
  Forms.resetPassword in 'Forms.resetPassword.pas' {FormResetPassword: TWebForm} {*.html},
  Forms.home in 'Forms.home.pas' {FormHome: TWebForm} {*.html};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormHome, FormHome);
  Application.Run;
end.
