program MedEcareWC;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  Forms.main in 'Forms.main.pas' {FormMain: TWebForm} {*.html},
  Forms.login in 'Forms.login.pas' {FormLogin: TWebForm} {*.html},
  Forms.forgotPassword in 'Forms.forgotPassword.pas' {FormForgotPassword: TWebForm} {*.html};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormForgotPassword, FormForgotPassword);
  Application.Run;
end.
