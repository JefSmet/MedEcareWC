program MedEcareWC;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  Forms.main in 'Views\Forms.main.pas' {FormMain: TWebForm} {*.html} ,
  Forms.login in 'Views\Forms.login.pas' {FormLogin: TWebForm} {*.html} ,
  Forms.forgotPassword in 'Views\Forms.forgotPassword.pas' {FormForgotPassword: TWebForm} {*.html} ,
  Forms.resetPassword in 'Views\Forms.resetPassword.pas' {FormResetPassword: TWebForm} {*.html} ,
  Forms.home in 'Views\Forms.home.pas' {FormHome: TWebForm} {*.html} ,
  view.base in 'Views\view.base.pas' {ViewBase: TWebForm} {*.html} ,
  model.AppManager in 'Models\model.AppManager.pas',
  model.Authorisation in 'Models\model.Authorisation.pas' {Authorisation: TWebDataModule} ,
  model.Activity in 'Models\model.Activity.pas',
  model.Person in 'Models\model.Person.pas',
  model.RefreshToken in 'Models\model.RefreshToken.pas',
  model.Role in 'Models\model.Role.pas',
  model.ShiftType in 'Models\model.ShiftType.pas',
  model.ShiftTypeRate in 'Models\model.ShiftTypeRate.pas',
  model.User in 'Models\model.User.pas',
  model.UserConstraint in 'Models\model.UserConstraint.pas',
  model.UserRole in 'Models\model.UserRole.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
