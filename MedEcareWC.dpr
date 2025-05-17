program MedEcareWC;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  Forms.main in 'Views\Forms.main.pas' {FormMain: TWebForm} {*.html},
  Forms.login in 'Views\Forms.login.pas' {FormLogin: TWebForm},
  Forms.forgotPassword in 'Views\Forms.forgotPassword.pas' {FormForgotPassword: TWebForm},
  Forms.resetPassword in 'Views\Forms.resetPassword.pas' {FormResetPassword: TWebForm},
  Forms.home in 'Views\Forms.home.pas' {FormHome: TWebForm},
  view.base in 'Views\view.base.pas' {ViewBase: TWebForm},
  model.AppManager in 'Models\model.AppManager.pas',
  model.Authorisation in 'Models\model.Authorisation.pas' {Authorisation: TWebDataModule},
  orm.Activity in 'ORM\orm.Activity.pas',
  orm.Person in 'ORM\orm.Person.pas',
  orm.RefreshToken in 'ORM\orm.RefreshToken.pas',
  orm.Role in 'ORM\orm.Role.pas',
  orm.ShiftType in 'ORM\orm.ShiftType.pas',
  orm.ShiftTypeRate in 'ORM\orm.ShiftTypeRate.pas',
  orm.User in 'ORM\orm.User.pas',
  orm.UserConstraint in 'ORM\orm.UserConstraint.pas',
  orm.UserRole in 'ORM\orm.UserRole.pas',
  Forms.wachtlijst.readOnly in 'Views\Forms.wachtlijst.readOnly.pas' {FormWachtlijstReadOnly: TWebForm},
  uTransFixed in 'MTR\uTransFixed.pas',
  uTransRecords in 'MTR\uTransRecords.pas',
  uPrestatieModel in 'MTR\uPrestatieModel.pas',
  uMtrBuilderIntf in 'MTR\uMtrBuilderIntf.pas',
  uMtrBuildersSpoed in 'MTR\uMtrBuildersSpoed.pas',
  uMtrBuilderFactory in 'MTR\uMtrBuilderFactory.pas',
  uMtrWriter in 'MTR\uMtrWriter.pas',
  uPrestatieRestClient in 'MTR\uPrestatieRestClient.pas',
  Forms.registerUser in 'Views\Forms.registerUser.pas' {FormRegisterUser: TWebForm},
  Forms.calendar in 'Views\Forms.calendar.pas' {Form1: TWebForm},
  Forms.verlof.user in 'Views\Forms.verlof.user.pas' {FormVerlofUser: TWebForm},
  model.MedEcareDB in 'Models\model.MedEcareDB.pas' {MedEcareDB: TWebDataModule},
  middleware.httponly in 'middleware.httponly.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
