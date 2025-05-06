program MedEcareWC;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  Forms.main in 'Views\Forms.main.pas' {FormMain: TWebForm} {*.html},
  Forms.login in 'Views\Forms.login.pas' {FormLogin: TWebForm} {*.html},
  Forms.forgotPassword in 'Views\Forms.forgotPassword.pas' {FormForgotPassword: TWebForm} {*.html},
  Forms.resetPassword in 'Views\Forms.resetPassword.pas' {FormResetPassword: TWebForm} {*.html},
  Forms.home in 'Views\Forms.home.pas' {FormHome: TWebForm} {*.html},
  view.base in 'Views\view.base.pas' {ViewBase: TWebForm} {*.html},
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
  Forms.wachtlijst.readOnly in 'Views\Forms.wachtlijst.readOnly.pas' {FormWachtlijstReadOnly: TWebForm} {*.html},
  uTransFixed in 'MTR\uTransFixed.pas',
  uTransRecords in 'MTR\uTransRecords.pas',
  uPrestatieModel in 'MTR\uPrestatieModel.pas',
  uMtrBuilderIntf in 'MTR\uMtrBuilderIntf.pas',
  uMtrBuildersSpoed in 'MTR\uMtrBuildersSpoed.pas',
  uMtrBuilderFactory in 'MTR\uMtrBuilderFactory.pas',
  uMtrWriter in 'MTR\uMtrWriter.pas',
  uPrestatieRestClient in 'MTR\uPrestatieRestClient.pas',
  Forms.registerUser in 'Views\Forms.registerUser.pas' {FormRegisterUser: TWebForm} {*.html},
  Forms.calendar in 'Views\Forms.calendar.pas' {Form1: TWebForm} {*.html},
  Forms.verlof.user in 'Views\Forms.verlof.user.pas' {FormVerlofUser: TWebForm} {*.html};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormVerlofUser, FormVerlofUser);
  Application.Run;

end.
