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
  Forms.registerUser in 'Views\Forms.registerUser.pas' {FormRegisterUser: TWebForm} {*.html},
  Forms.calendar in 'Views\Forms.calendar.pas' {Form1: TWebForm} {*.html},
  Forms.verlof.user.temp in 'Views\Forms.verlof.user.temp.pas' {FormVerlofUser2: TWebForm} {*.html},
  model.MedEcareDB in 'Models\model.MedEcareDB.pas' {MedEcareDB: TWebDataModule},
  middleware.httponly in 'middleware.httponly.pas',
  Forms.verlof.user in 'Views\Forms.verlof.user.pas' {FormVerlofUser: TWebForm} {*.html},
  Forms.planning in 'Views\Forms.planning.pas' {FormPlanning: TWebForm} {*.html},
  orm.Doctor in 'ORM\orm.Doctor.pas',
  Forms.settings in 'Views\Forms.settings.pas' {FormSettings: TWebForm} {*.html},
  Forms.shiftTypes in 'Views\Forms.shiftTypes.pas' {FormShiftTypes: TWebForm} {*.html},
  utils.SchoolVakanties in 'Utils\utils.SchoolVakanties.pas',
  orm.Roster in 'ORM\orm.Roster.pas',
  Forms.user in 'Views\Forms.user.pas' {FormUser: TWebForm} {*.html},
  Forms.roster in 'Views\Forms.roster.pas' {FormRoster: TWebForm} {*.html},
  Forms.roles in 'Views\Forms.roles.pas' {FormRoles: TWebForm} {*.html};

{$R *.res}

begin
  Application.Initialize;
  Application.ErrorType := TApplicationErrorType.aeSilent;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TViewBase, ViewBase);
  Application.CreateForm(TFormPlanning, FormPlanning);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormShiftTypes, FormShiftTypes);
  Application.CreateForm(TFormUser, FormUser);
  Application.CreateForm(TFormRoster, FormRoster);
  Application.CreateForm(TFormRoles, FormRoles);
  Application.Run;

end.
