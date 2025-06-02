unit Forms.settings;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls, Vcl.StdCtrls,
  WEBLib.StdCtrls, WEBLib.ExtCtrls, view.base, WEBLib.RegularExpressions,
  model.AppManager;

type
  TFormSettings = class(TViewBase)
    firstName: TWebEdit;
    lastName: TWebEdit;
    email: TWebEdit;
    confirmPassword: TWebEdit;
    oldPassword: TWebEdit;
    dateOfBirth: TWebDateTimePicker;
    newPassword: TWebEdit;
    changePassword: TWebButton;
    updateProfile: TWebButton;
    procedure WebFormCreate(Sender: TObject);
    procedure changePasswordClick(Sender: TObject);
  private
    FAppMananger: TAppManager;
    function IsValidEmail(const AEmail: string): Boolean;
    function isMatchingPassword(APassword, AConfirmPassword: string): Boolean;
    [async]
    procedure handleUpdateProfile(AFirstName, ALastName, AEmail: string;
      dateOfBirth: TDateTime);
  public
    { Public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.dfm}
{ TFormSettings }

procedure TFormSettings.changePasswordClick(Sender: TObject);
begin
  inherited;
  // maak nog een authenticatie route die oud password authenticate en dan nieuw wachtwoord in plek stelt
  // laat toast zien wanner succesvol of mislukt
end;

procedure TFormSettings.handleUpdateProfile(AFirstName, ALastName,
  AEmail: string; dateOfBirth: TDateTime);
begin
  try
    // update user nog implementeren in medecareDB dan data doorgeven via await
    // laat toast zien wanner succesvol of mislukt
  except
    on e: Exception do
      FAppMananger.ShowToast('Er is iets misgegaan: ' + e.Message);
  end;
end;

function TFormSettings.isMatchingPassword(APassword, AConfirmPassword
  : string): Boolean;
begin
  result := APassword.Equals(AConfirmPassword);
end;

function TFormSettings.IsValidEmail(const AEmail: string): Boolean;
const
  EmailRegex = '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$';
begin
  result := TRegEx.IsMatch(AEmail, EmailRegex);
end;

procedure TFormSettings.WebFormCreate(Sender: TObject);
begin
  FAppMananger := TAppManager.GetInstance;
  dateOfBirth.DateTime := FAppMananger.Auth.currentPerson.dateOfBirth;
  firstName.Text := FAppMananger.Auth.currentPerson.firstName;
  lastName.Text := FAppMananger.Auth.currentPerson.lastName;
  email.Text := FAppMananger.Auth.currentPerson.email;
end;

end.
