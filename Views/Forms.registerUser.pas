unit Forms.registerUser;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.Controls, WEBLib.StdCtrls,
  Vcl.StdCtrls;

type
  TFormRegisterUser = class(TViewBase)
    firstName: TWebEdit;
    email: TWebEdit;
    password: TWebEdit;
    role: TWebComboBox;
    lastName: TWebEdit;
    dateOfBirth: TWebDateTimePicker;
    register: TWebButton;
    [async]
    procedure registerClick(Sender: TObject);
  private
    procedure OnRegistered;
  public
    { Public declarations }
  end;

var
  FormRegisterUser: TFormRegisterUser;

implementation

{$R *.dfm}

procedure TFormRegisterUser.OnRegistered;
begin
      showMessage('banaan');
end;

procedure TFormRegisterUser.registerClick(Sender: TObject);
begin
  if await(Boolean, AppManager.Auth.RegisterNewUser(email.Text, password.Text,
    role.Values[role.ItemIndex].ToLower, firstName.Text, lastName.Text,
    dateOfBirth.DateTime)) then
  begin
    AppManager.ShowToast('Registratie voltooid! Je kan nu inloggen!',OnRegistered);

  end;

end;



end.
