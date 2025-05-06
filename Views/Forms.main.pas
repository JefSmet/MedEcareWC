unit Forms.main;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics,
  WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Menus, WEBLib.Menus, Vcl.Controls,
  WEBLib.ExtCtrls, Vcl.StdCtrls, WEBLib.StdCtrls, model.AppManager,
  WEBLib.WebTools;

type
  TFormMain = class(TWebForm)
    WebPanel1: TWebPanel;
    btnRegister: TWebButton;
    WebButton2: TWebButton;
    WebButton3: TWebButton;
    btnLogout: TWebButton;
    FormContainer: TWebPanel;
    [async]
    btnTest: TWebButton;
    [async]
    procedure WebFormCreate(Sender: TObject);
    [async]
    procedure btnLogoutClick(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure WebButton2Click(Sender: TObject);
  private
    FAppManager: TAppManager;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses Forms.login, Forms.forgotPassword, Forms.resetPassword, Forms.home;

procedure TFormMain.btnLogoutClick(Sender: TObject);
begin
  if await(Boolean, FAppManager.Auth.DoLogout) then
  begin
    FAppManager.Auth.ClearStorage;
    FAppManager.ShowLogin;
  end
  else
    showMessage('Er ging iets verkeerd, probeer later opnieuw.');
end;

procedure TFormMain.btnRegisterClick(Sender: TObject);
begin
  FAppManager.ShowRegisterUser;
end;

procedure TFormMain.WebButton2Click(Sender: TObject);
begin
  FAppManager.ShowToast('dit is een zeer lange toastmessage om de regeleinden te controleren',
    procedure
    begin
      showMessage('daaaaag');
    end)
end;

procedure TFormMain.WebFormCreate(Sender: TObject);
var
  resetToken: string;
  userId: string;
begin
  FAppManager := TAppManager.GetInstance;
  FAppManager.SetFormContainerID(FormContainer.ElementID);
  // check op password reset
  if HasQueryParam('resetToken', resetToken) then
  begin
    // resetPassword page laden en token meegeven
    FAppManager.Auth.WebSessionStorage1.SetValue('resetToken', resetToken);
    FAppManager.ShowResetPassword();
    exit();
  end;

  // probeer eerst de refreshflow
  if await(Boolean, FAppManager.Auth.TryAutoLogin) then
    FAppManager.ShowHome
  else
    FAppManager.ShowLogin;
end;

end.
