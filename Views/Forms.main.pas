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
    WebButton1: TWebButton;
    WebButton2: TWebButton;
    WebButton3: TWebButton;
    btnLogout: TWebButton;
    FormContainer: TWebPanel;
    [async]
    procedure WebFormCreate(Sender: TObject);
    [async]
    procedure btnLogoutClick(Sender: TObject);
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
    showMessage('ik doe de if ni');
end;

procedure TFormMain.WebFormCreate(Sender: TObject);
var
  resetToken: string;
  userId: string;
begin
  FAppManager := TAppManager.GetInstance;
  FAppManager.SetFormContainerID(FormContainer.ElementID);
  // check op password reset
  if HasQueryParam('resetToken', resetToken) and
    HasQueryParam('userId', userId) then
  begin
    // resetPassword page laden en token meegeven
    FAppManager.Auth.WebSessionStorage1.SetValue('resetToken', resetToken);
    FAppManager.Auth.WebSessionStorage1.SetValue('userId', userId);
    FAppManager.ShowResetPassword();
  end;

  // probeer eerst de refreshflow
  if await(Boolean, FAppManager.Auth.TryAutoLogin) then
    FAppManager.ShowHome
  else
    FAppManager.ShowLogin;
end;

end.
