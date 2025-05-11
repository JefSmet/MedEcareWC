unit Forms.main;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics,
  WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Menus, WEBLib.Menus, Vcl.Controls,
  WEBLib.ExtCtrls, Vcl.StdCtrls, WEBLib.StdCtrls, model.AppManager,
  WEBLib.WebTools, WEBLib.Actions, WEBLib.WebCtrls;

type
  TFormMain = class(TWebForm)
    [async]
    acl: TWebElementActionList;
    [async]
    procedure WebFormCreate(Sender: TObject);
    [async]
    procedure btnLogoutClick(Sender: TObject);
    procedure acShowHomeExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    procedure acShowVerlofUserExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
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
    FAppManager.ShowToast('Er ging iets verkeerd, probeer later opnieuw.');
end;

procedure TFormMain.acShowVerlofUserExecute(Sender: TObject; Element: TJSHTMLElementRecord;
  Event: TJSEventParameter);
begin
FAppManager.ShowVerlofUser;
end;

procedure TFormMain.acShowHomeExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
FAppManager.ShowHome;
end;

procedure TFormMain.WebFormCreate(Sender: TObject);
var
  resetToken: string;
  userId: string;
begin
  FAppManager := TAppManager.GetInstance;
  FAppManager.SetFormContainerID('main-content');
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
