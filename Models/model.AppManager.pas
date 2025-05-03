unit model.AppManager;

interface

uses model.Authorisation, WEBLib.Forms, WEBLib.Controls;

type
  TAppManager = class
  strict private
    class var FInstance: TAppManager;
    constructor CreatePrivate;
    class destructor Destroy;
    constructor Create; deprecated 'Use TAppController.GetInstance instead of Create.';
  private
    FAuth: TAuthorisation;
    FFormContainerID: TElementID;
    FLoadedForm: TWebForm;
    procedure ShowForm(AForm: TWebFormClass);
  public
    destructor Destroy; override;
    class function GetInstance: TAppManager;
    procedure SetFormContainerID(AFormContainerID: TElementID);
    property Auth: TAuthorisation read FAuth;
    procedure ShowHome;
    procedure ShowForgotPassword;
    procedure ShowLogin;
    procedure ShowResetPassword;
    procedure ShowWachtlijstReadOnly;
  end;

implementation

uses
  System.SysUtils, System.Classes, Vcl.Dialogs, Forms.home,
  Forms.forgotPassword, Forms.login, Forms.resetPassword,
  Forms.wachtlijst.readOnly;

{ TAppManager }

constructor TAppManager.CreatePrivate;
begin
  inherited Create;
  FAuth := TAuthorisation.Create(nil);
end;

constructor TAppManager.Create;
begin
  // Prevent direct call
  raise EInvalidOperation.Create('Use TAppController.GetInstance instead of TAppController.Create.');
end;

destructor TAppManager.Destroy;
begin
  if Assigned(FLoadedForm) then
  begin
    FLoadedForm.free;
  end;
  FAuth.free;
  inherited Destroy;
end;

class function TAppManager.GetInstance: TAppManager;
begin
  if not Assigned(FInstance) then
    FInstance := TAppManager.CreatePrivate;
  Result := FInstance;
end;

class destructor TAppManager.Destroy;
begin
  if Assigned(FInstance) then
    FInstance.free;
end;

procedure TAppManager.SetFormContainerID(AFormContainerID: TElementID);
begin
  FFormContainerID := AFormContainerID;
end;

procedure TAppManager.ShowForgotPassword;
begin
  ShowForm(TFormForgotPassword);
end;

procedure TAppManager.ShowForm(AForm: TWebFormClass);
begin
  if Assigned(FLoadedForm) then
    FLoadedForm.free;
  Application.CreateForm(AForm, FFormContainerID, FLoadedForm);
end;

procedure TAppManager.ShowHome;
begin
  ShowForm(TFormHome);
end;

procedure TAppManager.ShowLogin;
begin
  ShowForm(TFormLogin);
end;

procedure TAppManager.ShowResetPassword;
begin
  ShowForm(TFormResetPassword);
end;

procedure TAppManager.ShowWachtlijstReadOnly;
begin
  ShowForm(TFormWachtlijstReadOnly);
end;

end.
