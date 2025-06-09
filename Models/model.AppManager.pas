unit model.AppManager;

interface

uses model.Authorisation, WEBLib.Forms, WEBLib.Controls, SysUtils,
  model.MedEcareDB, Web;

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
    FToastShowing: Boolean;
    FDB: TMedEcareDB;
    procedure ShowForm(AForm: TWebFormClass);
    procedure DoAppError(Sender: TObject; AError: TApplicationError; var Handled: boolean);
  public
    destructor Destroy; override;
    class function GetInstance: TAppManager;
    procedure SetFormContainerID(AFormContainerID: TElementID);
    property Auth: TAuthorisation read FAuth;
    property DB: TMedEcareDB read FDB;
    procedure ShowHome;
    procedure ShowForgotPassword;
    procedure ShowLogin;
    procedure ShowResetPassword;
    procedure ShowWachtlijstReadOnly;
    procedure ShowVerlofUser;
    procedure ShowRegisterUser;
    procedure ShowToast(AMessage: string; ACaption: string = ''; ADelay: integer = 5000); overload;
    procedure ShowToast(AMessage: string; AOnFinished: TProc; ACaption: string = ''; ADelay: integer = 5000); overload;
    procedure ToggleSidebar(AVisible : boolean);
    procedure SayHi;
  end;

implementation

uses
  System.Classes, Vcl.Dialogs, Forms.home,
  Forms.forgotPassword, Forms.login, Forms.resetPassword, Forms.user,
  Forms.wachtlijst.readOnly, Forms.registerUser, Forms.verlof.user, Forms.planning, Forms.settings, Forms.shiftTypes,
  Forms.roster;

{ TAppManager }

constructor TAppManager.CreatePrivate;
begin
  inherited Create;
  Application.OnError:=DoAppError;
  FAuth := TAuthorisation.Create(nil);
  FDB := TMedEcareDB.Create(nil);
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
  FDB.free;
  inherited Destroy;
end;

procedure TAppManager.DoAppError(Sender: TObject; AError: TApplicationError; var Handled: boolean);
begin
ShowToast('Er ging iets mis: '+AError.AMessage);
Handled:=True;
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

procedure TAppManager.SayHi;
begin
ShowMessage('Hi');
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
//  ShowForm(TFormHome);
//ShowForm(TFormPlanning);
//ShowForm(TFormSettings);
//ShowForm(TFormShiftTypes);
//ShowForm(TFormUser);
ShowForm(TFormRoster);
end;


procedure TAppManager.ShowLogin;
begin
  ShowForm(TFormLogin);
end;

procedure TAppManager.ShowRegisterUser;
begin
  ShowForm(TFormRegisterUser);
end;

procedure TAppManager.ShowResetPassword;
begin
  ShowForm(TFormResetPassword);
end;

procedure TAppManager.ShowToast(AMessage: string; AOnFinished: TProc; ACaption: string; ADelay: integer);
begin
  if FToastShowing then
    exit();
  if ACaption='' then
    ACaption:=FormatDateTime('hh:mm dd/mm/yyyy',now);
  asm
    var toastEl = document.getElementById('myToast');
    var toastMessage = document.getElementById('toastMessage');
    var toastCaption = document.getElementById('toastTime');
    // ❗ Veiligheidscheck om crash te vermijden
    if (!toastEl || !toastMessage) return;

    // Verwijder vorige event handler indien nodig
    if (toastEl._handler) {
    toastEl.removeEventListener('hidden.bs.toast', toastEl._handler);
    toastEl._handler = null;
     }

    toastMessage.innerText = AMessage;
    toastCaption.innerText = ACaption;

    var self = this;
    var onFinished = AOnFinished;

    toastEl._handler = function () {
    self.FToastShowing = false;
    if (onFinished) {
    onFinished();
     }
    };

    toastEl.addEventListener('hidden.bs.toast', toastEl._handler, {once : true});

    // Probeer enkel dispose als instance nog bestaat
    var existingInstance = bootstrap.Toast.getInstance(toastEl);
    if (existingInstance) {
    existingInstance.dispose();
     }

    self.FToastShowing = true;

    // ❗ Probeer niet te showen als element verwijderd is
    if (toastEl) {
    var bsToast = new bootstrap.Toast(toastEl, { delay: ADelay });
    bsToast.show();
    }
  end;
end;

procedure TAppManager.ShowVerlofUser;
begin
  ShowForm(TFormVerlofUser);
end;

procedure TAppManager.ShowToast(AMessage: string; ACaption: string; ADelay: integer);
begin
  ShowToast(AMessage, nil, ACaption, ADelay);
end;

procedure TAppManager.ShowWachtlijstReadOnly;
begin
  ShowForm(TFormWachtlijstReadOnly);
end;

procedure TAppManager.ToggleSidebar(AVisible: boolean);
var sidebar : TJSElement;
begin
  sidebar:=document.getElementById('sidebarContainer');
  if AVisible then
  begin
    sidebar.classList.remove('d-none');
  end
  else
  begin
    sidebar.classList.add('d-none');
  end;
end;

end.
