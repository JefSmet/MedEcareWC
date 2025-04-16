unit Forms.main;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Menus, WEBLib.Menus, Vcl.Controls,
  WEBLib.ExtCtrls, Vcl.StdCtrls, WEBLib.StdCtrls;

type
  TFormMain = class(TWebForm)
    WebPanel1: TWebPanel;
    WebButton1: TWebButton;
    WebButton2: TWebButton;
    WebButton3: TWebButton;
    WebButton4: TWebButton;
    FormContainer: TWebPanel;
    procedure WebButton1Click(Sender: TObject);
    procedure WebButton2Click(Sender: TObject);
    procedure WebFormCreate(Sender: TObject);
  private
    FLoadedForm : TWebForm;
    procedure ShowForm( AForm: TWebFormClass );
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses Forms.login, Forms.forgotPassword, Forms.resetPassword, Forms.home;


procedure TFormMain.ShowForm(AForm: TWebFormClass);
begin
  if Assigned(FLoadedForm) then
    FLoadedForm.free;
  Application.CreateForm(AForm,FormContainer.ElementID,FLoadedForm);
end;

procedure TFormMain.WebButton1Click(Sender: TObject);
begin
  ShowForm(TFormLogin);
end;

procedure TFormMain.WebButton2Click(Sender: TObject);
begin
  ShowForm(TFormResetPassword);
end;

procedure TFormMain.WebFormCreate(Sender: TObject);
begin
  ShowForm(TFormHome);
end;

end.
