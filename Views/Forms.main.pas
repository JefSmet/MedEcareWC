unit Forms.main;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Menus, WEBLib.Menus, Vcl.Controls,
  WEBLib.ExtCtrls, Vcl.StdCtrls, WEBLib.StdCtrls, model.AppManager;

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
    FAppManager : TAppManager;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses Forms.login, Forms.forgotPassword, Forms.resetPassword, Forms.home;




procedure TFormMain.WebButton1Click(Sender: TObject);
begin
//
end;

procedure TFormMain.WebButton2Click(Sender: TObject);
begin
//
end;

procedure TFormMain.WebFormCreate(Sender: TObject);
begin
  FAppManager := TAppManager.GetInstance;
  FAppManager.SetFormContainerID(FormContainer.ElementID);
  FAppManager.ShowLogin();
end;

end.
