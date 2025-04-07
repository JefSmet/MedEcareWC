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
  private
    loadedForm : TWebForm;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses Forms.login;


procedure TFormMain.WebButton1Click(Sender: TObject);
begin
  if assigned(loadedForm) then
    loadedForm.free;
  Application.CreateForm(TFormLogin,FormContainer.ElementID,loadedForm);
end;

end.