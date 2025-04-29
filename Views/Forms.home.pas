unit Forms.home;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.Controls, Vcl.StdCtrls,
  WEBLib.StdCtrls;

type
  TFormHome = class(TViewBase)
    btnLogout: TWebButton;
    lblUserInfo: TWebLabel;
    lblWelcome: TWebLabel;
    procedure btnLogoutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHome: TFormHome;

implementation

{$R *.dfm}

procedure TFormHome.btnLogoutClick(Sender: TObject);
begin
  // model voor logout

end;

end.
