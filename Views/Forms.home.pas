unit Forms.home;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.Controls, Vcl.StdCtrls,
  WEBLib.StdCtrls;

type
  TFormHome = class(TViewBase)
    WebLabel1: TWebLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHome: TFormHome;

implementation

{$R *.dfm}

end.