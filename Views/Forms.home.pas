unit Forms.home;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.Controls, Vcl.StdCtrls,
  WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.Grids, WEBLib.DataGrid.Common, Vcl.Grids, WEBLib.DataGrid, WEBLib.WebCtrls,
  VCL.TMSFNCTypes, VCL.TMSFNCUtils, VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes, VCL.TMSFNCCustomControl, VCL.TMSFNCCalendar,
  WEBLib.DataGrid.DataAdapter.Base, WEBLib.DataGrid.DataAdapter.Custom, WEBLib.JQCtrls;

type
  TFormHome = class(TViewBase)
    DGCustomDataAdapter1: TDGCustomDataAdapter;
    WebDataGrid1: TWebDataGrid;
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
