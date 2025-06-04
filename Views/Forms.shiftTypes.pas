unit Forms.shiftTypes;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, WEBLib.DataGrid.Common, WEBLib.WebCtrls, Vcl.Controls, Vcl.Grids, WEBLib.DataGrid,
  Vcl.StdCtrls, WEBLib.StdCtrls;

type
  TFormShiftTypes = class(TViewBase)
    WebDataGrid1: TWebDataGrid;
    WebHTMLDiv1: TWebHTMLDiv;
    WebHTMLDiv2: TWebHTMLDiv;
    WebButton1: TWebButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormShiftTypes: TFormShiftTypes;

implementation

{$R *.dfm}

end.