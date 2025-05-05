unit Forms.wachtlijst.readOnly;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.Calendar, view.base;

type
  TFormWachtlijstReadOnly = class(TViewBase)
    WebCalendar1: TWebCalendar;
    procedure WebCalendar1DateSelected(Sender: TObject; ADate: TDateTime);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWachtlijstReadOnly: TFormWachtlijstReadOnly;

implementation

{$R *.dfm}

procedure TFormWachtlijstReadOnly.WebCalendar1DateSelected(Sender: TObject;
  ADate: TDateTime);
begin
  ShowMessage(formatDateTime('dd-mm-yyyy', WebCalendar1.SelectedDate));
end;

end.
