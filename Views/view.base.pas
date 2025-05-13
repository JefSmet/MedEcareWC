unit view.base;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, model.AppManager, VCL.Controls;

type
  TViewBase = class(TWebForm)
  private
    FAppManager: TAppManager;
  protected
    procedure DoCreate; override;
  public
    property AppManager: TAppManager read FAppManager;
  end;

var
  ViewBase: TViewBase;

implementation

{$R *.dfm}
{ TFormBase }

procedure TViewBase.DoCreate;
begin
  inherited;
  Self.Align:=TAlign.alClient;
  FAppManager := TAppManager.GetInstance;
end;

end.
