unit view.base;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, model.AppManager;

type
  TViewBase = class(TWebForm)
  private
    FAppManager: TAppManager;
  public
    constructor Create(AOwner: TComponent); override;
    property AppManager: TAppManager read FAppManager;
  end;

implementation

{$R *.dfm}

{ TFormBase }

constructor TViewBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppManager:=TAppManager.GetInstance;
end;

end.
