unit viewcontroller.base;

interface

uses model.appcontroller, WEBLib.Forms;

type
  TViewControllerBase = class
  private
    FAppController: TAppController;
    FView: TWebForm;
  public
    constructor Create(AView: TWebForm; AAppController: TAppController);
    property AppController: TAppController read FAppController;
    property View: TWebForm read FView;
  end;

implementation

{ TViewControllerBase }

constructor TViewControllerBase.Create(AView: TWebForm; AAppController: TAppController);
begin
  inherited Create;
  FView:=AView;
  FAppController:=AAppController;
end;

end.
