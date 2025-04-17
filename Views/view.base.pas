unit view.base;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, model.AppManager;

type
  TViewBase = class(TWebForm)
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
  private
    FAppController: TAppManager;
//    FViewController: TViewControllerBase;
  protected
    procedure DoCreateViewController; virtual; abstract;
  public
    constructor Create(AOwner: TComponent; AAppController: TAppManager); reintroduce;
//    property AppController: TAppController read FAppController;
//    property ViewController: TViewControllerBase read FViewController;
  end;

implementation

{$R *.dfm}

{ TFormBase }

constructor TViewBase.Create(AOwner: TComponent; AAppController: TAppManager);
begin
  inherited Create(AOwner);
  FAppController:=AAppController;
end;

procedure TViewBase.WebFormCreate(Sender: TObject);
begin
  DoCreateViewController;
end;

procedure TViewBase.WebFormDestroy(Sender: TObject);
begin
//  if Assigned(FViewController) then
//    FViewController.Free;
end;

end.
