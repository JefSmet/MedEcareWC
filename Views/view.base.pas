unit view.base;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, model.appcontroller, viewcontroller.base;

type
  TViewBase = class(TWebForm)
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
  private
    FAppController: TAppController;
    FViewController: TViewControllerBase;
  protected
    procedure DoCreateViewController; virtual; abstract;
  public
    constructor Create(AOwner: TComponent; AAppController: TAppController); reintroduce;
    property AppController: TAppController read FAppController;
    property ViewController: TViewControllerBase read FViewController;
  end;

implementation

{$R *.dfm}

{ TFormBase }

constructor TViewBase.Create(AOwner: TComponent; AAppController: TAppController);
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
  if Assigned(FViewController) then
    FViewController.Free;
end;

end.
