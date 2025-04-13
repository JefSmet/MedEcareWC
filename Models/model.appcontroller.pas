unit model.appcontroller;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules;

type
  TAppController = class(TWebDataModule)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AppController: TAppController;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
