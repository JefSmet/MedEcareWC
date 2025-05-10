unit model.MedEcareDB;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules, WEBLib.REST;

type
  TMedEcareDB = class(TWebDataModule)
    WebHttpRequest1: TWebHttpRequest;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MedEcareDB: TMedEcareDB;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
