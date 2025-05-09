unit model.MedEcareDB;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Modules;

type
  TMedEcareDB = class(TWebDataModule)
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
