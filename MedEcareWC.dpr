program MedEcareWC;

{$R *.dres}

uses
  Vcl.Forms,
  WEBLib.Forms,
  Forms.main in 'Forms.main.pas' {Form1: TWebForm} {*.html},
  Forms.login in 'Forms.login.pas' {FormLogin: TWebForm} {*.html};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormLogin, FormLogin);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
