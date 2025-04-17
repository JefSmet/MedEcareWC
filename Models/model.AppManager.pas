unit model.AppManager;

interface

uses model.Authorisation;

type
  TAppManager = class
  strict private
    class var FInstance: TAppManager;
    constructor CreatePrivate;
    class destructor Destroy;
    constructor Create; deprecated 'Use TAppController.GetInstance instead of Create.';
  private
    FAuth: TAuthorisation;
  public
    destructor Destroy; override;
    class function GetInstance: TAppManager;

    property Auth: TAuthorisation read FAuth;
  end;

implementation

uses
  System.SysUtils, System.Classes, Vcl.Dialogs;

{ TAppManager }

constructor TAppManager.CreatePrivate;
begin
  inherited Create;
  FAuth:=TAuthorisation.Create(nil);
  ShowMessage('AppManager Constructed');
end;

constructor TAppManager.Create;
begin
  // Prevent direct call
  raise EInvalidOperation.Create(
    'Use TAppController.GetInstance instead of TAppController.Create.'
  );
end;

destructor TAppManager.Destroy;
begin
  FAuth.Free;
  inherited Destroy;
end;

class function TAppManager.GetInstance: TAppManager;
begin
  if not Assigned(FInstance) then
    FInstance := TAppManager.CreatePrivate;
  Result := FInstance;
end;


class destructor TAppManager.Destroy;
begin
  if Assigned(FInstance) then
    FInstance.Free;
end;

end.

