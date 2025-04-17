unit model.AppManager;

interface

uses model.Authorisation;

type
  TAppController = class
  strict private
    class var FInstance: TAppController;
    constructor CreatePrivate;
    class destructor Destroy;
  private
    FAuth: TAuthorisation;
  public
    constructor Create; deprecated 'Use TAppController.GetInstance instead of Create.';
    destructor Destroy; override;
    class function GetInstance: TAppController;

    property Auth: TAuthorisation read FAuth;
  end;

implementation

uses
  System.SysUtils, System.Classes;

{ TAppController }

constructor TAppController.CreatePrivate;
begin
  inherited Create;
  FAuth:=TAuthorisation.Create(nil);
end;

constructor TAppController.Create;
begin
  // Prevent direct call
  raise EInvalidOperation.Create(
    'Use TAppController.GetInstance instead of TAppController.Create.'
  );
end;

destructor TAppController.Destroy;
begin
  FAuth.Free;
  inherited Destroy;
end;

class function TAppController.GetInstance: TAppController;
begin
  if not Assigned(FInstance) then
    FInstance := TAppController.CreatePrivate;
  Result := FInstance;
end;

class destructor TAppController.Destroy;
begin
  if Assigned(FInstance) then
    FInstance.Free;
end;

end.

