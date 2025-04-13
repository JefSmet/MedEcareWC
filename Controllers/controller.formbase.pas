unit controller.formbase;

interface

uses model.appcontroller, view.base;

type
  TControllerFormBase = class
  private

  public
    constructor Create(AForm: TViewBase);

  end;

implementation

{ TControllerFormBase<T> }

constructor TControllerFormBase.Create(AForm: TViewBase);
begin
  inherited Create;

end;

end.
