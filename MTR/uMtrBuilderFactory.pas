unit uMtrBuilderFactory;

interface

uses uMtrBuilderIntf, uMtrBuildersSpoed, uPrestatieModel;

function GetBuilder(PT: TPrestType): IMtrBuilder;

implementation

uses System.SysUtils;

function GetBuilder(PT: TPrestType): IMtrBuilder;
begin
  case PT of
    ptAmbulance:
      Result := TAmbuBuilder.Create;
    ptMUG:
      Result := TMugBuilder.Create;
    ptOrthopedics:
      Result := TOrthoBuilder.Create;
    ptSurgery:
      Result := TSurgeryBuilder.Create;
    ptAnaesthesia:
      Result := TAnaesthesiaBuilder.Create;
    ptEcho:
      Result := TEchoBuilder.Create;
  else
    raise Exception.Create('Geen builder voor prestatietype');
  end;
end;

end.
