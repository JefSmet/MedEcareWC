unit uMtrBuildersSpoed;

interface

uses
  System.SysUtils, uMtrBuilderIntf, uPrestatieModel, uTransRecords;

type
  TAmbuBuilder = class(TInterfacedObject, IMtrBuilder)
  public
    function Build(const P: TPrestatie): TMtrDetail;
  end;

  TMugBuilder = class(TInterfacedObject, IMtrBuilder)
  public
    function Build(const P: TPrestatie): TMtrDetail;
  end;

  TOrthoBuilder = class(TInterfacedObject, IMtrBuilder)
  public
    function Build(const P: TPrestatie): TMtrDetail;
  end;

  TSurgeryBuilder = class(TInterfacedObject, IMtrBuilder)
  public
    function Build(const P: TPrestatie): TMtrDetail;
  end;

  TAnaesthesiaBuilder = class(TInterfacedObject, IMtrBuilder)
  public
    function Build(const P: TPrestatie): TMtrDetail;
  end;

  TEchoBuilder = class(TInterfacedObject, IMtrBuilder)
  public
    function Build(const P: TPrestatie): TMtrDetail;
  end;

implementation

procedure FillVast(const P: TPrestatie; var D: TMtrDetail);
begin
  D.UniekNr := P.PatientID;
  D.VisitID := P.VisitID;
  D.VolgNr := P.Counter;
  D.ExecDate := P.ExecDate;
  D.TotDate := P.ExecDate;
  D.RizivNr := P.RizivNr;
  D.Aantal := P.Aantal;
end;

function TAmbuBuilder.Build(const P: TPrestatie): TMtrDetail;
begin
  // FillChar(Result, SizeOf(Result), 0);
  Result := default (TMtrDetail);
  FillVast(P, Result);
  Result.RitID := P.Ambu.RitID;
  Result.AmbuBon := P.Ambu.AmbuBon;
end;

function TMugBuilder.Build(const P: TPrestatie): TMtrDetail;
begin
  // FillChar(Result, SizeOf(Result), 0);
  Result := default (TMtrDetail);
  FillVast(P, Result);
  Result.RitID := P.Mug.RitID;
  Result.AmbuBon := P.Mug.AmbuBon;
  Result.MugFlag := P.Mug.MugFlag; // ‘4’
end;

function TOrthoBuilder.Build(const P: TPrestatie): TMtrDetail;
begin
  // FillChar(Result, SizeOf(Result), 0);
  Result := default (TMtrDetail);
  FillVast(P, Result);
  Result.ImplNr := P.Ortho.ImplNr;
  Result.OpNr := P.Ortho.OpNr;
end;

function TSurgeryBuilder.Build(const P: TPrestatie): TMtrDetail;
begin
  // FillChar(Result, SizeOf(Result), 0)
  Result := default (TMtrDetail);;
  FillVast(P, Result);
  Result.OpNr := P.Surg.OpNr;
end;

function TAnaesthesiaBuilder.Build(const P: TPrestatie): TMtrDetail;
begin
  // FillChar(Result, SizeOf(Result), 0);
  Result := default (TMtrDetail);
  FillVast(P, Result);
  Result.AnestInt := P.Ana.AnestInt;
  Result.AnestRiz := P.Ana.AnestRiz;
end;

function TEchoBuilder.Build(const P: TPrestatie): TMtrDetail;
begin
  // FillChar(Result, SizeOf(Result), 0);
  Result := default (TMtrDetail);
  FillVast(P, Result);
  Result.Device := P.Echo.Device;
end;

end.
