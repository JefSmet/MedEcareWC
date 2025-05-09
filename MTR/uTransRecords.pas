unit uTransRecords;

interface

uses
  System.SysUtils, uTransFixed;

type
  TMtrHeader = record
    FileDate: TDate;
    Version : string;
    ModuleVer: string;
    function ToLine: string;
  end;

  TMtrTrailer = record
    RecordCount: Integer;
    function ToLine: string;
  end;

  /// <summary>
  ///  Superset van 964 posities.  Bevat nu ook
  ///  – alle ALTIJD verplichte zones
  ///  – buffer voor deprecated zones die blanco moeten blijven.
  /// </summary>
  TMtrDetail = record
    {========= vaste verplichte zones (spec §5) ===============================}
    UniekNr   : string;   // 1-12
    VisitID   : string;   // 13-32
    VolgNr    : Integer;  // 33-37
    ExecDate  : TDate;    // 38-45
    TotDate   : TDate;    // 46-53
    DokInt    : string;   // 54-59   intern arts-id
    RizivNr   : string;   // 60-65   nomenclatuur
    Aantal    : Integer;  // 67-70
    DokRiz    : string;   // 840-850 uitvoerende arts RIZIV
    NormExec  : Char;     // 170     1=geconv,2=niet
    EuroFlag  : Char;     // 422     sinds euro altijd '2'

    {========= spoed / variabele velden ======================================}
    RitID     : string;   // 807,11
    AmbuBon   : string;   // 949,15
    MugFlag   : Char;     // 551,1
    ImplNr    : string;   // 923,12
    OpNr      : string;   // 260,12
    AnestInt  : string;   // 275,6
    AnestRiz  : string;   // 863,11
    Device    : string;   // 886,11

    {========= buffer voor deprecated / niet-gebruikte zones ==================}
    DeprecatedBloc : string; // 84 spaties (lengte = alle obsolete zones)

    function  ToLine: string;
    procedure BlankDeprecated;          // zet buffer vol spaties
  private
    function DateToStr8(const D: TDate): string;
  end;

implementation

{ TMtrHeader -----------------------------------------------------------------}

function TMtrHeader.ToLine: string;
begin
  Result := 'TTT' +
            FormatDateTime('yyyymmdd', FileDate) +
            StringOfChar(' ', 6) +
            TFixed.PadRight(Version,    10) +
            TFixed.PadRight(ModuleVer,  10) +
            CRLF;
end;

{ TMtrTrailer ----------------------------------------------------------------}

function TMtrTrailer.ToLine: string;
begin
  Result := 'EEE' + TFixed.PadLeft(RecordCount.ToString, 6) + CRLF;
end;

{ TMtrDetail -----------------------------------------------------------------}

procedure TMtrDetail.BlankDeprecated;
begin
  // 84 posities (som van alle officieel gedepr. zones in spec 1.16)
  DeprecatedBloc := StringOfChar(' ', 84);
end;

function TMtrDetail.DateToStr8(const D: TDate): string;
begin
  Result := FormatDateTime('yyyymmdd', D);
end;

function TMtrDetail.ToLine: string;
var
  L: string;
begin
  // -------- altijd verplichte blok -----------------------------------------
  L :=
    TFixed.PadRight(UniekNr,   12) +
    TFixed.PadRight(VisitID,   20) +
    TFixed.PadLeft (VolgNr.ToString, 5) +
    DateToStr8(ExecDate) +
    DateToStr8(TotDate) +
    TFixed.PadRight(DokInt,   6) +
    TFixed.PadLeft (RizivNr,  7) +
    TFixed.PadLeft (Aantal.ToString, 4) +
    TFixed.PadRight(DokRiz,  11) +
    NormExec +
    EuroFlag;

  // -------- spoed / variabele velden ---------------------------------------
  L := L +
       TFixed.PadRight(RitID,    11) +
       TFixed.PadRight(AmbuBon,  15) +
       MugFlag +
       TFixed.PadRight(ImplNr,   12) +
       TFixed.PadRight(OpNr,     12) +
       TFixed.PadRight(AnestInt,  6) +
       TFixed.PadRight(AnestRiz, 11) +
       TFixed.PadRight(Device,   11);

  // -------- blanco houden van obsolete zones --------------------------------
  L := L + DeprecatedBloc;

  // -------- final padding tot 964 -------------------------------------------
  Assert(Length(L) <= TRANS_DETAIL_LEN, 'Detail te lang');
  Result := L.PadRight(TRANS_DETAIL_LEN) + CRLF;
end;

end.

