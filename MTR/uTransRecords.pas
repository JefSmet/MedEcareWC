unit uTransRecords;

interface

uses
  System.SysUtils, uTransFixed;

type
  TMtrHeader = record
    FileDate: TDate;
    Version: string;
    ModuleVer: string;
    function ToLine: string;
  end;

  TMtrTrailer = record
    RecordCount: Integer;
    function ToLine: string;
  end;

  /// <summary>Superset van 964 posities; enkel gebruikte zones worden ingevuld.</summary>
  TMtrDetail = record
    // ========= verplichte zones =========
    UniekNr, VisitID: string;
    VolgNr: Integer;
    ExecDate, TotDate: TDate;
    RizivNr: string;
    Aantal: Integer;
    // ========= spoed-specifiek (fragment) =========
    RitID, AmbuBon: string; // ambulance / MUG
    MugFlag: Char; // ‘4’ in zone 551,1
    ImplNr, OpNr: string; // ortho / heelkunde
    AnestInt, AnestRiz: string; // anesthesie
    Device: string; // echografie
    // … voeg zo nodig extra velden toe …

    function ToLine: string;
  private
    function DateToStr8(const D: TDate): string;
  end;

implementation

{ TMtrHeader ---------------------------------------------------------------- }

function TMtrHeader.ToLine: string;
begin
  Result := 'TTT' + FormatDateTime('yyyymmdd', FileDate) + StringOfChar(' ', 6)
    + TFixed.PadRight(Version, 10) + TFixed.PadRight(ModuleVer, 10) + CRLF;
end;

{ TMtrTrailer --------------------------------------------------------------- }

function TMtrTrailer.ToLine: string;
begin
  Result := 'EEE' + TFixed.PadLeft(RecordCount.ToString, 6) + CRLF;
end;

{ TMtrDetail ---------------------------------------------------------------- }

function TMtrDetail.DateToStr8(const D: TDate): string;
begin
  Result := FormatDateTime('yyyymmdd', D);
end;

function TMtrDetail.ToLine: string;
var
  L: string;
begin
  L := TFixed.PadRight(UniekNr, 12) + TFixed.PadRight(VisitID, 20) +
    TFixed.PadLeft(VolgNr.ToString, 5) + DateToStr8(ExecDate) +
    DateToStr8(TotDate) + TFixed.PadLeft(RizivNr, 7) +
  // 935,7; spec laat 60,6 toe :contentReference[oaicite:2]{index=2}:contentReference[oaicite:3]{index=3}
    TFixed.PadLeft(Aantal.ToString, 4);

  // ---- spoedvelden (voorbeeld, posities zie spec) ------------------------
  L := L + TFixed.PadRight(RitID, 11) + // 807,11
    TFixed.PadRight(AmbuBon, 15) + // 949,15
    MugFlag + // 551,1
    TFixed.PadRight(ImplNr, 12) + // 923,12
    TFixed.PadRight(OpNr, 12) + // 260,12
    TFixed.PadRight(AnestInt, 6) + // 275,6
    TFixed.PadRight(AnestRiz, 11) + // 863,11
    TFixed.PadRight(Device, 11); // 886,11

  // vul tot 964 pos
  Result := L.PadRight(TRANS_DETAIL_LEN) + CRLF;
  Assert(Length(L) <= TRANS_DETAIL_LEN, 'Detail te lang');
end;

end.
