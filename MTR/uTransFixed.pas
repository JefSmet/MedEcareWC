unit uTransFixed;

interface

uses System.SysUtils;

type
  /// <summary>Helpers om tekst exact op vaste lengte te krijgen.</summary>
  TFixed = record
    class function PadRight(const S: string; Len: Integer): string; static;
    class function PadLeft(const S: string; Len: Integer; C: Char = '0')
      : string; static;
  end;

const
  TRANS_VERSION = 'Versie 1.16';
  // spec §Header :contentReference[oaicite:0]{index=0}:contentReference[oaicite:1]{index=1}
  TRANS_MODULE_VER = '84'; // vrij te kiezen
  TRANS_DETAIL_LEN = 964; // excl. CRLF
  CRLF = #13#10;

implementation

class function TFixed.PadRight(const S: string; Len: Integer): string;
begin
  Result := Copy(S, 1, Len).PadRight(Len);
end;

class function TFixed.PadLeft(const S: string; Len: Integer; C: Char): string;
begin
  Result := Copy(S, 1, Len).PadLeft(Len, C);
end;

end.
