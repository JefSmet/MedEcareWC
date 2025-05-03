unit uPrestatieRestClient;

{ -----------------------------------------------------------------------------
  Spoed-prestaties ophalen via HTTP-GET (JSON-array) en omzetten naar het
  variant-record TPrestatie.  Asynchroon: de caller krijgt de data via
  een callback zodra de Ajax-request voltooid is.
  ----------------------------------------------------------------------------- }

interface

uses
  SysUtils, JS, Web, // pas2js RTL
  WEBLib.REST, // TWebHttpRequest
  uPrestatieModel; // variant-record & enums

type
  /// <summary>
  /// Callback-type: wordt aangeroepen zodra de array met prestaties klaar is.
  /// Bij fout of lege dataset is Length(AData)=0.
  /// </summary>
  TPrestCallback = reference to procedure(const AData: TArray<TPrestatie>);

  /// <summary>Kleine, dependency-vrije REST-client voor TMS WEB Core.</summary>
  TPrestRestClientWeb = class
  private
    FBaseUrl: string;
    function JsonToPrest(O: TJSObject): TPrestatie;
    function ISOToDate(const S: string): TDate;
  public
    constructor Create(const ABaseUrl: string);
    /// <summary>
    /// Haal spoedprestaties op van &lt;baseUrl&gt;/prestaties/spoed?since=YYYY-MM-DD.
    /// Resultaat komt asynchroon in Callback.
    /// </summary>
    procedure GetSpoedPrestaties(const ASince: TDate; const ACallback: TPrestCallback);
  end;

implementation

{ ****************************************************************************** }
{ H U L P F U N C T I E S }
{ ****************************************************************************** }

function TPrestRestClientWeb.ISOToDate(const S: string): TDate;
begin
  // Verwacht 'YYYY-MM-DD' (tijddeel negeren)
  Result := EncodeDate(StrToInt(Copy(S, 1, 4)), StrToInt(Copy(S, 6, 2)), StrToInt(Copy(S, 9, 2)));
end;

function GetStr(O: TJSObject; const Key: string): string; inline;
begin
  if O[Key] = undefined then
    Result := ''
  else
    Result := string(O[Key]);
end;

function GetInt(O: TJSObject; const Key: string): Int64; inline;
begin
  Result := StrToInt64Def(GetStr(O, Key), 0);
end;

{ ****************************************************************************** }
{ M A P P I N G }
{ ****************************************************************************** }

function TPrestRestClientWeb.JsonToPrest(O: TJSObject): TPrestatie;
begin
  // FillChar(Result, SizeOf(Result), 0);
  Result := default (TPrestatie);
  // ---- vaste velden --------------------------------------------------------
  Result.PrestatieID := GetInt(O, 'id');
  Result.PrestType := TPrestType(GetInt(O, 'prestType'));
  Result.PatientID := GetStr(O, 'patientId');
  Result.VisitID := GetStr(O, 'visitId');
  Result.Counter := GetInt(O, 'counter');
  Result.ExecDate := ISOToDate(GetStr(O, 'execDate'));
  Result.Aantal := GetInt(O, 'aantal');
  Result.RizivNr := GetStr(O, 'rizivNr');

  // ---- variant-deel --------------------------------------------------------
  case Result.PrestType of
    ptAmbulance:
      begin
        Result.Ambu.RitID := GetStr(O, 'ritId');
        Result.Ambu.AmbuBon := GetStr(O, 'ambuBon');
      end;

    ptMUG:
      begin
        Result.Mug.RitID := GetStr(O, 'ritId');
        Result.Mug.AmbuBon := GetStr(O, 'ambuBon');
        Result.Mug.MugFlag := '4'; // spec: AMBHO-code 4 voor MUG
      end;

    ptOrthopedics:
      begin
        Result.Ortho.ImplNr := GetStr(O, 'implantNr');
        Result.Ortho.OpNr := GetStr(O, 'opNr');
      end;

    ptSurgery:
      Result.Surg.OpNr := GetStr(O, 'opNr');

    ptAnaesthesia:
      begin
        Result.Ana.AnestInt := GetStr(O, 'anestInt');
        Result.Ana.AnestRiz := GetStr(O, 'anestRiz');
      end;

    ptEcho:
      Result.Echo.Device := GetStr(O, 'device');
  end;
end;

{ ****************************************************************************** }
{ C L A S S  I M P L }
{ ****************************************************************************** }

constructor TPrestRestClientWeb.Create(const ABaseUrl: string);
begin
  FBaseUrl := ABaseUrl.TrimRight(['/']);
end;

procedure TPrestRestClientWeb.GetSpoedPrestaties(const ASince: TDate; const ACallback: TPrestCallback);
var
  Req: TWebHttpRequest;
  Url: string;
begin
  Url := FBaseUrl + '/prestaties/spoed?since=' + FormatDateTime('yyyy-mm-dd', ASince);

  Req := TWebHttpRequest.Create(nil);
  Req.Url := Url;
  Req.Command := httpGET;

  // Req.OnResponse :=
  // procedure(Sender: TObject; ARequest: TJSXMLHttpRequest)
  // var
  // JArr : TJSArray;
  // I    : Integer;
  // Data : TArray<TPrestatie>;
  // begin
  // if ARequest.status = 200 then
  // begin
  // JArr := TJSArray(TJSJSON.parse(ARequest.responseText));
  // SetLength(Data, JArr.length);
  // for I := 0 to JArr.length-1 do
  // Data[I] := JsonToPrest(TJSObject(JArr[I]));
  // ACallback(Data);
  // end
  // else
  // ACallback([]); // fout of leeg
  // end;

  Req.Execute; // asynchroon
end;

end.
