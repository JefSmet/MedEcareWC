unit utils.SchoolVakanties;

interface

uses
  System.SysUtils, System.DateUtils, System.Generics.Collections;

type
  /// <summary>
  ///  Klasse met zuivere (pure) berekeningen voor christelijke feestdagen.
  /// </summary>
  TFeestdagen = class
  public
    /// <summary>Bereken de datum van Pasen (Paaszondag) voor jaar J.</summary>
    class function Pasen(const AJaar: Integer): TDate;
    /// <summary>Bereken de datum van Hemelvaart (Pasen + 39 dagen) voor jaar J.</summary>
    class function Hemelvaart(const AJaar: Integer): TDate;
    /// <summary>Bereken de datum van Pinksteren (Paaszondag + 49 dagen) voor jaar J.</summary>
    class function Pinksteren(const AJaar: Integer): TDate;
  end;

  /// <summary>
  ///  Klasse met zuivere (pure) berekeningen voor Belgische schoolvakanties op basis van christelijke feestdagen.
  ///  Alle methoden retourneren een nieuwe TList<TDate> met de datums (inclusief).
  /// </summary>
  TSchoolVakanties = class
  public
    /// <summary>Maakt een lijst met alle datums van de krokusvakantie (voorjaarsvakantie) voor jaar J.</summary>
    class function KrokusVakantie(const AJaar: Integer): TList<TDate>;
    /// <summary>Maakt een lijst met alle datums van de paasvakantie voor jaar J.</summary>
    class function PaasVakantie(const AJaar: Integer): TList<TDate>;
    /// <summary>Maakt een lijst met alle datums van de Hemelvaartvakantie (twee dagen) voor jaar J.</summary>
    class function HemelvaartVakantie(const AJaar: Integer): TList<TDate>;
    /// <summary>Maakt een lijst met alle datums van de herfstvakantie voor jaar J.</summary>
    class function HerfstVakantie(const AJaar: Integer): TList<TDate>;
    /// <summary>Maakt een lijst met alle datums van de kerstvakantie voor jaar J.</summary>
    class function KerstVakantie(const AJaar: Integer): TList<TDate>;
    /// <summary>Maakt een lijst met alle datums van de zomervakantie (1 juli t/m 31 augustus) voor jaar J.</summary>
    class function ZomerVakantie(const AJaar: Integer): TList<TDate>;

    /// <summary>
    ///  Maakt een TDictionary met per datum de naam van de Belgische schoolvakantie
    ///  (krokus, paas, hemelvaart, herfst, kerst, zomer) voor jaar J.
    ///  Duplicaten worden automatisch overschreven.
    /// </summary>
    class function BelgianVakanties(const AJaar: Integer): TDictionary<TDate, string>;
  end;

implementation

{ TFeestdagen }

class function TFeestdagen.Pasen(const AJaar: Integer): TDate;
var
  a, b, c, d, e, g, o, q, i, k, h, maand, dag: Integer;
begin
  // Stap 1: a = jaar mod 19
  a := AJaar mod 19;
  // Stap 2: b = jaar div 100, c = jaar mod 100
  b := AJaar div 100;
  c := AJaar mod 100;
  // Stap 3: d = b div 4, e = b mod 4
  d := b div 4;
  e := b mod 4;
  // Stap 4: g = (8*b + 13) div 25
  g := (8 * b + 13) div 25;
  // Stap 5: o = (11*(b - d - g) - 4) div 30
  o := (11 * (b - d - g) - 4) div 30;
  // Stap 6: h = (7*a + o + 6) div 11  (tijdelijke opslag)
  h := (7 * a + o + 6) div 11;
  // Stap 7: q = (19*a + (b - d - g) + 15 - h) mod 29
  q := (19 * a + (b - d - g) + 15 - h) mod 29;
  // Stap 8: i = c div 4, k = c mod 4
  i := c div 4;
  k := c mod 4;
  // Stap 9: h = ((32 + 2*e) + 2*i - k - q) mod 7
  h := ((32 + 2 * e) + 2 * i - k - q) mod 7;
  // Stap 10: maand = (90 + (q + h)) div 25
  maand := (90 + (q + h)) div 25;
  // Stap 11: dag = (19 + (q + h) + maand) mod 32
  dag := (19 + (q + h) + maand) mod 32;
  // Resultaat: datum van Paaszondag
  Result := EncodeDate(AJaar, maand, dag);
end;

class function TFeestdagen.Hemelvaart(const AJaar: Integer): TDate;
begin
  // Hemelvaart = Pasen + 39 dagen
  Result := IncDay(Pasen(AJaar), 39);
end;

class function TFeestdagen.Pinksteren(const AJaar: Integer): TDate;
begin
  // Pinksteren = Paaszondag + 49 dagen
  Result := IncDay(Pasen(AJaar), 49);
end;

{ TSchoolVakanties }

class function TSchoolVakanties.KrokusVakantie(const AJaar: Integer): TList<TDate>;
var
  pasenDatum, maandagPasen, startVak, endVak, d: TDate;
begin
  Result := TList<TDate>.Create;
  pasenDatum := TFeestdagen.Pasen(AJaar);
  maandagPasen := StartOfTheWeek(pasenDatum);
  startVak := IncDay(maandagPasen, -42);
  endVak := IncDay(startVak, 6);
  d := startVak;
  while d <= endVak do
  begin
    Result.Add(d);
    d := IncDay(d, 1);
  end;
end;

class function TSchoolVakanties.PaasVakantie(const AJaar: Integer): TList<TDate>;
var
  pasenDatum, maandPasen, startVak, endVak, d: TDate;
  i: Integer;
begin
  Result := TList<TDate>.Create;
  pasenDatum := TFeestdagen.Pasen(AJaar);

  if MonthOf(pasenDatum) = 3 then
  begin
    startVak := IncDay(pasenDatum, 1);
    endVak := IncDay(startVak, 13);
  end
  else if pasenDatum > EncodeDate(AJaar, 4, 15) then
  begin
    maandPasen := StartOfTheWeek(pasenDatum);
    startVak := IncDay(maandPasen, -7);
    endVak := IncDay(startVak, 14);
  end
  else
  begin
    for i := 1 to 7 do
      if DayOfTheWeek(EncodeDate(AJaar, 4, i)) = 1 then
      begin
        startVak := EncodeDate(AJaar, 4, i);
        Break;
      end;
    endVak := IncDay(startVak, 13);
  end;

  d := startVak;
  while d <= endVak do
  begin
    Result.Add(d);
    d := IncDay(d, 1);
  end;
end;

class function TSchoolVakanties.HemelvaartVakantie(const AJaar: Integer): TList<TDate>;
var
  startVak: TDate;
begin
  Result := TList<TDate>.Create;
  startVak := TFeestdagen.Hemelvaart(AJaar);
  Result.Add(startVak);
  Result.Add(IncDay(startVak, 1));
end;

class function TSchoolVakanties.HerfstVakantie(const AJaar: Integer): TList<TDate>;
var
  novEerste, startVak, endVak, d: TDate;
begin
  Result := TList<TDate>.Create;
  novEerste := EncodeDate(AJaar, 11, 1);

  if DayOfTheWeek(novEerste) = 7 then
    startVak := IncDay(novEerste, 1)
  else
    startVak := StartOfTheWeek(novEerste);

  endVak := IncDay(startVak, 6);
  d := startVak;
  while d <= endVak do
  begin
    Result.Add(d);
    d := IncDay(d, 1);
  end;
end;

class function TSchoolVakanties.KerstVakantie(const AJaar: Integer): TList<TDate>;
var
  blokStart, blokEnd, d: TDate;
begin
  Result := TList<TDate>.Create;

  // Deel 1: 25 dec vorig jaar → begin januari AJaar
  blokStart := EncodeDate(AJaar - 1, 12, 25);
  case DayOfTheWeek(blokStart) of
    6: blokStart := IncDay(blokStart, 2);
    7: blokStart := IncDay(blokStart, 1);
  else
    blokStart := StartOfTheWeek(blokStart);
  end;
  blokEnd := IncDay(blokStart, 13);
  d := blokStart;
  while d <= blokEnd do
  begin
    if YearOf(d) = AJaar then
      Result.Add(d);
    d := IncDay(d, 1);
  end;

  // Deel 2: 25 dec AJaar → einde december AJaar
  blokStart := EncodeDate(AJaar, 12, 25);
  case DayOfTheWeek(blokStart) of
    6: blokStart := IncDay(blokStart, 2);
    7: blokStart := IncDay(blokStart, 1);
  else
    blokStart := StartOfTheWeek(blokStart);
  end;
  blokEnd := IncDay(blokStart, 13);
  d := blokStart;
  while d <= blokEnd do
  begin
    if YearOf(d) = AJaar then
      Result.Add(d);
    d := IncDay(d, 1);
  end;

  Result.Sort;
  if Result.Count > 1 then
  begin
    d := 0;
    while (Result.Count > 1) and (Result[0] = Result[1]) do
      Result.Delete(0);
  end;
end;

class function TSchoolVakanties.ZomerVakantie(const AJaar: Integer): TList<TDate>;
var
  startVak, endVak, d: TDate;
begin
  Result := TList<TDate>.Create;
  startVak := EncodeDate(AJaar, 7, 1);
  endVak := EncodeDate(AJaar, 8, 31);
  d := startVak;
  while d <= endVak do
  begin
    Result.Add(d);
    d := IncDay(d, 1);
  end;
end;

class function TSchoolVakanties.BelgianVakanties(const AJaar: Integer): TDictionary<TDate, string>;
var
  VakDict: TDictionary<TDate, string>;
  TempList: TList<TDate>;
  d: TDate;
begin
  VakDict := TDictionary<TDate, string>.Create;
  try
    // Krokusvakantie
    TempList := KrokusVakantie(AJaar);
    try
      for d in TempList do
        VakDict.AddOrSetValue(d, 'Krokusvakantie');
    finally
      TempList.Free;
    end;

    // Paasvakantie
    TempList := PaasVakantie(AJaar);
    try
      for d in TempList do
        VakDict.AddOrSetValue(d, 'Paasvakantie');
    finally
      TempList.Free;
    end;

    // Hemelvaartvakantie
    TempList := HemelvaartVakantie(AJaar);
    try
      for d in TempList do
        VakDict.AddOrSetValue(d, 'Hemelvaartvakantie');
    finally
      TempList.Free;
    end;

    // Herfstvakantie
    TempList := HerfstVakantie(AJaar);
    try
      for d in TempList do
        VakDict.AddOrSetValue(d, 'Herfstvakantie');
    finally
      TempList.Free;
    end;

    // Kerstvakantie
    TempList := KerstVakantie(AJaar);
    try
      for d in TempList do
        VakDict.AddOrSetValue(d, 'Kerstvakantie');
    finally
      TempList.Free;
    end;

    // Zomervakantie
    TempList := ZomerVakantie(AJaar);
    try
      for d in TempList do
        VakDict.AddOrSetValue(d, 'Zomervakantie');
    finally
      TempList.Free;
    end;

    Result := VakDict;
  except
    VakDict.Free;
    raise;
  end;
end;

end.
