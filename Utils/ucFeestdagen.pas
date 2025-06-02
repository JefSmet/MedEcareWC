unit ucFeestdagen;

interface

uses System.Generics.Collections;

type
  TDagenInPeriode =class
  public
    class function Aantal(const ADayOfTheWeek: Integer; const AFirstDate, ASecondDate: TDate): Integer;  // ISO 8601: DayMonday: Integer = 1 .. DaySunday: Integer = 7 cfr System.DateUtils.DayOfTheWeek
    class function Aantal_(const ADayOfWeek: Integer; const AFirstDate, ASecondDate: TDate): Integer;  // Zondag=1 ... Zaterdag=7 cfr System.SysUtils.DayOfWeek
    class function AantalWerkdagen(const AFirstDate, ASecondDate : TDate; const ALastDayInclusive: Boolean = True) : Integer; overload;
    class function AantalWerkdagen(const AYear, AMonth: Word; const ALastDayInclusive: Boolean = True): Integer; overload;
    class function AantalWeekEnds(const AFirstDate, ASecondDate : TDate) : Integer; overload;
    class function AantalWeekEnds(const AYear, AMonth: Word): Integer; overload;
  end;

  TFeestdagen = class
  private
    class var FJaar: Word;
    class var FHemelvaart: TDate;
    class var FOfferfeest: TDate;
    class var FPasen: TDate;
    class var FPinksteren: TDate;
    class var FWettelijke: TList<TDate>;
    class var FWettelijkeDateText: TDictionary<TDate,String>;
    class constructor Create;
    class destructor Destroy;
    class procedure SetJaar(const Value: Word);
  public
    class function Hemelvaart(const AJaar: Word): TDate; overload;
    class function Hemelvaart(const ADate: TDate): TDate; overload;
    class function Offerfeest(const AJaar: Word): TDate; overload;
    class function Offerfeest(const ADate: TDate): TDate; overload;
    class function Pasen(const AJaar: Word): TDate; overload;
    class function Pasen(const ADate: TDate): TDate; overload;
    class function Pinksteren(const AJaar: Word): TDate; overload;
    class function Pinksteren(const ADate: TDate): TDate; overload;
    class function Wettelijke(const AJaar: Word): TList<TDate>; overload;
    class function Wettelijke(const ADate: TDate): TList<TDate>; overload;
    class function Wettelijke_DateText(const AJaar: Word): TDictionary<TDate,String>; overload;
    class function Wettelijke_DateText(const ADate: TDate): TDictionary<TDate,String>; overload;
    class function IsFeestdag(const ADate: TDate): Boolean;
  end;

  TSchoolvakanties = class
  private
    class var FJaar: Word;
    class var FHemelvaartVakantie: TList<TDate>;
    class var FHerfstVakantie: TList<TDate>;
    class var FJoodsNieuwjaar: TList<TDate>;
    class var FKerstVakantie: TList<TDate>;
    class var FKrokusVakantie: TList<TDate>;
    class var FPaasVakantie: TList<TDate>;
    class var FRamadan: TList<TDate>;
    class var FZomerVakantie: TList<TDate>;
    class var FLijst: TDictionary<TDate,String>;
    class constructor Create;
    class destructor Destroy;
    class procedure SetJaar(const Value: Word);
  public
    class function HemelvaartVakantie(const AJaar: Word): TList<TDate>;
    class function HerfstVakantie(const AJaar: Word): TList<TDate>;
    class function JoodsNieuwjaar(const AJaar: Word): TList<TDate>;
    class function KerstVakantie(const AJaar: Word): TList<TDate>;
    class function KrokusVakantie(const AJaar: Word): TList<TDate>;
    class function PaasVakantie(const AJaar: Word): TList<TDate>;
    class function Ramadan(const AJaar: Word): TList<TDate>;
    class function ZomerVakantie(const AJaar: Word): TList<TDate>;
    class function Lijst(const AJaar: Word): TDictionary<TDate,String>;
    class function IsVakantie(const ADate: TDate): Boolean;
  end;

implementation

uses System.Math, System.SysUtils, System.DateUtils;

const
  HEBREW_EPOCH = -693348; //-1951163; //Corresponds to 7 October 3761 B.C.E. O.S
// Joodse maanden //
  NISAN = 1;
  IYYAR = 2;
  SIVAN = 3;
  TAMUZ = 4;
  AV = 5;
  ELUL = 6;
  TISHREI = 7;
  CHESHVAN = 8;
  KISLEV = 9;
  TEVET = 10;
  SHVAT = 11;
  ADAR_I = 12;
  ADAR_II = 13;

function LQuotient(const x,y: integer): integer;
begin
result:=0;
if y<=0 then
  exit;
if x>=0 then
  result:=x div y
else
  result:= integer((x mod y = 0)) - 1 - (-x) div y;
end;

function LMod(const x,y: integer): integer;
begin
if (x >= 0) then
  begin
  if (y >= 0) then
    result:=x mod y
  else
    result:= y - ((-x) mod (-y));
  end
else
  begin
  if (y >= 0) then
    result:= (y - ((-x) mod y)) mod y
  else
    result:= -((-x) mod (-y));
  end;
end;

function Molad(const Jaar: integer;const Maand: Word): double;
var
  nMonthsElapsed: Integer;
begin
nMonthsElapsed := Maand - TISHREI + lquotient(235*Jaar - 234, 19);
Result:=HEBREW_EPOCH - 876/25920 + nMonthsElapsed*29.53059413580246913580246913580247;
end;

function HebrewCalendarElapsedDays(Jaar: Integer): Integer;
var
  nDay: Integer;
begin
nDay := Trunc(Molad(Jaar, TISHREI) - HEBREW_EPOCH + 0.5);
if (LMod(3*(nDay+1), 7)<3) then
  inc(nDay);
Result:=nDay;
end;

function HebrewNewYearDelay(Jaar: Integer): Word;
var
  ny0, ny1, ny2: Integer;
begin
ny0 := HebrewCalendarElapsedDays(Jaar - 1);
ny1 := HebrewCalendarElapsedDays(Jaar);
ny2 := HebrewCalendarElapsedDays(Jaar + 1);
Result := 0;
if (ny2 - ny1 = 356) then
  Result := 2
else
  if (ny1 - ny0 = 382) then
    Result := 1;
end;

{ TFeestdagen }

class constructor TFeestdagen.Create;
begin
FWettelijke:=TList<TDate>.Create;
FWettelijkeDateText:=TDictionary<TDate,String>.Create;
FJaar:=yearof(now);
FHemelvaart:=0;
FOfferfeest:=0;
FPasen:=0;
FPinksteren:=0;
end;

class destructor TFeestdagen.Destroy;
begin
if Assigned(FWettelijke) then
  FWettelijke.Free;
if Assigned(FWettelijkeDateText) then
  FWettelijkeDateText.Free;
  inherited;
end;

class function TFeestdagen.Hemelvaart(const AJaar: Word): TDate;
begin
SetJaar(AJaar);
if FHemelvaart=0 then
  FHemelvaart:=Pasen(FJaar)+39;
Result:=FHemelvaart;
end;

class function TFeestdagen.Hemelvaart(const ADate: TDate): TDate;
begin
Result:=Hemelvaart(YearOf(ADate));
end;

class function TFeestdagen.IsFeestdag(const ADate: TDate): Boolean;
begin
Result := Wettelijke(ADate).Contains(Trunc(ADate));
end;

class function TFeestdagen.Offerfeest(const AJaar: Word): TDate;
begin
SetJaar(AJaar);
if FOfferfeest=0 then
  FOfferfeest:=trunc((FJaar-1900)*(19*354+11*355)/360*12+1421.44)+98;
Result:=FOfferfeest;
end;

class function TFeestdagen.Offerfeest(const ADate: TDate): TDate;
begin
Result:=Offerfeest(YearOf(ADate));
end;

class function TFeestdagen.Pasen(const ADate: TDate): TDate;
begin
Result:=Pasen(YearOf(ADate));
end;

class function TFeestdagen.Pinksteren(const ADate: TDate): TDate;
begin
Result:=Pinksteren(YearOf(ADate));
end;

class function TFeestdagen.Pinksteren(const AJaar: Word): TDate;
begin
SetJaar(AJaar);
if FPinksteren=0 then
  FPinksteren:=Pasen(FJaar)+49;
Result:=FPinksteren;
end;

class function TFeestdagen.Pasen(const AJaar: Word): TDate;
var
  a,b,c,d,e,g,o,q,i,k,h,dag,maand:word;
begin
SetJaar(AJaar);
if FPasen=0 then
  begin
  a:=FJaar mod 19;
  DivMod(FJaar,100,b,c);
  DivMod(b,4,d,e);
  g:=(8*b+13) div 25;
  O:=(11*(b-d-g)-4) div 30;
  dag:=(7*a+O+6) div 11;
  Q:=(19*a +(b-d-g)+15-dag) mod 29;
  DivMod(c,4,i,k);
  H:=((32+2*e)+2*i-k-Q) mod 7;
  maand:=(90+(q+h)) div 25;
  dag:=(19+(q+h)+maand) mod 32;
  FPasen:=EncodeDate(FJaar,maand,dag);
  end;
Result:=FPasen;
end;

class procedure TFeestdagen.SetJaar(const Value: Word);
begin
if FJaar<>Value then
  begin
  FJaar:=Value;
  FHemelvaart:=0;
  FOfferfeest:=0;
  FPasen:=0;
  FPinksteren:=0;
  FWettelijke.Clear;
  FWettelijkeDateText.Clear;
  end;
end;

class function TFeestdagen.Wettelijke(const ADate: TDate): TList<TDate>;
begin
Result:=Wettelijke(YearOf(ADate));
end;

class function TFeestdagen.Wettelijke(const AJaar: Word): TList<TDate>;
begin
SetJaar(AJaar);
if FWettelijke.Count=0 then
  begin
  FWettelijke.AddRange(Wettelijke_DateText(FJaar).Keys);
  FWettelijke.Sort;
  end;
Result:=FWettelijke;
end;

class function TFeestdagen.Wettelijke_DateText(const ADate: TDate): TDictionary<TDate, String>;
begin
Result:=Wettelijke_DateText(YearOf(ADate));
end;

class function TFeestdagen.Wettelijke_DateText(const AJaar: Word): TDictionary<TDate, String>;
begin
SetJaar(AJaar);
if FWettelijkeDateText.Count=0 then
  begin
  FWettelijkeDateText.Add(EncodeDate(fjaar,1,1),'Nieuwjaar');
  FWettelijkeDateText.Add(Pasen(fjaar)+1,'Paasmaandag');
  FWettelijkeDateText.Add(EncodeDate(fjaar,5,1),'Dag van de arbeid');
  FWettelijkeDateText.Add(Pasen(fjaar)+39,'O.H. Hemelvaart');
  FWettelijkeDateText.Add(Pasen(fjaar)+50,'Pinkstermaandag');
  FWettelijkeDateText.Add(EncodeDate(fjaar,7,21),'Nationale feestdag');
  FWettelijkeDateText.Add(EncodeDate(fjaar,8,15),'Moederdag');
  FWettelijkeDateText.Add(EncodeDate(fjaar,11,1),'Allerheiligen');
  FWettelijkeDateText.Add(EncodeDate(fjaar,11,11),'Wapenstilstand');
  FWettelijkeDateText.Add(EncodeDate(fjaar,12,25),'Kerstmis');
  end;
Result:=FWettelijkeDateText;
end;

{ TDagen }

class function TDagenInPeriode.AantalWeekEnds(const AYear, AMonth: Word): Integer;
begin
Result:=AantalWeekEnds( EncodeDate(AYear,AMonth,1), EncodeDate(AYear,AMonth,DaysInAMonth(AYear,AMonth)));
end;

class function TDagenInPeriode.AantalWeekEnds(const AFirstDate, ASecondDate: TDate): Integer;
var
  lSat,
  lSun: Integer;
begin
lSat:=Aantal(DaySaturday,AFirstDate,ASecondDate);
lSun:=Aantal(DaySunday,AFirstDate,ASecondDate);
Result:=lSat+lSun;
end;

class function TDagenInPeriode.AantalWerkdagen(const AYear, AMonth: Word; const ALastDayInclusive: Boolean): Integer;
begin
Result:=AantalWerkdagen(
  EncodeDate(AYear,AMonth,1),
  EncodeDate(AYear,AMonth,DaysInAMonth(AYear,AMonth)),
  ALastDayInclusive
  );
end;

class function TDagenInPeriode.Aantal_(const ADayOfWeek: Integer; const AFirstDate, ASecondDate: TDate): Integer;
var
  lDayOfWeek,
  lDaySpan,
  lRemainder,
  lSinceLastDay: Integer;
  lStart,
  lEnd: TDate;
  lFS: TFormatSettings;
begin
lFS:=TFormatSettings.Create;
// lDayOfWeek: 0=Sunday .. 6=Saturday
lDayOfWeek:=aDayOfWeek-1;
if (aFirstDate < aSecondDate) then
  begin
  lStart := trunc(aFirstDate);
  lEnd := trunc(aSecondDate);
  end
else
  begin
  lStart := trunc(aSecondDate);
  lEnd := trunc(aFirstDate);
  end;
lDaySpan := trunc(lEnd)-trunc(lStart);
Result:=Floor(lDaySpan/7);
lRemainder:= lDaySpan mod 7;
lSinceLastDay:=DayOfWeek(lEnd)-lDayOfWeek-1;
if lSinceLastDay<0 then
  inc(lSinceLastDay,7);
if lRemainder>=lSinceLastDay then
  inc(Result);
end;

class function TDagenInPeriode.Aantal(const ADayOfTheWeek: Integer; const AFirstDate, ASecondDate: TDate): Integer;
const
  lDagen: Array of integer = [0,2,3,4,5,6,7,1];
begin
if (ADayOfTheWeek<1) or (ADayOfTheWeek>7) then
  raise EArgumentOutOfRangeException.Create('Dag van de week moet tussen 1 en 7 (inclusief) liggen.');
Result:=Aantal_(lDagen[ADayOfTheWeek],AFirstDate,ASecondDate);
end;

class function TDagenInPeriode.AantalWerkdagen(const AFirstDate, ASecondDate: TDate; const ALastDayInclusive: Boolean): Integer;
var
  lStart,
  lEnd: TDate;
  lStartDOW,
  lEndDOW: Integer;
begin
if (aFirstDate < aSecondDate) then
  begin
  lStart := aFirstDate;
  lEnd := aSecondDate;
  end
else
  begin
  lStart := aSecondDate;
  lEnd := aFirstDate;
  end;
lStartDOW := DayOfTheWeek(lStart);
lEndDOW := DayOfTheWeek(lEnd);
if (lEndDOW >= lStartDOW) then
  lEndDOW := Min(lEndDOW,6)
else
  Inc(lEndDOW,5);
Result := 5*WeeksBetween(lEnd,lStart) + (lEndDOW - Min(lStartDOW,6));
if aLastDayInclusive then
  inc(Result);
end;

{ TSchoolvakanties }

class constructor TSchoolvakanties.Create;
begin
FJaar:=YearOf(now);
FHemelvaartVakantie:=TList<TDate>.Create;
FHerfstVakantie:=TList<TDate>.Create;
FJoodsNieuwjaar:=TList<TDate>.Create;
FKerstVakantie:=TList<TDate>.Create;
FKrokusVakantie:=TList<TDate>.Create;
FPaasVakantie:=TList<TDate>.Create;
FRamadan:=TList<TDate>.Create;
FZomerVakantie:=TList<TDate>.Create;
FLijst:=TDictionary<TDate,String>.Create;
end;

class destructor TSchoolvakanties.Destroy;
begin
if Assigned(FHemelvaartVakantie) then FHemelvaartVakantie.Free;
if Assigned(FHerfstVakantie) then FHerfstVakantie.Free;
if Assigned(FJoodsNieuwjaar) then FJoodsNieuwjaar.Free;
if Assigned(FKerstVakantie) then FKerstVakantie.Free;
if Assigned(FKrokusVakantie) then FKrokusVakantie.Free;
if Assigned(FPaasVakantie) then FPaasVakantie.Free;
if Assigned(FRamadan) then FRamadan.Free;
if Assigned(FZomerVakantie) then FZomerVakantie.Free;
if Assigned(FLijst) then FLijst.Free;
end;

class function TSchoolvakanties.HemelvaartVakantie(const AJaar: Word): TList<TDate>;
var
  lDate: TDate;
begin
SetJaar(AJaar);
if FHemelvaartVakantie.Count=0 then
  begin
  lDate:=TFeestdagen.Hemelvaart(FJaar);
  FHemelvaartVakantie.Add(lDate);
  FHemelvaartVakantie.Add(lDate+1);
  end;
Result:=FHemelvaartVakantie;
end;

class function TSchoolvakanties.HerfstVakantie(const AJaar: Word): TList<TDate>;
var
  lStart,
  lEnd: TDate;
begin
SetJaar(AJaar);
if FHerfstVakantie.Count=0 then
  begin
  lStart:=EncodeDate(FJaar,11,1);
  if DayOfTheWeek(lStart)=7 then
    lStart:=lStart+1
  else
    lStart:=StartOfTheWeek(lStart);
  lEnd:=lStart+6;
  while lStart<=lEnd do
    begin
    FHerfstVakantie.Add(lStart);
    lStart:=lStart+1;
    end;
  end;
Result:=FHerfstVakantie;
end;

class function TSchoolvakanties.IsVakantie(const ADate: TDate): Boolean;
begin
Result:=Lijst(YearOf(ADate)).ContainsKey(Trunc(ADate));
end;

class function TSchoolvakanties.JoodsNieuwjaar(const AJaar: Word): TList<TDate>;
var
  lDate: TDate;
  ElapsedDays,
  NewYearDelay: Integer;
begin
SetJaar(AJaar);
if FJoodsNieuwjaar.Count=0 then
  begin
  ElapsedDays:=HebrewCalendarElapsedDays(FJaar);
  NewYearDelay:=HebrewNewYearDelay(FJaar);
  lDate:=HEBREW_EPOCH + ElapsedDays + NewYearDelay;
  FJoodsNieuwjaar.Add(lDate);
  FJoodsNieuwjaar.Add(lDate+1);
  end;
Result:=FJoodsNieuwjaar;
end;

class function TSchoolvakanties.KerstVakantie(const AJaar: Word): TList<TDate>;
var
  lStart,
  lEnd: TDate;
begin
SetJaar(AJaar);
if FKerstVakantie.Count=0 then
  begin
  lStart:=EncodeDate(FJaar,12,25);
  if DayOfTheWeek(lStart)>5 then
    begin
    if DayOfTheWeek(lStart)=6 then
      lStart:=lStart+2
    else
      lStart:=lStart+1;
    end
  else
    lStart:=StartOfTheWeek(lStart);
  lEnd:=lStart+13;
  while (lStart<=lEnd) and (YearOf(lStart)=FJaar) do
    begin
    FKerstVakantie.Add(lStart);
    lStart:=lStart+1;
    end;
  lStart:=EncodeDate(FJaar-1,12,25);
  if DayOfTheWeek(lStart)>5 then
    begin
    if DayOfTheWeek(lStart)=6 then
      lStart:=lStart+2
    else
      lStart:=lStart+1;
    end
  else
    lStart:=StartOfTheWeek(lStart);
  lEnd:=lStart+13;
  while (lStart<=lEnd) do
    begin
    if YearOf(lStart)=FJaar then
      FKerstVakantie.Add(lStart);
    lStart:=lStart+1;
    end;
  end;
Result:=FKerstVakantie;
end;

class function TSchoolvakanties.KrokusVakantie(const AJaar: Word): TList<TDate>;
var
  lStart,
  lEnd: TDate;
begin
SetJaar(AJaar);
if FKrokusVakantie.Count=0 then
  begin
  lStart:=StartOfTheWeek(TFeestdagen.Pasen(FJaar));
  lStart:=lStart-42;
  lEnd:=lStart+6;
  while lStart<=lEnd do
    begin
    FKrokusVakantie.Add(lStart);
    lStart:=lStart+1;
    end;
  end;
Result:=FKrokusVakantie;
end;

class function TSchoolvakanties.Lijst(const AJaar: Word): TDictionary<TDate, String>;
var
  lDate: TDate;
begin
SetJaar(AJaar);
if FLijst.Count=0 then
  begin
  for lDate in HemelvaartVakantie(FJaar) do
    FLijst.Add(lDate,'HemelvaartVakantie');
  for lDate in HerfstVakantie(FJaar) do
    FLijst.Add(lDate,'HerfstVakantie');
  for lDate in KerstVakantie(FJaar) do
    FLijst.Add(lDate,'KerstVakantie');
  for lDate in KrokusVakantie(FJaar) do
    FLijst.Add(lDate,'KrokusVakantie');
  for lDate in PaasVakantie(FJaar) do
    FLijst.Add(lDate,'PaasVakantie');
  for lDate in ZomerVakantie(FJaar) do
    FLijst.Add(lDate,'ZomerVakantie');
  end;
Result:=FLijst;
end;

class function TSchoolvakanties.PaasVakantie(const AJaar: Word): TList<TDate>;
var
  lStart,
  lEnd: TDate;
  I: Integer;
begin
SetJaar(AJaar);
if FPaasVakantie.Count=0 then
  begin
  lStart:=TFeestdagen.Pasen(FJaar);
  if MonthOf(lStart)=3 then
    begin
    lStart:=lStart+1;
    lEnd:=lStart+13;
    end
  else
    if lStart>EncodeDate(FJaar,4,15) then
      begin
      lStart:=StartOfTheWeek(lStart)-7;
      lEnd:=lStart+14;
      end
    else
      begin
      for I := 1 to 7 do
        if DayOfTheWeek(EncodeDate(FJaar,4,I))=1 then
          break;
      lStart:=EncodeDate(FJaar,4,I);
      lEnd:=lStart+13;
      end;
  while lStart<=lEnd do
    begin
    FPaasVakantie.Add(lStart);
    lStart:=lStart+1;
    end;
  end;
Result:=FPaasVakantie;
end;

class function TSchoolvakanties.Ramadan(const AJaar: Word): TList<TDate>;
var
  lStart,
  lEnd: TDate;
begin
SetJaar(AJaar);
if FRamadan.Count=0 then
  begin
  lStart:=trunc((FJaar-1900)*(19*354+11*355)/360*12+1421.44);
  lEnd:=lStart+30;
  while lStart<=lEnd do
    begin
    FRamadan.Add(lStart);
    lStart:=lStart+1;
    end;
  end;
Result:=FRamadan;
end;

class procedure TSchoolvakanties.SetJaar(const Value: Word);
begin
if FJaar<>Value then
  begin
  FJaar:=Value;
  FHemelvaartVakantie.Clear;
  FHerfstVakantie.Clear;
  FJoodsNieuwjaar.Clear;
  FKerstVakantie.Clear;
  FKrokusVakantie.Clear;
  FPaasVakantie.Clear;
  FRamadan.Clear;
  FZomerVakantie.Clear;
  FLijst.Clear;
  end;
end;

class function TSchoolvakanties.ZomerVakantie(const AJaar: Word): TList<TDate>;
var
  lStart,
  lEnd: TDate;
begin
SetJaar(AJaar);
if FZomerVakantie.Count=0 then
  begin
  lStart:=EncodeDate(FJaar,7,1);
  lEnd:=EncodeDate(FJaar,8,31);
  while lStart<=lEnd do
    begin
    FZomerVakantie.Add(lStart);
    lStart:=lStart+1;
    end;
  end;
Result:=FZomerVakantie;
end;

end.
