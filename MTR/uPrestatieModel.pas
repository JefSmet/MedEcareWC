unit uPrestatieModel;

interface

type
  { -- type-alias zodat de echte lengte in comments blijft staan ------------ }
  TFixedStr = string; // overal toepassen

  TPrestType = (ptAmbulance, ptMUG, ptOrthopedics, ptSurgery, ptAnaesthesia,
    ptEcho, ptUnknown);

  TAmbuSub = record
    RitID: TFixedStr; // max 15
    AmbuBon: TFixedStr; // max 15
  end;

  TMugSub = record
    RitID: TFixedStr;
    AmbuBon: TFixedStr;
    MugFlag: Char; // 1
  end;

  TOrthoSub = record
    ImplNr: TFixedStr; // 12
    OpNr: TFixedStr; // 12
  end;

  TSurgSub = record
    OpNr: TFixedStr; // 12
  end;

  TAnaSub = record
    AnestInt: TFixedStr; // 6
    AnestRiz: TFixedStr; // 11
  end;

  TEchoSub = record
    Device: TFixedStr; // 11
  end;

  TPrestatie = record
    { --------- TRANS-basisvelden ------------------------------------------- }
    PrestatieID: Int64;
    PrestType: TPrestType;
    PatientID: TFixedStr; // 12
    VisitID: TFixedStr; // 20
    Counter: Integer; // 5
    ExecDate: TDate;
    Aantal: Integer; // 4
    RizivNr: TFixedStr; // 7
    DoctorExecInt: TFixedStr; // 6
    DoctorExecRiz: TFixedStr; // 11

    { --------- subblokken -------------------------------------------------- }
    Ambu: TAmbuSub;
    Mug: TMugSub;
    Ortho: TOrthoSub;
    Surg: TSurgSub;
    Ana: TAnaSub;
    Echo: TEchoSub;
  end;

implementation

end.
