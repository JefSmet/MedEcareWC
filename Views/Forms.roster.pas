unit Forms.roster;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.Controls, WEBLib.Lists,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.DataGrid.Common,
  Vcl.Grids, WEBLib.DataGrid, WEBLib.WebCtrls, System.Generics.Collections,
  orm.ShiftType, model.AppManager;

type
  TFormRoster = class(TViewBase)
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
  private
    { Private declarations }
    FShiftTypes: TList<TShiftType>;
    FAppManager: TAppManager;
    [async]
    procedure GetShiftTypes;
  public
    { Public declarations }
  end;

var
  FormRoster: TFormRoster;

implementation

{$R *.dfm}

procedure TFormRoster.GetShiftTypes;
var
  ShiftType: TShiftType;
  sb : TStringBuilder;
begin
  await(FAppManager.DB.GetShiftTypes(FShiftTypes));
  sb := TStringBuilder.Create;
  try
    for ShiftType in FShiftTypes do
    begin
      sb.Append('<tr><td>' + ShiftType.Name + '</td></tr>');
    end;
    document.getElementById('sourceListTableBody').innerHTML := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TFormRoster.WebFormCreate(Sender: TObject);
begin
  FAppManager := TAppManager.GetInstance;
  FShiftTypes := TList<TShiftType>.Create;
  GetShiftTypes;
  //Source list
  // on add wordt er van de sourcelist de huidige toegevoegd aan de nieuwe lijst
  // on move up gebruik je de move functie van TList om de volgorde te wijzigen dat de huidige een lagere index krijgt
  // on move down hetzelfde maar dan omlaag
  //Na elke actie lijst terug opnieuw renderen
  //moest het nog niet logisch zijn het datatype is ShiftType
  //New list
  //bij opslaan moet eerst heel de tabel gedelete worden dan alles volgens de nieuwe volgorde opslaan
end;

procedure TFormRoster.WebFormDestroy(Sender: TObject);
begin
  FShiftTypes.Free;
end;

end.
