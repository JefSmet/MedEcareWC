unit Forms.roster;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, Vcl.Controls, WEBLib.Lists,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.DataGrid.Common,
  Vcl.Grids, WEBLib.DataGrid, WEBLib.WebCtrls, System.Generics.Collections,
  orm.ShiftType, model.AppManager, WEBLib.Actions;

type
  TShiftTypeNameAndId = record
    id: string;
    name: string;
  end;

type
  TFormRoster = class(TViewBase)
    acl: TWebElementActionList;
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    procedure AddToRoster;
    procedure renderLists;
    procedure aclacAddExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    procedure aclacSelectCellExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    procedure aclacMoveUpExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    procedure aclacMoveDownExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    procedure aclacDeleteExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    procedure aclacSaveExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
  private
    { Private declarations }
    FShiftTypes: TList<TShiftType>;
    FAddToRosterShiftTypes: TList<TShiftTypeNameAndId>;
    FAppManager: TAppManager;
    FCurrentShiftType: TShiftType;
    FCurrentShiftTypeNameAndId: TShiftTypeNameAndId;
    [async]
    procedure GetShiftTypes;
    procedure MoveUp;
    procedure MoveDown;
    [async]
    procedure SaveRoster;
  public
    { Public declarations }
  end;

var
  FormRoster: TFormRoster;

implementation

{$R *.dfm}

procedure TFormRoster.aclacAddExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  inherited;
AddToRoster;
renderLists;
end;

procedure TFormRoster.aclacDeleteExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  shiftTypeNameAndId : TShiftTypeNameAndId;
begin
  inherited;
for shiftTypeNameAndId in FAddToRosterShiftTypes do
begin
  if shiftTypeNameAndId.id = FCurrentShiftTypeNameAndId.id then
  begin
    FAddToRosterShiftTypes.Remove(shiftTypeNameAndId);
    renderLists;
    break;
  end;
end;
end;

procedure TFormRoster.aclacMoveDownExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  inherited;
MoveDown;
end;

procedure TFormRoster.aclacMoveUpExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  inherited;
MoveUp;
end;

procedure TFormRoster.aclacSaveExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  inherited;
  SaveRoster;
end;

procedure TFormRoster.aclacSelectCellExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  shiftType:TShiftType;
begin
  inherited;
if element.element.classList.contains('sourceTableCell') then
begin
  for shiftType in FShiftTypes do
  begin
    if shiftType.Id = element.element.id then
    begin
    FCurrentShiftType := shiftType;
    break;
    end;
  end;
end
else
begin
  for shiftType in FShiftTypes do
  begin
    if element.element.id = shiftType.Id then
    begin
      FCurrentShiftTypeNameAndId.Id := shiftType.Id;
      FCurrentShiftTypeNameAndId.Name := shiftType.Name;
      break;
    end;
  end;
end;
end;

procedure TFormRoster.AddToRoster;
var
 emptyShiftType : TShiftType;
begin
  if FCurrentShiftType.Id = '' then
  exit;
  FCurrentShiftTypeNameAndId.id := FCurrentShiftType.id;
  FCurrentShiftTypeNameAndId.name := FCurrentShiftType.name;
  FAddToRosterShiftTypes.Add(FCurrentShiftTypeNameAndId);
  FCurrentShiftType := emptyShiftType;
end;

procedure TFormRoster.GetShiftTypes;
var
  ShiftType: TShiftType;
  sb: TStringBuilder;
begin
  await(FAppManager.DB.GetShiftTypes(FShiftTypes));
  sb := TStringBuilder.Create;
  try
    for ShiftType in FShiftTypes do
    begin
      sb.AppendFormat('<tr><td id="%s" class="cell sourceTableCell">%s</td></tr>',
        [ShiftType.id, ShiftType.name]).AppendLine;
    end;
    document.getElementById('sourceListTableBody').innerHTML := sb.ToString;
    acl.BindActions;
  finally
    sb.Free;
  end;
end;

procedure TFormRoster.MoveUp;
var
  Index: Integer;
begin
  Index := FAddToRosterShiftTypes.IndexOf(FCurrentShiftTypeNameAndId);
  if (Index > 0) then
  begin
    FAddToRosterShiftTypes.Move(Index, Index - 1);
    renderLists;
  end;
end;

procedure TFormRoster.MoveDown;
var
  Index: Integer;
begin
  Index := FAddToRosterShiftTypes.IndexOf(FCurrentShiftTypeNameAndId);
  if (Index < FAddToRosterShiftTypes.Count - 1) then
  begin
    FAddToRosterShiftTypes.Move(Index, Index + 1);
    renderLists;
  end;
end;

procedure TFormRoster.renderLists;
var
  sb: TStringBuilder;
  ShiftTypeNameAndId: TShiftTypeNameAndId;
begin
  GetShiftTypes;
  sb := TStringBuilder.Create;
  try
    for ShiftTypeNameAndId in FAddToRosterShiftTypes do
    begin
      sb.AppendFormat('<tr><td id="%s" class="cell">%s</td></tr>',
        [ShiftTypeNameAndId.id, ShiftTypeNameAndId.name]).AppendLine;
    end;
    document.getElementById('rosterListTableBody').innerHTML := sb.ToString;
    acl.BindActions;
  finally
    sb.Free;
  end;
end;

procedure TFormRoster.WebFormCreate(Sender: TObject);
begin
  FAppManager := TAppManager.GetInstance;
  FShiftTypes := TList<TShiftType>.Create;
  FAddToRosterShiftTypes := TList<TShiftTypeNameAndId>.Create;
  renderLists;
  // record met shifttypeId en naam voor toAddList
  // Source list
  // on add wordt er van de sourcelist de huidige toegevoegd aan de nieuwe lijst
  // on move up gebruik je de move functie van TList om de volgorde te wijzigen dat de huidige een lagere index krijgt
  // on move down hetzelfde maar dan omlaag
  // Na elke actie lijst terug opnieuw renderen
  // moest het nog niet logisch zijn het datatype is ShiftType
  // New list
  // bij opslaan moet eerst heel de tabel gedelete worden dan alles volgens de nieuwe volgorde opslaan nieuwe back-end route
end;

procedure TFormRoster.WebFormDestroy(Sender: TObject);
begin
  FShiftTypes.Free;
  FAddToRosterShiftTypes.Free;
end;

procedure TFormRoster.SaveRoster;
var
  ShiftTypeNameAndId: TShiftTypeNameAndId;
  JSONArray: string;
  sb: TStringBuilder;
  i : integer;
begin
  sb := TStringBuilder.Create;
  try
    sb.Append('{"rosters":[');
    for i := 0 to FAddToRosterShiftTypes.Count - 1 do
    begin
      ShiftTypeNameAndId := FAddToRosterShiftTypes[i];
      sb.AppendFormat('{"id":%d,"shiftTypeId":"%s"}', 
        [i + 1, ShiftTypeNameAndId.id]);
      if i < FAddToRosterShiftTypes.Count - 1 then
        sb.Append(',');
    end;
    sb.Append(']}');
    JSONArray := sb.ToString;
  finally
    sb.Free;
  end;
  
  try
    await(FAppManager.DB.PostRoster(JSONArray));
    // Optional: Show success message or refresh data
  except
    on E: Exception do
      ShowMessage('Error saving roster: ' + E.Message);
  end;
end;

end.
