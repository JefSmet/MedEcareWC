unit Forms.shiftTypes;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, WEBLib.DataGrid.Common,
  WEBLib.WebCtrls, Vcl.Controls, Vcl.Grids, WEBLib.DataGrid,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.Actions, orm.ShiftType,
  System.Generics.Collections, model.AppManager;

type
  TFormShiftTypes = class(TViewBase)
    acl: TWebElementActionList;
    edtStartHour: TWebEdit;
    edtName: TWebEdit;
    edtDurationMinutes: TWebEdit;
    edtStartMinute: TWebEdit;
    dpActiveUntil: TWebDateTimePicker;
    dpActiveFrom: TWebDateTimePicker;
    procedure showNewShiftType(Sender: TObject; Element: TJSHTMLElementRecord;
      Event: TJSEventParameter);
    [async]
    procedure WebFormCreate(Sender: TObject);
    procedure aclacHideNewShiftTypeExecute(Sender: TObject;
      Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]
    procedure GetShiftTypes;
    procedure renderShiftTypes;
    procedure buildShiftTypesTable;
    procedure WebFormDestroy(Sender: TObject);
    procedure aclacShowUpdateShiftTypeExecute(Sender: TObject;
      Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]
    procedure aclacEditExecute(Sender: TObject; Element: TJSHTMLElementRecord;
      Event: TJSEventParameter);
  private
    { Private declarations }
    FAppManager: TAppManager;
    FShiftTypes: TList<TShiftType>;
    FFormSection: TJSElement;
  public
    { Public declarations }
  end;

var
  FormShiftTypes: TFormShiftTypes;

const
  defaultDateText: string = 'dd/mm/jjjj';

implementation

{$R *.dfm}

procedure TFormShiftTypes.aclacEditExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  shiftTypeId: string;
  toUpdateShiftType: TShiftType;
begin
  FFormSection.setAttribute('method', 'put');

  FFormSection.classList.remove('d-none');
  shiftTypeId := Element.Element.id;
  FFormSection.setAttribute('id',shiftTypeId);
  await(FAppManager.db.getShiftTypeById(shiftTypeId, toUpdateShiftType));
  edtName.Text := toUpdateShiftType.Name;
  edtStartHour.Text := toUpdateShiftType.StartHour.ToString;
  edtStartMinute.Text := toUpdateShiftType.StartMinute.ToString;
  edtDurationMinutes.Text := toUpdateShiftType.DurationMinutes.ToString;
  dpActiveUntil.DateTime := toUpdateShiftType.ActiveUntil;
  dpActiveFrom.DateTime := toUpdateShiftType.ActiveFrom;
end;

procedure TFormShiftTypes.aclacHideNewShiftTypeExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  newShiftTypeForm: TJSElement;
begin
  inherited;
  newShiftTypeForm := document.getElementById('formSection');
  FFormSection.classList.add('d-none');
  if not FFormSection.getAttribute('method').Equals('') then
    FFormSection.setAttribute('method', '');

  edtStartHour.Text := '';
  edtName.Text := '';
  edtDurationMinutes.Text := '';
  edtStartMinute.Text := '';
  dpActiveUntil.Date := 0;
  dpActiveFrom.Date := 0;
  dpActiveUntil.Text := defaultDateText;

end;

procedure TFormShiftTypes.GetShiftTypes;
begin
  FShiftTypes.Clear;
  try
    begin
      await(FAppManager.db.GetShiftTypes(FShiftTypes));
    end;

  except
    on e: exception do
    begin
      FAppManager.ShowToast('Er ging iets mis:' + e.Message);
    end;

  end;
end;

procedure TFormShiftTypes.aclacShowUpdateShiftTypeExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  inherited;
  FFormSection.setAttribute('method', 'put');
  FFormSection.classList.remove('d-none');
  edtStartHour.Text := '';
  edtName.Text := '';
  edtDurationMinutes.Text := '';
end;

procedure TFormShiftTypes.buildShiftTypesTable;
var
  sb: TStringBuilder;
  ShiftType: TShiftType;
  startTime, activeFromStr, activeUntilStr: string;
  tableBody: TJSElement;
begin
  tableBody := document.getElementById('shiftTypesTableBody');
  if not Assigned(tableBody) then
    Exit;

  sb := TStringBuilder.Create;
  try
    if FShiftTypes.Count = 0 then
    begin
      sb.AppendLine('<tr>');
      sb.AppendLine
        ('  <td colspan="7" style="border: 1px solid #ddd; padding: 8px; text-align: center; color: #666;">');
      sb.AppendLine
        ('    Geen shift types gevonden. Voeg een nieuwe toe met het formulier hierboven.');
      sb.AppendLine('  </td>');
      sb.AppendLine('</tr>');
    end
    else
    begin
      for ShiftType in FShiftTypes do
      begin
        startTime := Format('%d:%02d', [ShiftType.StartHour,
          ShiftType.StartMinute]);

        if ShiftType.ActiveFrom <> 0 then
          activeFromStr := FormatDateTime('dd-mm-yyyy', ShiftType.ActiveFrom)
        else
          activeFromStr := '-';

        if ShiftType.ActiveUntil <> 0 then
          activeUntilStr := FormatDateTime('dd-mm-yyyy', ShiftType.ActiveUntil)
        else
          activeUntilStr := '-';

        sb.AppendLine
          ('<tr style="cursor: pointer;" onmouseover="this.style.backgroundColor=''#f1f1f1''" onmouseout="this.style.backgroundColor=''''">');
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [ShiftType.id]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [ShiftType.Name]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [startTime]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%d</td>',
          [ShiftType.DurationMinutes]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [activeFromStr]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [activeUntilStr]).AppendLine;
        sb.AppendLine('  <td style="border: 1px solid #ddd; padding: 8px;">');
        sb.AppendFormat
          ('    <button id ="%s" class="update-button btn btn-warning" style="margin-right: 5px;">Bewerken</button>',
          [ShiftType.id]).AppendLine;
        sb.AppendFormat
          ('    <button id ="%s"class="btn btn-danger">Verwijderen</button>',
          [ShiftType.id]).AppendLine;
        sb.AppendLine('  </td>');
        sb.AppendLine('</tr>');

      end;
    end;

    tableBody.innerHTML := sb.ToString;
    acl.BindActions;
  finally
    sb.Free;
  end;
end;

procedure TFormShiftTypes.renderShiftTypes;
begin
  buildShiftTypesTable;
end;

procedure TFormShiftTypes.showNewShiftType(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  newShiftTypeForm: TJSElement;
begin
  inherited;
  newShiftTypeForm := document.getElementById('formSection');
  newShiftTypeForm.classList.remove('d-none');
  FFormSection.setAttribute('method', 'post');
  edtStartHour.Text := '';
  edtName.Text := '';
  edtDurationMinutes.Text := '';
end;

procedure TFormShiftTypes.WebFormCreate(Sender: TObject);
begin
  inherited;
  FAppManager := TAppManager.GetInstance;
  FShiftTypes := TList<TShiftType>.Create;
  FFormSection := document.getElementById('formSection');
  dpActiveUntil.Date := 0;
  dpActiveUntil.Text := defaultDateText;
  dpActiveFrom.Date := 0;
  dpActiveFrom.Text := defaultDateText;
  await(GetShiftTypes);
  await(buildShiftTypesTable);
end;

procedure TFormShiftTypes.WebFormDestroy(Sender: TObject);
begin
  inherited;
  FShiftTypes.Free;
end;

end.
