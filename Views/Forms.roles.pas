unit Forms.roles;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, WEBLib.WebCtrls, Vcl.Controls,
  Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.Actions, orm.Role,
  System.Generics.Collections, model.AppManager;

type
  TFormRoles = class(TViewBase)
    acl: TWebElementActionList;
    edtName: TWebEdit;
    procedure showNewRole(Sender: TObject; Element: TJSHTMLElementRecord;
      Event: TJSEventParameter);
    [async]
    procedure WebFormCreate(Sender: TObject);
    procedure aclacHideNewRoleExecute(Sender: TObject;
      Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]
    procedure GetRoles;
    procedure renderRoles;
    procedure buildRolesTable;
    procedure WebFormDestroy(Sender: TObject);
    procedure aclacShowUpdateRoleExecute(Sender: TObject;
      Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]
    procedure aclacEditExecute(Sender: TObject; Element: TJSHTMLElementRecord;
      Event: TJSEventParameter);
    [async]
    procedure aclacSaveExecute(Sender: TObject; Element: TJSHTMLElementRecord;
      Event: TJSEventParameter);
    [async]
    procedure aclacDeleteExecute(Sender: TObject; Element: TJSHTMLElementRecord;
      Event: TJSEventParameter);
    procedure hideFormSection;
  private
    { Private declarations }
    FAppManager: TAppManager;
    FRoles: TList<TRole>;
    FFormSection: TJSElement;
  public
    { Public declarations }
  end;

var
  FormRoles: TFormRoles;

implementation

{$R *.dfm}

procedure TFormRoles.aclacEditExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  roleId: string;
  Role: TRole;
begin
  FFormSection.setAttribute('method', 'put');
  FFormSection.classList.remove('d-none');
  
  roleId := Element.Element.id;
  FFormSection.setAttribute('id', roleId);
  
  for Role in FRoles do
  begin
    if Role.id = roleId then
    begin
      edtName.Text := Role.Name;
      exit;
    end;
  end;
end;

procedure TFormRoles.aclacHideNewRoleExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  hideFormSection;
end;

procedure TFormRoles.aclacDeleteExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  roleId: string;
begin
  roleId := Element.Element.id;
  
  if window.confirm('Weet u zeker dat u deze rol wilt verwijderen?') then
  begin
    try
      await(FAppManager.db.DeleteRole(roleId));
      await(GetRoles);
      buildRolesTable;
      FAppManager.ShowToast('Rol succesvol verwijderd');
    except
      on e: exception do
      begin
        FAppManager.ShowToast('Fout bij verwijderen rol: ' + e.Message);
      end;
    end;
  end;
end;

procedure TFormRoles.GetRoles;
begin
  FRoles.Clear;
  try
    await(FAppManager.db.GetRoles(FRoles));
  except
    on e: exception do
    begin
      FAppManager.ShowToast('Er ging iets mis: ' + e.Message);
    end;
  end;
end;

procedure TFormRoles.hideFormSection;
begin
  inherited;
  FFormSection.classList.add('d-none');
  if not FFormSection.getAttribute('method').Equals('') then
    FFormSection.setAttribute('method', '');
  edtName.Text := '';
end;

procedure TFormRoles.aclacSaveExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  roleId: string;
begin
  try
    if FFormSection.getAttribute('method') = 'put' then
    begin
      roleId := FFormSection.id;
      await(FAppManager.db.PutRole(roleId, edtName.Text));
    end
    else
    begin
      await(FAppManager.db.PostRole(edtName.Text));
    end;
    
    await(GetRoles);
    buildRolesTable;
    hideFormSection;
    FAppManager.ShowToast('Rol succesvol opgeslagen');
  except
    on e: exception do
    begin
      FAppManager.ShowToast('Fout bij opslaan rol: ' + e.Message);
    end;
  end;
end;

procedure TFormRoles.aclacShowUpdateRoleExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  inherited;
  FFormSection.setAttribute('method', 'put');
  FFormSection.classList.remove('d-none');
  edtName.Text := '';
end;

procedure TFormRoles.buildRolesTable;
var
  sb: TStringBuilder;
  Role: TRole;
  tableBody: TJSElement;
begin
  tableBody := document.getElementById('rolesTableBody');
  if not Assigned(tableBody) then
    exit;

  sb := TStringBuilder.Create;
  try
    if FRoles.Count = 0 then
    begin
      sb.AppendLine('<tr>');
      sb.AppendLine('  <td colspan="3" style="border: 1px solid #ddd; padding: 8px; text-align: center; color: #666;">');
      sb.AppendLine('    Geen rollen gevonden. Voeg een nieuwe toe met het formulier hierboven.');
      sb.AppendLine('  </td>');
      sb.AppendLine('</tr>');
    end
    else
    begin
      for Role in FRoles do
      begin
        sb.AppendLine('<tr style="cursor: pointer;" onmouseover="this.style.backgroundColor=''#f1f1f1''" onmouseout="this.style.backgroundColor=''''">');
        sb.AppendFormat('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>', [Role.id]).AppendLine;
        sb.AppendFormat('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>', [Role.Name]).AppendLine;
        sb.AppendLine('  <td style="border: 1px solid #ddd; padding: 8px;">');
        sb.AppendFormat('    <button id="%s" class="edit-button btn btn-warning" style="margin-right: 5px;">Bewerken</button>', [Role.id]).AppendLine;
        sb.AppendFormat('    <button id="%s" class="delete-button btn btn-danger">Verwijderen</button>', [Role.id]).AppendLine;
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

procedure TFormRoles.renderRoles;
begin
  buildRolesTable;
end;

procedure TFormRoles.showNewRole(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  inherited;
  FFormSection.classList.remove('d-none');
  FFormSection.setAttribute('method', 'post');
  edtName.Text := '';
end;

procedure TFormRoles.WebFormCreate(Sender: TObject);
begin
  inherited;
  FAppManager := TAppManager.GetInstance;
  FRoles := TList<TRole>.Create;
  FFormSection := document.getElementById('formSection');
  
  await(GetRoles);
  await(buildRolesTable);
end;

procedure TFormRoles.WebFormDestroy(Sender: TObject);
begin
  inherited;
  FRoles.Free;
end;

end.