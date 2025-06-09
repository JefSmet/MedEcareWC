unit Forms.user;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, model.AppManager, orm.user,
  System.Generics.Collections, orm.Doctor, orm.Person,
  WEBLib.Actions, orm.Role, orm.UserRole, Vcl.Controls, Vcl.StdCtrls, WEBLib.StdCtrls;

type
  TFormUser = class(TViewBase)
    acl: TWebElementActionList;

    [async]
    edtEmail: TWebEdit;
    edtPassword: TWebEdit;
    edtVoornaam: TWebEdit;
    edtAchternaam: TWebEdit;
    edtRiziv: TWebEdit;
    dpGeboorteDatum: TWebDateTimePicker;
    cbRole: TWebComboBox;
    cbIsActiefStafflid: TWebComboBox;
    procedure buildTable;
    [async]
    procedure getUserList;
    [async]
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    procedure showNewUser(Sender: TObject; Element: TJSHTMLElementRecord;
      Event: TJSEventParameter);
    procedure hideFormSection;
    [async]
    procedure aclacSaveExecute(Sender: TObject; Element: TJSHTMLElementRecord;
      Event: TJSEventParameter);
    procedure aclacHideNewUserExecute(Sender: TObject;
      Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]
    procedure aclacEditExecute(Sender: TObject; Element: TJSHTMLElementRecord;
      Event: TJSEventParameter);
    [async]
    procedure GetRoles;
    [async]
    procedure GetUserRoles;
    [async]procedure aclacDeleteClickExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
  private
    FAppManager: TAppManager;
    FUserList: TList<TUser>;
    FRolesList: TList<TRole>;
    FUserRolesList: TList<TUserRole>;
    FCurrentUserRole: TUserRole;
    FFormSection: TJSElement;
    procedure ClearEdits;

  public
    { Public declarations }
  end;

var
  FormUser: TFormUser;

implementation

{$R *.dfm}

procedure TFormUser.GetRoles;
begin
  try
    await(FAppManager.DB.GetRoles(FRolesList));
  finally

  end;
end;

procedure TFormUser.getUserList;
begin
  try
    await(FAppManager.DB.getUsers(FUserList));
  except
    on e: exception do
      FAppManager.ShowToast('Er ging iets mis bij ophalen gebruikers: ' +
        e.Message);
  end;
end;

procedure TFormUser.GetUserRoles;
begin
  try
    await(FAppManager.DB.GetUserRoles(FUserRolesList));
  finally

  end;
end;

procedure TFormUser.buildTable;
var
  sb: TStringBuilder;
  user: TUser;
  tableBody: TJSElement;
  geboorteDatumStr, isActiefStr: string;
  I: Integer;
  roles,roleId: string;
  k: Integer;
begin
  tableBody := document.getElementById('shiftTypesTableBody');
  if not Assigned(tableBody) then
    Exit;

  sb := TStringBuilder.Create;
  try
    if FUserList.Count = 0 then
    begin
      sb.AppendLine('<tr>');
      sb.AppendLine
        ('  <td colspan="8" style="border: 1px solid #ddd; padding: 8px; text-align: center; color: #666;">');
      sb.AppendLine
        ('    Geen gebruikers gevonden. Voeg een nieuwe toe met het formulier hierboven.');
      sb.AppendLine('  </td>');
      sb.AppendLine('</tr>');
    end
    else
    begin
      for user in FUserList do
      begin
        if user.Person.DateOfBirth <> 0 then
          geboorteDatumStr := FormatDateTime('dd-mm-yyyy',
            user.Person.DateOfBirth)
        else
          geboorteDatumStr := '-';

        if user.Doctor.IsEnabledInShifts then
          isActiefStr := 'Ja'
        else
          isActiefStr := 'Nee';

        sb.AppendLine
          ('<tr style="cursor: pointer;" onmouseover="this.style.backgroundColor=''#f1f1f1''" onmouseout="this.style.backgroundColor=''''">');
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [user.Id]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [user.Email]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          ['***']).AppendLine; // Don't show actual password
        for I := 0 to FUserRolesList.Count - 1 do
        begin
          if user.Id = FUserRolesList[I].UserId then
          begin
            for k := 0 to FRolesList.Count - 1 do
            begin
             roleId := FUserRolesList[i].RoleId;
             if roleId = FRolesList[k].Id then
             begin
               roles := roles +FRolesList[k].Name+',';
               break;
             end;
            end;
          end;
        end;
        roles := copy(roles,1,roles.Length-1);
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [roles]).AppendLine;
          roles := '';
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [user.Person.FirstName]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [user.Person.LastName]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [geboorteDatumStr]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [user.Doctor.RizivNumber]).AppendLine;
        sb.AppendFormat
          ('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>',
          [isActiefStr]).AppendLine;
        sb.AppendLine('  <td style="border: 1px solid #ddd; padding: 8px;">');
        sb.AppendFormat
          ('    <button id="%s" class="edit-button btn btn-warning" style="margin-right: 5px;">Bewerken</button>',
          [user.Id]).AppendLine;
        sb.AppendFormat
          ('    <button id="%s" class="delete-button btn btn-danger">Verwijderen</button>',
          [user.Id]).AppendLine;
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

procedure TFormUser.ClearEdits;
begin
  edtEmail.Text := '';
  edtPassword.Text := '';
  edtVoornaam.Text :='';
  edtAchternaam.Text :='';
  edtRiziv.Text :='';
  dpGeboorteDatum.DateTime := 0;
  dpGeboorteDatum.Text := 'dd/mm/jjjj';
  cbRole.ItemIndex := -1;
  cbIsActiefStafflid.ItemIndex := -1;
end;

procedure TFormUser.showNewUser(Sender: TObject; Element: TJSHTMLElementRecord;
  Event: TJSEventParameter);
begin
  FFormSection.classList.remove('d-none');
  FFormSection.setAttribute('method', 'post');
  // Clear form fields
  // document.getElementById('edtEmail').AsElement.setAttribute('value', '');
  // document.getElementById('edtPassword').AsElement.setAttribute('value', '');
  // document.getElementById('edtVoornaam').AsElement.setAttribute('value', '');
  // document.getElementById('edtAchternaam').AsElement.setAttribute('value', '');
  // document.getElementById('edtGeboorteDatum').AsElement.setAttribute('value', '');
  // document.getElementById('edtRiziv').AsElement.setAttribute('value', '');
  // document.getElementById('edtIsActiefStafflid').AsElement.setAttribute('value', 'true');
end;

procedure TFormUser.hideFormSection;
begin
  FFormSection.classList.add('d-none');
  if not FFormSection.getAttribute('method').Equals('') then
    FFormSection.setAttribute('method', '');
  ClearEdits;
end;

procedure TFormUser.aclacSaveExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  UserId: string;
  Email, password, voornaam, achternaam, riziv,role: string;
  geboorteDatum: TDateTime;
  isActiefStaflid: Boolean;
begin
   try
   // Get form values
   email := edtEmail.Text;
   password := edtPassword.Text;
   voornaam := edtVoornaam.Text;
   achternaam := edtAchternaam.Text;
   riziv := edtRiziv.Text;
   isActiefStaflid := (LowerCase(cbIsActiefStafflid.Text) ='ja');
   role := cbRole.Text;

   if dpgeboorteDatum.Date >0 then
   geboorteDatum := dpGeboorteDatum.Date
   else
   geboorteDatum := 0;

   if FFormSection.getAttribute('method') = 'put' then
   begin
   userId := FFormSection.id;
   await(FAppManager.DB.PutPerson(userId,voornaam,achternaam,geboorteDatum));
   await(FAppManager.DB.PutUser(userId,email,password,role));
   await(FAppManager.DB.putDoctor(userId,riziv,isActiefStaflid));
   end
   else
   begin
   await(FAppManager.DB.PostRegister(email,password,role,voornaam,achternaam,geboortedatum,riziv,isActiefStaflid));
   end;

   await(getUserList);
   await(getUserRoles);
   buildTable;
   hideFormSection;
   except
   on e: exception do
   FAppManager.ShowToast('Er ging iets mis bij opslaan: ' + e.Message);
   end;
end;

procedure TFormUser.aclacHideNewUserExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  hideFormSection;
end;

procedure TFormUser.aclacDeleteClickExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
userId : string;
begin
try
 userId := Element.Element.id;
   FFormSection.setAttribute('id', userId);
await(FAppManager.DB.DeleteUser(userId));
await(FAppManager.db.DeleteDoctor(userId));
await(FAppManager.DB.DeletePerson(userId));

except on e:exception do

end;
await(GetUserList);
await(GetUserRoles);
buildTable;
end;

procedure TFormUser.aclacEditExecute(Sender: TObject;
  Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  UserId: string;
  user : TUser;
  userRole : TUserRole;
  role : TRole;
  roleName : string;
  I: Integer;
begin
   FFormSection.setAttribute('method', 'put');
   FFormSection.classList.remove('d-none');
   userId := Element.Element.id;
   FFormSection.setAttribute('id', userId);
   for user in FUserList do
     begin
       if user.Id = UserId then
       begin
       edtEmail.Text := user.Email;
       edtVoornaam.Text := user.Person.FirstName;
       edtAchternaam.Text := user.Person.LastName;
       edtRiziv.Text := user.Doctor.RizivNumber;
       if user.Doctor.IsEnabledInShifts then
       cbIsActiefStafflid.ItemIndex := 0
       else
       cbIsActiefStafflid.ItemIndex :=1;
       for userRole in FUserRolesList do
         begin
           if user.Id = userRole.UserId then
           begin
             for role in FRolesList do
             begin
               if userRole.RoleId = role.Id then
               begin
                 roleName := role.Name;
                 break;
               end;
             end;
           end;
         end;
       cbRole.Text := roleName;
         break;
       end;
     end;

end;

procedure TFormUser.WebFormCreate(Sender: TObject);
var
role : TRole;
begin
  FAppManager := TAppManager.GetInstance;
  FUserList := TList<TUser>.Create;
  FRolesList := TList<TRole>.Create;
  FUserRolesList := TList<TUserRole>.Create;
  FFormSection := document.getElementById('formSection');
  await(getUserList);
  await(GetRoles);
  await(GetUserRoles);

  for role in FRolesList do
  begin
    cbRole.Items.Add(role.Name);
  end;
  cbIsActiefStafflid.items.Add('ja');
  cbIsActiefStafflid.items.Add('nee');
  buildTable;
end;

procedure TFormUser.WebFormDestroy(Sender: TObject);
begin
  FUserList.Free;
  FRolesList.Free;
  FUserRolesList.Free;
end;

end.
