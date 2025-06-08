unit Forms.user;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, view.base, model.AppManager, orm.user,
  System.Generics.Collections, orm.Doctor, orm.Person,
  WEBLib.Actions;

type
  TFormUser = class(TViewBase)
    acl: TWebElementActionList;

    [async]
    procedure buildTable;
    [async]
    procedure getUserList;
    [async]
    procedure getCurrentDoctor;
    [async]
    procedure getCurrentPerson;
    [async]
    procedure WebFormCreate(Sender: TObject);
    procedure WebFormDestroy(Sender: TObject);
    procedure showNewUser(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    procedure hideFormSection;
    [async]
    procedure aclacSaveExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    procedure aclacHideNewUserExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
    [async]
    procedure aclacEditExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
  private
    FAppManager: TAppManager;
    FUserList: TList<TUser>;
    FCurrentUser : TUser;
    FCurrentPerson : TPerson;
    FCurrentDoctor : TDoctor;
    FFormSection: TJSElement;

  public
    { Public declarations }
  end;

var
  FormUser: TFormUser;

implementation

{$R *.dfm}

procedure TFormUser.getCurrentDoctor;
begin
try
await(FAppManager.DB.getDoctorById(FCurrentUser.Id,FCurrentDoctor));
except on e: exception do

end;
end;

procedure TFormUser.getCurrentPerson;
begin
 await( FAppManager.DB.getPersonById(FCurrentUser.Id,FCurrentPerson));
end;

procedure TFormUser.getUserList;
begin
  try
    await(FAppManager.DB.getUsers(FUserList));
    if FUserList.Count > 0 then
      FCurrentUser := FUserList[0];
  except
    on e: exception do
      FAppManager.ShowToast('Er ging iets mis bij ophalen gebruikers: ' + e.Message);
  end;
end;

procedure TFormUser.buildTable;
var
  sb: TStringBuilder;
  User: TUser;
  tableBody: TJSElement;
  geboorteDatumStr, isActiefStr: string;
begin
  tableBody := document.getElementById('shiftTypesTableBody');
  if not Assigned(tableBody) then
    Exit;

  sb := TStringBuilder.Create;
  try
    if FUserList.Count = 0 then
    begin
      sb.AppendLine('<tr>');
      sb.AppendLine('  <td colspan="8" style="border: 1px solid #ddd; padding: 8px; text-align: center; color: #666;">');
      sb.AppendLine('    Geen gebruikers gevonden. Voeg een nieuwe toe met het formulier hierboven.');
      sb.AppendLine('  </td>');
      sb.AppendLine('</tr>');
    end
    else
    begin
      for User in FUserList do
      begin
        await(FAppManager.DB.getPersonById(User.Id,FCurrentPerson));
        await(FAppManager.DB.getDoctorById(User.Id,FCurrentDoctor));
        if FCurrentPerson.DateOfBirth <> 0 then
          geboorteDatumStr := FormatDateTime('dd-mm-yyyy', FCurrentPerson.DateOfBirth)
        else
          geboorteDatumStr := '-';

        if FCurrentDoctor.IsEnabledInShifts then
          isActiefStr := 'Ja'
        else
          isActiefStr := 'Nee';

        sb.AppendLine('<tr style="cursor: pointer;" onmouseover="this.style.backgroundColor=''#f1f1f1''" onmouseout="this.style.backgroundColor=''''">');
        sb.AppendFormat('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>', [User.Id]).AppendLine;
        sb.AppendFormat('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>', [User.Email]).AppendLine;
        sb.AppendFormat('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>', ['***']).AppendLine; // Don't show actual password
        sb.AppendFormat('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>', [FCurrentPerson.FirstName]).AppendLine;
        sb.AppendFormat('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>', [FCurrentPerson.LastName]).AppendLine;
        sb.AppendFormat('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>', [geboorteDatumStr]).AppendLine;
        sb.AppendFormat('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>', [FCurrentDoctor.RizivNumber]).AppendLine;
        sb.AppendFormat('  <td style="border: 1px solid #ddd; padding: 8px;">%s</td>', [isActiefStr]).AppendLine;
        sb.AppendLine('  <td style="border: 1px solid #ddd; padding: 8px;">');
        sb.AppendFormat('    <button id="%s" class="edit-button btn btn-warning" style="margin-right: 5px;">Bewerken</button>', [User.Id]).AppendLine;
        sb.AppendFormat('    <button id="%s" class="delete-button btn btn-danger">Verwijderen</button>', [User.Id]).AppendLine;
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

procedure TFormUser.showNewUser(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  FFormSection.classList.remove('d-none');
  FFormSection.setAttribute('method', 'post');
  // Clear form fields
//  document.getElementById('edtEmail').AsElement.setAttribute('value', '');
//  document.getElementById('edtPassword').AsElement.setAttribute('value', '');
//  document.getElementById('edtVoornaam').AsElement.setAttribute('value', '');
//  document.getElementById('edtAchternaam').AsElement.setAttribute('value', '');
//  document.getElementById('edtGeboorteDatum').AsElement.setAttribute('value', '');
//  document.getElementById('edtRiziv').AsElement.setAttribute('value', '');
//  document.getElementById('edtIsActiefStafflid').AsElement.setAttribute('value', 'true');
end;

procedure TFormUser.hideFormSection;
begin
  FFormSection.classList.add('d-none');
  if not FFormSection.getAttribute('method').Equals('') then
    FFormSection.setAttribute('method', '');
end;

procedure TFormUser.aclacSaveExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  userId: string;
  email, password, voornaam, achternaam, riziv: string;
  geboorteDatum: TDateTime;
  isActiefStaflid: Boolean;
begin
//  try
//    // Get form values
//    email := document.getElementById('edtEmail').AsElement.getAttribute('value');
//    password := document.getElementById('edtPassword').AsElement.getAttribute('value');
//    voornaam := document.getElementById('edtVoornaam').AsElement.getAttribute('value');
//    achternaam := document.getElementById('edtAchternaam').AsElement.getAttribute('value');
//    riziv := document.getElementById('edtRiziv').AsElement.getAttribute('value');
//    isActiefStaflid := document.getElementById('edtIsActiefStafflid').AsElement.getAttribute('value') = 'true';
//
//    if document.getElementById('edtGeboorteDatum').AsElement.getAttribute('value') <> '' then
//      geboorteDatum := StrToDateTime(document.getElementById('edtGeboorteDatum').AsElement.getAttribute('value'))
//    else
//      geboorteDatum := 0;
//
//    if FFormSection.getAttribute('method') = 'put' then
//    begin
//      userId := FFormSection.id;
//      await(FAppManager.DB.PutUser(userId, email, password, voornaam, achternaam, geboorteDatum, riziv, isActiefStaflid));
//    end
//    else
//    begin
//      await(FAppManager.DB.PostUser(email, password, voornaam, achternaam, geboorteDatum, riziv, isActiefStaflid));
//    end;
//
//    await(getUserList);
//    buildTable;
//    hideFormSection;
//  except
//    on e: exception do
//      FAppManager.ShowToast('Er ging iets mis bij opslaan: ' + e.Message);
//  end;
end;

procedure TFormUser.aclacHideNewUserExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
begin
  hideFormSection;
end;

procedure TFormUser.aclacEditExecute(Sender: TObject; Element: TJSHTMLElementRecord; Event: TJSEventParameter);
var
  userId: string;
  toUpdateUser: TUser;
begin
//  FFormSection.setAttribute('method', 'put');
//  FFormSection.classList.remove('d-none');
//  userId := Element.Element.id;
//  FFormSection.setAttribute('id', userId);
//
//  await(FAppManager.DB.getUserById(userId, toUpdateUser));
//
//  document.getElementById('edtEmail').AsElement.setAttribute('value', toUpdateUser.Email);
//  document.getElementById('edtPassword').AsElement.setAttribute('value', ''); // Don't populate password
//  document.getElementById('edtVoornaam').AsElement.setAttribute('value', toUpdateUser.Voornaam);
//  document.getElementById('edtAchternaam').AsElement.setAttribute('value', toUpdateUser.Achternaam);
//  if toUpdateUser.GeboorteDatum <> 0 then
//    document.getElementById('edtGeboorteDatum').AsElement.setAttribute('value', FormatDateTime('yyyy-mm-dd', toUpdateUser.GeboorteDatum));
//  document.getElementById('edtRiziv').AsElement.setAttribute('value', toUpdateUser.Riziv);
//  if toUpdateUser.IsActiefStaflid then
//    document.getElementById('edtIsActiefStafflid').AsElement.setAttribute('value', 'true')
//  else
//    document.getElementById('edtIsActiefStafflid').AsElement.setAttribute('value', 'false');
end;

procedure TFormUser.WebFormCreate(Sender: TObject);
begin
  FAppManager := TAppManager.GetInstance;
  FUserList := TList<TUser>.create;
  FFormSection := document.getElementById('formSection');
  await(getUserList);
  buildTable;
end;

procedure TFormUser.WebFormDestroy(Sender: TObject);
begin
  FUserList.Free;
end;

end.
