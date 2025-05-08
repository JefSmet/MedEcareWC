unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, WEBLib.WebCtrls, Vcl.StdCtrls,
  WEBLib.StdCtrls, WEBLib.ExtCtrls, System.JSON, System.DateUtils, Types, Vcl.Grids, WEBLib.Grids, WEBLib.Calendar,
  WEBLib.ComCtrls;

type
  TLeaveType = (ltVacation, ltSick, ltPersonal, ltOther);
  TLeaveStatus = (lsPending, lsApproved, lsRejected);

  TLeaveRequest = class
  private
    FId: string;
    FUserId: string;
    FUserName: string;
    FStartDate: TDateTime;
    FEndDate: TDateTime;
    FLeaveType: TLeaveType;
    FReason: string;
    FStatus: TLeaveStatus;
    FCreatedAt: TDateTime;
  public
    property Id: string read FId write FId;
    property UserId: string read FUserId write FUserId;
    property UserName: string read FUserName write FUserName;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property EndDate: TDateTime read FEndDate write FEndDate;
    property LeaveType: TLeaveType read FLeaveType write FLeaveType;
    property Reason: string read FReason write FReason;
    property Status: TLeaveStatus read FStatus write FStatus;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;

  TestForm = class(TWebForm)
    pnlMain: TWebPanel;
    tabControl: TWebPageControl;
    tabCalendar: TWebTabSheet;
    tabList: TWebTabSheet;
    tabRequest: TWebTabSheet;
    calLeave: TWebCalendar;
    gridLeaves: TWebStringGrid;
    pnlRequest: TWebPanel;
    lblEmployee: TWebLabel;
    cmbEmployee: TWebComboBox;
    lblStartDate: TWebLabel;
    lblEndDate: TWebLabel;
    lblType: TWebLabel;
    cmbType: TWebComboBox;
    lblReason: TWebLabel;
    memoReason: TWebMemo;
    btnSubmit: TWebButton;
    procedure WebFormCreate(Sender: TObject);
    procedure btnSubmitClick(Sender: TObject);
    procedure calLeaveDrawCell(Sender: TObject; ACol, ARow: Integer;
      var Style: THTMLStyle; var HTML: string);
    procedure gridLeavesGetCellColor(Sender: TObject; ACol, ARow: Integer;
      var AColor: TColor);
  private
    FLeaveRequests: TList<TLeaveRequest>;
    procedure InitializeControls;
    procedure LoadMockData;
    procedure UpdateCalendar;
    procedure UpdateGrid;
    function CheckForConflicts(UserId: string; StartDate, EndDate: TDateTime): Boolean;
    procedure AddLeaveRequest(LeaveRequest: TLeaveRequest);
  public
    destructor Destroy; override;
  end;

var
  estForm: TestForm;

implementation

{$R *.dfm}

procedure TestForm.WebFormCreate(Sender: TObject);
begin
  FLeaveRequests := TList<TLeaveRequest>.Create;
  InitializeControls;
  LoadMockData;
  UpdateCalendar;
  UpdateGrid;
end;

procedure TestForm.InitializeControls;
begin
  // Initialize tab control
  tabControl.ActivePage := tabCalendar;

  // Setup leave type combo
  cmbType.Items.Clear;
  cmbType.Items.Add('Vacation');
  cmbType.Items.Add('Sick Leave');
  cmbType.Items.Add('Personal');
  cmbType.Items.Add('Other');
  cmbType.ItemIndex := 0;

  // Setup employee combo (mock data)
  cmbEmployee.Items.Clear;
  cmbEmployee.Items.Add('John Doe');
  cmbEmployee.Items.Add('Jane Smith');
  cmbEmployee.ItemIndex := 0;

  // Setup grid
  with gridLeaves do
  begin
    ColCount := 6;
    RowCount := 1;
    Cells[0, 0] := 'Employee';
    Cells[1, 0] := 'Start Date';
    Cells[2, 0] := 'End Date';
    Cells[3, 0] := 'Type';
    Cells[4, 0] := 'Reason';
    Cells[5, 0] := 'Status';
  end;
end;

procedure TestForm.LoadMockData;
var
  Leave: TLeaveRequest;
begin
  // Add some mock leave requests
  Leave := TLeaveRequest.Create;
  Leave.Id := TGUID.NewGuid.ToString;
  Leave.UserId := '1';
  Leave.UserName := 'John Doe';
  Leave.StartDate := EncodeDate(2025, 6, 10);
  Leave.EndDate := EncodeDate(2025, 6, 15);
  Leave.LeaveType := ltVacation;
  Leave.Reason := 'Summer holiday';
  Leave.Status := lsApproved;
  Leave.CreatedAt := Now;
  FLeaveRequests.Add(Leave);

  // Add more mock data as needed...
end;

procedure TestForm.UpdateCalendar;
begin
  calLeave.Invalidate;
end;

procedure TestForm.calLeaveDrawCell(Sender: TObject; ACol, ARow: Integer;
  var Style: THTMLStyle; var HTML: string);
var
  CellDate: TDateTime;
  Leave: TLeaveRequest;
begin
  CellDate := calLeave.CellDate[ACol, ARow];

  for Leave in FLeaveRequests do
  begin
    if (CellDate >= Leave.StartDate) and (CellDate <= Leave.EndDate) then
    begin
      case Leave.LeaveType of
        ltVacation: Style.BackgroundColor := 'rgba(59, 130, 246, 0.2)';
        ltSick: Style.BackgroundColor := 'rgba(239, 68, 68, 0.2)';
        ltPersonal: Style.BackgroundColor := 'rgba(139, 92, 246, 0.2)';
        ltOther: Style.BackgroundColor := 'rgba(107, 114, 128, 0.2)';
      end;
      HTML := HTML + Format('<div class="leave-item">%s</div>', [Leave.UserName]);
    end;
  end;
end;

procedure TestForm.UpdateGrid;
var
  Leave: TLeaveRequest;
  Row: Integer;
begin
  gridLeaves.RowCount := FLeaveRequests.Count + 1;

  for Row := 0 to FLeaveRequests.Count - 1 do
  begin
    Leave := FLeaveRequests[Row];
    gridLeaves.Cells[0, Row + 1] := Leave.UserName;
    gridLeaves.Cells[1, Row + 1] := DateToStr(Leave.StartDate);
    gridLeaves.Cells[2, Row + 1] := DateToStr(Leave.EndDate);
    gridLeaves.Cells[3, Row + 1] := TRttiEnumerationType.GetName(Leave.LeaveType);
    gridLeaves.Cells[4, Row + 1] := Leave.Reason;
    gridLeaves.Cells[5, Row + 1] := TRttiEnumerationType.GetName(Leave.Status);
  end;
end;

function TestForm.CheckForConflicts(UserId: string; StartDate, EndDate: TDateTime): Boolean;
var
  Leave: TLeaveRequest;
begin
  Result := False;
  for Leave in FLeaveRequests do
  begin
    if (Leave.UserId = UserId) and (Leave.Status <> lsRejected) then
    begin
      if (StartDate <= Leave.EndDate) and (EndDate >= Leave.StartDate) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TestForm.btnSubmitClick(Sender: TObject);
var
  NewLeave: TLeaveRequest;
begin
  if CheckForConflicts('1', dateStart.Date, dateEnd.Date) then
  begin
    ShowMessage('You already have leave scheduled during this period');
    Exit;
  end;

  NewLeave := TLeaveRequest.Create;
  NewLeave.Id := TGUID.NewGuid.ToString;
  NewLeave.UserId := '1';
  NewLeave.UserName := cmbEmployee.Text;
  NewLeave.StartDate := dateStart.Date;
  NewLeave.EndDate := dateEnd.Date;
  NewLeave.LeaveType := TLeaveType(cmbType.ItemIndex);
  NewLeave.Reason := memoReason.Text;
  NewLeave.Status := lsPending;
  NewLeave.CreatedAt := Now;

  AddLeaveRequest(NewLeave);
  ShowMessage('Leave request submitted successfully');

  // Reset form
  dateStart.Clear;
  dateEnd.Clear;
  memoReason.Clear;
end;

procedure TestForm.AddLeaveRequest(LeaveRequest: TLeaveRequest);
begin
  FLeaveRequests.Add(LeaveRequest);
  UpdateCalendar;
  UpdateGrid;
end;

procedure TestForm.gridLeavesGetCellColor(Sender: TObject; ACol, ARow: Integer;
  var AColor: TColor);
var
  Status: string;
begin
  if (ARow > 0) and (ACol = 5) then
  begin
    Status := gridLeaves.Cells[5, ARow];
    if Status = 'lsApproved' then
      AColor := RGB(34, 197, 94)
    else if Status = 'lsRejected' then
      AColor := RGB(239, 68, 68)
    else if Status = 'lsPending' then
      AColor := RGB(234, 179, 8);
  end;
end;

destructor TestForm.Destroy;
begin
  FLeaveRequests.Free;
  inherited;
end;

end.
