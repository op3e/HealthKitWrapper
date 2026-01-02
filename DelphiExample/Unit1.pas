unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.DateUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.Memo.Types
  {$IF DEFINED(IOS)}
  , HealthKitWrapper
  {$ENDIF}
  ;

type
  TPollOperation = (poNone, poAuthorization, poLatestHR, poRangeHR);

  TForm1 = class(TForm)
    MemoLog: TMemo;
    ToolBar1: TToolBar;
    LabelTitle: TLabel;
    VertScrollBox1: TVertScrollBox;
    LayoutButtons: TLayout;
    BtnInitialize: TButton;
    BtnShutdown: TButton;
    BtnRequestAuth: TButton;
    BtnQueryLatest: TButton;
    BtnQueryRange: TButton;
    BtnStream: TButton;
    BtnClearLog: TButton;
    TimerPoll: TTimer;
    TimerStream: TTimer;
    procedure BtnInitializeClick(Sender: TObject);
    procedure BtnShutdownClick(Sender: TObject);
    procedure BtnRequestAuthClick(Sender: TObject);
    procedure BtnQueryLatestClick(Sender: TObject);
    procedure BtnQueryRangeClick(Sender: TObject);
    procedure BtnStreamClick(Sender: TObject);
    procedure BtnClearLogClick(Sender: TObject);
    procedure TimerPollTimer(Sender: TObject);
    procedure TimerStreamTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FIsInitialized: Boolean;
    FIsStreaming: Boolean;
    FPollOperation: TPollOperation;
    procedure Log(const Msg: string);
    procedure UpdateButtonStates;
    {$IF DEFINED(IOS)}
    procedure CheckAuthStatus;
    procedure CheckLatestHRStatus;
    procedure CheckRangeHRStatus;
    function FormatTimestamp(Timestamp: Double): string;
    {$ENDIF}
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIsInitialized := False;
  FIsStreaming := False;
  FPollOperation := poNone;
  UpdateButtonStates;
  Log('Test app ready');
  {$IF DEFINED(IOS)}
  if HKW_IsHealthKitAvailable then
    Log('HealthKit: Available')
  else
    Log('HealthKit: NOT Available');
  {$ELSE}
  Log('Platform: Not iOS - wrapper disabled');
  {$ENDIF}
end;

procedure TForm1.Log(const Msg: string);
begin
  MemoLog.Lines.Add('[' + FormatDateTime('hh:nn:ss', Now) + '] ' + Msg);
  MemoLog.GoToTextEnd;
end;

procedure TForm1.UpdateButtonStates;
begin
  BtnRequestAuth.Enabled := FIsInitialized;
  BtnQueryLatest.Enabled := FIsInitialized;
  BtnQueryRange.Enabled := FIsInitialized;
  BtnStream.Enabled := FIsInitialized;

  if FIsStreaming then
    BtnStream.Text := 'Stop Streaming'
  else
    BtnStream.Text := 'Start Streaming';
end;

{$IF DEFINED(IOS)}
function TForm1.FormatTimestamp(Timestamp: Double): string;
var
  DT: TDateTime;
begin
  DT := UnixToDateTime(Trunc(Timestamp), False);
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', DT);
end;
{$ENDIF}

procedure TForm1.BtnInitializeClick(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Res: Integer;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  Res := HKW_Initialize;
  if Res = HKW_SUCCESS then
  begin
    FIsInitialized := True;
    Log('Initialize: SUCCESS');
  end
  else
    Log('Initialize: ERROR - ' + HKWErrorToString(Res));
  UpdateButtonStates;
  {$ELSE}
  Log('Initialize: Not available (not iOS)');
  {$ENDIF}
end;

procedure TForm1.BtnShutdownClick(Sender: TObject);
begin
  {$IF DEFINED(IOS)}
  TimerPoll.Enabled := False;
  TimerStream.Enabled := False;
  FPollOperation := poNone;
  if FIsStreaming then
    HKW_StopHeartRateStreaming;
  HKW_Shutdown;
  FIsInitialized := False;
  FIsStreaming := False;
  Log('Shutdown: Complete');
  UpdateButtonStates;
  {$ELSE}
  Log('Shutdown: Not available (not iOS)');
  {$ENDIF}
end;

procedure TForm1.BtnRequestAuthClick(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Res: Integer;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  Res := HKW_RequestAuthorization;
  if Res = HKW_SUCCESS then
  begin
    Log('Authorization: Request started, polling...');
    FPollOperation := poAuthorization;
    TimerPoll.Enabled := True;
  end
  else
    Log('Authorization: ERROR - ' + HKWErrorToString(Res));
  {$ELSE}
  Log('Authorization: Not available (not iOS)');
  {$ENDIF}
end;

procedure TForm1.BtnQueryLatestClick(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Res: Integer;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  Res := HKW_QueryLatestHeartRate;
  if Res = HKW_SUCCESS then
  begin
    Log('Latest HR: Query started, polling...');
    FPollOperation := poLatestHR;
    TimerPoll.Enabled := True;
  end
  else
    Log('Latest HR: ERROR - ' + HKWErrorToString(Res));
  {$ELSE}
  Log('Latest HR: Not available (not iOS)');
  {$ENDIF}
end;

procedure TForm1.BtnQueryRangeClick(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Res: Integer;
  EndTime, StartTime: Double;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  EndTime := DateTimeToUnix(Now, False);
  StartTime := EndTime - (24 * 60 * 60); // 24 hours ago

  Res := HKW_QueryHeartRateRange(StartTime, EndTime, 100);
  if Res = HKW_SUCCESS then
  begin
    Log('Range HR: Query started (last 24h), polling...');
    FPollOperation := poRangeHR;
    TimerPoll.Enabled := True;
  end
  else
    Log('Range HR: ERROR - ' + HKWErrorToString(Res));
  {$ELSE}
  Log('Range HR: Not available (not iOS)');
  {$ENDIF}
end;

procedure TForm1.BtnStreamClick(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Res: Integer;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  if FIsStreaming then
  begin
    TimerStream.Enabled := False;
    Res := HKW_StopHeartRateStreaming;
    FIsStreaming := False;
    if Res = HKW_SUCCESS then
      Log('Streaming: Stopped')
    else
      Log('Streaming: Stop error - ' + HKWErrorToString(Res));
  end
  else
  begin
    Res := HKW_StartHeartRateStreaming;
    if Res = HKW_SUCCESS then
    begin
      FIsStreaming := True;
      TimerStream.Enabled := True;
      Log('Streaming: Started');
    end
    else
      Log('Streaming: ERROR - ' + HKWErrorToString(Res));
  end;
  UpdateButtonStates;
  {$ELSE}
  Log('Streaming: Not available (not iOS)');
  {$ENDIF}
end;

procedure TForm1.BtnClearLogClick(Sender: TObject);
begin
  MemoLog.Lines.Clear;
end;

procedure TForm1.TimerPollTimer(Sender: TObject);
begin
  {$IF DEFINED(IOS)}
  case FPollOperation of
    poAuthorization: CheckAuthStatus;
    poLatestHR: CheckLatestHRStatus;
    poRangeHR: CheckRangeHRStatus;
  else
    TimerPoll.Enabled := False;
  end;
  {$ENDIF}
end;

{$IF DEFINED(IOS)}
procedure TForm1.CheckAuthStatus;
var
  Status: Integer;
  ErrorCode: Integer;
  Authorized: Boolean;
begin
  Status := HKW_GetAuthorizationStatus;

  if Status = HKW_STATUS_COMPLETED then
  begin
    TimerPoll.Enabled := False;
    FPollOperation := poNone;
    ErrorCode := 0;
    Authorized := HKW_GetAuthorizationResult(@ErrorCode);
    if Authorized then
      Log('Authorization: GRANTED')
    else
      Log('Authorization: DENIED');
  end
  else if Status = HKW_STATUS_ERROR then
  begin
    TimerPoll.Enabled := False;
    FPollOperation := poNone;
    Log('Authorization: ERROR - ' + string(AnsiString(HKW_GetLastErrorMessage)));
  end;
  // else still pending, keep polling
end;

procedure TForm1.CheckLatestHRStatus;
var
  Status: Integer;
  Sample: THKWHeartRateSample;
  ErrorCode: Integer;
  Count: Integer;
begin
  Status := HKW_GetLatestHeartRateStatus;

  if Status = HKW_STATUS_COMPLETED then
  begin
    TimerPoll.Enabled := False;
    FPollOperation := poNone;
    ErrorCode := 0;
    Count := HKW_GetLatestHeartRateResult(@Sample, @ErrorCode);
    if Count > 0 then
      Log(Format('Latest HR: %.0f BPM @ %s', [Sample.bpm, FormatTimestamp(Sample.timestamp)]))
    else
      Log('Latest HR: No data found');
  end
  else if Status = HKW_STATUS_ERROR then
  begin
    TimerPoll.Enabled := False;
    FPollOperation := poNone;
    Log('Latest HR: ERROR - ' + string(AnsiString(HKW_GetLastErrorMessage)));
  end;
end;

procedure TForm1.CheckRangeHRStatus;
var
  Status: Integer;
  Count, i: Integer;
  Sample: THKWHeartRateSample;
  ErrorCode: Integer;
  ShowCount: Integer;
begin
  Status := HKW_GetHeartRateRangeStatus;

  if Status = HKW_STATUS_COMPLETED then
  begin
    TimerPoll.Enabled := False;
    FPollOperation := poNone;
    Count := HKW_GetHeartRateRangeCount;
    Log(Format('Range HR: Found %d samples', [Count]));

    ShowCount := Count;
    if ShowCount > 5 then
      ShowCount := 5;

    for i := 0 to ShowCount - 1 do
    begin
      ErrorCode := 0;
      if HKW_GetHeartRateRangeResult(i, @Sample, @ErrorCode) = HKW_SUCCESS then
        Log(Format('  [%d] %.0f BPM @ %s', [i, Sample.bpm, FormatTimestamp(Sample.timestamp)]));
    end;

    if Count > 5 then
      Log(Format('  ... and %d more', [Count - 5]));

    HKW_ClearHeartRateRangeResults;
  end
  else if Status = HKW_STATUS_ERROR then
  begin
    TimerPoll.Enabled := False;
    FPollOperation := poNone;
    Log('Range HR: ERROR - ' + string(AnsiString(HKW_GetLastErrorMessage)));
  end;
end;
{$ENDIF}

procedure TForm1.TimerStreamTimer(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Count, ActualCount, i: Integer;
  Samples: array[0..31] of THKWHeartRateSample;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  Count := HKW_GetStreamingHeartRateCount;
  if Count > 0 then
  begin
    if Count > 32 then
      Count := 32;
    ActualCount := 0;
    if HKW_ReadStreamingHeartRates(@Samples[0], Count, @ActualCount) = HKW_SUCCESS then
    begin
      for i := 0 to ActualCount - 1 do
        Log(Format('STREAM: %.0f BPM', [Samples[i].bpm]));
    end;
  end;
  {$ENDIF}
end;

end.
