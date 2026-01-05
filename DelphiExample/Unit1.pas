unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.DateUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.Memo.Types,
  FMX.TabControl
  {$IF DEFINED(IOS)}
  , HealthKitWrapper
  {$ENDIF}
  ;

type
  TPollOperation = (poNone, poAuthorization, poLatestHR, poRangeHR, poLatestHRV, poRangeHRV);

  TForm1 = class(TForm)
    MemoLog: TMemo;
    ToolBar1: TToolBar;
    LabelTitle: TLabel;
    TabControl1: TTabControl;
    TabSettings: TTabItem;
    TabWatch: TTabItem;
    TabHeartRate: TTabItem;
    TabHRV: TTabItem;
    BtnInitialize: TButton;
    BtnShutdown: TButton;
    BtnRequestAuth: TButton;
    BtnClearLog: TButton;
    BtnWatchStatus: TButton;
    BtnWatch: TButton;
    BtnQueryLatest: TButton;
    BtnQueryRange: TButton;
    BtnStream: TButton;
    BtnQueryLatestHRV: TButton;
    BtnQueryHRVRange: TButton;
    TimerPoll: TTimer;
    TimerStream: TTimer;
    TimerWatch: TTimer;
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
    procedure BtnWatchStatusClick(Sender: TObject);
    procedure BtnWatchClick(Sender: TObject);
    procedure TimerWatchTimer(Sender: TObject);
    procedure BtnQueryLatestHRVClick(Sender: TObject);
    procedure BtnQueryHRVRangeClick(Sender: TObject);
  private
    FIsInitialized: Boolean;
    FIsStreaming: Boolean;
    FIsWatchMonitoring: Boolean;
    FPollOperation: TPollOperation;
    procedure Log(const Msg: string);
    procedure UpdateButtonStates;
    {$IF DEFINED(IOS)}
    procedure CheckAuthStatus;
    procedure CheckLatestHRStatus;
    procedure CheckRangeHRStatus;
    procedure CheckLatestHRVStatus;
    procedure CheckRangeHRVStatus;
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
  FIsWatchMonitoring := False;
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
  BtnWatchStatus.Enabled := FIsInitialized;
  BtnWatch.Enabled := FIsInitialized;
  BtnQueryLatestHRV.Enabled := FIsInitialized;
  BtnQueryHRVRange.Enabled := FIsInitialized;

  if FIsStreaming then
    BtnStream.Text := 'Stop Streaming'
  else
    BtnStream.Text := 'Start Streaming';

  if FIsWatchMonitoring then
    BtnWatch.Text := 'Stop Watch'
  else
    BtnWatch.Text := 'Start Watch';
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
  TimerWatch.Enabled := False;
  FPollOperation := poNone;
  if FIsStreaming then
    HKW_StopHeartRateStreaming;
  if FIsWatchMonitoring then
    HKW_SendWatchCommand('stopMonitoring');
  HKW_Shutdown;
  FIsInitialized := False;
  FIsStreaming := False;
  FIsWatchMonitoring := False;
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

procedure TForm1.BtnQueryLatestHRVClick(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Res: Integer;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  Res := HKW_QueryLatestHRV;
  if Res = HKW_SUCCESS then
  begin
    Log('Latest HRV: Query started, polling...');
    FPollOperation := poLatestHRV;
    TimerPoll.Enabled := True;
  end
  else
    Log('Latest HRV: ERROR - ' + HKWErrorToString(Res));
  {$ELSE}
  Log('Latest HRV: Not available (not iOS)');
  {$ENDIF}
end;

procedure TForm1.BtnQueryHRVRangeClick(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Res: Integer;
  EndTime, StartTime: Double;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  EndTime := DateTimeToUnix(Now, False);
  StartTime := EndTime - (24 * 60 * 60); // 24 hours ago

  Res := HKW_QueryHRVRange(StartTime, EndTime, 100);
  if Res = HKW_SUCCESS then
  begin
    Log('Range HRV: Query started (last 24h), polling...');
    FPollOperation := poRangeHRV;
    TimerPoll.Enabled := True;
  end
  else
    Log('Range HRV: ERROR - ' + HKWErrorToString(Res));
  {$ELSE}
  Log('Range HRV: Not available (not iOS)');
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

procedure TForm1.BtnWatchStatusClick(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Supported, Reachable: Boolean;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  Supported := HKW_IsWatchSupported;
  Reachable := HKW_IsWatchReachable;

  Log('Watch Supported: ' + BoolToStr(Supported, True));
  Log('Watch Reachable: ' + BoolToStr(Reachable, True));

  if not Supported then
    Log('WARNING: WatchConnectivity not supported')
  else if not Reachable then
    Log('WARNING: Watch not reachable - ensure watch app is running')
  else
    Log('Watch is connected and reachable');
  {$ELSE}
  Log('Watch Status: Not available (not iOS)');
  {$ENDIF}
end;

procedure TForm1.BtnWatchClick(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Res: Integer;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  if FIsWatchMonitoring then
  begin
    TimerWatch.Enabled := False;
    Res := HKW_SendWatchCommand('stopMonitoring');
    FIsWatchMonitoring := False;
    if Res = HKW_SUCCESS then
      Log('Watch: Sent stopMonitoring')
    else
      Log('Watch: Stop error - ' + HKWErrorToString(Res));
  end
  else
  begin
    Res := HKW_SendWatchCommand('startMonitoring');
    if Res = HKW_SUCCESS then
    begin
      FIsWatchMonitoring := True;
      TimerWatch.Enabled := True;
      Log('Watch: Sent startMonitoring');
    end
    else
    begin
      Log('Watch: ERROR - ' + HKWErrorToString(Res));
      Log('  -> ' + string(AnsiString(HKW_GetLastErrorMessage)));
    end;
  end;
  UpdateButtonStates;
  {$ELSE}
  Log('Watch: Not available (not iOS)');
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
    poLatestHRV: CheckLatestHRVStatus;
    poRangeHRV: CheckRangeHRVStatus;
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

procedure TForm1.CheckLatestHRVStatus;
var
  Status: Integer;
  Sample: THKWHeartRateVariabilitySample;
  ErrorCode: Integer;
  Count: Integer;
begin
  Status := HKW_GetLatestHRVStatus;

  if Status = HKW_STATUS_COMPLETED then
  begin
    TimerPoll.Enabled := False;
    FPollOperation := poNone;
    ErrorCode := 0;
    Count := HKW_GetLatestHRVResult(@Sample, @ErrorCode);
    if Count > 0 then
      Log(Format('Latest HRV: %.1f ms (SDNN) @ %s', [Sample.sdnn, FormatTimestamp(Sample.timestamp)]))
    else
      Log('Latest HRV: No data found');
  end
  else if Status = HKW_STATUS_ERROR then
  begin
    TimerPoll.Enabled := False;
    FPollOperation := poNone;
    Log('Latest HRV: ERROR - ' + string(AnsiString(HKW_GetLastErrorMessage)));
  end;
end;

procedure TForm1.CheckRangeHRVStatus;
var
  Status: Integer;
  Count, i: Integer;
  Sample: THKWHeartRateVariabilitySample;
  ErrorCode: Integer;
  ShowCount: Integer;
begin
  Status := HKW_GetHRVRangeStatus;

  if Status = HKW_STATUS_COMPLETED then
  begin
    TimerPoll.Enabled := False;
    FPollOperation := poNone;
    Count := HKW_GetHRVRangeCount;
    Log(Format('Range HRV: Found %d samples', [Count]));

    ShowCount := Count;
    if ShowCount > 5 then
      ShowCount := 5;

    for i := 0 to ShowCount - 1 do
    begin
      ErrorCode := 0;
      if HKW_GetHRVRangeResult(i, @Sample, @ErrorCode) = HKW_SUCCESS then
        Log(Format('  [%d] %.1f ms (SDNN) @ %s', [i, Sample.sdnn, FormatTimestamp(Sample.timestamp)]));
    end;

    if Count > 5 then
      Log(Format('  ... and %d more', [Count - 5]));

    HKW_ClearHRVRangeResults;
  end
  else if Status = HKW_STATUS_ERROR then
  begin
    TimerPoll.Enabled := False;
    FPollOperation := poNone;
    Log('Range HRV: ERROR - ' + string(AnsiString(HKW_GetLastErrorMessage)));
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

procedure TForm1.TimerWatchTimer(Sender: TObject);
{$IF DEFINED(IOS)}
var
  Count, ActualCount, i: Integer;
  Samples: array[0..31] of THKWHeartRateSample;
{$ENDIF}
begin
  {$IF DEFINED(IOS)}
  Count := HKW_GetWatchHeartRateCount;
  if Count > 0 then
  begin
    if Count > 32 then
      Count := 32;
    ActualCount := 0;
    if HKW_ReadWatchHeartRates(@Samples[0], Count, @ActualCount) = HKW_SUCCESS then
    begin
      for i := 0 to ActualCount - 1 do
        Log(Format('WATCH: %.0f BPM', [Samples[i].bpm]));
    end;
  end;
  {$ENDIF}
end;

end.
