unit HealthKitWrapper;

interface

{$IF DEFINED(IOS)}

const
  LIB_HEALTHKIT_WRAPPER = 'libHealthKitWrapper.a';

  // Error Codes
  HKW_SUCCESS             = 0;
  HKW_ERROR_NOT_AVAILABLE = -1;
  HKW_ERROR_NOT_AUTHORIZED = -2;
  HKW_ERROR_NOT_INITIALIZED = -3;
  HKW_ERROR_OPERATION_PENDING = -4;
  HKW_ERROR_NO_DATA       = -5;
  HKW_ERROR_INVALID_PARAM = -6;
  HKW_ERROR_QUERY_FAILED  = -7;
  HKW_ERROR_UNKNOWN       = -99;

  // Operation Status
  HKW_STATUS_IDLE      = 0;
  HKW_STATUS_PENDING   = 1;
  HKW_STATUS_COMPLETED = 2;
  HKW_STATUS_ERROR     = 3;

type
  THKWHeartRateSample = packed record
    bpm: Double;
    timestamp: Double;
    sourceDeviceType: Integer;  // 0=unknown, 1=watch, 2=phone, 3=other
  end;
  PHKWHeartRateSample = ^THKWHeartRateSample;

  THKWHeartRateVariabilitySample = packed record
    sdnn: Double;               // SDNN value in milliseconds
    timestamp: Double;
    sourceDeviceType: Integer;  // 0=unknown, 1=watch, 2=phone, 3=other
  end;
  PHKWHeartRateVariabilitySample = ^THKWHeartRateVariabilitySample;

// Initialization
function HKW_Initialize: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_Initialize';

procedure HKW_Shutdown; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_Shutdown';

function HKW_IsHealthKitAvailable: Boolean; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_IsHealthKitAvailable';

// Authorization
function HKW_RequestAuthorization: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_RequestAuthorization';

function HKW_GetAuthorizationStatus: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetAuthorizationStatus';

function HKW_GetAuthorizationResult(outErrorCode: PInteger): Boolean; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetAuthorizationResult';

// Latest Heart Rate
function HKW_QueryLatestHeartRate: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_QueryLatestHeartRate';

function HKW_GetLatestHeartRateStatus: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetLatestHeartRateStatus';

function HKW_GetLatestHeartRateResult(outSample: PHKWHeartRateSample;
  outErrorCode: PInteger): Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetLatestHeartRateResult';

// Historical Heart Rate
function HKW_QueryHeartRateRange(startTimestamp, endTimestamp: Double;
  maxResults: Integer): Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_QueryHeartRateRange';

function HKW_GetHeartRateRangeStatus: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetHeartRateRangeStatus';

function HKW_GetHeartRateRangeCount: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetHeartRateRangeCount';

function HKW_GetHeartRateRangeResult(index: Integer;
  outSample: PHKWHeartRateSample; outErrorCode: PInteger): Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetHeartRateRangeResult';

procedure HKW_ClearHeartRateRangeResults; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_ClearHeartRateRangeResults';

// Latest Heart Rate Variability (HRV)
function HKW_QueryLatestHRV: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_QueryLatestHRV';

function HKW_GetLatestHRVStatus: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetLatestHRVStatus';

function HKW_GetLatestHRVResult(outSample: PHKWHeartRateVariabilitySample;
  outErrorCode: PInteger): Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetLatestHRVResult';

// Historical Heart Rate Variability (HRV)
function HKW_QueryHRVRange(startTimestamp, endTimestamp: Double;
  maxResults: Integer): Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_QueryHRVRange';

function HKW_GetHRVRangeStatus: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetHRVRangeStatus';

function HKW_GetHRVRangeCount: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetHRVRangeCount';

function HKW_GetHRVRangeResult(index: Integer;
  outSample: PHKWHeartRateVariabilitySample; outErrorCode: PInteger): Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetHRVRangeResult';

procedure HKW_ClearHRVRangeResults; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_ClearHRVRangeResults';

// Streaming Heart Rate
function HKW_StartHeartRateStreaming: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_StartHeartRateStreaming';

function HKW_StopHeartRateStreaming: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_StopHeartRateStreaming';

function HKW_IsHeartRateStreaming: Boolean; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_IsHeartRateStreaming';

function HKW_GetStreamingHeartRateCount: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetStreamingHeartRateCount';

function HKW_ReadStreamingHeartRates(outSamples: PHKWHeartRateSample;
  maxCount: Integer; outActualCount: PInteger): Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_ReadStreamingHeartRates';

// Watch Connectivity
function HKW_IsWatchSupported: Boolean; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_IsWatchSupported';

function HKW_IsWatchReachable: Boolean; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_IsWatchReachable';

function HKW_SendWatchCommand(command: PAnsiChar): Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_SendWatchCommand';

function HKW_GetWatchHeartRateCount: Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetWatchHeartRateCount';

function HKW_ReadWatchHeartRates(outSamples: PHKWHeartRateSample;
  maxCount: Integer; outActualCount: PInteger): Integer; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_ReadWatchHeartRates';

// Utility
function HKW_GetLastErrorMessage: PAnsiChar; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_GetLastErrorMessage';

procedure HKW_Reset; cdecl;
  external LIB_HEALTHKIT_WRAPPER name 'HKW_Reset';

// Helper function to convert error code to string
function HKWErrorToString(ErrorCode: Integer): string;

{$ENDIF}

implementation

{$IF DEFINED(IOS)}
uses
  System.SysUtils;

function HKWErrorToString(ErrorCode: Integer): string;
begin
  case ErrorCode of
    HKW_SUCCESS:             Result := 'Success';
    HKW_ERROR_NOT_AVAILABLE: Result := 'HealthKit not available';
    HKW_ERROR_NOT_AUTHORIZED: Result := 'Not authorized';
    HKW_ERROR_NOT_INITIALIZED: Result := 'Not initialized';
    HKW_ERROR_OPERATION_PENDING: Result := 'Operation pending';
    HKW_ERROR_NO_DATA:       Result := 'No data';
    HKW_ERROR_INVALID_PARAM: Result := 'Invalid parameter';
    HKW_ERROR_QUERY_FAILED:  Result := 'Query failed';
    HKW_ERROR_UNKNOWN:       Result := 'Unknown error';
  else
    Result := 'Error ' + IntToStr(ErrorCode);
  end;
end;

{$ENDIF}

end.
