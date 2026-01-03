//
//  HealthKitWrapper.h
//  HealthKitWrapper
//
//  C-compatible wrapper around HealthKit for Delphi FFI
//

#ifndef HealthKitWrapper_h
#define HealthKitWrapper_h

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// MARK: - Error Codes

typedef enum {
    HKW_SUCCESS = 0,
    HKW_ERROR_NOT_AVAILABLE = -1,
    HKW_ERROR_NOT_AUTHORIZED = -2,
    HKW_ERROR_NOT_INITIALIZED = -3,
    HKW_ERROR_OPERATION_PENDING = -4,
    HKW_ERROR_NO_DATA = -5,
    HKW_ERROR_INVALID_PARAM = -6,
    HKW_ERROR_QUERY_FAILED = -7,
    HKW_ERROR_UNKNOWN = -99
} HKWErrorCode;

// MARK: - Operation Status

typedef enum {
    HKW_STATUS_IDLE = 0,
    HKW_STATUS_PENDING = 1,
    HKW_STATUS_COMPLETED = 2,
    HKW_STATUS_ERROR = 3
} HKWOperationStatus;

// MARK: - Data Structures

typedef struct {
    double bpm;
    double timestamp;
    int sourceDeviceType;  // 0=unknown, 1=watch, 2=phone, 3=other
} HKWHeartRateSample;

// MARK: - Initialization

int HKW_Initialize(void);
void HKW_Shutdown(void);
bool HKW_IsHealthKitAvailable(void);

// MARK: - Authorization

int HKW_RequestAuthorization(void);
HKWOperationStatus HKW_GetAuthorizationStatus(void);
bool HKW_GetAuthorizationResult(int* outErrorCode);

// MARK: - Latest Heart Rate

int HKW_QueryLatestHeartRate(void);
HKWOperationStatus HKW_GetLatestHeartRateStatus(void);
int HKW_GetLatestHeartRateResult(HKWHeartRateSample* outSample, int* outErrorCode);

// MARK: - Historical Heart Rate

int HKW_QueryHeartRateRange(double startTimestamp, double endTimestamp, int maxResults);
HKWOperationStatus HKW_GetHeartRateRangeStatus(void);
int HKW_GetHeartRateRangeCount(void);
int HKW_GetHeartRateRangeResult(int index, HKWHeartRateSample* outSample, int* outErrorCode);
void HKW_ClearHeartRateRangeResults(void);

// MARK: - Streaming Heart Rate

int HKW_StartHeartRateStreaming(void);
int HKW_StopHeartRateStreaming(void);
bool HKW_IsHeartRateStreaming(void);
int HKW_GetStreamingHeartRateCount(void);
int HKW_ReadStreamingHeartRates(HKWHeartRateSample* outSamples, int maxCount, int* outActualCount);

// MARK: - Watch Connectivity

bool HKW_IsWatchSupported(void);
bool HKW_IsWatchReachable(void);
int HKW_SendWatchCommand(const char* command);
int HKW_GetWatchHeartRateCount(void);
int HKW_ReadWatchHeartRates(HKWHeartRateSample* outSamples, int maxCount, int* outActualCount);

// MARK: - Utility

const char* HKW_GetLastErrorMessage(void);
void HKW_Reset(void);

#ifdef __cplusplus
}
#endif

#endif /* HealthKitWrapper_h */
