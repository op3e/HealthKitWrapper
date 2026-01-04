//
//  HealthKitWrapper.m
//  HealthKitWrapper
//
//  C-compatible wrapper around HealthKit for Delphi FFI
//

#import <Foundation/Foundation.h>
#import <HealthKit/HealthKit.h>
#import <WatchConnectivity/WatchConnectivity.h>
#import "HealthKitWrapper.h"

#define STREAMING_BUFFER_MAX 100
#define WATCH_BUFFER_MAX 100

// MARK: - HKWManager Singleton

@interface HKWManager : NSObject <WCSessionDelegate>

@property (nonatomic, strong) HKHealthStore *healthStore;
@property (nonatomic, strong) WCSession *wcSession;

// Authorization state
@property (nonatomic, assign) HKWOperationStatus authStatus;
@property (nonatomic, assign) BOOL authResult;
@property (nonatomic, assign) int authErrorCode;

// Latest heart rate state
@property (nonatomic, assign) HKWOperationStatus latestHRStatus;
@property (nonatomic, assign) HKWHeartRateSample latestHRSample;
@property (nonatomic, assign) BOOL hasLatestHR;
@property (nonatomic, assign) int latestHRErrorCode;

// Range query state
@property (nonatomic, assign) HKWOperationStatus rangeHRStatus;
@property (nonatomic, strong) NSMutableArray *rangeHRSamples;
@property (nonatomic, assign) int rangeHRErrorCode;

// Latest HRV state
@property (nonatomic, assign) HKWOperationStatus latestHRVStatus;
@property (nonatomic, assign) HKWHeartRateVariabilitySample latestHRVSample;
@property (nonatomic, assign) BOOL hasLatestHRV;
@property (nonatomic, assign) int latestHRVErrorCode;

// HRV range query state
@property (nonatomic, assign) HKWOperationStatus rangeHRVStatus;
@property (nonatomic, strong) NSMutableArray *rangeHRVSamples;
@property (nonatomic, assign) int rangeHRVErrorCode;

// Streaming state
@property (nonatomic, strong) HKAnchoredObjectQuery *streamingQuery;
@property (nonatomic, strong) NSMutableArray *streamingBuffer;
@property (nonatomic, assign) BOOL isStreaming;

// Watch connectivity state
@property (nonatomic, strong) NSMutableArray *watchHeartRateBuffer;

// Error message
@property (nonatomic, strong) NSString *lastErrorMessage;

+ (instancetype)shared;
- (int)deviceTypeFromSource:(HKSourceRevision *)source;
- (void)processStreamingSamples:(NSArray<HKSample *> *)samples;

@end

@implementation HKWManager

+ (instancetype)shared {
    static HKWManager *instance = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        instance = [[HKWManager alloc] init];
    });
    return instance;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _rangeHRSamples = [NSMutableArray array];
        _rangeHRVSamples = [NSMutableArray array];
        _streamingBuffer = [NSMutableArray array];
        _watchHeartRateBuffer = [NSMutableArray array];
        _authStatus = HKW_STATUS_IDLE;
        _latestHRStatus = HKW_STATUS_IDLE;
        _rangeHRStatus = HKW_STATUS_IDLE;
        _latestHRVStatus = HKW_STATUS_IDLE;
        _rangeHRVStatus = HKW_STATUS_IDLE;

        // Setup WatchConnectivity
        if ([WCSession isSupported]) {
            _wcSession = [WCSession defaultSession];
            _wcSession.delegate = self;
            [_wcSession activateSession];
        }
    }
    return self;
}

// MARK: - WCSessionDelegate

- (void)session:(WCSession *)session activationDidCompleteWithState:(WCSessionActivationState)activationState error:(NSError *)error {
    if (error) {
        NSLog(@"WCSession activation failed: %@", error.localizedDescription);
    }
}

- (void)sessionDidBecomeInactive:(WCSession *)session {
    // Handle session becoming inactive
}

- (void)sessionDidDeactivate:(WCSession *)session {
    // Reactivate session
    [session activateSession];
}

- (void)session:(WCSession *)session didReceiveMessage:(NSDictionary<NSString *,id> *)message {
    NSString *type = message[@"type"];

    if ([type isEqualToString:@"heartRate"]) {
        NSNumber *bpm = message[@"bpm"];
        NSNumber *timestamp = message[@"timestamp"];

        if (bpm && timestamp) {
            HKWHeartRateSample sample;
            sample.bpm = [bpm doubleValue];
            sample.timestamp = [timestamp doubleValue];
            sample.sourceDeviceType = 1; // watch

            @synchronized(self.watchHeartRateBuffer) {
                [self.watchHeartRateBuffer addObject:[NSValue valueWithBytes:&sample objCType:@encode(HKWHeartRateSample)]];

                // Ring buffer behavior
                if (self.watchHeartRateBuffer.count > WATCH_BUFFER_MAX) {
                    [self.watchHeartRateBuffer removeObjectAtIndex:0];
                }
            }
        }
    }
}

- (int)deviceTypeFromSource:(HKSourceRevision *)source {
    if (!source || !source.productType) {
        return 0; // unknown
    }
    NSString *product = source.productType;
    if ([product containsString:@"Watch"]) {
        return 1; // watch
    } else if ([product containsString:@"iPhone"]) {
        return 2; // phone
    }
    return 3; // other
}

- (void)processStreamingSamples:(NSArray<HKSample *> *)samples {
    if (!samples || samples.count == 0) return;

    HKUnit *bpmUnit = [[HKUnit countUnit] unitDividedByUnit:[HKUnit minuteUnit]];

    @synchronized(self.streamingBuffer) {
        for (HKQuantitySample *sample in samples) {
            if (![sample isKindOfClass:[HKQuantitySample class]]) continue;

            HKWHeartRateSample cSample;
            cSample.bpm = [sample.quantity doubleValueForUnit:bpmUnit];
            cSample.timestamp = [sample.endDate timeIntervalSince1970];
            cSample.sourceDeviceType = [self deviceTypeFromSource:sample.sourceRevision];

            [self.streamingBuffer addObject:[NSValue valueWithBytes:&cSample objCType:@encode(HKWHeartRateSample)]];

            // Ring buffer behavior
            if (self.streamingBuffer.count > STREAMING_BUFFER_MAX) {
                [self.streamingBuffer removeObjectAtIndex:0];
            }
        }
    }
}

@end

// MARK: - C API Implementation

// MARK: Initialization

int HKW_Initialize(void) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        if (![HKHealthStore isHealthDataAvailable]) {
            mgr.lastErrorMessage = @"HealthKit not available on this device";
            return HKW_ERROR_NOT_AVAILABLE;
        }

        mgr.healthStore = [[HKHealthStore alloc] init];
        return HKW_SUCCESS;
    }
}

void HKW_Shutdown(void) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        // Stop streaming if active
        if (mgr.isStreaming && mgr.streamingQuery) {
            [mgr.healthStore stopQuery:mgr.streamingQuery];
            mgr.streamingQuery = nil;
            mgr.isStreaming = NO;
        }

        mgr.healthStore = nil;
        [mgr.rangeHRSamples removeAllObjects];
        [mgr.streamingBuffer removeAllObjects];
    }
}

bool HKW_IsHealthKitAvailable(void) {
    return [HKHealthStore isHealthDataAvailable];
}

// MARK: Authorization

int HKW_RequestAuthorization(void) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        if (!mgr.healthStore) {
            mgr.lastErrorMessage = @"Not initialized. Call HKW_Initialize first.";
            return HKW_ERROR_NOT_INITIALIZED;
        }

        if (mgr.authStatus == HKW_STATUS_PENDING) {
            return HKW_ERROR_OPERATION_PENDING;
        }

        mgr.authStatus = HKW_STATUS_PENDING;
        mgr.authResult = NO;
        mgr.authErrorCode = HKW_SUCCESS;

        HKQuantityType *heartRateType = [HKQuantityType quantityTypeForIdentifier:HKQuantityTypeIdentifierHeartRate];
        HKQuantityType *hrvType = [HKQuantityType quantityTypeForIdentifier:HKQuantityTypeIdentifierHeartRateVariabilitySDNN];
        NSSet *readTypes = [NSSet setWithObjects:heartRateType, hrvType, nil];

        [mgr.healthStore requestAuthorizationToShareTypes:nil
                                                readTypes:readTypes
                                               completion:^(BOOL success, NSError *error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                if (error) {
                    mgr.authStatus = HKW_STATUS_ERROR;
                    mgr.authErrorCode = HKW_ERROR_NOT_AUTHORIZED;
                    mgr.lastErrorMessage = error.localizedDescription;
                    mgr.authResult = NO;
                } else {
                    mgr.authStatus = HKW_STATUS_COMPLETED;
                    mgr.authResult = success;
                    mgr.authErrorCode = HKW_SUCCESS;
                }
            });
        }];

        return HKW_SUCCESS;
    }
}

HKWOperationStatus HKW_GetAuthorizationStatus(void) {
    return [HKWManager shared].authStatus;
}

bool HKW_GetAuthorizationResult(int* outErrorCode) {
    HKWManager *mgr = [HKWManager shared];
    if (outErrorCode) {
        *outErrorCode = mgr.authErrorCode;
    }
    return mgr.authResult;
}

// MARK: Latest Heart Rate

int HKW_QueryLatestHeartRate(void) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        if (!mgr.healthStore) {
            mgr.lastErrorMessage = @"Not initialized. Call HKW_Initialize first.";
            return HKW_ERROR_NOT_INITIALIZED;
        }

        if (mgr.latestHRStatus == HKW_STATUS_PENDING) {
            return HKW_ERROR_OPERATION_PENDING;
        }

        mgr.latestHRStatus = HKW_STATUS_PENDING;
        mgr.hasLatestHR = NO;
        mgr.latestHRErrorCode = HKW_SUCCESS;

        HKQuantityType *heartRateType = [HKQuantityType quantityTypeForIdentifier:HKQuantityTypeIdentifierHeartRate];
        NSSortDescriptor *sortByDate = [[NSSortDescriptor alloc] initWithKey:HKSampleSortIdentifierEndDate ascending:NO];

        HKSampleQuery *query = [[HKSampleQuery alloc]
            initWithSampleType:heartRateType
                     predicate:nil
                         limit:1
               sortDescriptors:@[sortByDate]
                resultsHandler:^(HKSampleQuery *query, NSArray *results, NSError *error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                if (error) {
                    mgr.latestHRStatus = HKW_STATUS_ERROR;
                    mgr.latestHRErrorCode = HKW_ERROR_QUERY_FAILED;
                    mgr.lastErrorMessage = error.localizedDescription;
                } else if (results.count == 0) {
                    mgr.latestHRStatus = HKW_STATUS_COMPLETED;
                    mgr.hasLatestHR = NO;
                    mgr.latestHRErrorCode = HKW_SUCCESS;
                } else {
                    HKQuantitySample *sample = results.firstObject;
                    HKUnit *bpmUnit = [[HKUnit countUnit] unitDividedByUnit:[HKUnit minuteUnit]];

                    HKWHeartRateSample hrSample;
                    hrSample.bpm = [sample.quantity doubleValueForUnit:bpmUnit];
                    hrSample.timestamp = [sample.endDate timeIntervalSince1970];
                    hrSample.sourceDeviceType = [mgr deviceTypeFromSource:sample.sourceRevision];
                    mgr.latestHRSample = hrSample;

                    mgr.hasLatestHR = YES;
                    mgr.latestHRStatus = HKW_STATUS_COMPLETED;
                    mgr.latestHRErrorCode = HKW_SUCCESS;
                }
            });
        }];

        [mgr.healthStore executeQuery:query];
        return HKW_SUCCESS;
    }
}

HKWOperationStatus HKW_GetLatestHeartRateStatus(void) {
    return [HKWManager shared].latestHRStatus;
}

int HKW_GetLatestHeartRateResult(HKWHeartRateSample* outSample, int* outErrorCode) {
    HKWManager *mgr = [HKWManager shared];

    if (outErrorCode) {
        *outErrorCode = mgr.latestHRErrorCode;
    }

    if (!mgr.hasLatestHR) {
        return 0;
    }

    if (outSample) {
        *outSample = mgr.latestHRSample;
    }

    return 1;
}

// MARK: Historical Heart Rate

int HKW_QueryHeartRateRange(double startTimestamp, double endTimestamp, int maxResults) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        if (!mgr.healthStore) {
            mgr.lastErrorMessage = @"Not initialized. Call HKW_Initialize first.";
            return HKW_ERROR_NOT_INITIALIZED;
        }

        if (mgr.rangeHRStatus == HKW_STATUS_PENDING) {
            return HKW_ERROR_OPERATION_PENDING;
        }

        if (startTimestamp >= endTimestamp) {
            mgr.lastErrorMessage = @"Invalid date range: start must be before end";
            return HKW_ERROR_INVALID_PARAM;
        }

        if (maxResults <= 0) {
            maxResults = 1000;
        }

        mgr.rangeHRStatus = HKW_STATUS_PENDING;
        [mgr.rangeHRSamples removeAllObjects];
        mgr.rangeHRErrorCode = HKW_SUCCESS;

        NSDate *startDate = [NSDate dateWithTimeIntervalSince1970:startTimestamp];
        NSDate *endDate = [NSDate dateWithTimeIntervalSince1970:endTimestamp];

        HKQuantityType *heartRateType = [HKQuantityType quantityTypeForIdentifier:HKQuantityTypeIdentifierHeartRate];
        NSPredicate *predicate = [HKQuery predicateForSamplesWithStartDate:startDate endDate:endDate options:HKQueryOptionStrictEndDate];
        NSSortDescriptor *sortByDate = [[NSSortDescriptor alloc] initWithKey:HKSampleSortIdentifierEndDate ascending:YES];

        HKSampleQuery *query = [[HKSampleQuery alloc]
            initWithSampleType:heartRateType
                     predicate:predicate
                         limit:maxResults
               sortDescriptors:@[sortByDate]
                resultsHandler:^(HKSampleQuery *query, NSArray *results, NSError *error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                if (error) {
                    mgr.rangeHRStatus = HKW_STATUS_ERROR;
                    mgr.rangeHRErrorCode = HKW_ERROR_QUERY_FAILED;
                    mgr.lastErrorMessage = error.localizedDescription;
                } else {
                    HKUnit *bpmUnit = [[HKUnit countUnit] unitDividedByUnit:[HKUnit minuteUnit]];

                    for (HKQuantitySample *sample in results) {
                        HKWHeartRateSample cSample;
                        cSample.bpm = [sample.quantity doubleValueForUnit:bpmUnit];
                        cSample.timestamp = [sample.endDate timeIntervalSince1970];
                        cSample.sourceDeviceType = [mgr deviceTypeFromSource:sample.sourceRevision];

                        [mgr.rangeHRSamples addObject:[NSValue valueWithBytes:&cSample objCType:@encode(HKWHeartRateSample)]];
                    }

                    mgr.rangeHRStatus = HKW_STATUS_COMPLETED;
                    mgr.rangeHRErrorCode = HKW_SUCCESS;
                }
            });
        }];

        [mgr.healthStore executeQuery:query];
        return HKW_SUCCESS;
    }
}

HKWOperationStatus HKW_GetHeartRateRangeStatus(void) {
    return [HKWManager shared].rangeHRStatus;
}

int HKW_GetHeartRateRangeCount(void) {
    return (int)[HKWManager shared].rangeHRSamples.count;
}

int HKW_GetHeartRateRangeResult(int index, HKWHeartRateSample* outSample, int* outErrorCode) {
    HKWManager *mgr = [HKWManager shared];

    if (outErrorCode) {
        *outErrorCode = mgr.rangeHRErrorCode;
    }

    if (index < 0 || index >= (int)mgr.rangeHRSamples.count) {
        return HKW_ERROR_INVALID_PARAM;
    }

    if (outSample) {
        NSValue *value = mgr.rangeHRSamples[index];
        [value getValue:outSample];
    }

    return HKW_SUCCESS;
}

void HKW_ClearHeartRateRangeResults(void) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];
        [mgr.rangeHRSamples removeAllObjects];
        mgr.rangeHRStatus = HKW_STATUS_IDLE;
    }
}

// MARK: Latest Heart Rate Variability (HRV)

int HKW_QueryLatestHRV(void) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        if (!mgr.healthStore) {
            mgr.lastErrorMessage = @"Not initialized. Call HKW_Initialize first.";
            return HKW_ERROR_NOT_INITIALIZED;
        }

        if (mgr.latestHRVStatus == HKW_STATUS_PENDING) {
            return HKW_ERROR_OPERATION_PENDING;
        }

        mgr.latestHRVStatus = HKW_STATUS_PENDING;
        mgr.hasLatestHRV = NO;
        mgr.latestHRVErrorCode = HKW_SUCCESS;

        HKQuantityType *hrvType = [HKQuantityType quantityTypeForIdentifier:HKQuantityTypeIdentifierHeartRateVariabilitySDNN];
        NSSortDescriptor *sortByDate = [[NSSortDescriptor alloc] initWithKey:HKSampleSortIdentifierEndDate ascending:NO];

        HKSampleQuery *query = [[HKSampleQuery alloc]
            initWithSampleType:hrvType
                     predicate:nil
                         limit:1
               sortDescriptors:@[sortByDate]
                resultsHandler:^(HKSampleQuery *query, NSArray *results, NSError *error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                if (error) {
                    mgr.latestHRVStatus = HKW_STATUS_ERROR;
                    mgr.latestHRVErrorCode = HKW_ERROR_QUERY_FAILED;
                    mgr.lastErrorMessage = error.localizedDescription;
                } else if (results.count == 0) {
                    mgr.latestHRVStatus = HKW_STATUS_COMPLETED;
                    mgr.hasLatestHRV = NO;
                    mgr.latestHRVErrorCode = HKW_SUCCESS;
                } else {
                    HKQuantitySample *sample = results.firstObject;
                    // HRV is stored in seconds, convert to milliseconds
                    HKUnit *secondUnit = [HKUnit secondUnit];
                    double sdnnSeconds = [sample.quantity doubleValueForUnit:secondUnit];
                    double sdnnMs = sdnnSeconds * 1000.0;

                    HKWHeartRateVariabilitySample hrvSample;
                    hrvSample.sdnn = sdnnMs;
                    hrvSample.timestamp = [sample.endDate timeIntervalSince1970];
                    hrvSample.sourceDeviceType = [mgr deviceTypeFromSource:sample.sourceRevision];
                    mgr.latestHRVSample = hrvSample;

                    mgr.hasLatestHRV = YES;
                    mgr.latestHRVStatus = HKW_STATUS_COMPLETED;
                    mgr.latestHRVErrorCode = HKW_SUCCESS;
                }
            });
        }];

        [mgr.healthStore executeQuery:query];
        return HKW_SUCCESS;
    }
}

HKWOperationStatus HKW_GetLatestHRVStatus(void) {
    return [HKWManager shared].latestHRVStatus;
}

int HKW_GetLatestHRVResult(HKWHeartRateVariabilitySample* outSample, int* outErrorCode) {
    HKWManager *mgr = [HKWManager shared];

    if (outErrorCode) {
        *outErrorCode = mgr.latestHRVErrorCode;
    }

    if (!mgr.hasLatestHRV) {
        return 0;
    }

    if (outSample) {
        *outSample = mgr.latestHRVSample;
    }

    return 1;
}

// MARK: Historical Heart Rate Variability (HRV)

int HKW_QueryHRVRange(double startTimestamp, double endTimestamp, int maxResults) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        if (!mgr.healthStore) {
            mgr.lastErrorMessage = @"Not initialized. Call HKW_Initialize first.";
            return HKW_ERROR_NOT_INITIALIZED;
        }

        if (mgr.rangeHRVStatus == HKW_STATUS_PENDING) {
            return HKW_ERROR_OPERATION_PENDING;
        }

        if (startTimestamp >= endTimestamp) {
            mgr.lastErrorMessage = @"Invalid date range: start must be before end";
            return HKW_ERROR_INVALID_PARAM;
        }

        if (maxResults <= 0) {
            maxResults = 1000;
        }

        mgr.rangeHRVStatus = HKW_STATUS_PENDING;
        [mgr.rangeHRVSamples removeAllObjects];
        mgr.rangeHRVErrorCode = HKW_SUCCESS;

        NSDate *startDate = [NSDate dateWithTimeIntervalSince1970:startTimestamp];
        NSDate *endDate = [NSDate dateWithTimeIntervalSince1970:endTimestamp];

        HKQuantityType *hrvType = [HKQuantityType quantityTypeForIdentifier:HKQuantityTypeIdentifierHeartRateVariabilitySDNN];
        NSPredicate *predicate = [HKQuery predicateForSamplesWithStartDate:startDate endDate:endDate options:HKQueryOptionStrictEndDate];
        NSSortDescriptor *sortByDate = [[NSSortDescriptor alloc] initWithKey:HKSampleSortIdentifierEndDate ascending:YES];

        HKSampleQuery *query = [[HKSampleQuery alloc]
            initWithSampleType:hrvType
                     predicate:predicate
                         limit:maxResults
               sortDescriptors:@[sortByDate]
                resultsHandler:^(HKSampleQuery *query, NSArray *results, NSError *error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                if (error) {
                    mgr.rangeHRVStatus = HKW_STATUS_ERROR;
                    mgr.rangeHRVErrorCode = HKW_ERROR_QUERY_FAILED;
                    mgr.lastErrorMessage = error.localizedDescription;
                } else {
                    // HRV is stored in seconds, convert to milliseconds
                    HKUnit *secondUnit = [HKUnit secondUnit];

                    for (HKQuantitySample *sample in results) {
                        double sdnnSeconds = [sample.quantity doubleValueForUnit:secondUnit];
                        double sdnnMs = sdnnSeconds * 1000.0;

                        HKWHeartRateVariabilitySample cSample;
                        cSample.sdnn = sdnnMs;
                        cSample.timestamp = [sample.endDate timeIntervalSince1970];
                        cSample.sourceDeviceType = [mgr deviceTypeFromSource:sample.sourceRevision];

                        [mgr.rangeHRVSamples addObject:[NSValue valueWithBytes:&cSample objCType:@encode(HKWHeartRateVariabilitySample)]];
                    }

                    mgr.rangeHRVStatus = HKW_STATUS_COMPLETED;
                    mgr.rangeHRVErrorCode = HKW_SUCCESS;
                }
            });
        }];

        [mgr.healthStore executeQuery:query];
        return HKW_SUCCESS;
    }
}

HKWOperationStatus HKW_GetHRVRangeStatus(void) {
    return [HKWManager shared].rangeHRVStatus;
}

int HKW_GetHRVRangeCount(void) {
    return (int)[HKWManager shared].rangeHRVSamples.count;
}

int HKW_GetHRVRangeResult(int index, HKWHeartRateVariabilitySample* outSample, int* outErrorCode) {
    HKWManager *mgr = [HKWManager shared];

    if (outErrorCode) {
        *outErrorCode = mgr.rangeHRVErrorCode;
    }

    if (index < 0 || index >= (int)mgr.rangeHRVSamples.count) {
        return HKW_ERROR_INVALID_PARAM;
    }

    if (outSample) {
        NSValue *value = mgr.rangeHRVSamples[index];
        [value getValue:outSample];
    }

    return HKW_SUCCESS;
}

void HKW_ClearHRVRangeResults(void) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];
        [mgr.rangeHRVSamples removeAllObjects];
        mgr.rangeHRVStatus = HKW_STATUS_IDLE;
    }
}

// MARK: Streaming Heart Rate

int HKW_StartHeartRateStreaming(void) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        if (!mgr.healthStore) {
            mgr.lastErrorMessage = @"Not initialized. Call HKW_Initialize first.";
            return HKW_ERROR_NOT_INITIALIZED;
        }

        if (mgr.isStreaming) {
            return HKW_SUCCESS; // Already streaming
        }

        HKQuantityType *heartRateType = [HKQuantityType quantityTypeForIdentifier:HKQuantityTypeIdentifierHeartRate];

        // Start from now (no anchor = get future samples only)
        NSPredicate *predicate = [HKQuery predicateForSamplesWithStartDate:[NSDate date] endDate:nil options:HKQueryOptionStrictStartDate];

        HKAnchoredObjectQuery *query = [[HKAnchoredObjectQuery alloc]
            initWithType:heartRateType
               predicate:predicate
                  anchor:nil
                   limit:HKObjectQueryNoLimit
          resultsHandler:^(HKAnchoredObjectQuery *query, NSArray<__kindof HKSample *> *addedSamples, NSArray<HKDeletedObject *> *deletedObjects, HKQueryAnchor *newAnchor, NSError *error) {
            if (!error) {
                [mgr processStreamingSamples:addedSamples];
            }
        }];

        query.updateHandler = ^(HKAnchoredObjectQuery *query, NSArray<__kindof HKSample *> *addedSamples, NSArray<HKDeletedObject *> *deletedObjects, HKQueryAnchor *newAnchor, NSError *error) {
            if (!error) {
                [mgr processStreamingSamples:addedSamples];
            }
        };

        mgr.streamingQuery = query;
        mgr.isStreaming = YES;
        [mgr.healthStore executeQuery:query];

        return HKW_SUCCESS;
    }
}

int HKW_StopHeartRateStreaming(void) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        if (!mgr.healthStore) {
            return HKW_ERROR_NOT_INITIALIZED;
        }

        if (!mgr.isStreaming || !mgr.streamingQuery) {
            return HKW_SUCCESS; // Not streaming
        }

        [mgr.healthStore stopQuery:mgr.streamingQuery];
        mgr.streamingQuery = nil;
        mgr.isStreaming = NO;

        return HKW_SUCCESS;
    }
}

bool HKW_IsHeartRateStreaming(void) {
    return [HKWManager shared].isStreaming;
}

int HKW_GetStreamingHeartRateCount(void) {
    HKWManager *mgr = [HKWManager shared];
    @synchronized(mgr.streamingBuffer) {
        return (int)mgr.streamingBuffer.count;
    }
}

int HKW_ReadStreamingHeartRates(HKWHeartRateSample* outSamples, int maxCount, int* outActualCount) {
    if (!outSamples || maxCount <= 0) {
        return HKW_ERROR_INVALID_PARAM;
    }

    HKWManager *mgr = [HKWManager shared];
    int count = 0;

    @synchronized(mgr.streamingBuffer) {
        count = MIN((int)mgr.streamingBuffer.count, maxCount);

        for (int i = 0; i < count; i++) {
            NSValue *value = mgr.streamingBuffer[i];
            [value getValue:&outSamples[i]];
        }

        // Remove consumed samples
        if (count > 0) {
            [mgr.streamingBuffer removeObjectsInRange:NSMakeRange(0, count)];
        }
    }

    if (outActualCount) {
        *outActualCount = count;
    }

    return HKW_SUCCESS;
}

// MARK: Utility

const char* HKW_GetLastErrorMessage(void) {
    HKWManager *mgr = [HKWManager shared];
    if (!mgr.lastErrorMessage) {
        return "";
    }
    return [mgr.lastErrorMessage UTF8String];
}

void HKW_Reset(void) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        // Stop streaming
        if (mgr.isStreaming && mgr.streamingQuery) {
            [mgr.healthStore stopQuery:mgr.streamingQuery];
            mgr.streamingQuery = nil;
            mgr.isStreaming = NO;
        }

        // Reset all states
        mgr.authStatus = HKW_STATUS_IDLE;
        mgr.latestHRStatus = HKW_STATUS_IDLE;
        mgr.rangeHRStatus = HKW_STATUS_IDLE;
        mgr.latestHRVStatus = HKW_STATUS_IDLE;
        mgr.rangeHRVStatus = HKW_STATUS_IDLE;

        [mgr.rangeHRSamples removeAllObjects];
        [mgr.rangeHRVSamples removeAllObjects];
        [mgr.streamingBuffer removeAllObjects];
        [mgr.watchHeartRateBuffer removeAllObjects];

        mgr.lastErrorMessage = nil;
    }
}

// MARK: Watch Connectivity

bool HKW_IsWatchSupported(void) {
    return [WCSession isSupported];
}

bool HKW_IsWatchReachable(void) {
    HKWManager *mgr = [HKWManager shared];
    if (!mgr.wcSession) {
        return false;
    }
    return mgr.wcSession.isReachable;
}

int HKW_SendWatchCommand(const char* command) {
    @autoreleasepool {
        HKWManager *mgr = [HKWManager shared];

        if (!mgr.wcSession) {
            mgr.lastErrorMessage = @"Watch connectivity not supported";
            return HKW_ERROR_NOT_AVAILABLE;
        }

        if (!mgr.wcSession.isReachable) {
            mgr.lastErrorMessage = @"Watch is not reachable";
            return HKW_ERROR_NOT_AVAILABLE;
        }

        NSString *cmdString = [NSString stringWithUTF8String:command];
        NSDictionary *message = @{@"command": cmdString};

        [mgr.wcSession sendMessage:message
                      replyHandler:nil
                      errorHandler:^(NSError *error) {
            mgr.lastErrorMessage = error.localizedDescription;
        }];

        return HKW_SUCCESS;
    }
}

int HKW_GetWatchHeartRateCount(void) {
    HKWManager *mgr = [HKWManager shared];
    @synchronized(mgr.watchHeartRateBuffer) {
        return (int)mgr.watchHeartRateBuffer.count;
    }
}

int HKW_ReadWatchHeartRates(HKWHeartRateSample* outSamples, int maxCount, int* outActualCount) {
    if (!outSamples || maxCount <= 0) {
        return HKW_ERROR_INVALID_PARAM;
    }

    HKWManager *mgr = [HKWManager shared];
    int count = 0;

    @synchronized(mgr.watchHeartRateBuffer) {
        count = MIN((int)mgr.watchHeartRateBuffer.count, maxCount);

        for (int i = 0; i < count; i++) {
            NSValue *value = mgr.watchHeartRateBuffer[i];
            [value getValue:&outSamples[i]];
        }

        // Remove consumed samples
        if (count > 0) {
            [mgr.watchHeartRateBuffer removeObjectsInRange:NSMakeRange(0, count)];
        }
    }

    if (outActualCount) {
        *outActualCount = count;
    }

    return HKW_SUCCESS;
}
