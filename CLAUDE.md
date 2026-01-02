# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build dynamic framework for iOS device
xcodebuild -scheme HealthKitWrapper -configuration Release -sdk iphoneos build

# Build STATIC LIBRARY for Delphi (important flags for compatibility)
xcodebuild -scheme HealthKitWrapper -configuration Release -sdk iphoneos \
    MACH_O_TYPE=staticlib \
    IPHONEOS_DEPLOYMENT_TARGET=14.0 \
    CLANG_MODULES_AUTOLINK=NO \
    OTHER_CFLAGS="-fno-objc-msgsend-selector-stubs"

# Copy static library for Delphi
cp ~/Library/Developer/Xcode/DerivedData/Build/Products/Release-iphoneos/HealthKitWrapper.framework/HealthKitWrapper \
   /path/to/delphi/project/libHealthKitWrapper.a

# Clean build
xcodebuild -scheme HealthKitWrapper clean
```

## Project Structure

- `HealthKitWrapper/` - Xcode framework source (Objective-C)
- `TestApp/` - Swift test app for testing the framework
- `DelphiExample/` - Delphi/Pascal bindings and example code

## Architecture

HealthKitWrapper is an Objective-C framework exposing a **C-compatible API** for Delphi FFI. It wraps Apple's HealthKit for heart rate data access.

### Files
- `HealthKitWrapper.h` - Public C API (types, function declarations with `extern "C"`)
- `HealthKitWrapper.m` - Implementation with `HKWManager` singleton

### C API Pattern (Polling-Based Async)
All HealthKit operations are async. The API uses polling instead of callbacks:
1. Call `HKW_Query*()` to start an operation (returns immediately)
2. Poll `HKW_Get*Status()` until `HKW_STATUS_COMPLETED` or `HKW_STATUS_ERROR`
3. Call `HKW_Get*Result()` to retrieve the data

### Key Types
- `HKWErrorCode` - Error codes (HKW_SUCCESS, HKW_ERROR_NOT_INITIALIZED, etc.)
- `HKWOperationStatus` - IDLE, PENDING, COMPLETED, ERROR
- `HKWHeartRateSample` - struct with bpm (double), timestamp (double), sourceDeviceType (int)

### API Groups
- **Init**: `HKW_Initialize()`, `HKW_Shutdown()`, `HKW_IsHealthKitAvailable()`
- **Auth**: `HKW_RequestAuthorization()`, `HKW_GetAuthorizationStatus()`, `HKW_GetAuthorizationResult()`
- **Latest HR**: `HKW_QueryLatestHeartRate()`, `HKW_GetLatestHeartRateStatus()`, `HKW_GetLatestHeartRateResult()`
- **Historical HR**: `HKW_QueryHeartRateRange()`, `HKW_GetHeartRateRangeStatus()`, `HKW_GetHeartRateRangeCount()`, `HKW_GetHeartRateRangeResult()`
- **Streaming HR**: `HKW_StartHeartRateStreaming()`, `HKW_StopHeartRateStreaming()`, `HKW_ReadStreamingHeartRates()`
