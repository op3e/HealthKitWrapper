# HealthKitWrapper

A C-compatible wrapper around Apple's HealthKit framework, designed for use with Delphi and other languages that support C FFI.

## Features

- HealthKit authorization
- Query latest heart rate
- Query historical heart rate data (date range)
- Real-time heart rate streaming
- Polling-based async pattern (no callbacks required)
- watchOS companion app for triggering on-demand heart rate readings

## Building the Static Library

### Prerequisites

- macOS with Xcode installed
- iOS SDK 14.0 or later

### Build Commands

```bash
cd /path/to/HealthKitWrapper

# Build static library for iOS device (arm64)
xcodebuild -scheme HealthKitWrapper \
    -configuration Release \
    -sdk iphoneos \
    MACH_O_TYPE=staticlib \
    IPHONEOS_DEPLOYMENT_TARGET=14.0 \
    CLANG_MODULES_AUTOLINK=NO \
    OTHER_CFLAGS="-fno-objc-msgsend-selector-stubs"

# The library will be at:
# ~/Library/Developer/Xcode/DerivedData/Build/Products/Release-iphoneos/HealthKitWrapper.framework/HealthKitWrapper

# Copy and rename to .a file
cp ~/Library/Developer/Xcode/DerivedData/Build/Products/Release-iphoneos/HealthKitWrapper.framework/HealthKitWrapper \
   /path/to/your/delphi/project/libHealthKitWrapper.a
```

**Important build flags:**
- `MACH_O_TYPE=staticlib` - Builds a static library instead of dynamic framework
- `IPHONEOS_DEPLOYMENT_TARGET=14.0` - Sets minimum iOS version
- `OTHER_CFLAGS="-fno-objc-msgsend-selector-stubs"` - Uses classic objc_msgSend for compatibility with Delphi's linker

## Delphi Integration

### Prerequisites: PAServer Setup

PAServer (Platform Assistant Server) runs on your Mac and allows RAD Studio on Windows to build and deploy iOS apps.

#### Installing PAServer

1. **Locate the installer** on your Windows machine:
   ```
   C:\Program Files (x86)\Embarcadero\Studio\23.0\PAServer\PAServer23.0.pkg
   ```
   (Version number may vary based on your RAD Studio version)

2. **Copy to Mac** and install:
   - Transfer the `.pkg` file to your Mac
   - Double-click to install
   - Follow the installation prompts

3. **Run PAServer** on your Mac:
   ```bash
   # Navigate to PAServer location (typically)
   cd /Applications/PAServer-23.0

   # Or run from anywhere
   /Applications/PAServer-23.0/paserver
   ```

4. **Set a password** when prompted (you'll need this in RAD Studio)

#### Configuring RAD Studio Connection

1. In RAD Studio, go to **Tools > Options > Deployment > Connection Profile Manager**

2. Click **Add** to create a new profile:
   - **Profile name:** e.g., "MyMac"
   - **Platform:** iOS Device 64-bit
   - **Host name:** Your Mac's IP address or hostname
   - **Port:** 64211 (default)
   - **Password:** The password you set when starting PAServer

3. Click **Test Connection** to verify

#### PAServer Tips

- PAServer must be running on the Mac whenever you build/deploy iOS apps
- The Mac and Windows machine must be on the same network
- If connection fails, check:
  - Firewall settings on Mac (allow incoming connections)
  - PAServer is running (`ps aux | grep paserver`)
  - Correct IP address/hostname

#### Importing iOS SDK

After connecting PAServer, you need to import the iOS SDK:

1. Go to **Tools > Options > SDK Manager**
2. Click **Add...**
3. Select **iOS Device 64-bit** platform
4. Select your connection profile
5. RAD Studio will detect and import the SDK from your Mac's Xcode
6. Click **OK** to complete the import

This may take several minutes as it downloads SDK files from the Mac.

#### Library Placement for PAServer

The static library (`libHealthKitWrapper.a`) must be accessible to PAServer:

**Option 1: Project folder (Recommended)**
- Place `libHealthKitWrapper.a` in your Delphi project folder on Windows
- Delphi will transfer it to the Mac automatically

**Option 2: Mac local path**
- Copy the library to a location on the Mac:
  ```bash
  cp libHealthKitWrapper.a ~/PAServer/
  ```
- Add the Mac path to your Delphi search path

### Step 1: Copy Files to Your Project

Copy these files to your Delphi project folder:
- `libHealthKitWrapper.a` (the built static library)
- `DelphiExample/HealthKitWrapper.pas` (Pascal bindings)

### Step 2: Add Unit to Project

Add `HealthKitWrapper` to your project's uses clause:

```pascal
uses
  HealthKitWrapper;
```

### Step 3: Configure Project Options

In RAD Studio, go to **Project > Options** and configure for **iOS Device 64-bit**:

#### Search Path
Add `.` (current directory) to **Delphi Compiler > Search path**

#### Linker Options
Add to **Delphi Compiler > Linking > Options**:
```
-framework HealthKit -framework WatchConnectivity -lobjc -arch arm64
```

#### HealthKit Framework in SDK Manager
1. Go to **Tools > Options > SDK Manager**
2. Select your iOS SDK
3. In **Remote Paths**, click an existing Framework entry
4. Click **Add new path item**
5. Select **Framework** radio button
6. **Framework Name:** `HealthKit`
7. **Path:** `$(SDKROOT)/System/Library/Frameworks`
8. Click **Update Local File Cache**
9. Click **Save**

### Step 4: Add Info.plist Keys

In **Project > Options > Version Info**, add these keys to your iOS configuration:

```
NSHealthShareUsageDescription=This app needs access to your health data
NSHealthUpdateUsageDescription=This app needs permission to write health data
```

Or add them directly to the `VerInfo_Keys` in your .dproj file.

### Step 5: Add HealthKit Entitlement

Ensure your app has the HealthKit entitlement. In your `Entitlement.TemplateiOS.xml`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>com.apple.developer.healthkit</key>
    <true/>
    <key>com.apple.developer.healthkit.access</key>
    <array/>
</dict>
</plist>
```

## API Usage

### Initialization

```pascal
var
  Res: Integer;
begin
  // Check if HealthKit is available
  if HKW_IsHealthKitAvailable then
  begin
    // Initialize the wrapper
    Res := HKW_Initialize;
    if Res = HKW_SUCCESS then
      ShowMessage('Initialized')
    else
      ShowMessage('Error: ' + HKWErrorToString(Res));
  end;
end;
```

### Request Authorization

```pascal
procedure RequestAuth;
var
  Res: Integer;
begin
  Res := HKW_RequestAuthorization;
  if Res = HKW_SUCCESS then
  begin
    // Start polling for completion
    Timer1.Enabled := True;
  end;
end;

procedure Timer1Timer(Sender: TObject);
var
  Status: Integer;
  Authorized: Boolean;
  ErrorCode: Integer;
begin
  Status := HKW_GetAuthorizationStatus;

  if Status = HKW_STATUS_COMPLETED then
  begin
    Timer1.Enabled := False;
    ErrorCode := 0;
    Authorized := HKW_GetAuthorizationResult(@ErrorCode);
    if Authorized then
      ShowMessage('Authorized!')
    else
      ShowMessage('Denied');
  end
  else if Status = HKW_STATUS_ERROR then
  begin
    Timer1.Enabled := False;
    ShowMessage('Error: ' + string(AnsiString(HKW_GetLastErrorMessage)));
  end;
  // else still pending, keep polling
end;
```

### Query Latest Heart Rate

```pascal
procedure QueryLatest;
var
  Res: Integer;
begin
  Res := HKW_QueryLatestHeartRate;
  if Res = HKW_SUCCESS then
    Timer1.Enabled := True; // Poll for results
end;

procedure CheckLatestResult;
var
  Status: Integer;
  Sample: THKWHeartRateSample;
  ErrorCode: Integer;
  Count: Integer;
begin
  Status := HKW_GetLatestHeartRateStatus;

  if Status = HKW_STATUS_COMPLETED then
  begin
    Timer1.Enabled := False;
    ErrorCode := 0;
    Count := HKW_GetLatestHeartRateResult(@Sample, @ErrorCode);
    if Count > 0 then
      ShowMessage(Format('Heart Rate: %.0f BPM', [Sample.bpm]));
  end;
end;
```

### Query Historical Data

```pascal
procedure QueryRange;
var
  Res: Integer;
  StartTime, EndTime: Double;
begin
  EndTime := DateTimeToUnix(Now, False);
  StartTime := EndTime - (24 * 60 * 60); // 24 hours ago

  Res := HKW_QueryHeartRateRange(StartTime, EndTime, 100);
  if Res = HKW_SUCCESS then
    Timer1.Enabled := True;
end;

procedure CheckRangeResult;
var
  Status, Count, i: Integer;
  Sample: THKWHeartRateSample;
  ErrorCode: Integer;
begin
  Status := HKW_GetHeartRateRangeStatus;

  if Status = HKW_STATUS_COMPLETED then
  begin
    Timer1.Enabled := False;
    Count := HKW_GetHeartRateRangeCount;

    for i := 0 to Count - 1 do
    begin
      ErrorCode := 0;
      if HKW_GetHeartRateRangeResult(i, @Sample, @ErrorCode) = HKW_SUCCESS then
        Memo1.Lines.Add(Format('%.0f BPM', [Sample.bpm]));
    end;

    HKW_ClearHeartRateRangeResults; // Free memory
  end;
end;
```

### Real-time Streaming

```pascal
procedure StartStreaming;
begin
  if HKW_StartHeartRateStreaming = HKW_SUCCESS then
    StreamTimer.Enabled := True;
end;

procedure StopStreaming;
begin
  StreamTimer.Enabled := False;
  HKW_StopHeartRateStreaming;
end;

procedure StreamTimerTimer(Sender: TObject);
var
  Count, ActualCount, i: Integer;
  Samples: array[0..31] of THKWHeartRateSample;
begin
  Count := HKW_GetStreamingHeartRateCount;
  if Count > 0 then
  begin
    if Count > 32 then Count := 32;
    ActualCount := 0;

    if HKW_ReadStreamingHeartRates(@Samples[0], Count, @ActualCount) = HKW_SUCCESS then
    begin
      for i := 0 to ActualCount - 1 do
        Memo1.Lines.Add(Format('Stream: %.0f BPM', [Samples[i].bpm]));
    end;
  end;
end;
```

### Watch Companion App

The watch companion app allows you to trigger on-demand heart rate readings from the Apple Watch. Heart rate data is sent from the watch to the iOS app via WatchConnectivity.

#### Building the Watch App

1. Open `HealthKitWrapper.xcodeproj` in Xcode
2. Select **File > New > Target**
3. Choose **watchOS > App**
4. Name it "WatchApp" and add to the HealthKitWrapper project
5. Replace the generated files with the contents from the `WatchApp` folder
6. Configure the watch app:
   - Set **WKCompanionAppBundleIdentifier** in Info.plist to match your iOS app's bundle ID
   - Ensure HealthKit capability is enabled
   - Add the HealthKit entitlement

#### Using Watch Heart Rate Data

```pascal
procedure CheckWatchReachability;
begin
  if HKW_IsWatchSupported then
  begin
    if HKW_IsWatchReachable then
      LblWatchStatus.Text := 'Watch Connected'
    else
      LblWatchStatus.Text := 'Watch Not Reachable';
  end
  else
    LblWatchStatus.Text := 'Watch Not Supported';
end;

procedure StartWatchMonitoring;
begin
  // Send command to watch to start monitoring
  HKW_SendWatchCommand('startMonitoring');
  WatchTimer.Enabled := True;
end;

procedure StopWatchMonitoring;
begin
  WatchTimer.Enabled := False;
  HKW_SendWatchCommand('stopMonitoring');
end;

procedure WatchTimerTimer(Sender: TObject);
var
  Count, ActualCount, i: Integer;
  Samples: array[0..31] of THKWHeartRateSample;
begin
  Count := HKW_GetWatchHeartRateCount;
  if Count > 0 then
  begin
    if Count > 32 then Count := 32;
    ActualCount := 0;

    if HKW_ReadWatchHeartRates(@Samples[0], Count, @ActualCount) = HKW_SUCCESS then
    begin
      for i := 0 to ActualCount - 1 do
        Memo1.Lines.Add(Format('Watch: %.0f BPM', [Samples[i].bpm]));
    end;
  end;
end;
```

#### Deploying Watch App with Delphi

Since Delphi cannot build watchOS apps, you must embed the watch app into the Delphi app bundle after building. Use the `embed_watch_app.sh` script to automate this.

**Prerequisites:**
- Xcode installed with watchOS SDK
- Delphi app built via PAServer (but not yet deployed)
- iPhone connected to Mac

**Usage:**

```bash
# Basic usage (auto-detects connected device)
./embed_watch_app.sh -d /path/to/your/DelphiApp.app

# Full example with PAServer path
./embed_watch_app.sh -d ~/PAServer/scratch-dir/myprofile/Project1.app

# With custom options
./embed_watch_app.sh \
    -d ~/PAServer/scratch-dir/myprofile/Project1.app \
    -p /path/to/HealthKitWrapper.xcodeproj \
    -t TestWatchApp \
    -i DEVICE-UUID
```

**Options:**

| Option | Description |
|--------|-------------|
| `-d, --delphi-app PATH` | **(Required)** Path to the Delphi .app bundle |
| `-p, --project PATH` | Xcode project path (default: HealthKitWrapper.xcodeproj) |
| `-t, --target NAME` | Watch target name (default: TestWatchApp) |
| `-D, --derived-data PATH` | Xcode derived data path |
| `-i, --device-id ID` | Device UUID for deployment (auto-detected if not specified) |
| `-h, --help` | Show help |

**Workflow:**

1. In RAD Studio, **Build** your Delphi project (Shift+F9), but do NOT deploy
2. On Mac, run the embed script:
   ```bash
   ./embed_watch_app.sh -d ~/PAServer/scratch-dir/myprofile/Project1.app
   ```
3. The script will:
   - Build the watch app
   - Embed it into the Delphi app bundle
   - Re-sign everything
   - Deploy to your iPhone
4. Open the **Watch** app on iPhone
5. Find your app under **Available Apps** and tap **Install**

**Finding Your Delphi App Path:**

The Delphi app is built in the PAServer scratch directory on your Mac:
```bash
# List PAServer scratch directories
ls ~/PAServer/scratch-dir/

# Common path pattern
~/PAServer/scratch-dir/<connection-name>/Project1.app
```

#### Watch Commands

| Command | Description |
|---------|-------------|
| `startMonitoring` | Start workout session and heart rate monitoring on watch |
| `stopMonitoring` | Stop workout session and monitoring |

#### Watch API Functions

| Function | Description |
|----------|-------------|
| `HKW_IsWatchSupported` | Returns true if WatchConnectivity is available |
| `HKW_IsWatchReachable` | Returns true if the paired watch is reachable |
| `HKW_SendWatchCommand` | Send a command string to the watch app |
| `HKW_GetWatchHeartRateCount` | Get count of pending heart rate samples from watch |
| `HKW_ReadWatchHeartRates` | Read heart rate samples received from watch |

### Cleanup

```pascal
procedure FormDestroy(Sender: TObject);
begin
  HKW_Shutdown;
end;
```

## Error Codes

| Code | Constant | Description |
|------|----------|-------------|
| 0 | HKW_SUCCESS | Operation successful |
| -1 | HKW_ERROR_NOT_AVAILABLE | HealthKit not available on device |
| -2 | HKW_ERROR_NOT_AUTHORIZED | User denied authorization |
| -3 | HKW_ERROR_NOT_INITIALIZED | Call HKW_Initialize first |
| -4 | HKW_ERROR_OPERATION_PENDING | Another operation is in progress |
| -5 | HKW_ERROR_NO_DATA | No data found |
| -6 | HKW_ERROR_INVALID_PARAM | Invalid parameter passed |
| -7 | HKW_ERROR_QUERY_FAILED | HealthKit query failed |

## Status Values

| Value | Constant | Description |
|-------|----------|-------------|
| 0 | HKW_STATUS_IDLE | No operation in progress |
| 1 | HKW_STATUS_PENDING | Operation in progress |
| 2 | HKW_STATUS_COMPLETED | Operation completed successfully |
| 3 | HKW_STATUS_ERROR | Operation failed |

## Data Structures

### THKWHeartRateSample

```pascal
THKWHeartRateSample = packed record
  bpm: Double;           // Heart rate in beats per minute
  timestamp: Double;     // Unix timestamp (seconds since 1970)
  sourceDeviceType: Integer;  // 0=unknown, 1=watch, 2=phone, 3=other
end;
```

## Requirements

- iOS 14.0 or later
- Physical iOS device (HealthKit doesn't work in simulator)
- HealthKit entitlement in provisioning profile
- RAD Studio with iOS development configured

## Example Project

See the `DelphiExample` folder for a complete working example with:
- `libHealthKitWrapper.a` - Pre-built static library (iOS arm64)
- `HealthKitWrapper.pas` - Pascal bindings
- `Unit1.pas` - Example form with all features
- `Unit1.fmx` - FireMonkey form layout
- `Entitlement.TemplateiOS.xml` - HealthKit entitlement template

To use: copy all files to your Delphi project folder and follow the integration steps above.

## License

MIT License
