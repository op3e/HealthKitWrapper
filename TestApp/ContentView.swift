import SwiftUI

struct ContentView: View {
    @State private var logMessages: [String] = []
    @State private var isInitialized = false
    @State private var isAuthorized = false
    @State private var isStreaming = false
    @State private var isWatchMonitoring = false
    @State private var pollTimer: Timer?
    @State private var streamTimer: Timer?
    @State private var watchTimer: Timer?
    @State private var selectedTab = 0

    var body: some View {
        VStack(spacing: 0) {
            // Log area at top
            LogView(logMessages: logMessages)

            Divider()

            // Tab bar with content
            TabView(selection: $selectedTab) {
                SettingsTab(
                    isInitialized: $isInitialized,
                    isAuthorized: $isAuthorized,
                    isStreaming: $isStreaming,
                    isWatchMonitoring: $isWatchMonitoring,
                    log: log,
                    initialize: initialize,
                    shutdown: shutdown,
                    requestAuth: requestAuth,
                    clearLog: { logMessages.removeAll() }
                )
                .tabItem {
                    Label("Settings", systemImage: "gear")
                }
                .tag(0)

                WatchTab(
                    isInitialized: isInitialized,
                    isWatchMonitoring: $isWatchMonitoring,
                    log: log,
                    checkWatchStatus: checkWatchStatus,
                    startWatchMonitoring: startWatchMonitoring,
                    stopWatchMonitoring: stopWatchMonitoring
                )
                .tabItem {
                    Label("Watch", systemImage: "applewatch")
                }
                .tag(1)

                HeartRateTab(
                    isInitialized: isInitialized,
                    isStreaming: $isStreaming,
                    log: log,
                    queryLatest: queryLatest,
                    queryRange: queryRange,
                    startStreaming: startStreaming,
                    stopStreaming: stopStreaming
                )
                .tabItem {
                    Label("Heart Rate", systemImage: "heart.fill")
                }
                .tag(2)

                HRVTab(
                    isInitialized: isInitialized,
                    log: log,
                    queryLatestHRV: queryLatestHRV,
                    queryHRVRange: queryHRVRange
                )
                .tabItem {
                    Label("HRV", systemImage: "waveform.path.ecg")
                }
                .tag(3)
            }
        }
        .onAppear {
            log("Test app ready")
            log("HK Available: \(HKW_IsHealthKitAvailable())")
        }
    }

    private func log(_ message: String) {
        let timestamp = DateFormatter.localizedString(from: Date(), dateStyle: .none, timeStyle: .medium)
        logMessages.append("[\(timestamp)] \(message)")
    }

    // MARK: - Settings Functions

    private func initialize() {
        let result = HKW_Initialize()
        if result == HKW_SUCCESS.rawValue {
            isInitialized = true
            log("Initialize SUCCESS")
        } else {
            log("ERROR Initialize: \(errorString(result))")
        }
    }

    private func shutdown() {
        stopPolling()
        stopStreamTimer()
        stopWatchTimer()
        HKW_Shutdown()
        isInitialized = false
        isAuthorized = false
        isStreaming = false
        isWatchMonitoring = false
        log("Shutdown complete")
    }

    private func requestAuth() {
        let result = HKW_RequestAuthorization()
        if result == HKW_SUCCESS.rawValue {
            log("Auth request started, polling...")
            startPolling { [self] in
                let status = HKW_GetAuthorizationStatus()
                if status == HKW_STATUS_COMPLETED {
                    var errorCode: Int32 = 0
                    let authorized = HKW_GetAuthorizationResult(&errorCode)
                    isAuthorized = authorized
                    log("Auth completed: \(authorized ? "AUTHORIZED" : "DENIED")")
                    return true
                } else if status == HKW_STATUS_ERROR {
                    log("ERROR Auth failed: \(String(cString: HKW_GetLastErrorMessage()))")
                    return true
                }
                return false
            }
        } else {
            log("ERROR starting auth: \(errorString(result))")
        }
    }

    // MARK: - Heart Rate Functions

    private func queryLatest() {
        let result = HKW_QueryLatestHeartRate()
        if result == HKW_SUCCESS.rawValue {
            log("Latest HR query started...")
            startPolling { [self] in
                let status = HKW_GetLatestHeartRateStatus()
                if status == HKW_STATUS_COMPLETED {
                    var sample = HKWHeartRateSample()
                    var errorCode: Int32 = 0
                    let count = HKW_GetLatestHeartRateResult(&sample, &errorCode)
                    if count > 0 {
                        let date = Date(timeIntervalSince1970: sample.timestamp)
                        let formatter = DateFormatter()
                        formatter.dateStyle = .short
                        formatter.timeStyle = .short
                        log("Latest: \(Int(sample.bpm)) BPM @ \(formatter.string(from: date))")
                    } else {
                        log("No heart rate data found")
                    }
                    return true
                } else if status == HKW_STATUS_ERROR {
                    log("ERROR query: \(String(cString: HKW_GetLastErrorMessage()))")
                    return true
                }
                return false
            }
        } else {
            log("ERROR starting query: \(errorString(result))")
        }
    }

    private func queryRange() {
        let end = Date()
        let start = end.addingTimeInterval(-24 * 60 * 60)

        let result = HKW_QueryHeartRateRange(start.timeIntervalSince1970, end.timeIntervalSince1970, 100)
        if result == HKW_SUCCESS.rawValue {
            log("Range query started (last 24h)...")
            startPolling { [self] in
                let status = HKW_GetHeartRateRangeStatus()
                if status == HKW_STATUS_COMPLETED {
                    let count = HKW_GetHeartRateRangeCount()
                    log("Found \(count) HR samples in last 24 hours")

                    let showCount = min(count, 5)
                    for i in 0..<showCount {
                        var sample = HKWHeartRateSample()
                        var errorCode: Int32 = 0
                        if HKW_GetHeartRateRangeResult(i, &sample, &errorCode) == HKW_SUCCESS.rawValue {
                            let date = Date(timeIntervalSince1970: sample.timestamp)
                            let formatter = DateFormatter()
                            formatter.timeStyle = .short
                            log("  [\(i)] \(Int(sample.bpm)) BPM @ \(formatter.string(from: date))")
                        }
                    }
                    if count > 5 {
                        log("  ... and \(count - 5) more")
                    }
                    HKW_ClearHeartRateRangeResults()
                    return true
                } else if status == HKW_STATUS_ERROR {
                    log("ERROR range query: \(String(cString: HKW_GetLastErrorMessage()))")
                    return true
                }
                return false
            }
        } else {
            log("ERROR starting range query: \(errorString(result))")
        }
    }

    private func startStreaming() {
        let result = HKW_StartHeartRateStreaming()
        if result == HKW_SUCCESS.rawValue {
            isStreaming = true
            log("Streaming started")

            streamTimer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { [self] _ in
                readStreamingData()
            }
        } else {
            log("ERROR starting stream: \(errorString(result))")
        }
    }

    private func stopStreaming() {
        stopStreamTimer()
        let result = HKW_StopHeartRateStreaming()
        isStreaming = false
        if result == HKW_SUCCESS.rawValue {
            log("Streaming stopped")
        }
    }

    private func readStreamingData() {
        let count = HKW_GetStreamingHeartRateCount()
        if count > 0 {
            var samples = [HKWHeartRateSample](repeating: HKWHeartRateSample(), count: Int(count))
            var actualCount: Int32 = 0

            samples.withUnsafeMutableBufferPointer { buffer in
                _ = HKW_ReadStreamingHeartRates(buffer.baseAddress, count, &actualCount)
            }

            for i in 0..<Int(actualCount) {
                log("STREAM: \(Int(samples[i].bpm)) BPM")
            }
        }
    }

    // MARK: - HRV Functions

    private func queryLatestHRV() {
        let result = HKW_QueryLatestHRV()
        if result == HKW_SUCCESS.rawValue {
            log("Latest HRV query started...")
            startPolling { [self] in
                let status = HKW_GetLatestHRVStatus()
                if status == HKW_STATUS_COMPLETED {
                    var sample = HKWHeartRateVariabilitySample()
                    var errorCode: Int32 = 0
                    let count = HKW_GetLatestHRVResult(&sample, &errorCode)
                    if count > 0 {
                        let date = Date(timeIntervalSince1970: sample.timestamp)
                        let formatter = DateFormatter()
                        formatter.dateStyle = .short
                        formatter.timeStyle = .short
                        log("Latest HRV: \(String(format: "%.1f", sample.sdnn)) ms @ \(formatter.string(from: date))")
                    } else {
                        log("No HRV data found")
                    }
                    return true
                } else if status == HKW_STATUS_ERROR {
                    log("ERROR HRV query: \(String(cString: HKW_GetLastErrorMessage()))")
                    return true
                }
                return false
            }
        } else {
            log("ERROR starting HRV query: \(errorString(result))")
        }
    }

    private func queryHRVRange() {
        let end = Date()
        let start = end.addingTimeInterval(-24 * 60 * 60)

        let result = HKW_QueryHRVRange(start.timeIntervalSince1970, end.timeIntervalSince1970, 100)
        if result == HKW_SUCCESS.rawValue {
            log("HRV range query started (last 24h)...")
            startPolling { [self] in
                let status = HKW_GetHRVRangeStatus()
                if status == HKW_STATUS_COMPLETED {
                    let count = HKW_GetHRVRangeCount()
                    log("Found \(count) HRV samples in last 24 hours")

                    let showCount = min(count, 5)
                    for i in 0..<showCount {
                        var sample = HKWHeartRateVariabilitySample()
                        var errorCode: Int32 = 0
                        if HKW_GetHRVRangeResult(i, &sample, &errorCode) == HKW_SUCCESS.rawValue {
                            let date = Date(timeIntervalSince1970: sample.timestamp)
                            let formatter = DateFormatter()
                            formatter.timeStyle = .short
                            log("  [\(i)] \(String(format: "%.1f", sample.sdnn)) ms @ \(formatter.string(from: date))")
                        }
                    }
                    if count > 5 {
                        log("  ... and \(count - 5) more")
                    }
                    HKW_ClearHRVRangeResults()
                    return true
                } else if status == HKW_STATUS_ERROR {
                    log("ERROR HRV range query: \(String(cString: HKW_GetLastErrorMessage()))")
                    return true
                }
                return false
            }
        } else {
            log("ERROR starting HRV range query: \(errorString(result))")
        }
    }

    // MARK: - Watch Functions

    private func checkWatchStatus() {
        let supported = HKW_IsWatchSupported()
        let reachable = HKW_IsWatchReachable()

        log("Watch Supported: \(supported)")
        log("Watch Reachable: \(reachable)")

        if !supported {
            log("WatchConnectivity not supported on this device")
        } else if !reachable {
            log("Watch not reachable - ensure watch app is running")
        } else {
            log("Watch is connected and reachable")
        }
    }

    private func startWatchMonitoring() {
        let result = HKW_SendWatchCommand("startMonitoring")
        if result == HKW_SUCCESS.rawValue {
            isWatchMonitoring = true
            log("Sent startMonitoring to watch")

            watchTimer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { [self] _ in
                readWatchData()
            }
        } else {
            log("ERROR sending to watch: \(errorString(result))")
            if let msg = String(cString: HKW_GetLastErrorMessage(), encoding: .utf8), !msg.isEmpty {
                log("  -> \(msg)")
            }
        }
    }

    private func stopWatchMonitoring() {
        stopWatchTimer()
        let result = HKW_SendWatchCommand("stopMonitoring")
        isWatchMonitoring = false
        if result == HKW_SUCCESS.rawValue {
            log("Sent stopMonitoring to watch")
        } else {
            log("ERROR sending stop to watch: \(errorString(result))")
        }
    }

    private func readWatchData() {
        let count = HKW_GetWatchHeartRateCount()
        if count > 0 {
            var samples = [HKWHeartRateSample](repeating: HKWHeartRateSample(), count: Int(count))
            var actualCount: Int32 = 0

            samples.withUnsafeMutableBufferPointer { buffer in
                _ = HKW_ReadWatchHeartRates(buffer.baseAddress, count, &actualCount)
            }

            for i in 0..<Int(actualCount) {
                log("WATCH: \(Int(samples[i].bpm)) BPM")
            }
        }
    }

    // MARK: - Helpers

    private func startPolling(check: @escaping () -> Bool) {
        stopPolling()
        pollTimer = Timer.scheduledTimer(withTimeInterval: 0.1, repeats: true) { timer in
            if check() {
                timer.invalidate()
                self.pollTimer = nil
            }
        }
    }

    private func stopPolling() {
        pollTimer?.invalidate()
        pollTimer = nil
    }

    private func stopStreamTimer() {
        streamTimer?.invalidate()
        streamTimer = nil
    }

    private func stopWatchTimer() {
        watchTimer?.invalidate()
        watchTimer = nil
    }

    private func errorString(_ code: Int32) -> String {
        switch code {
        case HKW_SUCCESS.rawValue: return "SUCCESS"
        case HKW_ERROR_NOT_AVAILABLE.rawValue: return "NOT_AVAILABLE"
        case HKW_ERROR_NOT_AUTHORIZED.rawValue: return "NOT_AUTHORIZED"
        case HKW_ERROR_NOT_INITIALIZED.rawValue: return "NOT_INITIALIZED"
        case HKW_ERROR_OPERATION_PENDING.rawValue: return "OPERATION_PENDING"
        case HKW_ERROR_NO_DATA.rawValue: return "NO_DATA"
        case HKW_ERROR_INVALID_PARAM.rawValue: return "INVALID_PARAM"
        case HKW_ERROR_QUERY_FAILED.rawValue: return "QUERY_FAILED"
        default: return "UNKNOWN(\(code))"
        }
    }
}

// MARK: - Log View

struct LogView: View {
    let logMessages: [String]

    var body: some View {
        ScrollViewReader { proxy in
            ScrollView {
                LazyVStack(alignment: .leading, spacing: 4) {
                    ForEach(Array(logMessages.enumerated()), id: \.offset) { index, message in
                        Text(message)
                            .font(.system(.caption, design: .monospaced))
                            .foregroundColor(messageColor(message))
                            .id(index)
                    }
                }
                .padding(.horizontal)
            }
            .frame(height: 200)
            .background(Color(.systemGray6))
            .onChange(of: logMessages.count) { _ in
                if let last = logMessages.indices.last {
                    withAnimation {
                        proxy.scrollTo(last, anchor: .bottom)
                    }
                }
            }
        }
    }

    private func messageColor(_ message: String) -> Color {
        if message.contains("ERROR") || message.contains("failed") {
            return .red
        } else if message.contains("SUCCESS") || message.contains("complete") || message.contains("AUTHORIZED") {
            return .green
        } else if message.contains("BPM") || message.contains("ms @") {
            return .orange
        }
        return .primary
    }
}

// MARK: - Settings Tab

struct SettingsTab: View {
    @Binding var isInitialized: Bool
    @Binding var isAuthorized: Bool
    @Binding var isStreaming: Bool
    @Binding var isWatchMonitoring: Bool
    let log: (String) -> Void
    let initialize: () -> Void
    let shutdown: () -> Void
    let requestAuth: () -> Void
    let clearLog: () -> Void

    var body: some View {
        ScrollView {
            VStack(spacing: 16) {
                GroupBox(label: Label("Lifecycle", systemImage: "power")) {
                    VStack(spacing: 12) {
                        HStack(spacing: 12) {
                            Button("Initialize") { initialize() }
                                .buttonStyle(TestButtonStyle(color: .blue))

                            Button("Shutdown") { shutdown() }
                                .buttonStyle(TestButtonStyle(color: .red))
                        }
                    }
                    .padding(.top, 8)
                }

                GroupBox(label: Label("Authorization", systemImage: "lock.shield")) {
                    VStack(spacing: 12) {
                        Button("Request Authorization") { requestAuth() }
                            .buttonStyle(TestButtonStyle(color: .green))
                            .disabled(!isInitialized)

                        HStack {
                            Circle()
                                .fill(isAuthorized ? Color.green : Color.gray)
                                .frame(width: 10, height: 10)
                            Text(isAuthorized ? "Authorized" : "Not Authorized")
                                .font(.caption)
                                .foregroundColor(.secondary)
                        }
                    }
                    .padding(.top, 8)
                }

                GroupBox(label: Label("Status", systemImage: "info.circle")) {
                    VStack(alignment: .leading, spacing: 8) {
                        StatusRow(label: "Initialized", value: isInitialized)
                        StatusRow(label: "Authorized", value: isAuthorized)
                        StatusRow(label: "Streaming", value: isStreaming)
                        StatusRow(label: "Watch Active", value: isWatchMonitoring)
                    }
                    .padding(.top, 8)
                }

                Button("Clear Log") { clearLog() }
                    .buttonStyle(TestButtonStyle(color: .gray))
            }
            .padding()
        }
    }
}

struct StatusRow: View {
    let label: String
    let value: Bool

    var body: some View {
        HStack {
            Text(label)
                .foregroundColor(.secondary)
            Spacer()
            Image(systemName: value ? "checkmark.circle.fill" : "xmark.circle")
                .foregroundColor(value ? .green : .gray)
        }
    }
}

// MARK: - Watch Tab

struct WatchTab: View {
    let isInitialized: Bool
    @Binding var isWatchMonitoring: Bool
    let log: (String) -> Void
    let checkWatchStatus: () -> Void
    let startWatchMonitoring: () -> Void
    let stopWatchMonitoring: () -> Void

    var body: some View {
        ScrollView {
            VStack(spacing: 16) {
                GroupBox(label: Label("Apple Watch", systemImage: "applewatch")) {
                    VStack(spacing: 12) {
                        Button("Check Watch Status") { checkWatchStatus() }
                            .buttonStyle(TestButtonStyle(color: .cyan))
                            .disabled(!isInitialized)

                        Button(isWatchMonitoring ? "Stop Watch" : "Start Watch") {
                            if isWatchMonitoring {
                                stopWatchMonitoring()
                            } else {
                                startWatchMonitoring()
                            }
                        }
                        .buttonStyle(TestButtonStyle(color: isWatchMonitoring ? .red : .mint))
                        .disabled(!isInitialized)
                    }
                    .padding(.top, 8)
                }

                GroupBox(label: Label("Info", systemImage: "info.circle")) {
                    Text("The watch app must be installed and running to receive heart rate data via WatchConnectivity.")
                        .font(.caption)
                        .foregroundColor(.secondary)
                        .padding(.top, 8)
                }
            }
            .padding()
        }
    }
}

// MARK: - Heart Rate Tab

struct HeartRateTab: View {
    let isInitialized: Bool
    @Binding var isStreaming: Bool
    let log: (String) -> Void
    let queryLatest: () -> Void
    let queryRange: () -> Void
    let startStreaming: () -> Void
    let stopStreaming: () -> Void

    var body: some View {
        ScrollView {
            VStack(spacing: 16) {
                GroupBox(label: Label("Queries", systemImage: "magnifyingglass")) {
                    VStack(spacing: 12) {
                        Button("Query Latest Heart Rate") { queryLatest() }
                            .buttonStyle(TestButtonStyle(color: .orange))
                            .disabled(!isInitialized)

                        Button("Query Last 24 Hours") { queryRange() }
                            .buttonStyle(TestButtonStyle(color: .orange))
                            .disabled(!isInitialized)
                    }
                    .padding(.top, 8)
                }

                GroupBox(label: Label("Streaming", systemImage: "waveform")) {
                    VStack(spacing: 12) {
                        Button(isStreaming ? "Stop Stream" : "Start Stream") {
                            if isStreaming {
                                stopStreaming()
                            } else {
                                startStreaming()
                            }
                        }
                        .buttonStyle(TestButtonStyle(color: isStreaming ? .red : .purple))
                        .disabled(!isInitialized)

                        if isStreaming {
                            HStack {
                                ProgressView()
                                    .scaleEffect(0.8)
                                Text("Streaming active...")
                                    .font(.caption)
                                    .foregroundColor(.secondary)
                            }
                        }
                    }
                    .padding(.top, 8)
                }
            }
            .padding()
        }
    }
}

// MARK: - HRV Tab

struct HRVTab: View {
    let isInitialized: Bool
    let log: (String) -> Void
    let queryLatestHRV: () -> Void
    let queryHRVRange: () -> Void

    var body: some View {
        ScrollView {
            VStack(spacing: 16) {
                GroupBox(label: Label("Heart Rate Variability", systemImage: "waveform.path.ecg")) {
                    VStack(spacing: 12) {
                        Button("Query Latest HRV") { queryLatestHRV() }
                            .buttonStyle(TestButtonStyle(color: .indigo))
                            .disabled(!isInitialized)

                        Button("Query Last 24 Hours") { queryHRVRange() }
                            .buttonStyle(TestButtonStyle(color: .indigo))
                            .disabled(!isInitialized)
                    }
                    .padding(.top, 8)
                }

                GroupBox(label: Label("About HRV", systemImage: "info.circle")) {
                    VStack(alignment: .leading, spacing: 8) {
                        Text("HRV (Heart Rate Variability) measures the variation in time between heartbeats, expressed as SDNN in milliseconds.")
                            .font(.caption)
                            .foregroundColor(.secondary)

                        Text("HRV data is typically recorded by Apple Watch during sleep, Breathe sessions, and throughout the day.")
                            .font(.caption)
                            .foregroundColor(.secondary)
                    }
                    .padding(.top, 8)
                }
            }
            .padding()
        }
    }
}

// MARK: - Button Style

struct TestButtonStyle: ButtonStyle {
    let color: Color

    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .font(.system(.body, design: .rounded, weight: .medium))
            .foregroundColor(.white)
            .frame(maxWidth: .infinity)
            .padding(.vertical, 12)
            .background(color.opacity(configuration.isPressed ? 0.7 : 1.0))
            .cornerRadius(10)
    }
}

#Preview {
    ContentView()
}
