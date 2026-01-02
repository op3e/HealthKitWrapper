import SwiftUI

struct ContentView: View {
    @State private var logMessages: [String] = []
    @State private var isInitialized = false
    @State private var isAuthorized = false
    @State private var isStreaming = false
    @State private var pollTimer: Timer?
    @State private var streamTimer: Timer?

    var body: some View {
        NavigationView {
            VStack(spacing: 0) {
                // Log area
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
                    .frame(maxHeight: 300)
                    .background(Color(.systemGray6))
                    .onChange(of: logMessages.count) { _ in
                        if let last = logMessages.indices.last {
                            proxy.scrollTo(last, anchor: .bottom)
                        }
                    }
                }

                Divider()

                // Buttons
                ScrollView {
                    VStack(spacing: 12) {
                        Group {
                            HStack(spacing: 12) {
                                Button("Initialize") { initialize() }
                                    .buttonStyle(TestButtonStyle(color: .blue))

                                Button("Shutdown") { shutdown() }
                                    .buttonStyle(TestButtonStyle(color: .red))
                            }

                            Button("Request Authorization") { requestAuth() }
                                .buttonStyle(TestButtonStyle(color: .green))
                                .disabled(!isInitialized)
                        }

                        Divider().padding(.vertical, 8)

                        Group {
                            Button("Query Latest Heart Rate") { queryLatest() }
                                .buttonStyle(TestButtonStyle(color: .orange))
                                .disabled(!isInitialized)

                            Button("Query Last 24 Hours") { queryRange() }
                                .buttonStyle(TestButtonStyle(color: .orange))
                                .disabled(!isInitialized)
                        }

                        Divider().padding(.vertical, 8)

                        Group {
                            HStack(spacing: 12) {
                                Button(isStreaming ? "Stop Stream" : "Start Stream") {
                                    if isStreaming {
                                        stopStreaming()
                                    } else {
                                        startStreaming()
                                    }
                                }
                                .buttonStyle(TestButtonStyle(color: isStreaming ? .red : .purple))
                                .disabled(!isInitialized)
                            }
                        }

                        Divider().padding(.vertical, 8)

                        Button("Clear Log") { logMessages.removeAll() }
                            .buttonStyle(TestButtonStyle(color: .gray))
                    }
                    .padding()
                }
            }
            .navigationTitle("HKWrapper Test")
            .navigationBarTitleDisplayMode(.inline)
        }
        .onAppear {
            log("Test app ready")
            log("HK Available: \(HKW_IsHealthKitAvailable())")
        }
    }

    private func messageColor(_ message: String) -> Color {
        if message.contains("ERROR") || message.contains("failed") {
            return .red
        } else if message.contains("SUCCESS") || message.contains("✓") {
            return .green
        } else if message.contains("BPM") {
            return .orange
        }
        return .primary
    }

    private func log(_ message: String) {
        let timestamp = DateFormatter.localizedString(from: Date(), dateStyle: .none, timeStyle: .medium)
        logMessages.append("[\(timestamp)] \(message)")
    }

    // MARK: - API Functions

    private func initialize() {
        let result = HKW_Initialize()
        if result == HKW_SUCCESS.rawValue {
            isInitialized = true
            log("✓ Initialize SUCCESS")
        } else {
            log("ERROR Initialize: \(errorString(result))")
        }
    }

    private func shutdown() {
        stopPolling()
        stopStreamTimer()
        HKW_Shutdown()
        isInitialized = false
        isAuthorized = false
        isStreaming = false
        log("✓ Shutdown complete")
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
                    log("✓ Auth completed: \(authorized ? "AUTHORIZED" : "DENIED")")
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

    private func queryLatest() {
        let result = HKW_QueryLatestHeartRate()
        if result == HKW_SUCCESS.rawValue {
            log("Latest HR query started, polling...")
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
                        log("✓ Latest: \(Int(sample.bpm)) BPM @ \(formatter.string(from: date))")
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
        let start = end.addingTimeInterval(-24 * 60 * 60) // 24 hours ago

        let result = HKW_QueryHeartRateRange(start.timeIntervalSince1970, end.timeIntervalSince1970, 100)
        if result == HKW_SUCCESS.rawValue {
            log("Range query started (last 24h), polling...")
            startPolling { [self] in
                let status = HKW_GetHeartRateRangeStatus()
                if status == HKW_STATUS_COMPLETED {
                    let count = HKW_GetHeartRateRangeCount()
                    log("✓ Found \(count) samples in last 24 hours")

                    // Show first few samples
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
            log("✓ Streaming started")

            // Start timer to read streaming data
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
            log("✓ Streaming stopped")
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
