import SwiftUI
import HealthKit

struct ContentView: View {
    @Bindable var sessionManager: WorkoutSessionManager

    var body: some View {
        VStack(spacing: 16) {
            // Heart rate display
            VStack(spacing: 4) {
                Image(systemName: "heart.fill")
                    .font(.system(size: 40))
                    .foregroundColor(sessionManager.isMonitoring ? .red : .gray)

                if let hr = sessionManager.currentHeartRate {
                    Text("\(Int(hr))")
                        .font(.system(size: 48, weight: .bold, design: .rounded))
                    Text("BPM")
                        .font(.caption)
                        .foregroundColor(.secondary)
                } else {
                    Text("--")
                        .font(.system(size: 48, weight: .bold, design: .rounded))
                        .foregroundColor(.gray)
                    Text("BPM")
                        .font(.caption)
                        .foregroundColor(.secondary)
                }
            }

            // Status
            Text(sessionManager.statusMessage)
                .font(.caption2)
                .foregroundColor(.secondary)
                .multilineTextAlignment(.center)

            // Start/Stop button
            Button(action: {
                if sessionManager.isMonitoring {
                    sessionManager.stopMonitoring()
                } else {
                    sessionManager.startMonitoring()
                }
            }) {
                HStack {
                    Image(systemName: sessionManager.isMonitoring ? "stop.fill" : "play.fill")
                    Text(sessionManager.isMonitoring ? "Stop" : "Start")
                }
                .frame(maxWidth: .infinity)
            }
            .buttonStyle(.borderedProminent)
            .tint(sessionManager.isMonitoring ? .red : .green)
        }
        .padding()
    }
}
