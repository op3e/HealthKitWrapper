import SwiftUI

@main
struct WatchApp: App {
    @State private var sessionManager = WorkoutSessionManager.shared

    var body: some Scene {
        WindowGroup {
            ContentView(sessionManager: sessionManager)
        }
    }
}
