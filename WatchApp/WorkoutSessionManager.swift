import Foundation
import HealthKit
import WatchConnectivity
import Observation

@Observable
@MainActor
class WorkoutSessionManager: NSObject {
    static let shared = WorkoutSessionManager()

    private let healthStore = HKHealthStore()
    private var workoutSession: HKWorkoutSession?
    private var workoutBuilder: HKLiveWorkoutBuilder?
    private var heartRateQuery: HKAnchoredObjectQuery?

    var currentHeartRate: Double?
    var isMonitoring = false
    var statusMessage = "Ready"

    private var wcSession: WCSession?

    override init() {
        super.init()
        setupWatchConnectivity()
        requestAuthorization()
    }

    // MARK: - Watch Connectivity

    private func setupWatchConnectivity() {
        if WCSession.isSupported() {
            wcSession = WCSession.default
            wcSession?.delegate = self
            wcSession?.activate()
        }
    }

    private nonisolated func sendHeartRateToPhone(_ heartRate: Double, timestamp: Date) {
        guard let session = WCSession.default.isReachable ? WCSession.default : nil else { return }

        let message: [String: Any] = [
            "type": "heartRate",
            "bpm": heartRate,
            "timestamp": timestamp.timeIntervalSince1970
        ]

        session.sendMessage(message, replyHandler: nil) { error in
            print("Failed to send heart rate: \(error.localizedDescription)")
        }
    }

    // MARK: - HealthKit Authorization

    private func requestAuthorization() {
        guard HKHealthStore.isHealthDataAvailable() else {
            statusMessage = "HealthKit not available"
            return
        }

        let heartRateType = HKQuantityType.quantityType(forIdentifier: .heartRate)!
        let workoutType = HKObjectType.workoutType()

        let readTypes: Set<HKObjectType> = [heartRateType]
        let shareTypes: Set<HKSampleType> = [heartRateType, workoutType]

        healthStore.requestAuthorization(toShare: shareTypes, read: readTypes) { [weak self] success, error in
            Task { @MainActor in
                if success {
                    self?.statusMessage = "Ready"
                } else {
                    self?.statusMessage = "Authorization denied"
                }
            }
        }
    }

    // MARK: - Workout Session

    func startMonitoring() {
        guard !isMonitoring else { return }

        let configuration = HKWorkoutConfiguration()
        configuration.activityType = .other
        configuration.locationType = .unknown

        do {
            workoutSession = try HKWorkoutSession(healthStore: healthStore, configuration: configuration)
            workoutBuilder = workoutSession?.associatedWorkoutBuilder()

            workoutSession?.delegate = self
            workoutBuilder?.delegate = self

            workoutBuilder?.dataSource = HKLiveWorkoutDataSource(
                healthStore: healthStore,
                workoutConfiguration: configuration
            )

            let startDate = Date()
            workoutSession?.startActivity(with: startDate)
            workoutBuilder?.beginCollection(withStart: startDate) { [weak self] success, error in
                Task { @MainActor in
                    if success {
                        self?.isMonitoring = true
                        self?.statusMessage = "Monitoring..."
                        self?.startHeartRateQuery()
                    } else {
                        self?.statusMessage = "Failed to start"
                    }
                }
            }
        } catch {
            statusMessage = "Error: \(error.localizedDescription)"
        }
    }

    func stopMonitoring() {
        guard isMonitoring else { return }

        stopHeartRateQuery()

        workoutSession?.end()

        workoutBuilder?.endCollection(withEnd: Date()) { [weak self] success, error in
            self?.workoutBuilder?.finishWorkout { workout, error in
                Task { @MainActor in
                    self?.isMonitoring = false
                    self?.currentHeartRate = nil
                    self?.statusMessage = "Stopped"
                    self?.workoutSession = nil
                    self?.workoutBuilder = nil
                }
            }
        }
    }

    // MARK: - Heart Rate Query

    private func startHeartRateQuery() {
        let heartRateType = HKQuantityType.quantityType(forIdentifier: .heartRate)!

        let predicate = HKQuery.predicateForSamples(
            withStart: Date(),
            end: nil,
            options: .strictStartDate
        )

        heartRateQuery = HKAnchoredObjectQuery(
            type: heartRateType,
            predicate: predicate,
            anchor: nil,
            limit: HKObjectQueryNoLimit
        ) { [weak self] query, samples, deletedObjects, anchor, error in
            self?.processHeartRateSamples(samples)
        }

        heartRateQuery?.updateHandler = { [weak self] query, samples, deletedObjects, anchor, error in
            self?.processHeartRateSamples(samples)
        }

        if let query = heartRateQuery {
            healthStore.execute(query)
        }
    }

    private func stopHeartRateQuery() {
        if let query = heartRateQuery {
            healthStore.stop(query)
            heartRateQuery = nil
        }
    }

    private nonisolated func processHeartRateSamples(_ samples: [HKSample]?) {
        guard let samples = samples as? [HKQuantitySample], let sample = samples.last else { return }

        let heartRateUnit = HKUnit.count().unitDivided(by: .minute())
        let heartRate = sample.quantity.doubleValue(for: heartRateUnit)

        Task { @MainActor [weak self] in
            self?.currentHeartRate = heartRate
        }

        // Send to phone
        sendHeartRateToPhone(heartRate, timestamp: sample.startDate)
    }
}

// MARK: - HKWorkoutSessionDelegate

extension WorkoutSessionManager: HKWorkoutSessionDelegate {
    nonisolated func workoutSession(_ workoutSession: HKWorkoutSession, didChangeTo toState: HKWorkoutSessionState, from fromState: HKWorkoutSessionState, date: Date) {
        Task { @MainActor [weak self] in
            switch toState {
            case .running:
                self?.statusMessage = "Monitoring..."
            case .ended:
                self?.statusMessage = "Ended"
            case .paused:
                self?.statusMessage = "Paused"
            default:
                break
            }
        }
    }

    nonisolated func workoutSession(_ workoutSession: HKWorkoutSession, didFailWithError error: Error) {
        Task { @MainActor [weak self] in
            self?.statusMessage = "Error: \(error.localizedDescription)"
            self?.isMonitoring = false
        }
    }
}

// MARK: - HKLiveWorkoutBuilderDelegate

extension WorkoutSessionManager: HKLiveWorkoutBuilderDelegate {
    nonisolated func workoutBuilder(_ workoutBuilder: HKLiveWorkoutBuilder, didCollectDataOf collectedTypes: Set<HKSampleType>) {
        // Heart rate updates come through the anchored query
    }

    nonisolated func workoutBuilderDidCollectEvent(_ workoutBuilder: HKLiveWorkoutBuilder) {
        // Handle workout events if needed
    }
}

// MARK: - WCSessionDelegate

extension WorkoutSessionManager: WCSessionDelegate {
    nonisolated func session(_ session: WCSession, activationDidCompleteWith activationState: WCSessionActivationState, error: Error?) {
        if let error = error {
            print("WCSession activation failed: \(error.localizedDescription)")
        }
    }

    nonisolated func session(_ session: WCSession, didReceiveMessage message: [String: Any]) {
        // Handle commands from iPhone
        if let command = message["command"] as? String {
            Task { @MainActor [weak self] in
                switch command {
                case "startMonitoring":
                    self?.startMonitoring()
                case "stopMonitoring":
                    self?.stopMonitoring()
                default:
                    break
                }
            }
        }
    }
}
