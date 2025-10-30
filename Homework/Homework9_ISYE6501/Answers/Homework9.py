import simpy
import random

def passenger(env, name, id_check, scanners):
    arrival_time = env.now
    with id_check.request() as req:
        yield req
        yield env.timeout(random.expovariate(1 / 0.75))  # ID check service time

    # Choose shortest scanner queue
    scanner = min(scanners, key=lambda s: len(s.queue))
    with scanner.request() as req:
        yield req
        yield env.timeout(random.uniform(0.5, 1))  # Personal check time

    wait_time = env.now - arrival_time
    wait_times.append(wait_time)

def arrival_process(env, id_check, scanners, rate):
    i = 0
    while True:
        yield env.timeout(random.expovariate(rate))
        env.process(passenger(env, f"Passenger {i}", id_check, scanners))
        i += 1

# Parameters
arrival_rate = 5  # λ = 5 per minute
wait_times = []

# Simulation setup
env = simpy.Environment()
id_check = simpy.Resource(env, capacity=3)  # Try 2, 3, 4...
scanners = [simpy.Resource(env, capacity=1) for _ in range(3)]  # Try 2, 3, 4...

env.process(arrival_process(env, id_check, scanners, arrival_rate))
env.run(until=1000)

# Results
average_wait = sum(wait_times) / len(wait_times)
print(f"Average wait time: {average_wait:.2f} minutes")
