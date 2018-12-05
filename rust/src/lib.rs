#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum GuardEvent {
    BeginShift(GuardID),
    FallAsleep,
    WakeUp,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Timestamp {
    year: u32,
    month: u8,
    day: u8,
    hour: u8,
    min: u8,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Event {
    timestamp: Timestamp,
    kind: GuardEvent,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
struct GuardSleepData {
    total_sleeping: usize,
    min_sleeping: Vec<u8>,
}

type GuardID = u32;

pub mod day4 {
    use super::{Event, GuardEvent, GuardID, GuardSleepData, Timestamp};
    use failure::Error;
    use std::collections::HashMap;

    // TODO: implement prase() fromStr or whatever
    fn parse_guard_id(text: &str) -> Option<u32> {
        // Find and split on #, split on the next space, that part is then the ID (between # and
        // next space)
        if let Some(i) = text.find('#') {
            let (_, x) = text.split_at(i);
            let mut it = x.chars();
            it.next();
            let v: Vec<&str> = it.as_str().split(' ').collect();

            return Some(v[0].parse::<u32>().unwrap());
        }

        None
    }

    // TODO: implement prase() fromStr or whatever
    fn parse(text: &str) -> Vec<Event> {
        text.lines()
            .map(|l| {
                // [1518-11-01 00:00] Guard #10 begins shift
                // [1518-11-01 00:05] falls asleep
                // [1518-11-01 00:25] wakes up
                // [1518-11-01 00:30] falls asleep
                // [1518-11-01 00:55] wakes up
                // [1518-11-01 23:58] Guard #99 begins shift
                // This is beautiful parsing. Way better than applicative parsing with trifecta in
                // Haskell. Amirite? ...? :D I really need to learn Rust parsers
                let m = l.replace(" ", "-");
                let m = m.replace("[", "");
                let m = m.replace("]", "");
                let m = m.replace(":", "-");
                let m: Vec<&str> = m.split('-').collect();

                let t = Timestamp {
                    year: m[0].parse::<u32>().unwrap(),
                    month: m[1].parse::<u8>().unwrap(),
                    day: m[2].parse::<u8>().unwrap(),
                    hour: m[3].parse::<u8>().unwrap(),
                    min: m[4].parse::<u8>().unwrap(),
                };

                // TODO: get rid of this
                let mut kind: GuardEvent = GuardEvent::BeginShift(0);

                if let Some(_) = l.find("begins") {
                    let id = parse_guard_id(l).unwrap();
                    kind = GuardEvent::BeginShift(id);
                }

                if let Some(_) = l.find("asleep") {
                    kind = GuardEvent::FallAsleep;
                }

                if let Some(_) = l.find("wakes") {
                    kind = GuardEvent::WakeUp;
                }

                let e = Event { timestamp: t, kind };

                e
            })
            .collect()
    }

    fn part1(events_by_guard: HashMap<GuardID, Vec<Event>>) -> u32 {
        let mut aggregated_data: HashMap<GuardID, GuardSleepData> = HashMap::new();
        // As we iterate over the timestamp, keep track of the minute of the last timestamp.
        // That way we can calculate e.g., time spent sleeping by current - last minute
        let mut last_min: u8 = 0;

        for (guard_id, current_guard_events) in events_by_guard.iter() {
            // TODO: Make this nicer with better entry API search for or_insert
            for event in current_guard_events {
                println!("{:#?}", event);
                match aggregated_data.get_mut(guard_id) {
                    Some(value) => {
                        if let GuardEvent::WakeUp = &event.kind {
                            let min_asleep = event.timestamp.min - last_min;
                            value.total_sleeping += min_asleep as usize;

                            // Add all minutes when guard slept
                            for i in last_min..event.timestamp.min {
                                value.min_sleeping.push(i);
                            }
                        }
                    }
                    None => {
                        aggregated_data.insert(
                            *guard_id,
                            GuardSleepData {
                                total_sleeping: 0,
                                min_sleeping: Vec::new(),
                            },
                        );
                    }
                }

                last_min = event.timestamp.min;
            }
        }

        // Sort the aggregated data by total time sleeping in descending order
        let mut iter: Vec<(&u32, &GuardSleepData)> = aggregated_data.iter().collect();
        iter.sort_by(|a, b| b.1.total_sleeping.cmp(&a.1.total_sleeping));

        // This should now hold the entry with the most time asleep
        let (id_guard_most_asleep, data_guard_most_asleep) = iter[0];

        let mut sleep_frequencies: HashMap<u8, usize> = HashMap::new();

        // Count how often the guard was asleep for each minute
        // TODO: Understand what is happening re borrow deref etc
        for min in data_guard_most_asleep.min_sleeping.clone() {
            *sleep_frequencies.entry(min).or_insert(0) += 1 as usize;
        }

        // Sort the sleep frequences from above in descending order
        let mut freqs: Vec<(&u8, &usize)> = sleep_frequencies.iter().collect();

        freqs.sort_by(|a, b| b.1.cmp(a.1));

        println!("id: {:?}", freqs);

        let (min_most_often_asleep, _) = freqs[0];

        *id_guard_most_asleep * *min_most_often_asleep as u32
    }

    pub fn run(data: &str) -> Result<String, Error> {
        let mut parsed = parse(&data);
        parsed.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

        let mut events_by_guard: HashMap<GuardID, Vec<Event>> = HashMap::new();
        let mut current_guard_id: Option<GuardID> = None;

        for event in parsed {
            if let GuardEvent::BeginShift(id) = event.kind {
                current_guard_id = Some(id);
            };

            let cur = current_guard_id.unwrap();
            events_by_guard.entry(cur).or_default().push(event);
        }

        println!("id: {:#?}", events_by_guard);
        let solution1 = part1(events_by_guard);
        println!("{}", solution1);

        Ok(String::from("foo"))
    }
}
