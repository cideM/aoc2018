pub mod day4 {
    use failure::Error;
    use lazy_static::lazy_static;
    use regex::Regex;
    use std::collections::HashMap;
    use std::str::FromStr;

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
        min: Minute,
    }

    impl FromStr for Event {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            lazy_static! {
                static ref RE: Regex =
                    Regex::new(r"\[(?P<y>\d+)-(?P<m>\d+)-(?P<d>\d+)\W+(?P<h>\d+):(?P<min>\d+)\].*")
                        .unwrap();
            }

            let caps = RE.captures(s).unwrap();

            let t = Timestamp {
                year: caps["y"].parse()?,
                month: caps["m"].parse()?,
                day: caps["d"].parse()?,
                hour: caps["h"].parse()?,
                min: caps["min"].parse()?,
            };

            let mut kind: Option<GuardEvent> = None;

            if let Some(_) = s.find("begins") {
                let id: GuardID = s.parse::<GuardID>().unwrap();
                kind = Some(GuardEvent::BeginShift(id));
            }

            if let Some(_) = s.find("asleep") {
                kind = Some(GuardEvent::FallAsleep);
            }

            if let Some(_) = s.find("wakes") {
                kind = Some(GuardEvent::WakeUp);
            }

            Ok(Event {
                timestamp: t,
                kind: kind.unwrap(),
            })
        }
    }

    #[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
    struct Event {
        timestamp: Timestamp,
        kind: GuardEvent,
    }

    #[derive(Debug, Eq, PartialEq, Clone)]
    struct GuardSleepData {
        total_sleeping: TimeAsleep,
        sleep_frequency_per_min: HashMap<Minute, TimeAsleep>,
    }

    #[derive(Debug, Eq, PartialOrd, PartialEq, Hash, Ord, Clone, Copy)]
    struct GuardID(u32);

    type Minute = u8;
    type TimeAsleep = usize;
    type MinutesMostSlept = HashMap<Minute, (GuardID, TimeAsleep)>;

    impl FromStr for GuardID {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let re = Regex::new(r"#(?P<id>\d+)").unwrap();

            let caps = re.captures(s).unwrap();

            Ok(GuardID(caps["id"].parse()?))
        }
    }

    fn aggregate_sleep_data(
        events_by_guard: HashMap<GuardID, Vec<Event>>,
    ) -> HashMap<GuardID, GuardSleepData> {
        let mut aggregated_data: HashMap<GuardID, GuardSleepData> = HashMap::new();
        // As we iterate over the timestamp, keep track of the minute of the last timestamp.
        // That way we can calculate e.g., time spent sleeping by current - last minute
        let mut last_min: Minute = 0;

        for (guard_id, current_guard_events) in events_by_guard.iter() {
            for event in current_guard_events {
                match aggregated_data.get_mut(guard_id) {
                    Some(x) => {
                        if let GuardEvent::WakeUp = &event.kind {
                            let min_asleep = event.timestamp.min - last_min;
                            x.total_sleeping += min_asleep as TimeAsleep;

                            // Add all minutes when guard slept
                            for i in last_min..event.timestamp.min {
                                *x.sleep_frequency_per_min.entry(i).or_insert(0) += 1 as TimeAsleep;
                            }
                        }
                    }
                    None => {
                        aggregated_data.insert(
                            *guard_id,
                            GuardSleepData {
                                total_sleeping: 0,
                                sleep_frequency_per_min: HashMap::new(),
                            },
                        );
                    }
                }

                last_min = event.timestamp.min;
            }
        }

        aggregated_data
    }

    fn part1(sleep_data: &HashMap<GuardID, GuardSleepData>) -> u32 {
        // Sort the aggregated data by total time sleeping in descending order
        let (id_most_asleep, data_most_asleep) = sleep_data
            .iter()
            .max_by_key(|(_, ref data)| -> TimeAsleep { data.total_sleeping })
            .unwrap();

        // Sort the sleep frequences from above in descending order
        let (min_most_asleep, _) = data_most_asleep
            .sleep_frequency_per_min
            .iter()
            .max_by_key(|(_, ref freq)| -> TimeAsleep { **freq })
            .unwrap();

        id_most_asleep.0 * *min_most_asleep as u32
    }

    fn part2(sleep_data: &HashMap<GuardID, GuardSleepData>) -> u32 {
        let mut min_most_slept: MinutesMostSlept = HashMap::new();

        // Iterate over the sleep data per guard, and then over the frequencies
        // of that guard, per minute. Create a new map which maps from a minute,
        // to a tuple of guard ID and time asleep spent on that minute.
        // Whenever we find a guard who has slept more on any particular minute than what we have
        // in our map, we update the entry. At the end we have a map which tells us which guard has
        // slept most on any particular minute.
        for (guard_id, sleep_data) in sleep_data.iter() {
            for (min, time_asleep) in sleep_data.sleep_frequency_per_min.iter() {

                let mut e = *min_most_slept
                    .entry(*min)
                    .or_insert((*guard_id, *time_asleep));

                let (_, ref cur_time_asleep) = e;

                if time_asleep > cur_time_asleep {
                    e = (*guard_id, *time_asleep);
                }
            }
        }

        let (min, (guard_id, _)) = min_most_slept
            .iter()
            .max_by_key(|(_, (_, time_asleep))| -> TimeAsleep { *time_asleep })
            .unwrap();

        guard_id.0 * *min as u32
    }

    pub fn run(data: &str) -> Result<String, Error> {
        let mut events: Vec<Event> = data.lines().map(|x| x.parse::<Event>().unwrap()).collect();
        events.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

        let mut events_by_guard: HashMap<GuardID, Vec<Event>> = HashMap::new();
        let mut current_guard_id: Option<GuardID> = None;

        for event in events {
            if let GuardEvent::BeginShift(id) = event.kind {
                current_guard_id = Some(id);
            };

            let cur = current_guard_id.unwrap();
            events_by_guard.entry(cur).or_default().push(event);
        }

        let aggregated_data = aggregate_sleep_data(events_by_guard);

        let solution1 = part1(&aggregated_data);
        let solution2 = part2(&aggregated_data);

        Ok(format!("{:#?} {:#?}", solution1, solution2))
    }
}
