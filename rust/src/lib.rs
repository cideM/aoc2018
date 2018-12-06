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
                                x.sleep_frequency_per_min
                                    .entry(i)
                                    .and_modify(|e| *e += 1 as TimeAsleep)
                                    .or_insert(0);
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
                min_most_slept
                    .entry(*min)
                    .and_modify(|e| {
                        if time_asleep > &e.1 {
                            e.0 = *guard_id;
                            e.1 = *time_asleep;
                        }
                    })
                    .or_insert((*guard_id, *time_asleep));
            }
        }

        let (min, (guard_id, _)) = min_most_slept
            .iter()
            .max_by_key(|(_, (_, time_asleep))| -> TimeAsleep { *time_asleep })
            .unwrap();

        guard_id.0 * u32::from(*min)
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

pub mod day5 {
    use failure::Error;
    use std::sync::{Arc, Mutex};
    use std::thread;

    #[derive(Debug, Eq, PartialEq)]
    pub enum PolymerCheck {
        Fuse,
        Keep,
    }

    pub fn check_pair(a: char, b: char) -> PolymerCheck {
        if a.to_lowercase().collect::<Vec<_>>() != b.to_lowercase().collect::<Vec<_>>() {
            return PolymerCheck::Keep;
        }

        if a.is_uppercase() && b.is_uppercase() {
            return PolymerCheck::Keep;
        }

        if a.is_lowercase() && b.is_lowercase() {
            return PolymerCheck::Keep;
        }

        PolymerCheck::Fuse
    }

    pub fn react_polymer(s: &str) -> String {
        let mut fuse_occurred: bool = false;
        let orig_len = s.len();
        let mut out: String = String::with_capacity(orig_len);
        let chars = s.chars().enumerate();
        let mut last_char: Option<char> = None;

        for (i, c) in chars {
            if last_char.is_none() {
                last_char = Some(c);
                continue;
            }

            match check_pair(last_char.unwrap(), c) {
                PolymerCheck::Keep => {
                    out.push(last_char.unwrap());
                    last_char = Some(c);

                    if i == orig_len - 1 {
                        out.push(c);
                    }
                }
                PolymerCheck::Fuse => {
                    last_char = None;
                    fuse_occurred = true;
                }
            }
        }

        if fuse_occurred == true {
            react_polymer(&out)
        } else {
            out
        }
    }

    pub fn run(data: &str) -> Result<String, Error> {
        let letters = "abcdefghijklmnopqrstuvwxyz";

        let res: Arc<Mutex<(Option<usize>, Option<usize>)>> = Arc::new(Mutex::new((None, None)));
        let mut hs = Vec::new();

        // I need to clone the data twice. Once here for this thread,
        // and then several times later on for the other threads. Ugly but it works and it's
        // acceptable for this part.
        let data1 = data.to_owned();
        let res1 = Arc::clone(&res);

        hs.push(thread::spawn(move || {
            let len = react_polymer(&data1).len();
            let mut solutions = res1.lock().unwrap();

            solutions.1 = Some(len);
        }));

        // Remove the letter and its uppercase variant from the source, then react it.
        // There's a great optimization by realizing that you can also just react once and then
        // remove the letters. I got that from a reddit post https://blog.jle.im/entry/alchemical-groups.html
        for c in letters.chars() {
            let res = Arc::clone(&res);
            let data = data.to_owned();

            let h = thread::spawn(move || {
                let cleaned = data
                    .replace(c, "")
                    .replace(&c.to_uppercase().to_string(), "");
                let len = react_polymer(&cleaned).len();
                let mut solutions = res.lock().unwrap();

                if let Some(min) = solutions.0 {
                    if len < min {
                        solutions.0 = Some(len);
                    }
                } else {
                    solutions.0 = Some(len);
                }
            });

            hs.push(h);
        }

        for h in hs {
            h.join().unwrap();
        }

        let (min, _) = *res.lock().unwrap();

        Ok(format!("length: {}, shortest: {}", min.unwrap(), 0))
    }
}

pub mod day6 {
    #[derive(Eq, PartialEq, Debug)]
    pub struct Point {
        pub x: isize,
        pub y: isize,
    }

    // Taxicab Distance between (x_1,y_1) and (x_2,y_2)=|x_2-x_1|+|y_2-y_1|

    pub fn distance(p1: &Point, p2: &Point) -> isize {
        let dx = p2.x - p1.x;
        let dy = p2.y - p1.y;

        dx.abs() + dy.abs()
    }

    pub fn find_center(ps: &[Point]) -> Option<&Point> {
        // Iterate over the points. Put each point (base) in a singleton vector,
        // zip it with all other points, and calculate the distance between base
        // and the other points (comp). Fold that zipped iterator down to the
        // average distance between base and all comps.
        ps.iter()
            .map(|p| {
                let total_dist: isize = ps.iter().map(|comp| distance(p, comp)).sum();

                (p, total_dist / ps.len() as isize)
            })
            .min_by_key(|(_, avg)| avg.clone())
            .map(|(p, _)| p)
    }

    pub fn find_edge_points(ps: &[Point]) -> Vec<&Point> {
        let min_x = ps.iter().min_by_key(|p| p.x).map(|p| p.x).unwrap();
        let max_x = ps.iter().max_by_key(|p| p.x).map(|p| p.x).unwrap();
        let min_y = ps.iter().min_by_key(|p| p.y).map(|p| p.y).unwrap();
        let max_y = ps.iter().max_by_key(|p| p.y).map(|p| p.y).unwrap();

        let edge_coords = vec![min_x, min_y, max_x, max_y];

        ps.iter()
            .filter(|p| {
                edge_coords
                    .iter()
                    .any(|&coord| p.x == coord || p.y == coord)
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::day5::{check_pair, react_polymer, PolymerCheck};
    use super::day6::{find_center, find_edge_points, Point};

    #[test]
    fn it_checks_pairs() {
        assert_eq!(check_pair('a', 'A'), PolymerCheck::Fuse);
        assert_eq!(check_pair('A', 'A'), PolymerCheck::Keep);
        assert_eq!(check_pair('B', 'A'), PolymerCheck::Keep);
        assert_eq!(check_pair('b', 'b'), PolymerCheck::Keep);
        assert_eq!(check_pair('A', 'b'), PolymerCheck::Keep);
        assert_eq!(check_pair('B', 'b'), PolymerCheck::Fuse);
    }

    #[test]
    fn it_reacts() {
        assert_eq!(react_polymer("aAbbAb"), String::from("bbAb"));
        assert_eq!(
            react_polymer("dabAcCaCBAcCcaDA"),
            String::from("dabCBAcaDA")
        );
        assert_eq!(
            react_polymer("YyLlXxYKkbNnQqBFfxXbyYWwBhHyYTCBbCjIiqwtTWQ"),
            String::from("YTCCjq")
        );
    }

    #[test]
    fn it_finds_center() {
        let ps: Vec<Point> = vec![(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
            .iter()
            .map(|(x, y)| Point { x: *x, y: *y })
            .collect();

        assert_eq!(find_center(&ps).unwrap(), &Point { x: 3, y: 4 });
    }

    #[test]
    fn it_finds_edges() {
        let ps: Vec<Point> = vec![(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
            .iter()
            .map(|(x, y)| Point { x: *x, y: *y })
            .collect();

        assert_eq!(
            find_edge_points(&ps),
            vec![
                &Point { x: 1, y: 1 },
                &Point { x: 1, y: 6 },
                &Point { x: 8, y: 3 },
                &Point { x: 8, y: 9 }
            ]
        );
    }
}
