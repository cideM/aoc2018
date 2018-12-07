// TODO: separate files
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
                        if *time_asleep > e.1 {
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
    use failure::Error;
    use std::collections::HashMap;
    use std::str::FromStr;

    #[derive(Eq, PartialEq, Debug, Hash, Ord, PartialOrd, Clone)]
    pub struct Point {
        pub x: isize,
        pub y: isize,
    }

    impl Point {
        pub fn is_on_border(&self, g: &Grid) -> bool {
            g.origin.x == self.x
                || g.origin.y == self.y
                || g.opposite.x == self.x
                || g.opposite.y == self.y
        }

        // TODO: tests
        pub fn find_closest<'a>(&self, ps: &'a [Point]) -> Vec<&'a Point> {
            let mut closest_points = Vec::new();
            let mut closest_dist = None;

            for p_comp in ps.iter() {
                let dist = distance(self, p_comp);

                match closest_dist {
                    Some(d) => {
                        if dist < d {
                            closest_points.clear();
                            closest_points.push(p_comp);

                            closest_dist = Some(dist);
                        }

                        if dist == d {
                            closest_points.push(p_comp);
                        }
                    }
                    None => {
                        closest_points.push(p_comp);
                        closest_dist = Some(dist);
                    }
                }
            }

            closest_points
        }
    }

    impl FromStr for Point {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let ss: Vec<&str> = s.split(',').collect();

            Ok(Point {
                x: ss[0].trim().parse()?,
                y: ss[1].trim().parse()?,
            })
        }
    }

    #[derive(Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
    pub struct Grid {
        origin: Point,
        opposite: Point,
        width: isize,
        height: isize,
    }

    impl Grid {
        // TODO: This should not need isized since it cant have neg width height
        pub fn new(opposite: Point) -> Grid {
            Grid {
                origin: Point { x: 0, y: 0 },
                opposite: opposite.clone(),
                width: opposite.x,
                height: opposite.y,
            }
        }
    }

    impl<'a> IntoIterator for &'a Grid {
        type Item = Point;
        type IntoIter = GridIterator<'a>;

        fn into_iter(self) -> Self::IntoIter {
            GridIterator {
                origin: &self.origin,
                opposite: &self.opposite,
                width: self.width,
                height: self.height,
                current_x: self.origin.x,
                current_y: self.origin.y,
                done: false,
            }
        }
    }

    #[derive(Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
    pub struct GridIterator<'a> {
        origin: &'a Point,
        opposite: &'a Point,
        width: isize,
        height: isize,
        current_x: isize,
        current_y: isize,
        done: bool,
    }

    impl<'a> Iterator for GridIterator<'a> {
        type Item = Point;

        fn next(&mut self) -> Option<Point> {
            let current_point = Point {
                x: self.current_x,
                y: self.current_y,
            };

            if self.current_x < self.width {
                self.current_x += 1;
            } else if self.current_x == self.width && self.current_y < self.height {
                self.current_x = self.origin.x;
                self.current_y += 1;
            } else if self.current_x == self.width && self.current_y == self.height && !self.done {
                self.done = true;

                return Some(current_point);
            }

            if !self.done {
                return Some(current_point);
            } else {
                return None;
            }
        }
    }

    pub fn distance(p1: &Point, p2: &Point) -> isize {
        let dx = p2.x - p1.x;
        let dy = p2.y - p1.y;

        dx.abs() + dy.abs()
    }

    pub fn run(data: &str) -> Result<String, Error> {
        let points: Vec<Point> = data.lines().map(|l| l.parse::<Point>().unwrap()).collect();

        let max_x = points.iter().max_by_key(|p| p.x).map(|p| p.x).unwrap();
        let max_y = points.iter().max_by_key(|p| p.y).map(|p| p.y).unwrap();

        let grid: Grid = Grid::new(Point {
            x: max_x * 2,
            y: max_y * 2,
        });

        // Maps from the points we get per CLI to points generated in grid
        let mut owned_points: HashMap<&Point, Vec<Point>> = HashMap::new();

        for cur_p in grid.into_iter() {
            let closest = cur_p.find_closest(&points);

            if closest.len() == 1 {
                owned_points.entry(closest[0]).or_default().push(cur_p);
            }
        }

        let size_largest_area: usize = owned_points
            .iter()
            .filter(|(_, owned_ps)| {
                owned_ps.iter().all(|owned| {
                    // println!("{:?}", owned);
                    owned.is_on_border(&grid) == false
                })
            })
            .map(|(_, owned_points)| owned_points.len())
            .max()
            .unwrap();

        // part2
        // size of the region containing all locations
        // which have a total distance to all given coordinates of less than 10000
        let mut size = 0;

        for cur_p in grid.into_iter() {
            let total: isize = points.iter().map(|p| distance(&p, &cur_p)).sum();

            if total < 10000 {
                size += 1;
            };
        }

        println!("{}", size);

        Ok(format!("{:#?}", size_largest_area))
    }
}

#[cfg(test)]
mod tests {
    use super::day5::{check_pair, react_polymer, PolymerCheck};
    use super::day6::{Grid, Point};

    fn test_ps() -> Vec<Point> {
        vec![(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
            .iter()
            .map(|(x, y)| Point { x: *x, y: *y })
            .collect()
    }

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
    fn it_finds_closest() {
        let ps = test_ps();
        let p = Point { x: 8, y: 3 };

        assert_eq!(p.find_closest(&ps).len(), 1);

        assert_eq!(p.find_closest(&ps)[0], &Point { x: 8, y: 3 });

        let ps: Vec<Point> = vec![(5, 0), (1, 0)]
            .iter()
            .map(|(x, y)| Point { x: *x, y: *y })
            .collect();

        let p = Point { x: 3, y: 0 };

        assert_eq!(p.find_closest(&ps).len(), 2);

        assert_eq!(
            p.find_closest(&ps),
            vec![&Point { x: 5, y: 0 }, &Point { x: 1, y: 0 }]
        )
    }

    #[test]

    fn it_checks_if_point_is_on_border() {
        let p = Point { x: 8, y: 3 };
        let g = Grid::new(Point { x: 8, y: 0 });

        assert_eq!(p.is_on_border(&g), true);

        // Haha the point is actually outside the grid... whatever. FIXME
        let p = Point { x: 8, y: 3 };
        let g = Grid::new(Point { x: 2, y: 3 });

        assert_eq!(p.is_on_border(&g), true)
    }
}
