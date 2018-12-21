// TODO: Finish this (it's currently only implemented in Haskell)
use failure::Error;

#[derive(Debug)]
enum Direction {
    Vertical,
    Horizontal,
}

#[derive(Debug)]
enum TravelDirection {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug)]
enum TrackPart {
    Track(Direction),
    Intersection,
    Corner,
}

#[derive(Debug)]
struct Car {
    x: usize,
    y: usize,
    travel_direction: TravelDirection,
}

pub fn run(data: &str) -> Result<String, Error> {
    use std::collections::HashMap;

    let mut tracks: HashMap<String, TrackPart> = HashMap::new();
    let mut cars: Vec<Car> = Vec::new();

    for (y, line) in data.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c != ' ' {
                let matched_car = match c {
                    '^' => Some(Car {
                        x,
                        y,
                        travel_direction: TravelDirection::Up,
                    }),
                    'v' => Some(Car {
                        x,
                        y,
                        travel_direction: TravelDirection::Down,
                    }),
                    '>' => Some(Car {
                        x,
                        y,
                        travel_direction: TravelDirection::Right,
                    }),
                    '<' => Some(Car {
                        x,
                        y,
                        travel_direction: TravelDirection::Left,
                    }),
                    _ => None,
                };

                if let Some(car) = matched_car {
                    cars.push(car);
                }

                let matched_track_part = match c {
                    '|' => Some(TrackPart::Track(Direction::Vertical)),
                    '-' => Some(TrackPart::Track(Direction::Horizontal)),
                    '+' => Some(TrackPart::Intersection),
                    '\\' => Some(TrackPart::Corner),
                    '/' => Some(TrackPart::Corner),
                    _ => None,
                };

                if let Some(track_part) = matched_track_part {
                    let key = format!("{:05}{:05}", x, y);
                    tracks.insert(key, track_part);
                };
            }
        }
    }

    println!("{:?}", tracks);
    println!("{:?}", cars);

    Ok((String::from("foo")))
}
