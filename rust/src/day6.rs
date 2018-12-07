use failure::Error;
use std::collections::HashMap;
use std::str::FromStr;

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
        let total: isize = points.iter().map(|p| p.distance(&cur_p)).sum();

        if total < 10000 {
            size += 1;
        };
    }

    Ok(format!("{:#?} {}", size_largest_area, size))
}

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

    pub fn distance(&self, p2: &Point) -> isize {
        let dx = p2.x - self.x;
        let dy = p2.y - self.y;

        dx.abs() + dy.abs()
    }

    pub fn find_closest<'a>(&self, ps: &'a [Point]) -> Vec<&'a Point> {
        let mut closest_points = Vec::new();
        let mut closest_dist = None;

        for p_comp in ps.iter() {
            let dist = self.distance(p_comp);

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

#[cfg(test)]
mod tests {
    use super::{Grid, Point};

    fn test_ps() -> Vec<Point> {
        vec![(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
            .iter()
            .map(|(x, y)| Point { x: *x, y: *y })
            .collect()
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
