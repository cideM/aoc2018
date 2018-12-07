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

#[cfg(test)]
mod tests {
    use super::{check_pair, react_polymer, PolymerCheck};

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
}
