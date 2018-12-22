use failure::Error;

fn to_individual_digits(num: u32) -> Vec<u32> {
    num.to_string()
        .chars()
        .map(|d| d.to_digit(10).unwrap())
        .collect()
}

fn add_recipe<'a>(scores: &'a mut Vec<u32>, elves: &'a mut [u32]) -> () {
    let mut new_recipe_score = 0;
    let mut elf_steps = vec![];

    for &index in elves.iter() {
        let current_recipe_score = scores[(index as usize)];
        new_recipe_score += current_recipe_score;
        elf_steps.push(1 + current_recipe_score);
    }

    let mut new_recipe_score = to_individual_digits(new_recipe_score);

    scores.append(&mut new_recipe_score);

    let scores_len = scores.len();

    for (index, &steps) in elf_steps.iter().enumerate() {
        let current_index = elves[index];
        let new_index = current_index + steps;
        elves[index] = new_index % (scores_len as u32);
    }
}

fn part1(input: &str) -> String {
    println!("{}", input);
    let recipes_to_make = input.parse::<usize>().unwrap();
    let mut scores = vec![3, 7];
    let mut elves = vec![0, 1];

    while scores.len() <= recipes_to_make + 10 {
        add_recipe(&mut scores, &mut elves);
    }

    scores[recipes_to_make..recipes_to_make + 10]
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>()
        .concat()
}

fn part2(input: &str) -> String {
    let pattern: Vec<u32> = input
        .chars()
        .map(|char| char.to_digit(10).unwrap())
        .collect();

    let mut scores = vec![3, 7];
    let mut elves = vec![0, 1];

    let mut part2 = 0;

    loop {
        if scores[..].ends_with(&pattern[..]) {
            part2 = scores.len() - pattern.len();
            break;
        } else if scores[..scores.len() - 1].ends_with(&pattern[..]) {
            part2 = scores.len() - pattern.len() - 1;
            break;
        }
        add_recipe(&mut scores, &mut elves);
    }

    format!("{}", part2)
}

pub fn run(input: &str) -> Result<String, Error> {
    let input = input.trim();
    let part1_solution = part1(input);
    let part2_solution = part2(input);

    Ok(format!(
        "part1: {:?} part2: {:?}",
        part1_solution, part2_solution
    ))
}
