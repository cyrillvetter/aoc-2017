use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    println!("{}", input);
    Solution::Empty
}

pub fn part_two(input: &str) -> Solution {
    Solution::Empty
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 10);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 10);
        assert_eq!(part_one(&input), Solution::Empty);
        assert_eq!(part_two(&input), Solution::Empty);
    }
}
