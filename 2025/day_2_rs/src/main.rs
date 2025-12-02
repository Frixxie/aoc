use std::fs;

fn is_invalid_id(s1: &str, s2: &str) -> bool {
    s1 == s2
}

fn check_invalid(rate_id: &str) -> bool {
    let mid = rate_id.len() / 2;
    let (start, end) = rate_id.split_at(mid);
    is_invalid_id(start, end)
}

fn gen_range(start_id: i32, end_id: i32) -> Vec<String> {
    (start_id..=end_id).map(|n| n.to_string()).collect()
}

fn parse_rate_id_range(range_str: &str) -> (i32, i32) {
    let parts: Vec<&str> = range_str.split('-').collect();
    let start_id = parts[0].parse::<i32>().unwrap();
    let end_id = parts[1].parse::<i32>().unwrap();
    (start_id, end_id)
}

fn parse_rate_id_list(input: &str) -> Vec<&str> {
    input.split(',').collect()
}

fn read_input(file_path: &str) -> Vec<(i32, i32)> {
    let contents = fs::read_to_string(file_path).expect("Failed to read file");
    let ranges = parse_rate_id_list(contents.trim());
    ranges.iter().map(|r| parse_rate_id_range(r)).collect()
}

fn main() {
    let file_path = "test_input.txt";
    let ranges = read_input(file_path);
    let all_ranges: Vec<Vec<String>> = ranges
        .iter()
        .map(|(start, end)| gen_range(*start, *end))
        .collect();
    let all_strings: Vec<String> = all_ranges.into_iter().flatten().collect();
    let invalid: Vec<&String> = all_strings.iter().filter(|s| check_invalid(s)).collect();
    let invalid_ids: Vec<i32> = invalid.iter().map(|s| s.parse::<i32>().unwrap()).collect();
    let sum: i32 = invalid_ids.iter().sum();
    println!("{}", sum);
}
