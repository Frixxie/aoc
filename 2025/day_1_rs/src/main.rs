use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug)]
enum Operation {
    R(i32),
    L(i32),
}

impl Operation {
    fn parse(line: &str) -> Option<Operation> {
        let line = line.trim();
        if line.is_empty() {
            return None;
        }
        let (op, num_str) = line.split_at(1);
        let num = num_str.trim().parse::<i32>().ok()?;
        match op {
            "R" => Some(Operation::R(num)),
            "L" => Some(Operation::L(num)),
            _ => None,
        }
    }
}

fn inc_count(s: i32, c: i32) -> i32 {
    if s == 0 { c + 1 } else { c }
}

fn apply_operation_prime(op: &Operation, s: i32, c: i32) -> (i32, i32) {
    match op {
        Operation::R(n) => {
            let mut l = 0;
            let mut res = s;
            for _ in 0..*n {
                res = (res + 1) % 100;
                if res == 0 {
                    println!("huzzah");
                    l += 1;
                }
            }
            dbg!(res, c + l)
        }
        Operation::L(n) => {
            let mut l = 0;
            let mut res = s;
            for _ in 0..*n {
                res = (res - 1 + 100) % 100;
                if res == 0 {
                    println!("huzzah");
                    l += 1
                }
            }
            dbg!(res, c + l)
        }
    }
}

fn apply_operation(op: &Operation, s: i32, c: i32) -> (i32, i32) {
    match op {
        Operation::R(n) => {
            let mut res = s;
            for _ in 0..*n {
                res = (res + 1) % 100;
            }
            dbg!(res, inc_count(res, c))
        }
        Operation::L(n) => {
            let mut res = s;
            for _ in 0..*n {
                res = (res - 1 + 100) % 100;
            }
            dbg!(res, inc_count(res, c))
        }
    }
}

fn read_input(path: &str) -> io::Result<Vec<Operation>> {
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);
    Ok(reader
        .lines()
        .filter_map(|line| line.ok())
        .filter_map(|line| Operation::parse(&line))
        .collect())
}

fn main() {
    match read_input("input.txt") {
        Ok(operations) => {
            let final_state = operations
                .iter()
                .fold((50, 0), |(s, c), op| apply_operation(op, s, c));
            println!("{:?}", final_state);

            let final_state_prime = operations
                .iter()
                .fold((50, 0), |(s, c), op| apply_operation_prime(op, s, c));
            println!("{:?}", final_state_prime);
        }
        Err(e) => {
            eprintln!("Error reading input: {}", e);
        }
    }
}
