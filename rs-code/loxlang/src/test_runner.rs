use crate::execution_env::{Deps, ExecEnv, Value};
use crate::parse::chumsky_parser::{lexer, program_parser};
use crate::runtime::Runtime;
use chumsky::Parser;
use regex::Regex;
use std::fs;
use std::path::Path;

/// A test dependency implementation that captures printed output
pub struct TestDeps {
    printed: Vec<String>,
    time: f64,
}

impl TestDeps {
    pub fn new() -> Self {
        Self {
            printed: Vec::new(),
            time: 0.0,
        }
    }

    pub fn get_output(self) -> Vec<String> {
        self.printed
    }
}

impl Deps for TestDeps {
    fn print<'src>(&mut self, v: Value<'src>) {
        self.printed.push(format!("{v}"));
    }

    fn clock(&mut self) -> f64 {
        let time = self.time;
        self.time += 1.0;
        time
    }
}

/// Represents an expected output at a specific line
#[derive(Debug, Clone, PartialEq)]
pub struct ExpectedOutput {
    pub line: usize,
    pub value: String,
}

/// Represents a test case parsed from a .lox file
#[derive(Debug)]
pub struct TestCase {
    pub name: String,
    pub source: String,
    pub expected_outputs: Vec<ExpectedOutput>,
}

impl TestCase {
    /// Parse a test case from a file
    pub fn from_file(path: &Path) -> std::io::Result<Self> {
        let source = fs::read_to_string(path)?;
        let name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
            .to_string();

        let expected_outputs = Self::parse_expected_outputs(&source);

        Ok(TestCase {
            name,
            source,
            expected_outputs,
        })
    }

    /// Parse expected outputs from comments in the source code
    /// Looks for patterns like: // expect: value
    fn parse_expected_outputs(source: &str) -> Vec<ExpectedOutput> {
        let re = Regex::new(r"//\s*expect:\s*(.+)").unwrap();
        let mut outputs = Vec::new();

        for (line_num, line) in source.lines().enumerate() {
            if let Some(captures) = re.captures(line) {
                if let Some(value_match) = captures.get(1) {
                    let value = value_match.as_str().trim().to_string();
                    outputs.push(ExpectedOutput {
                        line: line_num + 1,
                        value,
                    });
                }
            }
        }

        outputs
    }

    /// Strip comments from source code for parsing
    /// The test case format includes comments like "// expect: value" which need to be
    /// stripped before parsing since the lexer doesn't handle comments
    fn strip_comments(source: &str) -> String {
        source
            .lines()
            .map(|line| {
                // Find the position of "//" and strip everything after it
                if let Some(pos) = line.find("//") {
                    &line[..pos]
                } else {
                    line
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Run the test case and return the actual output
    pub fn run(&self) -> Result<Vec<String>, String> {
        let deps = TestDeps::new();
        let env = ExecEnv::new(deps);
        let mut runtime = Runtime::new(env);

        // Strip comments from source before parsing
        let source_without_comments = Self::strip_comments(&self.source);

        let tokens = lexer()
            .parse(&source_without_comments)
            .into_result()
            .map_err(|e| format!("Lexical error: {:?}", e))?;
        let program = program_parser()
            .parse(&tokens)
            .into_result()
            .map_err(|e| format!("Parse error: {:?}", e))?;
        let program = crate::resolution::resolve(program, &source_without_comments)
            .map_err(|e| format!("Resolution error: {}", e))?;

        runtime
            .run_program(&program)
            .map_err(|e| format!("Runtime error: {}", e))?;

        Ok(runtime.into_deps().get_output())
    }

    /// Check if the test case passes
    pub fn check(&self) -> TestResult {
        let actual = match self.run() {
            Ok(output) => output,
            Err(err) => {
                return TestResult {
                    name: self.name.clone(),
                    passed: false,
                    expected: self
                        .expected_outputs
                        .iter()
                        .map(|e| e.value.clone())
                        .collect(),
                    actual: vec![],
                    error: Some(err),
                }
            }
        };

        let expected: Vec<String> = self
            .expected_outputs
            .iter()
            .map(|e| e.value.clone())
            .collect();

        let passed = actual == expected;

        TestResult {
            name: self.name.clone(),
            passed,
            expected,
            actual,
            error: None,
        }
    }
}

/// Result of running a test case
#[derive(Debug)]
pub struct TestResult {
    pub name: String,
    pub passed: bool,
    pub expected: Vec<String>,
    pub actual: Vec<String>,
    pub error: Option<String>,
}

impl TestResult {
    pub fn print_summary(&self) {
        if self.passed {
            println!("✓ {}", self.name);
        } else {
            println!("✗ {}", self.name);
            if let Some(err) = &self.error {
                println!("  Error: {}", err);
            } else {
                println!("  Expected: {:?}", self.expected);
                println!("  Actual:   {:?}", self.actual);
            }
        }
    }
}

/// Run all test cases in a directory
pub fn run_test_directory(dir: &Path) -> std::io::Result<Vec<TestResult>> {
    let mut results = Vec::new();

    if !dir.exists() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("Directory not found: {}", dir.display()),
        ));
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() {
            let test_case = TestCase::from_file(&path)?;
            let result = test_case.check();
            results.push(result);
        }
    }

    Ok(results)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expected_outputs() {
        let source = r#"
{
  var a = "before";
  print a; // expect: before

  a = "after";
  print a; // expect: after

  print a = "arg"; // expect: arg
  print a; // expect: arg
}
"#;
        let outputs = TestCase::parse_expected_outputs(source);
        assert_eq!(outputs.len(), 4);
        assert_eq!(outputs[0].value, "before");
        assert_eq!(outputs[1].value, "after");
        assert_eq!(outputs[2].value, "arg");
        assert_eq!(outputs[3].value, "arg");
    }

    #[test]
    fn test_run_simple_test_case() {
        let test_case = TestCase {
            name: "simple_test".to_string(),
            source: r#"
var a = "hello";
print a; // expect: hello
print 1 + 2; // expect: 3
"#
            .to_string(),
            expected_outputs: vec![
                ExpectedOutput {
                    line: 3,
                    value: "hello".to_string(),
                },
                ExpectedOutput {
                    line: 4,
                    value: "3".to_string(),
                },
            ],
        };

        let result = test_case.check();
        assert!(result.passed, "Test should pass");
        assert_eq!(result.actual, vec!["hello", "3"]);
    }
}
