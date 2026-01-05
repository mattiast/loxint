use loxlang::test_runner::{run_test_directory, TestCase};
use std::path::PathBuf;

#[test]
fn run_all_test_cases() {
    // Get the workspace root (3 levels up from this file's location)
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();

    let test_dir = workspace_root.join("test-cases");

    assert!(test_dir.exists(), "Workspace root directory does not exist");

    let results = run_test_directory(&test_dir).expect("Failed to run test directory");

    if results.is_empty() {
        println!("No test cases found in test-cases directory");
        return;
    }

    let mut passed = 0;
    let mut failed = 0;

    for result in &results {
        result.print_summary();
        if result.passed {
            passed += 1;
        } else {
            failed += 1;
        }
    }

    println!("\n=== Test Summary ===");
    println!("Passed: {}", passed);
    println!("Failed: {}", failed);
    println!("Total:  {}", passed + failed);

    assert!(failed == 0, "Some test cases failed. See summary above.");
}

#[test]
fn test_assignment_local() {
    // Test a specific test case to demonstrate usage
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

    let test_case = TestCase {
        name: "assignment_local".to_string(),
        source: source.to_string(),
        expected_outputs: vec![
            loxlang::test_runner::ExpectedOutput {
                line: 3,
                value: "before".to_string(),
            },
            loxlang::test_runner::ExpectedOutput {
                line: 6,
                value: "after".to_string(),
            },
            loxlang::test_runner::ExpectedOutput {
                line: 8,
                value: "arg".to_string(),
            },
            loxlang::test_runner::ExpectedOutput {
                line: 9,
                value: "arg".to_string(),
            },
        ],
    };

    let result = test_case.check();
    result.print_summary();
    assert!(result.passed, "Assignment local test should pass");
}
