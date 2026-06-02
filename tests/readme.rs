use std::process::Command;

#[test]
fn readme_is_up_to_date() {
    let output = Command::new("python3")
        .args(["tools/render_readme.py", "--check"])
        .output()
        .expect("failed to run README renderer");

    assert!(
        output.status.success(),
        "README.md is not up to date\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}
