pub fn unescape_lit(got: &str) -> String {
    let mut out = String::new();
    let mut escaped = false;
    for c in got[1..got.len() - 1].chars() {
        if escaped || c != '\\' {
            out.push(c);
            escaped = false;
        } else {
            escaped = true;
        }
    }
    out
}
