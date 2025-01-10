def test_string_chars() = {
  assert_eq(String.chars("H\t, d!"), [`H, `\t, `,, ` , `d, `!])
}