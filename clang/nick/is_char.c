fn int nick_is_char(char c) {
  return nick_letter_to_b64(c) >= 0;
}
