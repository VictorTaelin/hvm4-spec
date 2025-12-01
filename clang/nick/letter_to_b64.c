fn int nick_letter_to_b64(char c) {
  if (c == '_') {
    return 0;
  }
  if (c >= 'a' && c <= 'z') {
    return 1 + (c - 'a');
  }
  if (c >= 'A' && c <= 'Z') {
    return 27 + (c - 'A');
  }
  if (c >= '0' && c <= '9') {
    return 53 + (c - '0');
  }
  if (c == '$') {
    return 63;
  }
  return -1;
}
