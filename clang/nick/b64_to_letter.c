fn char nick_b64_to_letter(int b64) {
  if (b64 == 0) {
    return '_';
  }
  if (b64 >= 1 && b64 <= 26) {
    return 'a' + (b64 - 1);
  }
  if (b64 >= 27 && b64 <= 52) {
    return 'A' + (b64 - 27);
  }
  if (b64 >= 53 && b64 <= 62) {
    return '0' + (b64 - 53);
  }
  if (b64 == 63) {
    return '$';
  }
  return '?';
}
