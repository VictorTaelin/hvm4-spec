fn void print_name(FILE *f, u32 n) {
  if (n < 64) {
    fputc(nick_alphabet[n], f);
  } else {
    print_name(f, n / 64);
    fputc(nick_alphabet[n % 64], f);
  }
}
