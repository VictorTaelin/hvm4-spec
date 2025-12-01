fn void print_str_name(u32 n) {
  if (n < 64) {
    print_str_putc(nick_alphabet[n]);
  } else {
    print_str_name(n / 64);
    print_str_putc(nick_alphabet[n % 64]);
  }
}
