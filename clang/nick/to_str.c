fn void nick_to_str(u32 name, char *buf, u32 buf_size) {
  // Extract 4 characters from the 24-bit name (6 bits each)
  // Names are stored most significant first
  char tmp[5];
  int len = 0;
  for (int i = 3; i >= 0; i--) {
    int b64 = (name >> (i * 6)) & 0x3F;
    if (b64 != 0 || len > 0) {  // Skip leading underscores (zeros)
      tmp[len++] = nick_b64_to_letter(b64);
    }
  }
  if (len == 0) {
    tmp[len++] = '_';  // Empty name becomes single underscore
  }
  tmp[len] = '\0';
  // Copy to output buffer
  for (int i = 0; i < len && i < (int)buf_size - 1; i++) {
    buf[i] = tmp[i];
  }
  buf[len < (int)buf_size - 1 ? len : buf_size - 1] = '\0';
}
