char tolower(char c) {
  return c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c;
}
