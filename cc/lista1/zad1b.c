char tolower( char c ) {
  if (c >= 'A' && c <= 'Z')
    c += 'a' - 'A'; // replace capitalism by lowercasism.
  return c;
}
