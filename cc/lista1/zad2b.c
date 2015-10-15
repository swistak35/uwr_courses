char tolower(char * s);

void tolower_str2(char * s) {
  while(*s != '\0') {
    *s = tolower(s);
    ++s;
  }
}

