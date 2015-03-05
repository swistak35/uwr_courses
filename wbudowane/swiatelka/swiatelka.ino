const int led1pin = 13;
const int led2pin = 12;
const int led3pin = 8;
const int led4pin = 7;

void setup() {
  pinMode(led1pin, OUTPUT);
  pinMode(led2pin, OUTPUT);
  pinMode(led3pin, OUTPUT);
  pinMode(led4pin, OUTPUT);
}

void loop() {
  digitalWrite(led1pin, HIGH);
  delay(500);
  digitalWrite(led1pin, LOW);

  digitalWrite(led2pin, HIGH);
  delay(500);
  digitalWrite(led2pin, LOW);

  digitalWrite(led3pin, HIGH);
  delay(500);
  digitalWrite(led3pin, LOW);

  digitalWrite(led4pin, HIGH);
  delay(500);
  digitalWrite(led4pin, LOW);
  
  delay(1000);
}
