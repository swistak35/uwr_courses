const int ledPins[] = { 2, 4, 7, 8, 12, 13 };
const float inputVolt = 3.3;

float voltage = 0.0;
int val;

void setup() {
  pinMode(ledPins[0], OUTPUT);
  pinMode(ledPins[1], OUTPUT);
  pinMode(ledPins[2], OUTPUT);
  pinMode(ledPins[3], OUTPUT);
  pinMode(ledPins[4], OUTPUT);
  pinMode(ledPins[5], OUTPUT);
  analogReference(EXTERNAL);
  Serial.begin(9600);
}

void switchLeds() {
  for (int i = 0; i < 6; i++) {
    digitalWrite(ledPins[i], LOW);
  }
  
  if (voltage > 1.0) { digitalWrite(ledPins[0], HIGH); }
  if (voltage > 1.2) { digitalWrite(ledPins[1], HIGH); }
  if (voltage > 1.3) { digitalWrite(ledPins[2], HIGH); }
  if (voltage > 1.4) { digitalWrite(ledPins[3], HIGH); }
  if (voltage > 1.5) { digitalWrite(ledPins[4], HIGH); }
  if (voltage > 1.6) { digitalWrite(ledPins[5], HIGH); }
}

void loop() {
  val = analogRead(A0);
  voltage = float(val * inputVolt) / 1023.0;
  Serial.println(voltage);
  switchLeds();
  delay(200);
}
