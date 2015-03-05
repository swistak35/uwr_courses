const int buttonPin = 2;
const int ledPin = 13;

int switches[20];
int current_free = 0;
int current_timer = 0;
int ledState = 0;

int buttonState = 0;
int previousButtonState = 0;
unsigned long now;

void setup() {
  pinMode(ledPin, OUTPUT);

  pinMode(buttonPin, INPUT);
  
  for (int i = 0; i < 20; i++) {
    switches[i] = 0;
  }
  
  Serial.begin(9600);
}

void debug_info() {
  Serial.println("----------------");
  Serial.print("BTN = ");
  Serial.println(buttonState);
}

void switch_led() {
  if (ledState == 1) {
    ledState = 0;
    analogWrite(ledPin, LOW);
  } else {
    ledState = 1;
    analogWrite(ledPin, 150);
  }
}

void loop() {
  now = millis();
  
  buttonState = digitalRead(buttonPin);
  
  if (previousButtonState != buttonState) {
    switches[current_free] = now + 1000;
    current_free++;
    current_free %= 20;
  }
  
  if (now > switches[current_timer] && switches[current_timer] != 0) {
    switch_led();
    switches[current_timer] = 0;
    current_timer++;
    current_timer %= 20;
  }
  
  previousButtonState = buttonState;
}
