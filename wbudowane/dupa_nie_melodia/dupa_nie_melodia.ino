const int speakerPin = A2;
const int lightSensorPin = A0;
const int ledPin = 11;

#define MIN_LIGHT 100
#define MAX_LIGHT 800
#define MIN_TONE 900
#define MAX_TONE 1600
#define RECORDING_MAX_SIZE 300

unsigned int lightSensorValue = 0;
unsigned int toneValue = 0;
unsigned int frequency = 50;

unsigned int systemState = 0;

int recording[RECORDING_MAX_SIZE];
int recording_size = 0;
int i, x;

// Buttons: Left, Right, Record, Play
const int buttonPin[] = {7, 8, 4, 2};
int buttonState[] = {0, 0, 0, 0};
int lastButtonState[] = {0, 0, 0, 0};

void setup() {
  Serial.begin(9600);
  pinMode(buttonPin[0], INPUT);
  pinMode(buttonPin[1], INPUT);
  pinMode(buttonPin[2], INPUT);
  pinMode(buttonPin[3], INPUT);
  pinMode(ledPin, OUTPUT);
  
  digitalWrite(ledPin, LOW);
}



void debug() {
  Serial.print(lightSensorValue);
  Serial.print(" | ");
  Serial.print(buttonState[0]);
  Serial.print(" | ");
  Serial.print(buttonState[1]);
  Serial.print(" | ");
  Serial.print(buttonState[2]);
  Serial.print(" | ");
    Serial.print(buttonState[3]);
  Serial.print(" | ");
  Serial.println(toneValue);
  delay(200);
}

int checkButton(int i) {
  lastButtonState[i] = buttonState[i];
  buttonState[i] = digitalRead(buttonPin[i]);
  if (lastButtonState[i] == 1 && buttonState[i] == 0) {
    return 1;
  } else {
    return 0;
  }
}

void loop() {
  if (checkButton(0) == 1) {
    Serial.println(0);
    frequency -= 2;
  }
  
  if (checkButton(1) == 1) {
    Serial.println(1);
    frequency += 2;
  }
  
 
  if (systemState == 0) {
    if (checkButton(2)) {
    Serial.println(2);
      systemState = 1;
    }
    if (checkButton(3)) {
    Serial.println(3);
      systemState = 2;
    }
  } else if (systemState == 1) {
    i = 0;
    digitalWrite(ledPin, HIGH);
    while (i < RECORDING_MAX_SIZE && !checkButton(2)) {   
      lightSensorValue = analogRead(lightSensorPin);
      Serial.println(lightSensorValue);
      toneValue = map(lightSensorValue, MIN_LIGHT, MAX_LIGHT, MIN_TONE, MAX_TONE);
      tone(speakerPin, toneValue);
      delay(frequency);
      recording[i] = toneValue;
      i++;
    }
    digitalWrite(ledPin, LOW);
    recording_size = i;
    noTone(speakerPin);
    systemState = 0;
  } else if (systemState == 2) {
    i = 0;
    digitalWrite(ledPin, HIGH);
    while (i < recording_size) {
      tone(speakerPin, recording[i]);
      delay(frequency);
      i++;
    }
    digitalWrite(ledPin, LOW);
    noTone(speakerPin);
    systemState = 0;
  }
}
