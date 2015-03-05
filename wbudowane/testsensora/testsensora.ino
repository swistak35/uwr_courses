const int speakerPin = 5;
const int lightSensorPin = A1;
const int recordButtonPin = 4;
const int playButtonPin = 2;
const int ledPin = 11;

#define MIN_LIGHT 100
#define MAX_LIGHT 800
#define MIN_TONE 900
#define MAX_TONE 1600
#define RECORDING_MAX_SIZE 50

unsigned int lightSensorValue = 0;
unsigned int toneValue = 0;
unsigned int recordButtonValue = 0;
unsigned int playButtonValue = 0;
unsigned int frequency = 50;

unsigned int systemState = 0;

int recording[RECORDING_MAX_SIZE];
int recording_size = 0;
int i;

void setup() {
  Serial.begin(9600);
  pinMode(recordButtonPin, INPUT);
  pinMode(playButtonPin, INPUT);
  pinMode(ledPin, OUTPUT);
  
  digitalWrite(ledPin, LOW);
  tone(speakerPin, 2000, 1000);
}

void debug() {
  Serial.print(lightSensorValue);
  Serial.print(" | ");
  Serial.print(recordButtonValue);
  Serial.print(" | ");
  Serial.print(playButtonValue);
  Serial.print(" | ");
  Serial.println(toneValue);
  delay(200);
}

void loop() {
    recordButtonValue = digitalRead(recordButtonPin);
    playButtonValue = digitalRead(playButtonPin);
      lightSensorValue = analogRead(lightSensorPin);
      toneValue = map(lightSensorValue, MIN_LIGHT, MAX_LIGHT, MIN_TONE, MAX_TONE);
    debug();
}
