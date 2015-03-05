const int sensorPin = A0;
const int speakerPin = 8;
const int ledPin = 10;

unsigned int sensorValue = 0;
unsigned int voiceTone = 0;
unsigned int ledConstraint = 0;

void setup() {
  pinMode(ledPin, OUTPUT);      
  pinMode(speakerPin, OUTPUT);
  
  randomSeed(analogRead(1));
  ledConstraint = random(1, 1000);
  
  Serial.begin(9600);
}

void debug_info() {
  Serial.print("LIGHT = ");
  Serial.print(sensorValue);
  
  Serial.print(" | TONE = ");
  Serial.print(voiceTone);
  
  Serial.print(" | CONS = ");
  Serial.print(ledConstraint);
  
  Serial.println("");
  
  delay(200);
}

void loop(){
  sensorValue = analogRead(sensorPin);
  
  voiceTone = map(sensorValue, 1, 1000, 1, 2000);
  tone(speakerPin, voiceTone, 50);
  
  if (sensorValue > ledConstraint) {
    digitalWrite(ledPin, HIGH);
  } else {
    digitalWrite(ledPin, LOW);
  }
  
  debug_info();
  
  // analogWrite(ledPin, ledColor); 
}
