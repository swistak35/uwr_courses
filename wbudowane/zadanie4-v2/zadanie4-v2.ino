const int sensorPin = A0;
const int speakerPin = 8;
const int ledPin = 10;
const int buttonPin = 6;

unsigned int sensorValue = 0;
unsigned int delayTime = 0;
unsigned int ledConstraint = 0;
unsigned int buttonState = 0;
unsigned int current_status = 0;

void setup() {
  pinMode(ledPin, OUTPUT);      
  pinMode(speakerPin, OUTPUT);
  
  pinMode(buttonPin, INPUT);
  
  randomSeed(analogRead(1));
  
  reset_random();
//  current_status = 1;
  
  Serial.begin(9600);
}

void reset_random() {
  ledConstraint = random(600, 900);
  current_status = 0;
  digitalWrite(ledPin, LOW);
}

void debug_info() {
  Serial.print("LIGHT = ");
  Serial.print(sensorValue);
  
  Serial.print(" | DELAY = ");
  Serial.print(delayTime);
  
  Serial.print(" | CONS = ");
  Serial.print(ledConstraint);
  
  Serial.print(" | BTN = ");
  Serial.print(buttonState);
  
  Serial.println("");
}

void loop(){
  sensorValue = analogRead(sensorPin);
  buttonState = digitalRead(buttonPin);
  
  if (buttonState == 1) {
    reset_random();
  }
  
  if (current_status == 0) {    
    if (sensorValue >= ledConstraint) {
      current_status = 1;
      digitalWrite(ledPin, HIGH);
    } else {
      sensorValue = constrain(sensorValue, 1, ledConstraint);
      delayTime = map(sensorValue, 1, ledConstraint, 1000, 20);
      tone(speakerPin, 500, 10);
      delay(delayTime);
    }
  } else {
    tone(speakerPin, 2000, 200);
    delay(200);
  }
}
