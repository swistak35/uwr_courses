const int buttonMinPin = 12;
const int buttonMaxPin = 10;
const int ledPin = 8;
const int sensorPin = A0;

const int ledRange = 100;


int buttonMinState = 0;
int buttonMaxState = 0;
unsigned int sensorValue = 0;

unsigned int minValue = 0;
unsigned int maxValue = 100;
unsigned int ledColor = 0;
float ratio;

void setup() {
  pinMode(ledPin, OUTPUT);      

  pinMode(buttonMinPin, INPUT);    
  pinMode(buttonMaxPin, INPUT);
  
  recalculate_ratio();
  
  Serial.begin(9600);
}

void recalculate_ratio() {
  ratio = float(maxValue - minValue) / ledRange;
}

void debug_info() {
  Serial.println("----------------");
  Serial.print("BTN1 = ");
  Serial.println(buttonMinState);
  
  Serial.print("BTN2 = ");
  Serial.println(buttonMaxState);
  
  Serial.print("LIGHT = ");
  Serial.println(sensorValue);
  
  Serial.print("COLOR = ");
  Serial.println(ledColor);
  
  Serial.print("minValue = ");
  Serial.println(minValue);
  
  Serial.print("maxValue = ");
  Serial.println(maxValue);

  Serial.print("ratio = ");
  Serial.println(ratio);
}

void loop(){
  buttonMinState = digitalRead(buttonMinPin);
  buttonMaxState = digitalRead(buttonMaxPin);
  sensorValue = analogRead(sensorPin);
  
  if (buttonMinState == 1) {
    if (sensorValue > maxValue) {
      minValue = maxValue;
    } else {
      minValue = sensorValue;
    }
    recalculate_ratio();
    debug_info();
  }
  
  if (buttonMaxState == 1) {
    if (sensorValue < minValue) {
      maxValue = minValue;
    } else {
      maxValue = sensorValue;
    }
    recalculate_ratio();
    debug_info();
  }
  
  if (sensorValue > maxValue) {
    sensorValue = maxValue;
  }
  
  if (sensorValue < minValue) {
    sensorValue = minValue;
  }
  
  if (ratio > 0.0) {
    ledColor = ledRange - ((sensorValue - minValue) / ratio);
  } else {
    ledColor = 0;
  }
  analogWrite(ledPin, ledColor);  
}
