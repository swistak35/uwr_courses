int redPin = 3;
int greenPin = 5;
int bluePin = 6;
int buttonPin = 8;

int sensorXPin = A3;
int sensorYPin = A4;

int systemState = 0;
int xTarget, yTarget, xCurrent, yCurrent;
int sensorXValue, sensorYValue, previousSensorXValue, previousSensorYValue, moveInitX, moveMaxX, diff, moveMaxValueX;
int moveInitY, moveMaxY, moveMaxValueY;
int directionX, directionY;
 
void setup() {
  pinMode(redPin, OUTPUT);
  pinMode(greenPin, OUTPUT);
  pinMode(bluePin, OUTPUT);

  Serial.begin(9600);

  sensorXValue = analogRead(sensorXPin);
  sensorYValue = analogRead(sensorYPin);

  randomSeed(analogRead(0));
}
 
void setColor(int red, int green, int blue) {
  analogWrite(redPin, red);
  analogWrite(greenPin, green);
  analogWrite(bluePin, blue);
}

int lastButtonState = 0;
int buttonState = 0;

int checkButton() {
  lastButtonState = buttonState;
  buttonState = digitalRead(buttonPin);
  if (lastButtonState == 1 && buttonState == 0) {
    return 1;
  } else {
    return 0;
  }
}

#define SENSOR_TOLERANCE 0
#define SENSOR_START_TOLERANCE 5
#define WIN_TOLERANCE 70

void checkColorDisplay() {
  if (checkButton()) {
//    Serial.println("WYSWIETLAM");
    setColor(xTarget, yTarget, 0);
    delay(1000);
    setColor(xCurrent, yCurrent, 0);
  }
}

void readSensorValues() {
  previousSensorXValue = sensorXValue;
  sensorXValue = analogRead(sensorXPin);
    
  previousSensorYValue = sensorYValue;
  sensorYValue = analogRead(sensorYPin);
}

void loop() {
  if (systemState == 0) {
    delay(3000);
    xTarget = random(0, 256);
    yTarget = random(0, 256);
    setColor(xTarget, yTarget, 0);
    Serial.print("TARGET = (");
    Serial.print(xTarget);
    Serial.print(", ");
    Serial.print(yTarget);
    Serial.println(")");
    delay(3000);
    systemState = 1;
    xCurrent = 0;
    yCurrent = 0;
    setColor(xCurrent, yCurrent, 0);
  } else if (systemState == 1) {
    checkColorDisplay();
    readSensorValues();
    
    if ((abs(previousSensorXValue - sensorXValue) > SENSOR_START_TOLERANCE) || (abs(previousSensorYValue - sensorYValue) > SENSOR_START_TOLERANCE)) {
      Serial.println("Zaczeto ruch");
      systemState = 2;
      
      moveInitX = previousSensorXValue;
      moveMaxX = abs(previousSensorXValue - sensorXValue);
      if (sensorXValue > previousSensorXValue) {
        directionX = 1;
      } else {
        directionX = -1;
      }
      
      moveInitY = previousSensorYValue;
      moveMaxY = abs(previousSensorYValue - sensorYValue);
      if (sensorYValue > previousSensorYValue) {
        directionY = 1;
      } else {
        directionY = -1;
      }
    }
  } else if (systemState == 2) {
    checkColorDisplay();
    previousSensorXValue = sensorXValue;
    sensorXValue = analogRead(sensorXPin);

    previousSensorYValue = sensorYValue;
    sensorYValue = analogRead(sensorYPin);
    
    if ((abs(previousSensorXValue - sensorXValue) > SENSOR_TOLERANCE) || (abs(previousSensorYValue - sensorYValue) > SENSOR_TOLERANCE)) {
      // max check
      diff = directionX * (sensorXValue - moveInitX);
      if (diff > moveMaxX) {
        moveMaxX = diff;
      }

      diff = directionY * (sensorYValue - moveInitY);
      if (diff > moveMaxY) {
        moveMaxY = diff;
      }
    } else {
      //Serial.println("Koniec");
      Serial.print(directionX * moveMaxX);
      Serial.print(" | ");
      Serial.print(directionY * moveMaxY);
      Serial.print(" (");
      Serial.print(xCurrent);
      Serial.print(", ");
      Serial.print(yCurrent);
      Serial.println(")");
      systemState = 1;
      
      xCurrent = constrain(xCurrent + directionX * moveMaxX, 0, 255);
      yCurrent = constrain(yCurrent + directionY * moveMaxY, 0, 255);

      setColor(xCurrent, yCurrent, 0);
      
      if ((abs(xTarget - xCurrent) < WIN_TOLERANCE) && (abs(yTarget - yCurrent) < WIN_TOLERANCE)) {
        for (int j = 0; j < 10; j++) {
          delay(200);
          setColor(200, 0, 0);
          delay(200);
          setColor(0, 0, 0);
        }
        systemState = 0;
      }
    }
  } else {
    sensorXValue = analogRead(sensorXPin);
    xCurrent = map(sensorXValue, 200, 800, 0, 199);

    sensorYValue = analogRead(sensorYPin);
    yCurrent = map(sensorYValue, 200, 800, 0, 199);

    setColor(xCurrent, yCurrent, 0);
    //Serial.print(xCurrent);
    //Serial.print(" = ");
    //Serial.println(sensorValue);
  }
  delay(50);
}
