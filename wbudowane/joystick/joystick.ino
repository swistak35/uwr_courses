#define MIN 0
#define MAX 1023

#define ACCEPTED_LOW_X 200
#define ACCEPTED_HIGH_X 800
#define ACCEPTED_LOW_Y 200
#define ACCEPTED_HIGH_Y 800

#define MID_X 512
#define MID_Y 512

#define ACCEPTED_NEUTRAL_DIFF 15

#define ACCEPTED_X_DIFF_Y 100
#define ACCEPTED_Y_DIFF_X 100

#define MAX_MILLIS 1000
#define GAME_ROUNDS 5

const int axisXpin = 1;
const int axisYpin = 0;

int axisXvalue, axisYvalue;

const int ledPins[] = {13, 12, 8, 7};
// 13 - gora
// 12 - lewo
// 8 - prawo
// 7 - dol

int game_status = -1;
boolean accepted;
long int game_start;
int reward;
int result;
int curr_round;

void setup() {
  pinMode(ledPins[0], OUTPUT);
  pinMode(ledPins[1], OUTPUT);
  pinMode(ledPins[2], OUTPUT);
  pinMode(ledPins[3], OUTPUT);
  
  Serial.begin(9600);
}

void readAxis() {
  axisXvalue = analogRead(axisXpin);
  axisYvalue = analogRead(axisYpin);
}

void loop() {
  
  if (game_status == -1) {
    if (Serial.available()) {
      while (Serial.available()) { Serial.read(); }      
      result = 0;
      game_status = 0;
      curr_round = 0;
      Serial.println("Przygotuj sie...");
      delay(2000);
      Serial.println("Zaczynamy gre!");
    }  
  } else if (game_status == 0) {
    readAxis();
    // jesli mamy joystick posrodku
    if ((axisXvalue > MID_X - ACCEPTED_NEUTRAL_DIFF) &&
      (axisXvalue < MID_X + ACCEPTED_NEUTRAL_DIFF) &&
      (axisYvalue > MID_Y - ACCEPTED_NEUTRAL_DIFF) &&
      (axisYvalue < MID_Y + ACCEPTED_NEUTRAL_DIFF)) {
        game_status = random(1,4);
        game_start = millis();
        digitalWrite(ledPins[game_status - 1], HIGH);
        // zapal diode
        Serial.print("Runda ");
        Serial.println(curr_round+1);
    }
    // a jesli nie to czekamy az tam wroci/bedzie
  } else {
    readAxis();
    if (game_status == 1) {
      accepted = ((axisYvalue > ACCEPTED_HIGH_Y) && (axisXvalue > (MID_X - ACCEPTED_Y_DIFF_X)) && (axisXvalue < (MID_X + ACCEPTED_Y_DIFF_X)));
    } else if (game_status == 2) {
      accepted = ((axisXvalue > ACCEPTED_HIGH_X) && (axisYvalue > (MID_Y - ACCEPTED_X_DIFF_Y)) && (axisYvalue < (MID_Y + ACCEPTED_X_DIFF_Y)));
    } else if (game_status == 3) {
      accepted = ((axisXvalue < ACCEPTED_LOW_X) && (axisYvalue > (MID_Y - ACCEPTED_X_DIFF_Y)) && (axisYvalue < (MID_Y + ACCEPTED_X_DIFF_Y)));
    } else if (game_status == 4) {
      accepted = ((axisYvalue < ACCEPTED_LOW_Y) && (axisXvalue > (MID_X - ACCEPTED_Y_DIFF_X)) && (axisXvalue < (MID_X + ACCEPTED_Y_DIFF_X)));
    }
    
    if (accepted) {
      // wylacz wszystkie diody
      digitalWrite(ledPins[0], LOW);
      digitalWrite(ledPins[1], LOW);
      digitalWrite(ledPins[2], LOW);
      digitalWrite(ledPins[3], LOW);
      game_status = 0;
      reward = MAX_MILLIS - (millis() - game_start);
      if (reward < 0) {
        reward = 0;
      }
      result += reward;

      Serial.print("Nagroda ");
      Serial.print(reward);
      Serial.print(", lacznie ");
      Serial.println(result);
      
      curr_round++;
      
      if (curr_round == GAME_ROUNDS) {
        game_status = -1;
        Serial.print("Koniec gry. Wynik = ");
        Serial.println(result);
      }
    }
  }
}  
