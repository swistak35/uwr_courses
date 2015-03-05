#define NOTE_C4  262
#define NOTE_D4  294
#define NOTE_E4  330
#define NOTE_F4  349
#define NOTE_G4  392

int melody[] = {
  NOTE_E4, NOTE_E4, NOTE_E4, NOTE_E4, NOTE_E4, NOTE_E4,
  NOTE_E4, NOTE_G4, NOTE_C4, NOTE_D4, NOTE_E4,
  NOTE_F4, NOTE_F4, NOTE_F4, NOTE_F4, NOTE_F4, NOTE_E4, NOTE_E4, NOTE_E4,
  NOTE_E4, NOTE_D4, NOTE_D4, NOTE_E4, NOTE_D4, NOTE_G4,
  NOTE_E4, NOTE_E4, NOTE_E4, NOTE_E4, NOTE_E4, NOTE_E4,
  NOTE_E4, NOTE_G4, NOTE_C4, NOTE_D4, NOTE_E4,
  NOTE_F4, NOTE_F4, NOTE_F4, NOTE_F4, NOTE_F4, NOTE_E4, NOTE_E4, NOTE_E4,
  NOTE_G4, NOTE_G4, NOTE_F4, NOTE_D4, NOTE_C4
};

const int ledPin[] = {4, 7, 8, 12};
const int ledPins = 4;
const int speakerPin = 9;

int noteDuration, pauseBetweenNotes;
int thisNote = 0;
int thisLed = 0;

int noteDurations[] = {
  4, 4, 2, 4, 4, 2,
  4, 4, 4, 4, 1,
  4, 4, 4, 4, 4, 4, 4, 4,
  4, 4, 4, 4, 2, 2,
  4, 4, 2, 4, 4, 2,
  4, 4, 4, 4, 1,
  4, 4, 4, 4, 4, 4, 4, 4,
  4, 4, 4, 4, 1
};

void setup() {
  pinMode(ledPin[0], OUTPUT);
  pinMode(ledPin[1], OUTPUT);
  pinMode(ledPin[2], OUTPUT);
  pinMode(ledPin[3], OUTPUT);
}

void loop() {
  if (!(thisNote < sizeof(melody)/sizeof(int))) {
    thisNote = 0;
  }

  noteDuration = 1000/noteDurations[thisNote];
  tone(speakerPin, melody[thisNote],noteDuration);
  pauseBetweenNotes = noteDuration * 1.30;
  
  digitalWrite(ledPin[thisLed], HIGH);
  
  delay(pauseBetweenNotes);
  
  digitalWrite(ledPin[thisLed], LOW);

  thisNote++;
  thisLed = (thisLed + 1) % 4;
}
