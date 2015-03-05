#include <stdint.h>
#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/pgmspace.h>

#define DEFAULT_SAMPLE_RATE 8000

#if defined(ARDUINO) && ARDUINO >= 100
#include "Arduino.h"
#else
#include "WProgram.h"
#endif


int speakerPin = 11;
unsigned char const *sounddata_data=0;
int sounddata_length=0;
float rate_multiplier;
float volume_multiplier;
volatile uint16_t sample;
byte lastSample;


// This is called at 8000 Hz to load the next sample.
// ISR - Interrupt Service Routine
// to makro definiuje funkcję dla danego przerwania, w tym przypadku nasze przerwanie to TIMER1_COMPA_
ISR(TIMER1_COMPA_vect) {
  if (sample >= sounddata_length) {
    if (sample == sounddata_length + lastSample) {
      stopPlayback();
    } else {
      // Zjeżdzamy do 0, zeby nie bylo slychac "klikniecia"
      OCR2A = sounddata_length + lastSample - sample;
    }
  } else {
    OCR2A = int((pgm_read_byte(&sounddata_data[sample]) - 128) * volume_multiplier) + 128;
  }
  
  ++sample;
}

void startPlayback(unsigned char const *data, int length, float rate_m, float volume_m) {
  sounddata_data = data;
  sounddata_length = length;
  rate_multiplier = rate_m;
  volume_multiplier = volume_m;

  pinMode(speakerPin, OUTPUT);
  
  // Set up Timer 2 to do pulse width modulation on the speaker
  // pin.
  
  // Use internal clock (datasheet p.160)
  // _BV(nr_bitu) robi `int` z jedynką w danym bicie (shift)
  // EXCLK -> "
  // AS2 -> "When AS2 is written to zero, Timer/Counter2 is clocked from the I/O clock, clkI/O."
  // ASSR - asynchronous status register, rejestr w timerze2
  ASSR &= ~(_BV(EXCLK) | _BV(AS2));

  // WGM - waveform generation mode  
  // Ta kombinacja bitów WGM22, WGM21, WGM20, wlacza tryb fast PWM
  // strona 155 manuala
  TCCR2A |= _BV(WGM21) | _BV(WGM20);
  TCCR2B &= ~_BV(WGM22);
  
  // Do non-inverting PWM on pin OC2A (p.155)
  // On the Arduino this is pin 11.
  // "Clear OC2A on Compare Match, set OC2A at BOTTOM, (non-inverting mode)."
  // nie wiem czemu ruszamy też pin OC2B którykolwiek to jest
  TCCR2A = (TCCR2A | _BV(COM2A1)) & ~_BV(COM2A0);
  TCCR2A &= ~(_BV(COM2B1) | _BV(COM2B0));
  
  // No prescaler (p.158)
  // wybieramy clock source tutaj, że nam niepotrzebne
  TCCR2B = (TCCR2B & ~(_BV(CS12) | _BV(CS11))) | _BV(CS10);
  
  // Set initial pulse width to the first sample.
  // wysylamy dane na pin
  OCR2A = pgm_read_byte(&sounddata_data[0]);
  
  
  // Set up Timer 1 to send a sample every interrupt.
  
  cli(); // zablokuj przerwania
  
  // Set CTC mode (Clear Timer on Compare Match) (p.133)
  // Have to set OCR1A *after*, otherwise it gets reset to 0!
  // na stronach 122-123 jest wytłumaczone CTC
  TCCR1B = (TCCR1B & ~_BV(WGM13)) | _BV(WGM12);
  TCCR1A = TCCR1A & ~(_BV(WGM11) | _BV(WGM10));
  
  // No prescaler (p.134)
  TCCR1B = (TCCR1B & ~(_BV(CS12) | _BV(CS11))) | _BV(CS10);
  
  // Set the compare register (OCR1A).
  // OCR1A is a 16-bit register, so we have to do this with
  // interrupts disabled to be safe.
  OCR1A = F_CPU / int(rate_multiplier * DEFAULT_SAMPLE_RATE);    // 16e6 / 8000 = 2000
  
  // Enable interrupt when TCNT1 == OCR1A (p.136)
  TIMSK1 |= _BV(OCIE1A);
  
  lastSample = pgm_read_byte(&sounddata_data[sounddata_length-1]);
  sample = 0;
  
  sei(); // włącz przerwania
}

void stopPlayback() {
  // Disable playback per-sample interrupt.
  TIMSK1 &= ~_BV(OCIE1A);
  
  // Disable the per-sample timer completely.
  TCCR1B &= ~_BV(CS10);
  
  // Disable the PWM timer.
  TCCR2B &= ~_BV(CS10);
  
  digitalWrite(speakerPin, LOW);
}
