
int sensorPin = A0;            // select the input pin for the ldr
unsigned int sensorValue = 0;  // variable to store the value coming from the ldr

void setup()
{
  pinMode(13, OUTPUT);
  
  //Start Serial port
  Serial.begin(9600);        // start serial for output - for testing
}

void loop() {
  sensorValue = analogRead(sensorPin);
  Serial.println(sensorValue);
  delay(300);
  
  // For DEBUGGING - Print out our data, uncomment the lines below
  //Serial.print(sensorValue, DEC);     // print the value (0 to 1024)
  //Serial.println("");                   // print carriage return  
  //  delay(100);  
}
