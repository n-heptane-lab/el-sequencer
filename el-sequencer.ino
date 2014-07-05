/*

  This is a 'dumb' client that listens to commands over the serial
  port and toggles outputs on and off.

  The communication protocol is very simple. The most significant 7
  bits are the channel number and the least significant bit is on/off.

 */
#define hsiPin      A2
#define solenoidPin A3

void setup() {
  // The EL channels are on pins 2 through 9
  // Initialize the pins as outputs
  pinMode(2, OUTPUT);  // channel A
  pinMode(3, OUTPUT);  // channel B
  pinMode(4, OUTPUT);  // channel C
  pinMode(5, OUTPUT);  // channel D
  pinMode(6, OUTPUT);  // channel E
  pinMode(7, OUTPUT);  // channel F
  pinMode(8, OUTPUT);  // channel G
  pinMode(9, OUTPUT);  // channel H
  // We also have a status LEDs, pin 13
  // pinMode(ledPin, OUTPUT);

  // Use the analog lines for additional digital I/O
  pinMode(solenoidPin, OUTPUT);
  pinMode(hsiPin     , OUTPUT);
  digitalWrite(solenoidPin, LOW);
  digitalWrite(hsiPin     , HIGH);
  Serial.begin(9600);

  for (int i = 2; i < 10; i++) {
      digitalWrite(i, HIGH);
  }

}

void loop ()
{
    char val, channel, switchPos;
    while (Serial.available()) {
        val       = Serial.read();
        channel   = val >> 1;
        switchPos = val & 0x1;
        switch (channel) {
            case 0:
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 7:
                digitalWrite(channel+2, switchPos);
                break;
            case 8:
                digitalWrite(solenoidPin, switchPos);
                break;
            }

    }
}

#if 0
void loop()
{
    char val;
//    char val, channel, status;
//    static int toggle = 0;
//    static unsigned long lastChange = 0;
//    unsigned long currentMillis = millis();

#if 1
    while (Serial.available()) {
        val = Serial.read();
        if (val == 1) {
            digitalWrite(solenoidPin, HIGH);
        } else {
            digitalWrite(solenoidPin, LOW);
        }
    }
#endif
#if 0
    while (Serial.available()) {
        val = Serial.read();
        for (int c = 0; c < 8; c++) {
            digitalWrite(c+2, (val>>c)&1);
        }
    }
#endif
#if 0
    if (currentMillis > (lastChange + 1000))
    {
        lastChange = currentMillis;
        toggle = !toggle;
        digitalWrite(solenoidPin, toggle);
//        digitalWrite(ledPin, toggle);
    }
#endif
}
#endif