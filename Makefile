ARDUINO_DIR = /Users/stepcut/projects/arduino/Arduino.app/Contents/Resources/Java/
#ALTERNATE_CORE = SF32u4_boards-master
#ALTERNATE_CORE_PATH = /usr/share/arduino/hardware/SF32u4_boards-master
#ALTERNATE_CORE = proMicro
#ALTERNATE_CORE_PATH = /usr/share/arduino/hardware/proMicro

#NO_CORE_MAIN_CPP = "fee"


TARGET = el-sequencer.ino
ARDUINO_LIBS =

BOARD_TAG    = pro328
#ARDUINO_PORT = /dev/ttyUSB0
ARDUINO_PORT = /dev/cu.usbserial-A9014F4U

#BOARD_TAG    = promicro8
#ARDUINO_PORT = /dev/ttyACM0

#include /home/stepcut/n-heptane/projects/arduino/Arduino-Makefile/Arduino.mk
include /Users/stepcut/projects/arduino/Arduino-Makefile/Arduino.mk


