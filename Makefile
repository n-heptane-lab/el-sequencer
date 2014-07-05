ARDUINO_DIR = /usr/share/arduino/
#ALTERNATE_CORE = SF32u4_boards-master
#ALTERNATE_CORE_PATH = /usr/share/arduino/hardware/SF32u4_boards-master
#ALTERNATE_CORE = proMicro
#ALTERNATE_CORE_PATH = /usr/share/arduino/hardware/proMicro


TARGET = el-sequencer
ARDUINO_LIBS =

BOARD_TAG    = pro328
ARDUINO_PORT = /dev/ttyUSB0

#BOARD_TAG    = promicro8
#ARDUINO_PORT = /dev/ttyACM0

include /home/stepcut/n-heptane/projects/arduino/Arduino-Makefile/Arduino.mk


