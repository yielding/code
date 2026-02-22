#!/bin/sh
kotlinc $1 -include-runtime -d main.jar 
java -Xmx1024M -Xms256M -jar main.jar
rm main.jar
