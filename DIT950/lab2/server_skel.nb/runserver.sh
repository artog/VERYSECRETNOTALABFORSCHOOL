#!/bin/bash

# Run in project directory

# Possible need to change here
main_class=edu.gu.hajo.chat.server.Main

reg_port=6666
server_port=7777

# If this a lot of logging
#java -Djava.rmi.server.logCalls=true 

java -server -Djava.security.policy=security.policy -Djava.rmi.server.hostname=127.0.0.1 -cp target/classes ${main_class} ${reg_port} ${server_port}


exit 0

