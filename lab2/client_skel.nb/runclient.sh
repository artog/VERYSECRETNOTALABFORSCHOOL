#/bin/bash

# Run in project directory

# Possible need to change here
main_class=edu.gu.hajo.chat.client.Main

java -Djava.security.policy=security.policy -cp target/classes:../server_skel.nb/target/classes  ${main_class} pelle 111 127.0.0.1 6666 7890

exit 0

