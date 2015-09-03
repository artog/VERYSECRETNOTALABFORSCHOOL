import TSim.*;

public class Lab1 {

    public static void main(String[] args) {
        new Lab1(args);
    }

    public Lab1(String[] args) {
        TSimInterface tsi = TSimInterface.getInstance();

        try {

            tsi.setSpeed(1,20);
            while (true) {
                    SensorEvent e = tsi.getSensor(1);
                    System.err.println(e.toString());
            }
        }
        catch (CommandException e) {
            e.printStackTrace();    // or only e.getMessage() for the error
            System.exit(1);
        } catch (InterruptedException i) {
                i.printStackTrace();
        }   
        
    }
    
    class Train extends Thread {
        
        // Train Id enum
        enum TrainId {TOP, BOTTOM}

        TSimInterface tsi = TSimInterface.getInstance();


        TrainId id;
        Track track;

        int speed = 10;
        boolean skipSensor = false;

        public Train(int speed, TrainId id, Lab1 parent) {
            // aquire lock
            // Lab1.getSema ..

            if (id == TOP) {
                zone = 2;
            } else {
                zone = 6;
            } 

        }



        public void handleSensor(SensorEvent e) {
            if (skipSensor) { 
                //TODO: release lock 1
                skipSensor = false;
                return; 
            }
            
            stop();
            
            switch (track) {
                case One: 
                    //TODO: aquire lock 1
                    track = Three;

                    break;
                case Two:
                    //TODO: aquire lock 1
                    track = Four;

                    break;
                case Three:
                case Four:

                    if (speed < 0) {
                        //TODO: aquire lock 1
                        track = track == Three ? One : Two;
                    } else {
                        //TODO: aquire lock 3
                        //TODO: release lock 2 if track three
                        track = Five;
                        //TODO: flip switch a
                    }
                    break;
                case Five:
                    if (speed < 0) {
                        //TODO: try to aquire lock 2
                        
                        //if fail 
                        track = Four;
                        //switch a to bottom
                        
                        // else
                        track = Three;
                        //switch a to top
                        // end if
                        // release lock 3 
                    } else {
                        //TODO: try to aquire lock 4
                        
                        //if fail 
                        track = Seven;
                        //switch b to bottom
                        
                        // else
                        track = Six;
                        //switch b to top
                        // end if
                        // release lock 3
                    }
                    break;
                case Six:
                    if (speed < 0) {
                        // aquire lock 3
                        // switch b to top
                        // release lock 4
                    } else {
                        // aquire lock 5
                        // switch c to top
                        // release lock 4
                    }
                    break;
                case Seven:
                    if (speed < 0) {
                        // aquire lock 3
                        // switch b to bottom
                    } else {
                        // aquire lock 5
                        // switch c to bottom
                    }
                    break;
                case Eight:
                    if (speed < 0) {
                        //TODO: try to aquire lock 4
                        
                        //if fail 
                        track = Seven;
                        //switch c to bottom
                        
                        // else
                        track = Six;
                        //switch c to top
                        // end if
                        // release lock 5 
                    } else {
                        //TODO: try to aquire lock 6
                        
                        //if fail 
                        track = Ten;
                        //switch d to bottom
                        
                        // else
                        track = Nine;
                        //switch d to top
                        // end if
                        // release lock 5
                    }
                    break;
                case Nine:
                case Ten:

                    if (speed < 0) {
                        //TODO: aquire lock 5
                        //TODO: release lock 6 if track 9
                        track = Eight;
                        //TODO: flip switch d
                    } else {
                        //TODO: 
                    }
                    break;

                default: 
                    throw RuntimeException();


            }
            start();
            skipSensor = true;

        }


        public void start() { tsi.setSpeed(speed,id); }
        public void stop() { tsi.setSpeed(0,id); }

    }

    enum Track { 
        One,
        Two,
        Three,
        Four,
        Five,
        Six,
        Seven,
        Eight,
        Nine,
        Ten 
    }
}

