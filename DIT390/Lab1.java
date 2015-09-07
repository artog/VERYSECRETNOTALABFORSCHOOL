import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Lab1 {
    
    public final Semaphore topStation = new Semaphore(1,true);
    public final Semaphore crossing = new Semaphore(1,true);
    public final Semaphore rightBend = new Semaphore(1,true);
    public final Semaphore overtake = new Semaphore(1,true);
    public final Semaphore leftBend = new Semaphore(1,true);
    public final Semaphore bottomStation = new Semaphore(1,true);
    

    public static void main(String[] args) {
        new Lab1(args);
    }

    public Lab1(String[] args) {
        TSimInterface tsi = TSimInterface.getInstance();
        startTrains();
    }

    private void startTrains() {
        Train train1 = new Train(20,1,this);
        Train train2 = new Train(10,2,this);
        
        train1.start();
//        train2.start();
        
    }
    
    
    static class Train extends Thread {
        
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
        TSimInterface tsi = TSimInterface.getInstance();

        Lab1 parent;
        
        // Train Id enum
        final int TOP = 1;
        final int BOTTOM = 2;



        int id;
        Track track;

        int speed = 10;
        boolean skipSensor = true;

        public Train(int speed, int id, Lab1 parent) {
            
            this.parent = parent;
            this.id = id;

            try {
                if (id == TOP) {
                    track = Track.One;
                    System.err.println("Aquire lock topStation");
                    parent.topStation.acquire();
                } else if (id == BOTTOM) {
                        track = Track.Ten;
                        System.err.println("Aquire lock bottomStation");
                        parent.bottomStation.acquire();
                } else {
                    throw new RuntimeException("Unknown train id");
                }
            } catch (InterruptedException ex) {
                Logger.getLogger(Lab1.class.getName()).log(Level.SEVERE, null, ex);
            }

        }
        
        @Override
        public void run() {
            try {
                startTrain();
            } catch (CommandException ex) {
                Logger.getLogger(Lab1.class.getName()).log(Level.SEVERE, "Train "+id, ex);
            }
            while (true) {
                try {
                    SensorEvent e = tsi.getSensor(id);
                    
                    //Debug
                    System.err.println(e.toString());
                    
                    handleSensor(e);
                    
                } catch (CommandException ex) {
                    Logger.getLogger(Lab1.class.getName()).log(Level.SEVERE, "Train "+id, ex);
                } catch (InterruptedException ex) {
                    Logger.getLogger(Lab1.class.getName()).log(Level.SEVERE, "Train "+id, ex);
                }
            }
        }


        public void handleSensor(SensorEvent e) 
            throws InterruptedException, CommandException 
        {
            if (e.getStatus() == SensorEvent.INACTIVE) {
                return;
            }
            
            if (skipSensor) { 
                //TODO: release lock 1
                System.err.println("Skipping sensor");
                skipSensor = false;
                return; 
            }
            
            stopTrain();
            
            switch (track) {
                case One: 
                    //TODO: aquire lock 1
                    System.err.println("132: Aquire lock crossing");
                    parent.crossing.acquire();
                    track = Track.Three;

                    break;
                case Two:
                    //TODO: aquire lock 1
                    System.err.println("139: Aquire lock crossing");
                    parent.crossing.acquire();
                    track = Track.Four;

                    break;
                case Three:
                case Four:

                    if (speed < 0) {
                        //TODO: aquire lock 1
                        System.err.println("149: Aquire lock crossing");
                        parent.crossing.acquire();
                        track = track == Track.Three ? Track.One : Track.Two;
                    } else {
                        // Aquire lock 3
                        System.err.println("154: Aquire lock rightBend");
                        parent.rightBend.acquire();
                        
                        // Release lock 2 if track three
                        // and flip switch a
                        if (track == Track.Three) {
                            System.err.println("160: Release lock topStation");
                            parent.topStation.release();
                            tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                        } else {
                            tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                        }
                        
                        track = Track.Five;
                    }
                    break;
                case Five:
                    if (speed < 0) {
                        //TODO: try to aquire lock 2
                        System.err.println("173: Try acquire lock topStation");
                        boolean success = parent.topStation.tryAcquire();
                        if (!success) {
                            track = Track.Four;
                            //switch a to bottom
                            tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                        
                        } else {
                            track = Track.Three;
                            //switch a to top
                            tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                        }
                        // release lock 3 
                        System.err.println("186: Release lock rightBend");
                        parent.rightBend.release();
                    } else {
                        //TODO: try to aquire lock 4
                        System.err.println("190: Try acquire lock overtake");
                        boolean success = parent.overtake.tryAcquire();
                        
                        if (!success) {
                            track = Track.Seven;
                            //switch b to bottom
                            tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                            
                        } else {
                            track = Track.Six;
                            //switch b to top
                            tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                        }
                        // release lock 3
                        System.err.println("204: Release lock rightBend");
                        parent.rightBend.release();
                    }
                    break;
                case Six:
                    if (speed < 0) {
                        // aquire lock 3
                        System.err.println("211: Aquire lock rightBend");
                        parent.rightBend.acquire();
                        // switch b to top
                        tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    } else {
                        // aquire lock 5
                        System.err.println("217: Aquire lock leftBend");
                        parent.leftBend.acquire();
                        // switch c to top
                        tsi.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                    }
                    // release lock 4
                    System.err.println("223: Release lock overtake");
                    parent.overtake.release();
                    break;
                case Seven:
                    if (speed < 0) {
                        // aquire lock 3
                        System.err.println("229: Aquire lock rightBend");
                        parent.rightBend.acquire();
                        // switch b to bottom
                        tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                    } else {
                        // aquire lock 5
                        System.err.println("235: Aquire lock leftBend");
                        parent.leftBend.acquire();
                        // switch c to bottom
                        tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                    }
                    break;
                case Eight:
                    if (speed < 0) {
                        //TODO: try to aquire lock 4
                        System.err.println("244: Try Aquire lock overtake");
                        boolean success = parent.overtake.tryAcquire();
                        
                        if (!success) { 
                            track = Track.Seven;
                            //switch c to bottom
                            tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                        } else {
                            track = Track.Six;
                            //switch c to top
                            tsi.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                        }
                        // release lock 5 
                        System.err.println("257: Release lock leftBend");
                        parent.leftBend.release();
                    } else {
                        
                        //TODO: try to aquire lock 6
                        System.err.println("262: Try Aquire lock bottomStation");
                        boolean success = parent.bottomStation.tryAcquire();
                        
                        if (!success) { 
                            track = Track.Ten;
                            //switch d to bottom
                            tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
                        } else {
                            track = Track.Nine;
                            //switch d to top
                            tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
                        }
                        System.err.println("274: Release lock leftBend");
                        parent.leftBend.release();
                        // release lock 5
                    }
                    break;
                case Nine:
                case Ten:

                    if (speed < 0) {
                        //TODO: aquire lock 5
                        System.err.println("284: Aquire lock leftBend");
                        parent.leftBend.acquire();
                        //TODO: release lock 6 if track 9
                        //TODO: and flip switch d
                        if (track == Track.Nine) {
                            System.err.println("289: Release lock bottomStation");
                            parent.bottomStation.release();
                            tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
                        } else {
                            tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
                        }
                        track = Track.Eight;
                    } else {
                        //TODO: Stations 
                    }
                    break;

                default: 
                    throw new RuntimeException();


            }
            startTrain();
            skipSensor = true;

        }


        public void startTrain() throws CommandException { 
            System.err.println("Starting train "+id);
            tsi.setSpeed(id,speed); 
        }
        public void stopTrain() throws CommandException { 
            System.err.println("Stopping train "+id);
            tsi.setSpeed(id,0); 
        }
        

    }

}

