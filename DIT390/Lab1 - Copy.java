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
    
    public static final boolean LEFT_SWITCH = false;
    public static final boolean RIGHT_SWITCH = true;
    public static final boolean PRIMARY = true;
    public static final boolean SECONDARY = false;

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

        enum Switch{
            A(17, 7, RIGHT_SWITCH),
            B(15, 9, RIGHT_SWITCH),
            C(4,  9, LEFT_SWITCH),
            D(3, 11, LEFT_SWITCH);

            private final int x;
            private final int y;
            private final boolean direction;

            public Switch(int x, int y, boolean direction)
            {
                this.x = x;
                this.y = y;
                this.direction = direction;
            }

            public void setTrack(boolean toPrimary){
                if(toPrimary){
                    tsi.setSwitch(x, y, direction == LEFT_SWITCH : TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                }
                else{
                    tsi.setSwitch(x, y, direction == LEFT_SWITCH : TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
                }
            }
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
                    parent.crossing.acquire();
                    track = Track.Three;
                    break;
                case Two:
                    parent.crossing.acquire();
                    track = Track.Four;
                    break;
                case Three:
                case Four:
                    if (speed < 0) {
                        parent.crossing.acquire();
                        track = track == Track.Three ? Track.One : Track.Two;
                    } else {
                        parent.rightBend.acquire();
                        if (track == Track.Three) {
                            parent.topStation.release();
                            A.setTrack(PRIMARY);
                        } else {
                            A.setTrack(SECONDARY);
                        }
                        track = Track.Five;
                    }
                    break;
                case Five:
                    if (speed < 0) {
                        boolean trackAvailable = parent.topStation.tryAcquire();
                        A.setTrack(trackAvailable);
                        track = trackAvailable ? Tree : Four;
                    }
                    else{
                        boolean trackAvailable = parent.overtake.tryAcquire();
                        B.setTrack(trackAvailable);
                        track = trackAvailable ? Six : Seven;
                    }
                    parent.rightBend.release();
                    break;
                case Six:
                    if (speed < 0) {
                        parent.rightBend.acquire();
                        B.setTrack(PRIMARY);
                        track = Track.Five;
                    } else {
                        parent.leftBend.acquire();
                        C.setTrack(PRIMARY);
                        track = Track.Eight;
                    }
                    parent.overtake.release();
                    break;
                case Seven:
                    if (speed < 0) {
                        parent.rightBend.acquire();
                        B.setTrack(SECONDARY);
                        track = Track.Five;
                    } else {
                        parent.leftBend.acquire();
                        C.setTrack(SECONDARY);
                        track = Track.Eight;
                    }
                    break;
                case Eight:
                    if (speed < 0) {
                        boolean trackAvailable = parent.overtake.tryAcquire();
                        C.setTrack(trackAvailable);
                        track = trackAvailable ? Six : Seven;
                    }
                    else{
                        boolean trackAvailable = parent.bottomStation.tryAcquire();
                        D.setTrack(trackAvailable);
                        track = trackAvailable ? Nine : Ten;
                    }
                    parent.leftBend.release();
                    break;
                case Nine:
                case Ten:
                    if (speed < 0) {
                        parent.leftBend.acquire();
                        if (track == Track.Nine) {
                            parent.bottomStation.release();
                            D.setTrack(PRIMARY);
                        } else {
                            D.setTrack(SECONDARY);
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

