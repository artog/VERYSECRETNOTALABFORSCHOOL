import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.logging.Level;
import java.util.logging.Logger;

import static TSim.TSimInterface.*;

public class Lab1 {
    public static final int MAX_SPEED = 20;

    public static void main(String[] args) {
        Lab1 i = new Lab1(args);
    }

    public Lab1(String[] args) {
        TSimInterface tsi = TSimInterface.getInstance();
        int speed1 = 10, speed2 = 10;
        int simSpeed = 100;
        
        try {
            if (args.length > 0) {
                speed1 = Integer.parseInt(args[0]);
                if (speed1 > MAX_SPEED) {
                    speed1 = MAX_SPEED;
                    System.err.println("Speed for train one exceeding max limit, setting to max.");
                }
            }
            if (args.length > 1) {
                speed2 = Integer.parseInt(args[1]);
                if (speed2 > MAX_SPEED) {
                    speed2 = MAX_SPEED;
                    System.err.println("Speed for train two exceeding max limit, setting to max.");
                }
            }
            if (args.length > 2) {
                simSpeed = Integer.parseInt(args[2]);
            }
        } catch (NumberFormatException nex) {
            System.err.println("Invalid argument to program. Usage: java Lab1 <integer> <integer> <integer>");
        }
        
        startTrains(speed1, speed2, simSpeed);
    }

    private void startTrains(int trainSpeed1, int trainSpeed2, int simSpeed) {
        Train train1 = new Train(trainSpeed1, 1, simSpeed);
        Train train2 = new Train(trainSpeed2, 2, simSpeed);
        train1.start();
        train2.start();
    }
}

class Train extends Thread {   
    private static final TSimInterface tsi = TSimInterface.getInstance();

    private static final Semaphore topStation = new Semaphore(0,true);
    private static final Semaphore crossing = new Semaphore(1,true);
    private static final Semaphore rightBend = new Semaphore(1,true);
    private static final Semaphore overtake = new Semaphore(1,true);
    private static final Semaphore leftBend = new Semaphore(1,true);
    private static final Semaphore bottomStation = new Semaphore(0,true);
         
    // Train Id enum
    final int TOP_TRAIN = 1;
    final int BOTTOM_TRAIN = 2;
        
    // Wait time in milliseconds
    final int TIME_AT_STATION = 2000;

    int id;
    Track track;

    int direction;
    int speed;
    boolean releaseSensor = true;
    final int simSpeed;
    boolean stationStop = false;

    private Semaphore releaseLock = null;


    public Train(int speed, int id, int simSpeed) {
        this.simSpeed = simSpeed;
        this.id = id;
        this.speed = speed;

        if (id == TOP_TRAIN) {   
            track = Track.One;
            this.direction = 1;  
        }
        else if (id == BOTTOM_TRAIN) {
            track = Track.Nine;
            this.direction = -1;
        } else {
            throw new RuntimeException("Unknown train id");
        }
    }
        
    @Override
    public void run() {
        try {
            startTrain();
            while (true) {
                SensorEvent e = tsi.getSensor(id);
                handleSensor(e);
            }
        } catch (CommandException | InterruptedException ex) {
            Logger.getLogger(Lab1.class.getName()).log(Level.SEVERE, "Train "+id, ex);
        }
    }

    public void handleSensor(SensorEvent e) 
        throws InterruptedException, CommandException 
    {
        if (e.getStatus() == SensorEvent.INACTIVE) {
            if (stationStop) {
                stopTrain();
                int waitTime = TIME_AT_STATION + 2 * simSpeed * Math.abs(speed);
                Thread.sleep(waitTime);

                // Change direction and speed
                direction *= -1;
                speed *= -1;
                startTrain();
                stationStop = false;
            }
            return;
        }
            
        if (releaseSensor) { 
            switch (track) {
                case One:
                case Two:
                    if (direction < 0) {
                        crossing.release();
                    }
                    break;
                case Three:
                case Four:
                    if (direction > 0) {
                        crossing.release();
                    } else {
                        rightBend.release();
                    }
                    break;
                case Six:
                case Seven:
                    if (direction > 0) {
                        rightBend.release();
                    } else {
                        leftBend.release();
                    }
                    break;
                case Nine:
                case Ten:
                    if (direction > 0) {
                        leftBend.release();
                    }
                    break;   
            }

            // Release saved lock
            if (releaseLock != null) {
                releaseLock.release();
                releaseLock = null;
                System.err.println("Releasing special case");
            }

            releaseSensor = false;
            return; 
        }
        
        stopTrain();   
        switch (track) {
            case One: 
            case Two:
                if (direction > 0) {
                    crossing.acquire();
                    track = track == Track.One ? Track.Three : Track.Four;
                } else {
                    stationStop = true;
                }
                break;
            case Three:
            case Four:
                if (direction < 0) {
                    crossing.acquire();
                    track = track == Track.Three ? Track.One : Track.Two;
                } else {
                    enterSingleTrack(rightBend, track == Track.Three ? topStation : null, Switch.A);
                    track = Track.Five;
                }
                break;
            case Five:
                if (direction < 0) {
                    boolean trackAvailable = enterDualTrack(topStation, Switch.A);
                    track = trackAvailable ? Track.Three : Track.Four;
                }
                else{
                    boolean trackAvailable = enterDualTrack(overtake, Switch.B);
                    track = trackAvailable ? Track.Six : Track.Seven;
                }
                break;
            case Six:
            case Seven:
                if (direction < 0) {
                    enterSingleTrack(rightBend, track == Track.Six ? overtake : null, Switch.B);
                    track = Track.Five;
                } else {
                    enterSingleTrack(leftBend, track == Track.Six ? overtake : null, Switch.C);
                    track = Track.Eight;
                }
                break;
            case Eight:
                if (direction < 0) {
                    boolean trackAvailable = enterDualTrack(overtake, Switch.C);
                    track = trackAvailable ? Track.Six : Track.Seven;
                }
                else{
                    boolean trackAvailable = enterDualTrack(bottomStation, Switch.D);
                    track = trackAvailable ? Track.Nine : Track.Ten;
                }
                break;
            case Nine:
            case Ten:
                if (direction < 0) {
                    enterSingleTrack(leftBend, track == Track.Nine ? bottomStation : null, Switch.D);
                    track = Track.Eight;
                } else {
                    stationStop = true;
                }
                break;
        }
        startTrain();
        releaseSensor = true;
    }

    public void enterSingleTrack(Semaphore lockToAcquire, Semaphore lockToRelease, Switch s)
        throws CommandException, InterruptedException
    {
        lockToAcquire.acquire();
        if (lockToRelease != null) {
            // Save for release at next sensor
            releaseLock = lockToRelease;
            s.setTrack(Switch.PRIMARY);
        }
        else{
            s.setTrack(Switch.SECONDARY);
        }
    }

    public boolean enterDualTrack(Semaphore lockToAcquire, Switch s) 
        throws CommandException
    {
        boolean primaryAvailable = lockToAcquire.tryAcquire();
        s.setTrack(primaryAvailable);
        return primaryAvailable;
    }

    public void startTrain() throws CommandException { 
        tsi.setSpeed(id,speed); 
    }
    public void stopTrain() throws CommandException { 
        tsi.setSpeed(id,0); 
    }

    enum Switch{
        A(17, 7, SWITCH_RIGHT),
        B(15, 9, SWITCH_RIGHT),
        C(4,  9, SWITCH_LEFT),
        D(3, 11, SWITCH_LEFT);

        public static final boolean PRIMARY   = true;
        public static final boolean SECONDARY = false;
        
        private final int x;
        private final int y;
        private final int direction;

        Switch(int x, int y, int direction)
        {
            this.x = x;
            this.y = y;
            this.direction = direction;
        }

        public void setTrack(boolean toPrimary)
            throws CommandException
        {
            if(toPrimary){
                tsi.setSwitch(x, y, direction == SWITCH_LEFT ? SWITCH_LEFT : SWITCH_RIGHT);
            }
            else{
                tsi.setSwitch(x, y, direction == SWITCH_LEFT ? SWITCH_RIGHT : SWITCH_LEFT);
            }
        }
    }

    enum Track { 
        One,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten
    }
}