import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.*;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import static TSim.TSimInterface.*;

public class Lab2 {
    public static final int MAX_SPEED = 20;

    public static void main(String[] args) {
        Lab2 i = new Lab2(args);
    }

    public Lab2(String[] args) {
        TSimInterface tsi = TSimInterface.getInstance();
        tsi.setDebug(true);
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
            System.err.println("Invalid argument to program. Usage: java Lab2 <integer> <integer> <integer>");
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
    public static final TSimInterface tsi = TSimInterface.getInstance();
    private final static TrackMonitor monitor = new TrackMonitor();
         
    // Train Id enum
    final int TOP_TRAIN = 1;
    final int BOTTOM_TRAIN = 2;
        
    // Wait time in milliseconds
    final int TIME_AT_STATION = 2000;

    int id;
    Track track;
    Track oldTrack;

    int direction;
    int speed;
    final int simSpeed;
    boolean stationStop = false;
    boolean stationSkip = true;

    public Train(int speed, int id, int simSpeed) {
        this.simSpeed = simSpeed;
        this.id = id;
        this.speed = speed;

        if (id == TOP_TRAIN) {   
            track = Track.One;
            this.direction = 1;  
            this.oldTrack = Track.One;
        }
        else if (id == BOTTOM_TRAIN) {
            track = Track.Nine;
            this.direction = -1;
            this.oldTrack = Track.Nine;
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
            Logger.getLogger(Lab2.class.getName()).log(Level.SEVERE, "Train "+id, ex);
        }
    }

    public void handleSensor(SensorEvent e) 
        throws InterruptedException, CommandException 
    {

        if (stationSkip) {
            stationSkip = false;
            return;
        }
        if (e.getStatus() == SensorEvent.INACTIVE) {
            if (stationStop) {
                stopTrain();
                int waitTime = TIME_AT_STATION + 2 * simSpeed * Math.abs(speed);
                Thread.sleep(waitTime);

                // Change direction and speed
                direction *= -1;
                speed *= -1;
                startTrain();
                oldTrack = null;
                stationStop = false;
                stationSkip = true;
            }
            return;
        }
            
        if (oldTrack != null && oldTrack != track) { 
            switch (oldTrack) {
                case One:
                case Two:
                    if (direction > 0) {
                        monitor.leaveTrack(id,Track.Crossing);
                    }
                    break;
                case Three:
                case Four:
                    if (direction < 0) {
                        monitor.leaveTrack(id, Track.Crossing);
                    } else {
                        monitor.leaveTrack(id, oldTrack);
                    }
                    break;
                default:
                    monitor.leaveTrack(id, oldTrack);
                    break;   
            }
            oldTrack = null;
            return; 
        }
        
        stopTrain(); 
        oldTrack = track; 
        switch (track) {
            case One: 
            case Two:
                if (direction > 0) {
                    monitor.enterTrack(id, Track.Crossing);
                    track = track == Track.One ? Track.Three : Track.Four;
                } else {
                    stationStop = true;
                }
                break;
            case Three:
            case Four:
                if (direction < 0) {
                    monitor.enterTrack(id, Track.Crossing);
                    track = track == Track.Three ? Track.One : Track.Two;
                } else {
                    track = enterSingleTrack(Track.Five , Switch.A);
                }
                break;
            case Five:
                if (direction < 0) {
                    track = enterDualTrack(Track.Three, Track.Four, Switch.A);
                }
                else{
                    track = enterDualTrack(Track.Six, Track.Seven, Switch.B);
                }
                break;
            case Six:
            case Seven:
                if (direction < 0) {
                    track = enterSingleTrack(Track.Five, Switch.B);
                } else {
                    track = enterSingleTrack(Track.Eight, Switch.C);
                }
                break;
            case Eight:
                if (direction < 0) {
                    track = enterDualTrack(Track.Six, Track.Seven, Switch.C);
                }
                else{
                    track = enterDualTrack(Track.Nine, Track.Ten, Switch.D);
                }
                break;
            case Nine:
            case Ten:
                if (direction < 0) {
                    track = enterSingleTrack(Track.Eight, Switch.D);
                } else {
                    stationStop = true;
                }
                break;
        }
        startTrain();
    }

    public Track enterSingleTrack(Track target, Switch s)
        throws CommandException, InterruptedException
    {
        monitor.enterTrack(id, target);
        s.setTrack(track.isPrimary());
        return target;
        
    }

    public Track enterDualTrack(Track primary, Track secondary, Switch s) 
        throws CommandException, InterruptedException
    {
        if (!monitor.isBusy(id,primary)) {
            monitor.enterTrack(id, primary);
            s.setTrack(primary.isPrimary());
            return primary;
        } else {
            monitor.enterTrack(id, secondary);
            s.setTrack(secondary.isPrimary());
            return secondary;
        }
    }

    public void startTrain() throws CommandException { 
        Train.tsi.setSpeed(id,speed); 
    }
    public void stopTrain() throws CommandException { 
        Train.tsi.setSpeed(id,0); 
    }


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
            Train.tsi.setSwitch(x, y, direction == SWITCH_LEFT ? SWITCH_LEFT : SWITCH_RIGHT);
        }
        else{
            Train.tsi.setSwitch(x, y, direction == SWITCH_LEFT ? SWITCH_RIGHT : SWITCH_LEFT);
        }
    }
}

enum Track { 
    One(    Switch.PRIMARY),
    Two(    Switch.SECONDARY),
    Three(  Switch.PRIMARY),
    Four(   Switch.SECONDARY),
    Five(   Switch.PRIMARY),
    Six(    Switch.PRIMARY),
    Seven(  Switch.SECONDARY),
    Eight(  Switch.PRIMARY),
    Nine(   Switch.PRIMARY),
    Ten(    Switch.SECONDARY), 
    Crossing(Switch.PRIMARY);

    private boolean isPrimary; 

    Track(boolean isPrimary) {
        this.isPrimary = isPrimary;
    }

    public boolean isPrimary() {
        return isPrimary;
    }
}

class TrackMonitor {

    private final Lock lock = new ReentrantLock();
    private final Condition isBusy = lock.newCondition();

    private final Map<Track,Boolean> busyTrack = new HashMap<>();

    public TrackMonitor() {

        busyTrack.put(Track.One,false);
        busyTrack.put(Track.Two,false);
        busyTrack.put(Track.Three,true);
        busyTrack.put(Track.Four,false);
        busyTrack.put(Track.Five,false);
        busyTrack.put(Track.Six,false);
        busyTrack.put(Track.Seven,false);
        busyTrack.put(Track.Eight,false);
        busyTrack.put(Track.Nine,true);
        busyTrack.put(Track.Ten,false);
        busyTrack.put(Track.Crossing,false);

    }

    public boolean isBusy(int id ,Track t) {
        lock.lock();        
        boolean result = busyTrack.get(t);
        lock.unlock();
        return result;
    }

    public void enterTrack(int id, Track t) 
        throws InterruptedException 
    {
        lock.lock();        
        while (busyTrack.get(t)) isBusy.await();
        busyTrack.put(t,true);
        lock.unlock();
    }

    public void leaveTrack(int id, Track t) {
        lock.lock();
        busyTrack.put(t,false);
        isBusy.signal();
        lock.unlock();
    }
}