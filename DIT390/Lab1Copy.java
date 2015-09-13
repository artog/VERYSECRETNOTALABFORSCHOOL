import TSim.*;
import java.util.concurrent.Semaphore;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Lab1Copy {
    
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

    public static final int MAX_SPEED = 20;

    public static void main(String[] args) {
        Lab1 i = new Lab1(args);
    }

    public Lab1Copy(String[] args) {
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
        Train train1 = new Train(trainSpeed1, 1, this, simSpeed);
        Train train2 = new Train(trainSpeed2, 2, this, simSpeed);
        
        train1.start();
        train2.start();
        
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

            Switch(int x, int y, boolean direction)
            {
                this.x = x;
                this.y = y;
                this.direction = direction;
            }

            public void setTrack(boolean toPrimary)
                throws CommandException
            {
                if(toPrimary){
                    tsi.setSwitch(x, y, direction == LEFT_SWITCH ? TSimInterface.SWITCH_LEFT : TSimInterface.SWITCH_RIGHT);
                }
                else{
                    tsi.setSwitch(x, y, direction == LEFT_SWITCH ? TSimInterface.SWITCH_RIGHT : TSimInterface.SWITCH_LEFT);
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
        static TSimInterface tsi = TSimInterface.getInstance();

        Lab1Copy parent;
        
        // Train Id enum
        final int TOP = 1;
        final int BOTTOM = 2;
        
        // Wait time in milliseconds
        final int TIME_AT_STATION = 2000;



        int id;
        Track track;

        int direction;
        int speed;
        boolean releaseSensor = true;
        final int simSpeed;
        boolean stationStop = false;

        public Train(int speed, int id, Lab1Copy parent, int simSpeed) {
            this.simSpeed = simSpeed;
            this.parent = parent;
            this.id = id;
            this.speed = speed;

            try {
                if (id == TOP) {
                    
                    track = Track.One;
                    this.direction = 1;
                    parent.topStation.acquire();
                    
                } else if (id == BOTTOM) {
                    
                    track = Track.Nine;
                    this.direction = -1;
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
                if (stationStop) {
                    stopTrain();
                    //TODO: Stations
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
                            parent.crossing.release();
                        }
                        break;
                    case Three:
                    case Four:
                        if (direction > 0) {
                            parent.crossing.release();
                        } else {
                            parent.rightBend.release();
                        }
                        break;
                    case Six:
                    case Seven:
                        if (direction > 0) {
                            parent.rightBend.release();
                        } else {
                            parent.leftBend.release();
                        }
                        break;
                    case Nine:
                    case Ten:
                        if (direction > 0) {
                            parent.leftBend.release();
                        }
                        
                }
                releaseSensor = false;
                return; 
            }
            
            stopTrain();
            
            switch (track) {
                case One: 
                case Two:
                    if (direction > 0) {
                        parent.crossing.acquire();

                        if (track == Track.One) {
                            track = Track.Three;
                        } else {
                            track = Track.Four;
                        }
                    } else {
                        stationStop = true;
                    }
                    break;

                case Three:
                case Four:
                    if (direction < 0) {
                        parent.crossing.acquire();
                        track = track == Track.Three ? Track.One : Track.Two;
                    } else {
                        parent.rightBend.acquire();
                        if (track == Track.Three) {
                            parent.topStation.release();
                            Switch.A.setTrack(PRIMARY);
                        } else {
                            Switch.A.setTrack(SECONDARY);
                        }
                        track = Track.Five;
                    }
                    break;
                case Five:
                    if (direction < 0) {
                        boolean trackAvailable = parent.topStation.tryAcquire();
                        Switch.A.setTrack(trackAvailable);
                        track = trackAvailable ? Track.Three : Track.Four;
                    }
                    else{
                        boolean trackAvailable = parent.overtake.tryAcquire();
                        Switch.B.setTrack(trackAvailable);
                        track = trackAvailable ? Track.Six : Track.Seven;
                    }
                    break;
                case Six:
                    if (direction < 0) {
                        
                        parent.rightBend.acquire();
                        Switch.B.setTrack(PRIMARY);
                        track = Track.Five;
                    } else {
                        parent.leftBend.acquire();
                        Switch.C.setTrack(PRIMARY);
                        track = Track.Eight;
                    }
                    parent.overtake.release();
                    break;
                case Seven:
                    if (direction < 0) {
                        parent.rightBend.acquire();
                        Switch.B.setTrack(SECONDARY);
                        track = Track.Five;
                    } else {
                        parent.leftBend.acquire();
                        Switch.C.setTrack(SECONDARY);
                        track = Track.Eight;
                    }
                    break;
                case Eight:
                    if (direction < 0) {
                        boolean trackAvailable = parent.overtake.tryAcquire();
                        Switch.C.setTrack(trackAvailable);
                        track = trackAvailable ? Track.Six : Track.Seven;
                    }
                    else{
                        boolean trackAvailable = parent.bottomStation.tryAcquire();
                        Switch.D.setTrack(trackAvailable);
                        track = trackAvailable ? Track.Nine : Track.Ten;
                    }
                    break;
                case Nine:
                case Ten:
                    if (direction < 0) {
                        parent.leftBend.acquire();
                        if (track == Track.Nine) {
                            parent.bottomStation.release();
                            Switch.D.setTrack(PRIMARY);
                        } else {
                            Switch.D.setTrack(SECONDARY);
                        }
                        track = Track.Eight;
                    } else {
                        stationStop = true;
                    }
                    break;

                default: 
                    throw new RuntimeException();


            }
            startTrain();
            releaseSensor = true;
        }


        public void startTrain() throws CommandException { 
            tsi.setSpeed(id,speed); 
        }
        public void stopTrain() throws CommandException { 
            tsi.setSpeed(id,0); 
        }
    }

}

