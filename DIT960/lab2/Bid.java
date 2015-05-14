
import java.util.Objects;

public class Bid {
	final public String name;
	final public int bid;

	public Bid(String name, int bid) {
		this.name = name;
		this.bid = bid;
	}

	public int hashCode() {
		return 1 + 23*bid + 31*name.hashCode();
	}

    @Override
    public String toString() {
        return name + " " + bid;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof Bid)) return false;
        
        final Bid bid = (Bid) obj;
        
        if (!Objects.equals(this.name, bid.name)) {
            return false;
        }
        
        if (this.bid != bid.bid) {
            return false;
        }
        return true;
    }
    
    
}

