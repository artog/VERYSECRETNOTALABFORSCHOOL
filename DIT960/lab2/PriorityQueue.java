
import java.util.AbstractCollection;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Queue;


/**
 * PriorityQueue class implemented via the binary heap.
 */
public class PriorityQueue<AnyType> extends AbstractCollection<AnyType>
                                    implements Queue<AnyType>
{
    /**
     * Construct an empty PriorityQueue.
     */
    public PriorityQueue( )
    {
        currentSize = 0;
        cmp = null;
        array = (AnyType[]) new Object[ DEFAULT_CAPACITY + 1 ];
    }
    
    /**
     * Construct an empty PriorityQueue with a specified comparator.
     */
    public PriorityQueue( Comparator<? super AnyType> c )
    {
        currentSize = 0;
        cmp = c;
        array = (AnyType[]) new Object[ DEFAULT_CAPACITY + 1 ];
    }
    
     
    /**
     * Construct a PriorityQueue from another Collection.
     */
    public PriorityQueue( Collection<? extends AnyType> coll )
    {
        cmp = null;
        currentSize = coll.size( );
        array = (AnyType[]) new Object[ ( currentSize + 2 ) * 11 / 10 ];
        
        int i = 1;
        for( AnyType item : coll )
            array[ i++ ] = item;
        buildHeap( );
    }
    
    /**
     * Compares lhs and rhs using comparator if
     * provided by cmp, or the default comparator.
     */
    private int compare( AnyType lhs, AnyType rhs )
    {
        if( cmp == null )
            return ((Comparable)lhs).compareTo( rhs );
        else
            return cmp.compare( lhs, rhs );    
    }
    
    /**
     * Adds an item to this PriorityQueue.
     * @param x any object.
     * @return true.
     */
    public boolean add( AnyType x )
    {
        if( currentSize + 1 == array.length )
            doubleArray( );

            // Percolate up
        int hole = ++currentSize;
        array[ 0 ] = x;
        
        for( ; compare( x, array[ hole / 2 ] ) < 0; hole /= 2 )
            array[ hole ] = array[ hole / 2 ];
        array[ hole ] = x;
        
        return true;
    }
    
    /**
     * Returns the number of items in this PriorityQueue.
     * @return the number of items in this PriorityQueue.
     */
    public int size( )
    {
        return currentSize;
    }
    
    /**
     * Make this PriorityQueue empty.
     */
    public void clear( )
    {
        currentSize = 0;
    }
    
    /**
     * Returns an iterator over the elements in this PriorityQueue.
     * The iterator does not view the elements in any particular order.
     */
    public Iterator<AnyType> iterator( )
    {
        return new Iterator<AnyType>( )
        {
            int current = 0;
            
            public boolean hasNext( )
            {
                return current != size( );
            }
            
            public AnyType next( )
            {
                if( hasNext( ) )
                    return array[ ++current ];
                else
                    throw new NoSuchElementException( );
            }
            
            public void remove( ) { }
        };
    }
     
    /**
     * Returns the smallest item in the priority queue.
     * @return the smallest item.
     * @throws NoSuchElementException if empty.
     */
    public AnyType element( )
    {
        if( isEmpty( ) )
            throw new NoSuchElementException( );
        return array[ 1 ];
    }
    
    public AnyType peed() {
        return element();
    }
    
    /**
     * Removes the smallest item in the priority queue.
     * @return the smallest item.
     * @throws NoSuchElementException if empty.
     */
    public AnyType remove( )
    {
        AnyType minItem = element( );
        array[ 1 ] = array[ currentSize-- ];
        percolateDown( 1 );

        return minItem;
    }


    /**
     * Establish heap order property from an arbitrary
     * arrangement of items. Runs in linear time.
     */
    private void buildHeap( )
    {
        for( int i = currentSize / 2; i > 0; i-- )
            percolateDown( i );
    }

    private static final int DEFAULT_CAPACITY = 100;

    private int currentSize;   // Number of elements in heap
    private AnyType [ ] array; // The heap array
    private Comparator<? super AnyType> cmp;

    /**
     * Internal method to percolate down in the heap.
     * @param hole the index at which the percolate begins.
     */
    private void percolateDown( int hole )
    {
        int child;
        AnyType tmp = array[ hole ];

        for( ; hole * 2 <= currentSize; hole = child )
        {
            child = hole * 2;
            if( child != currentSize &&
                    compare( array[ child + 1 ], array[ child ] ) < 0 )
                child++;
            if( compare( array[ child ], tmp ) < 0 ) {
                array[ hole ] = array[ child ];
            } else {
                break;
            }
        }
        array[ hole ] = tmp;
    }
    /**
     * Internal method to percolate up in the heap.
     * @param hole the index at which the percolate begins.
     */
    private void percolateUp( int hole )
    {
        int child;
        AnyType tmp = array[ hole ];
        
        for( ; hole > 1 ; hole /= 2 ) {
            
            if (compare( tmp, array[ hole / 2 ] ) < 0) {
                array[ hole ] = array[ hole / 2 ];
            } else {
                break;
            }
        }
        array[ hole ] = tmp;
        
    }
    
    /**
     * Internal method to extend array.
     */
    private void doubleArray( )
    {
        AnyType [ ] newArray;

        newArray = (AnyType []) new Object[ array.length * 2 ];
        for( int i = 0; i < array.length; i++ )
            newArray[ i ] = array[ i ];
        array = newArray;
    }
    
    private int lookup(AnyType x) {
        int i = 1;
        while (i <= currentSize) {
            if (compare(array[i], x) == 0) {
                return i;
            } else if (compare(array[i], x) < 0) {
                i = i*2;
            } else {
                i = i*2+1;
            }
        }
        return currentSize+1;
    }
    
    public void update(AnyType old, AnyType notOld) {
        int index = lookup(old);
        
        if ( index == array.length ) {
            doubleArray();
        }
        
        array[index] = notOld;
        
        if (compare(old, notOld) < 0) {
            percolateDown(index);
        } else {
            percolateUp(index);
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (AnyType e : array) {
            if (e != null) {
                sb.append(e.toString()+", ");
            } else {
                sb.append("-,");
            }
        }
        sb.setCharAt(sb.length()-1, ']');
        return sb.toString();
    }

    
    
    @Override
    public boolean offer(AnyType e) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public AnyType poll() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public AnyType peek() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private void printDebug(String source) {
        System.err.println(source+": "+toString());
    }
}