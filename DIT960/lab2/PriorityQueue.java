
import java.util.AbstractCollection;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Queue;


/**
 * PriorityQueue class implemented via the binary heap.
 * @param <AnyType>
 */
public class PriorityQueue<AnyType> 
{
    
    private static final int DEFAULT_CAPACITY = 100;

    private int currentSize;   // Number of elements in heap
    private AnyType[ ] array; // The heap array
    private final Comparator<? super AnyType> cmp;
    
    private final Map<AnyType,Integer> indexMap = new HashMap<>();
    
    /**
     * Construct an empty PriorityQueue.
     */
    @SuppressWarnings("unchecked")
    public PriorityQueue( )
    {
        currentSize = 0;
        cmp = null;
        array = (AnyType[]) new Object[ DEFAULT_CAPACITY + 1 ];
    }
    
    /**
     * Construct an empty PriorityQueue with a specified comparator.
     * @param c
     */
    @SuppressWarnings("unchecked")
    public PriorityQueue( Comparator<? super AnyType> c )
    {
        currentSize = 0;
        cmp = c;
        array = (AnyType[]) new Object[ DEFAULT_CAPACITY + 1 ];
    }
    
     
    /**
     * Construct a PriorityQueue from another Collection.
     * @param coll
     */
    @SuppressWarnings("unchecked")
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
    @SuppressWarnings("unchecked")
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
        assert invariant() : showHeap();
        
        if( currentSize + 1 == array.length )
            doubleArray( );

            // Percolate up
        int hole = ++currentSize;
        array[ 0 ] = x;
        
        for( ; compare( x, array[ hole / 2 ] ) < 0; hole /= 2 ) {
            array[ hole ] = array[ hole / 2 ]; 
            indexMap.put(array[ hole ], hole);
        }
        
        array[ hole ] = x;
        indexMap.put(array[ hole ], hole);
        
        
        assert invariant() : showHeap();
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
    
    public boolean isEmpty() {
        return currentSize == 0;
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
                indexMap.put(array[ hole ], hole);
                
            } else {
                break;
            }
        }
        array[ hole ] = tmp;
        indexMap.put(array[ hole ], hole);
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
                indexMap.put(array[ hole ], hole);
            } else {
                break;
            }
        }
        array[ hole ] = tmp;
        indexMap.put(array[ hole ], hole);
        
    }
    
    /**
     * Internal method to extend array.
     */
    @SuppressWarnings("unchecked")
    private void doubleArray( )
    {
        AnyType [ ] newArray;

        newArray = (AnyType []) new Object[ array.length * 2 ];
        System.arraycopy(array, 0, newArray, 0, array.length);
        array = newArray;
    }
    
    private int lookup(AnyType x) {
        return indexMap.get(x);
    }
    
    public void update(AnyType old, AnyType notOld) {
        assert invariant() : showHeap();
        
        int index = lookup(old);
        
        if ( index == array.length ) {
            doubleArray();
        }
        
        array[index] = notOld;
        indexMap.put(notOld, index);
        indexMap.remove(old);
        
        if (compare(old, notOld) < 0) {
            percolateDown(index);
        } else {
            percolateUp(index);
        }
        
        assert invariant() : showHeap();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (AnyType e : array) {
            if (e != null) {
                sb.append(e.toString())
                  .append(", ");
            } else {
                sb.append("-,");
            }
        }
        sb.setCharAt(sb.length()-1, ']');
        return sb.toString();
    }

    
    private boolean invariant() {
        boolean gotNull = false;
        
        // Check completeness
        for (AnyType e : array) {
            if (e == null) {
                gotNull = true;
            } else {
                if (gotNull) {
                    return false;
                }
            }
        }
        
        // Check heap invariant
        for (int i = 1; i < currentSize; i++) {
            AnyType e = array[i];
            if (e == null) {
                break;
            }
            
            int rChild = 2*i+1;
            int lChild = 2*i;
            
            if (lChild > currentSize) {
                break;
            }
            
            if (compare(array[lChild],e) <= 0) {
                return false;
            }
            
            if (rChild > currentSize) {
                break;
            }
            
            if (compare(array[rChild],e) <= 0) {
                return false;
            }
            
            
        }
        
        // Check indexMap invariant
        for (int i = 1; i <= currentSize; i++) {
            Integer j = indexMap.get(array[i]);
            if (j == null || j != i) {
                return false;
            }
        }
        
        
        return true;
    }

    private String showHeap(){
        return toString();
    }
}