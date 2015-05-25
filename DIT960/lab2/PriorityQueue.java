
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
 */
public class PriorityQueue 
{
    /**
     * Initial capacity of the underlying array.
     */
    private static final int DEFAULT_CAPACITY = 100;
    
    /**
     * Number of elements currently in the heap.
     */
    private int currentSize;
    
    /**
     * The underlying array holding the heap.
     */
    private Bid[ ] array; 
    
    /**
     * The comparator used for the heap invariant.
     */
    private final Comparator<? super Bid> cmp;
    
    /**
     * A map holding each elements index, to get a O(1) lookup.
     */
    private final Map<Bid,Integer> indexMap = new HashMap<>();
    
 
    
    /**
     * Construct an empty PriorityQueue with a specified comparator.
     * @param c Comparator to compare bids
     */
    
    public PriorityQueue( Comparator<? super Bid> c )
    {
        currentSize = 0;
        cmp = c;
        array = new Bid[ DEFAULT_CAPACITY + 1 ];
    }
    
    
    /**
     * Complexity: O(1)
     * 
     * Compares lhs and rhs using comparator if
     * provided by cmp, or the default comparator.
     * 
     */
    
    private int compare(Bid lhs, Bid rhs)
    {
        return cmp.compare( lhs, rhs );    
    }
    
    /**
     * Adds an item to this PriorityQueue.
     * Complexity O(log n) or worst case O(N) when its a very unbalanced tree.
     * 
     * @param x Bid object.
     * @return true.
     */
    public boolean add( Bid x )
    {
        assert invariant() : showHeap();
        
        if( currentSize + 1 == array.length )
            doubleArray( );

        // Percolate up
        int hole = ++currentSize;
        array[hole] = x;
        percolateUp(hole);  
        
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
     * Returns the smallest item in the priority queue.
     * Complexity O(1)
     * 
     * @return the smallest item.
     * @throws NoSuchElementException if empty.
     */
    public Bid element( )
    {
        if( isEmpty( ) )
            throw new NoSuchElementException( );
        return array[ 1 ];
    }
    
    /**
     * Complexity O(1)
     * 
     * @return 
     */
    public boolean isEmpty() {
        return currentSize == 0;
    }
    
    
    /**
     * Removes the smallest item in the priority queue.
     * Complexity O(log n)
     * 
     * @return the smallest item.
     * @throws NoSuchElementException if empty.
     */
    public Bid remove( )
    {
        assert invariant() : showHeap();
        
        Bid minItem = element( );
        array[ 1 ] = array[ currentSize-- ];
        percolateDown( 1 );
        
        assert invariant() : showHeap();
        return minItem;
    }



    /**
     * Internal method to percolate down in the heap.
     * Complexity O(log n)
     * @param hole the index at which the percolate begins.
     */
    private void percolateDown( int hole )
    {
        int child;
        Bid tmp = array[ hole ];

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
     * Complexity O(log n)
     * Internal method to percolate up in the heap.
     * 
     * @param hole the index at which the percolate begins.
     */
    private void percolateUp( int hole )
    {
        int child;
        Bid tmp = array[ hole ];
        
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
     * Complexity O(n)
     */
    
    private void doubleArray( )
    {
        Bid [ ] newArray;

        newArray = (Bid []) new Object[ array.length * 2 ];
        System.arraycopy(array, 0, newArray, 0, array.length);
        array = newArray;
    }
    
    /**
     * Complexity O(1)
     * @param x
     * @return 
     */
    private int lookup(Bid x) {
        Integer index = indexMap.get(x);
        return index == null ? -1 : index;
    }
    
    
    /**
     * Complexity O(log n)
     * 
     * @param old
     * @param notOld 
     */
    public void update(Bid old, Bid notOld) {
        assert invariant() : showHeap();
        
        int index = lookup(old);
        
        if (index == -1) {
            add(notOld);
            return;
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

    /**
     * Complexity O(n)
     * 
     * @return 
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (Bid e : array) {
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

    /**
     * Complexity O(n)
     * Invariant:
     *   * Completness: All rows are full except the last 
     *                  which is filled from left to right.
     *   * Heap invariant: Every nodes value is less than 
     *                     both its children's value
     *   * Map with index: Every node have a corresponding key in the index map
     *                     and its value is its index in the array.  
     * 
     * 
     * @return 
     */
    private boolean invariant() {
        boolean gotNull = false;
        
        // Check completeness
        for (int i = 1; i <= currentSize; i++) {
            if (array[i] == null) {
                return false;
            }
        }
        
        // Check heap invariant
        for (int i = 1; i < currentSize; i++) {
            Bid e = array[i];
            
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