
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;


/**
 * PriorityQueue class implemented via the binary heap.
 * @param <AnyType>
 */
public class PriorityQueue<AnyType> 
{
    
    private static final int DEFAULT_CAPACITY = 100;

    private List<AnyType> list; // The heap array
    private final Comparator<? super AnyType> cmp;
    
    private final Map<AnyType,Integer> indexMap = new HashMap<>();
    
    
    /**
     * Construct an empty PriorityQueue with a specified comparator.
     * @param c
     */
    public PriorityQueue( Comparator<? super AnyType> c )
    {
        if (c == null) {
            throw new IllegalArgumentException("Comparator is null.");
        }
        cmp = c;
        list = new ArrayList<>();
    }
    
    /**
     * Compares lhs and rhs using comparator if
     * provided by cmp, or the default comparator.
     */
    private int compare( AnyType lhs, AnyType rhs )
    {
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
        

        // Percolate up
        int hole = list.size();
        list.add(x);
        
        percolateUp(hole);
        
//        for( ; compare( x, list.get(hole / 2)) < 0; hole /= 2 ) {
//            list.set( 
//                hole, 
//                list.get( hole / 2 )
//            ); 
//            
//            indexMap.put(list.get( hole ), hole);
//        }
//        
//        list.set(hole, x);
//        indexMap.put(list.get( hole ), hole);
        
        
        assert invariant() : showHeap();
        return true;
    }
    
    /**
     * Returns the number of items in this PriorityQueue.
     * @return the number of items in this PriorityQueue.
     */
    public int size( )
    {
        return list.size();
    }
    
    /**
     * Make this PriorityQueue empty.
     */
    public void clear( )
    {
        list.clear();
    }
    
     
    /**
     * Returns the smallest item in the priority queue.
     * @return the smallest item.
     * @throws NoSuchElementException if empty.
     */
    public AnyType element( )
    {
        if(isEmpty()) {
            throw new NoSuchElementException();
        }
        return list.get(0);
    }
    
    public boolean isEmpty() {
        return list.isEmpty();
    }
    
    
    /**
     * Removes the smallest item in the priority queue.
     * @return the smallest item.
     * @throws NoSuchElementException if empty.
     */
    public AnyType remove( )
    {
        AnyType minItem = element();
        
        list.set(
            0, 
            list.get(
                list.size()-1
            )
        );
        percolateDown(0);
        
        list.remove(list.size()-1);

        return minItem;
    }


    /**
     * Establish heap order property from an arbitrary
     * arrangement of items. Runs in linear time.
     */
    private void buildHeap( )
    {
        for( int i = (list.size() - 1) / 2; i >= 0; i--) {
            percolateDown(i);
        }
    }


    /**
     * Internal method to percolate down in the heap.
     * @param hole the index at which the percolate begins.
     */
    private void percolateDown( int hole )
    {
        int child;
        AnyType tmp = list.get(hole);
        
        for( ; hole * 2 < list.size()-1; hole = child )
        {
            child = hole * 2 + 1;
            if( child != list.size()-1 &&
                    compare( list.get( child + 1 ), list.get( child ) ) < 0 )
                child++;
            if( compare( list.get( child ), tmp ) < 0 ) {
                list.set( hole ,list.get( child ));
                indexMap.put(list.get( hole ), hole);
                
            } else {
                break;
            }
        }
        list.set( hole ,tmp);
        indexMap.put(list.get( hole ), hole);
    }
    /**
     * Internal method to percolate up in the heap.
     * @param hole the index at which the percolate begins.
     */
    private void percolateUp( int hole )
    {
        int child;
        AnyType tmp = list.get(hole);
        
        while (hole > 0) {
            
            if (compare( tmp, list.get( hole / 2 ) ) < 0) {
                list.set( hole , list.get( hole / 2 ));
                indexMap.put(list.get( hole ), hole);
            } else {
                break;
            }
            
            hole /= 2;
            
        }
        list.set( hole ,tmp);
        indexMap.put(list.get( hole ), hole);
        
    }
    
   
    
    private int lookup(AnyType x) {
        return indexMap.get(x);
    }
    
    public void update(AnyType old, AnyType notOld) {
        assert invariant() : showHeap();
        
        int index = lookup(old);
        
        
        list.set(index , notOld);
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
        for (AnyType e : list) {
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
        for (AnyType e : list) {
            if (e == null) {
                gotNull = true;
            } else {
                if (gotNull) {
                    return false;
                }
            }
        }
        
        // Check heap invariant
        for (int i = 0; i < list.size()-1; i++) {
            AnyType e = list.get(i);
            if (e == null) {
                break;
            }
            
            int rChild = 2*i+2;
            int lChild = 2*i+1;
            
            if (lChild > list.size()-1) {
                break;
            }
            
            if (compare(list.get(lChild),e) < 0) {
                System.err.println("l:"+lChild + " vs "+ i + " == " +compare(list.get(lChild),e));
                return false;
            }
            
            if (rChild > list.size()-1) {
                break;
            }
            
            if (compare(list.get(rChild),e) < 0) {
                System.err.println("r: "+rChild + " vs "+ i + " == " +compare(list.get(rChild),e));
                return false;
            }
            
            
        }
        
        // Check indexMap invariant
        for (int i = 0; i < list.size(); i++) {
            Integer j = indexMap.get(list.get(i));
            if (j == null || j != i) {
                System.err.println("indexMap fail at "+i);
                return false;
            }
        }
        
        
        return true;
    }

    private String showHeap(){
        return toString();
    }
}