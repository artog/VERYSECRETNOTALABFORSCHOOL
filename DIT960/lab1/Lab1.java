
public class Lab1 {
    /** Sorting algorithms **/

    /**
     * Insertion sort.
     * @param array 
     */
    public static void insertionSort(int[] array) {
        for (int i = 1; i < array.length; i++) {
            int element = array[i];
            int k = i - 1;
            while (k >= 0 && element < array[k]) {
                array[k+1] = array[k];
                k--;
            }
            array[k+1] = element;
        }
    }
    
    // Quicksort.

    public static void quickSort(int[] array) {
        quickSort(array, 0, array.length - 1);
    }

    // Quicksort part of an array
    private static void quickSort(int[] array, int begin, int end) {
        // Base case.
        if (begin >= end) return;
        
        // Takes the middle element as the pivot element,
        // thus resulting in faster time on sorted arrays.
        // This is also to avoid stackoverflow on the large arrays.
        swap(array, begin, (begin + end) / 2);

        // Partition the array.
        int pivot = partition(array, begin, end);

        // Now recursively quicksort the two partitions.
        quickSort(array, begin, pivot - 1);
        quickSort(array, pivot + 1, end);
    }

    // Partition part of an array, and return the index where the pivot
    // ended up.
    private static int partition(int[] array, int begin, int end) {
        // array[begin] will be the pivot element
        int low = begin+1;
        int high = end;
        int pivot = array[begin];
        
        while(pivot < array[high]){
            high--;
        }
        
        while (low <= high) {
            if (array[low] <= pivot) {
                low++;
            } else {
                swap(array,low,high);
                high--;
            }
        }

        swap(array, low-1, begin);
        return low-1;
    }

    // Swap two elements in an array
    private static void swap(int[] array, int i, int j) {
        int x = array[i];
        array[i] = array[j];
        array[j] = x;
    }

    // Mergesort.

    public static int[] mergeSort(int[] array) {
        return mergeSort(array, 0, array.length - 1);
    }

    // Mergesort part of an array
    private static int[] mergeSort(int[] array, int begin, int end) {
        // Base case: array of length 0 or 1.
        if (begin > end) return new int[0];
        if (begin == end) {
            int[] result = {array[begin]};
            return result;
        }

        // Midpoint of the array
        int mid = (begin+end)/2;

        // Recursively sort both halves of the array,
        // then merge the results.
        
        return merge(
                mergeSort(array,begin,mid), // Left
                mergeSort(array,mid+1,end)    // Right
        );
    }

    // Merge two sorted arrays into one
    private static int[] merge(int[] left, int[] right) {
        // The result array
        int[] result = new int[left.length + right.length];
        // How far we have got in the result array
        int i = 0;
        // How far we have got in the left array
        int j = 0;
        // How far we have got in the right array
        int k = 0;

        // Copy until end of one array
        while (j < left.length && k < right.length) {    
            result[i++] = left[j] < right[k] 
                ? left[j++] 
                : right[k++];
        } 

        // Copy all remaining elements in left 
        while (j < left.length) {
            result[i++] = left[j++]; 
        }
        
        // Copy all remaining elements in right
        while (k < right.length) {
            result[i++] = right[k++]; 
        }
        return result;
    }
}

