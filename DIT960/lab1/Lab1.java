
import java.util.Arrays;

public class Lab1 {
    /** Sorting algorithms **/

    /**
     * Insertion sort.
     * @param array 
     */
    public static void insertionSort(int[] array) {
        for (int i = 1; i < array.length; i++) {
            int k = i;
            while (k > 0 && array[k] < array[k-1]) {
                swap(array,k,k-1);
                k--;
            }
        }
    }

    // Quicksort.

    public static void quickSort(int[] array) {
        quickSort(array, 0, array.length - 1);
    }

    // Quicksort part of an array
    private static void quickSort(int[] array, int begin, int end) {
//        System.out.printf("Sorting %s from %d to %d%n",Arrays.toString(array),begin,end);
        // Base case.
        if (begin >= end) return;

        // Partition the array.
        int pivot = partition(array, begin, end);
//        System.out.println("Pivot at: "+pivot);
//        System.out.println("After partition: "+Arrays.toString(array));
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
//        System.out.printf("Part: l:%d h:%d %n",low,high);
        while (low <= high) {
            if (array[low] <= array[begin]) {
                low++;
            } else {
                swap(array,low,high);
                high--;
            }
//            System.out.printf("Part: l:%d h:%d %n",low,high);
        }
        swap(array, low-1, begin);
        return low-1;
    }

    // Swap two elements in an array
    private static void swap(int[] array, int i, int j) {
        int x = array[i];
        array[i] = array[j];
        array[j] = x;
//        System.out.printf("Swap %d with %d %n",i,j);
//        System.out.println("Now: "+Arrays.toString(array));
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
        int nextResult = 0;
        // How far we have got in the left array
        int nextLeft = 0;
        // How far we have got in the right array
        int nextRight = 0;

        // Idea: repeatedly copy one element from either the left or right array to the result array.
        while (nextResult < result.length) {
            
            if (left.length == nextLeft) {
               while (nextRight < right.length) {
                    result[nextResult++] = right[nextRight++]; 
                }
               break;
            }
            
            if (right.length == nextRight) {
               while (nextLeft < left.length) {
                    result[nextResult++] = left[nextLeft++]; 
                }
               break;
            }
            
            if (left[nextLeft] < right[nextRight]) {
                result[nextResult++] = left[nextLeft++];
            } else {
                result[nextResult++] = right[nextRight++];
            }
        }
        return result;
    }
    
    public static void main(String[] args) {
        int[] a = {3,2,1,0};
        System.out.println(Arrays.toString(a));
        mergeSort(a);
        System.out.println(Arrays.toString(a));
        
    }
}

