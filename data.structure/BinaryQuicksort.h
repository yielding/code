/******************************************************************************
 * File: BinaryQuicksort.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the "binary quicksort" algorithm, another name for MSD-
 * radix sort in base 2.  The algorithm is a non-comparison sort for sorting
 * integers that works by sorting the numbers one bit at a time, starting with
 * the most significant digit and ending with the least-significant digit.
 * Internally, the algorithm works by first sorting the numbers into ascending
 * order by the first bit, then recursively sorting each half of the numbers by
 * their second digit, etc.  For example, given the numbers 0-7 in scrambled
 * order, like this:
 *
 *         3   1   4   0   5   7   2   6
 *
 * We would write these numbers in binary to get
 *
 *       011 001 100 000 101 111 010 110
 *
 * First, we sort the numbers by just their first digit.  For example:
 *
 *       011 001 000 010 100 101 111 110
 *       ^               ^
 *       |               |
 *       |               +--- Numbers beginning with a 1
 *       |
 *       +------------------- Numbers beginning with a 0
 *
 *
 * Notice that we now have the array split into two regions - one region
 * consisting of values beginning with a 0 and one region consisting of values
 * beginning with a 1.  We now recursively descend into these two regions and
 * sort each one by their second digit.  This gives the following:
 *
 *       001 000 011 010 100 101 111 110
 *
 * From here we sort by the third digit:
 *
 *       000 001 010 011 100 101 110 111
 *
 * And the numbers are now in order.
 *
 * In order to convert this sketch of an algorithm into a concrete
 * implementation, we will need to devise a means for sorting a range of
 * numbers by just a single digit.  One option for doing this out-of-place is
 * to build two queues, one for numbers with a 0 in that bit and one for
 * numbers with a 1 in at that bit, then add each number in the range to the
 * appropriate queue.  We can then dequeue the numbers from the 0 queue and the
 * 1 queue, in order, and insert them back into the original array.  For
 * example, given these numbers, which we want to sort by the most significant
 * bit:
 *
 *       011 001 100 000 101 111 010 110
 *
 * We would build two queues and insert the numbers as follows:
 *
 *   Begins with 0: 011 001 000 010
 *   Begins with 1: 100 101 111 110
 *
 * We would then concatenate these two queues and store the result back into
 * the original array:
 *
 *      011 001 000 010 100 101 111 110
 *
 * While this works, it has the drawback that it requires O(n) auxiliary
 * storage.  Fortunately, there is a faster algorithm for solving this problem
 * that works in-place.  The intuition behind the algorithm is that we want to
 * reorder the elements so that the array ends up being split into two regions-
 * a region on the left consisting of all values that contain a 0 at the
 * appropriate bit, and a region on the right consisting of all values that
 * contain a 1 at the appropriate bit.  We can therefore build an algorithm
 * that scans inward from the two sides of the array looking for numbers that
 * violate this invariant, then exchange the numbers that are out of place.
 * For example, suppose we have this sequence of 0s and 1s:
 *
 *        0  1  0  1  0  1  1  0  1
 *
 * We would march two pointers in from the left and right side looking for a
 * point where elements are found that are out of place.  This would first find
 * this pair:
 *
 *        0  1  0  1  0  1  1  0  1
 *           ^                 ^
 *
 * We exchange those numbers, as shown here:
 *
 *        0  0  0  1  0  1  1  1  1
 *           ^                 ^
 *
 * and then march inward to find this next pair:
 *
 *        0  0  0  1  0  1  1  1  1
 *                 ^  ^
 *
 * which we swap to obtain
 *
 *        0  0  0  0  1  1  1  1  1
 *                 ^  ^
 *
 * and at this point are done.  This algorithm is very similar to the partition
 * step found in most quicksort implementations, and for this reason the MSD-
 * radix sort is often called binary quicksort.
 *
 * To analyze the runtime of the algorithm, note that running the partitioning
 * step on k integers takes O(k) time, since each element is visited at most
 * O(1) times.  Initially, this means that we do O(n) work to partition the
 * values by their first bit, then a total of O(n) work to partition each half
 * by the second bit, etc.  More generally, if the largest possible value that
 * can appear is U, there will be O(log U) partioning steps required, one for
 * each of the bits in U.  This means that the total runtime is O(n log U).
 * In cases where U = O(n), this is asympototically better than or comparable
 * to comparison sorts like quicksort or heap sort.
 */

#ifndef BinaryQuicksort_Included
#define BinaryQuicksort_Included

#include <climits>   // For CHAR_BIT
#include <iterator>
#include <limits>
#include <algorithm> // For std::iter_swap, std::rotate, std::find_if

/**
 * Function: BinaryQuicksort(RandomIterator begin, RandomIterator end);
 * Usage: BinaryQuicksort(v.begin(), v.end());
 * ------------------------------------------------------------------------
 * Applies the binary quicksort algorithm to sort the specified list of 
 * numbers.  It is assumes that the iterators are traversing a list of 
 * integral types, and will not function properly otherwise.
 */
template <typename RandomIterator>
void BinaryQuicksort(RandomIterator begin, RandomIterator end);

/* * * * * Implementation Below This Point * * * * */

namespace binaryquicksort_detail {
/* Utility function to partition the elements of a range by moving all 
  * elements in the range having a 0 in a given position to the right and all
  * elements in the range having a 1 in a given position to the left.  The
  * function then returns an iterator to the beginning of the range that
  * contains a 1.
  *
  * This algorithm works by having begin point one step past the end of the
  * range of values known to be 0 and end point at the range of values known
  * to be 1.  The endpoints are then marched inward until they collide (in
  * which case we're done) or a pair of mismatched elements are found.
  */
template <typename RandomIterator>
RandomIterator PartitionAtBit(RandomIterator begin, RandomIterator end, signed int bit) 
{
  /* Typedef defining the type of the elements being traversed. */
  typedef typename std::iterator_traits<RandomIterator>::value_type T;

  /* Compute the bitmask we'll use to test whether the bit is set. */
  const T bitmask = T(1) << bit;

  /* Move these two together until they meet or we find two elements that
    * are out of place.
    */
  while (true) {
    /* Find the first 1 after the 0s; it's either the end or we've just
      * found the element that's out of place.
      */
    while (begin < end && !(*begin & bitmask))
      ++ begin;

    /* If the begin is now sitting atop the end, we're done and all of the
      * remaining values are 1s.  We're therefore done.
      */
    if (begin == end)
      return begin;

    /* Otherwise, the end is just before the elements containing 1s.  Start
      * moving it over until we find a 0 or realize that we've already
      * found everything.
      *
      * Since we need to back up the end iterator before we read it, this
      * loop is written as a do ... while rather than as a while loop.
      */
    do {
      --end;
    } while (begin < end && !!(*end & bitmask));

    /* If the two are equal, we've found the crossover point and are done.
      * We can hand back this element as the pivot point.
      */
    if (begin == end)
      return begin;

    /* Otherwise, swap the two elements and repeat. */
    std::iter_swap(begin, end);
  }
}

  /* Utility function which actually performs the binary quicksort algorithm,
   * beginning with the specified bit.
   */
template <typename RandomIterator>
void BinaryQuicksortAtBit(RandomIterator begin, RandomIterator end, signed int bit) 
{
  /* Typedef defining the type of the elements being traversed. */
  typedef typename std::iterator_traits<RandomIterator>::value_type T;

  /* Borrowing an optimization technique from quicksort, we will have this
   * function work iteratively and recursively.  To avoid having a large
   * number of function calls made, we will iteratively process the larger
   * of the two partitions we find, and will recursively process the other.
   */

  /* If we've processed all the bits, or if the range has fewer than one
   * element in it, we're done.
   */
  while (bit >= 0 && std::distance(begin, end) > 1) {
    /* Apply the partitioning step on this bit and get the start of the 
     * range of values containing the 1s.
     */
    RandomIterator pivot = PartitionAtBit(begin, end, bit);

    /* Drop the index of the bit we're processing; this will cause the next
     * loop iteration to use the right bit and will make the recursive calls
     * correct.
     */
    --bit;

    /* Determine which range is larger - the range holding the 0s or the
     * range holding the 1s.  Based on which is smaller, recursively process
     * one of the ranges.
     */
    if (std::distance(begin, pivot) < std::distance(pivot, end)) {
      /* There are fewer numbers beginning with 0; go recursively sort
       * them.
       */
      BinaryQuicksortAtBit(begin, pivot, bit);
      begin = pivot;
    } else {
      /* There are fewer numbers beginning with 1; go recursively sort
       * them.
       */
      BinaryQuicksortAtBit(pivot, end, bit);
      end = pivot;
    }
  }
}

/* If we are dealing with signed numbers, then negative numbers will
  * incorrectly appear at the end of the range rather than the start, since
  * the signed two's-complement representation will cause the sign bit to
  * be set, making the negative values appear larger than positive values.
  * This function applies a rotation to the final array to pull the negative
  * values (if any) to the front.
  */
template <typename RandomIterator>
void RotateNegativeValues(RandomIterator begin, RandomIterator end) 
{
  /* Typedef defining the type of the elements being traversed. */
  typedef typename std::iterator_traits<RandomIterator>::value_type T;

  /* Walk forward until we find a negative value.  If we find one, do a
   * rotate to rectify the elements.
   */
  for (RandomIterator itr = begin; itr != end; ++itr) {
    /* If the value is negative, do a rotate starting here. */
    if (*itr < T(0)) {
      std::rotate(begin, itr, end);
      return;
    }
  }
}

}

/* Actual implementation of binary quicksort. */
template <typename RandomIterator>
void BinaryQuicksort(RandomIterator begin, RandomIterator end)
{
  /* Typedef defining the type of the elements being traversed. */
  typedef typename std::iterator_traits<RandomIterator>::value_type T;

  /* Find out how many bits we need to process. */
  const signed int kNumBits = (signed int)(CHAR_BIT * sizeof(T));

  /* Run binary quicksort on the elements, starting with the MSD. */
  binaryquicksort_detail::BinaryQuicksortAtBit(begin, end, kNumBits - 1);

  /* If the numbers are signed, we need to do a rotate to pull all of the
   * negative numbers to the front of the range, since otherwise (because
   * their MSB is set) they'll be at the end instead of the front.
   */
  if (std::numeric_limits<T>::is_signed)
    binaryquicksort_detail::RotateNegativeValues(begin, end);
}

#endif
