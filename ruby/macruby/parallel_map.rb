class Array
  def parallel_map(&block)
    result = []
    # Creating a group to synchronize block execution.
    group = Dispatch::Group.new
    # We will access the `result` array from within this serial queue,
    # as without a GIL we cannot assume array access to be thread-safe.
    result_queue = Dispatch::Queue.new('access-queue.#{result.object_id}')
    0.upto(size) do |idx|
      # Dispatch a task to the default concurrent queue.
      Dispatch::Queue.concurrent.async(group) do
        temp = block[self[idx]]
        result_queue.async(group) { result[idx] = temp }
      end
    end
    # Wait for all the blocks to finish.
    group.wait
    result
  end
end

[1, 2, 3, 4, 5].parallel_map { |x| puts x }