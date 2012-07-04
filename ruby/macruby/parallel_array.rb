framework "Foundation"

class Array
  def parallel_map(&block)
    result = []
    group = Dispatch::Group.new
    result_queue = Dispatch::Queue.new('access-queue.#{result.object_id}')
    0.upto(self.size) do |idx|
      Dispatch::Queue.concurrent.async(group) do
        temp = block[self[idx]]
        result_queue.async(group) { result[idx] = temp }
      end
    end

    group.wait
    result
  end
end
