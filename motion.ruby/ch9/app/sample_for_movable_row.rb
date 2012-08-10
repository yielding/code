class SampleForMovableRow < SampleForInsertableRow
  def tableView(tv, canMoveRowAtIndexPath:ip)
    @data_source.size > ip.row + 1
  end

  def tableView(tv, moveRowAtIndexPath:from, toIndexPath:to)
    f = from.row
    t = to.row

    while f < t
      @data_source.insert(f, @data_source.delete_at(f+1))
      f += 1
    end

    while f > t
      @data_source.insert(f, @data_source.delete_at(f-1))
      f -= 1
    end
  end

  def tableView(tv, targetIndexPathForMoveFromRowAtIndexPath:src,
                    toProposedIndexPath:dest)
    @data_source.size > src.row+1 ? dest : src
  end
end
