@links = [
  ["e", "f"],
  ["c", "b"],
  ["c", "f"],
  ["d", "e"],
  ["c", "e"],
  ["a", "b"],
]

Network = Struct.new(:name, :nodes)

@networks = [
  Network.new("s", ["a", "b"]),
]

# @nodes = (@networks + @links).flatten.uniq
@nodes = @links.flatten.uniq

routing_table = {}

@nodes.each do |node|
  routing_table[node] = {}
end

@links.each do |x1, x2|
  routing_table[x1][x2] = [1, "-"]
  routing_table[x2][x1] = [1, "-"]
end

# @networks.each do |network|
#   network.nodes.each do |node|
#     routing_table[node][network.name] = [1, "-"]
#   end
# end

puts routing_table.inspect

def update_link_node(source, target, rt, old_rt)
  old_rt[source].each do |source_target_node, source_target|
    source_target_dist, source_target_paths = source_target

    if old_rt[target].keys.include?(source_target_node)
      old_dist, old_paths = old_rt[target][source_target_node]
      if old_dist > source_target_dist + 1
        rt[target][source_target_node] = [source_target_dist + 1, [source]]
      elsif old_dist == source_target_dist + 1
        old_paths.push(source)
        old_paths.uniq!
      end
    else
      # puts rt.inspect
      # puts target.inspect
      # puts source_target_node.inspect
      # puts rt[target][source_target_node].inspect
      rt[target][source_target_node] = [source_target_dist + 1, [source]]
    end
  end

end

def update_table(old_rt)
  new_rt = Marshal.load( Marshal.dump(old_rt) )

  @nodes.each do |node|
    links_connected = @links.select {|link| link.include?(node) }
    link_connected_nodes = links_connected.flatten - [node]
    link_connected_nodes.each do |target_node|
      update_link_node(node, target_node, new_rt, old_rt)
    end

    # networks_connected = networks.filter {|network| network.nodes.include?(node) }
  end

  return new_rt
end


def display_rt(rt)
  puts "----------------------------------------------------------------------"
  rt.each do |node, node_rt|
    print "Node '#{node}': ["
    node_rt.each do |target, vals|
      print "("
      print target
      print " -> "
      print "("
      print vals.first
      print ","
      if vals.last.is_a?(Array)
        print vals.last.join("|")
      else
        print vals.last
      end
      print ")"
      print ")"
      print " "
    end
    puts "]"
  end
end

display_rt(routing_table)

rt2 = update_table(routing_table)
display_rt(rt2)

rt3 = update_table(rt2)
display_rt(rt3)

rt4 = update_table(rt3)
display_rt(rt4)

rt5 = update_table(rt4)
display_rt(rt5)

@links.push(["a", "d"])
rt5["a"]["d"] = [1, "-"]
rt5["d"]["a"] = [1, "-"]

rt6 = update_table(rt5)
display_rt(rt6)

rt7 = update_table(rt6)
display_rt(rt7)
