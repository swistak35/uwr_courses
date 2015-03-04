
puts "Pid: #{Process.pid}"

def fib(x)
  case x
    when 0 then 0
    when 1 then 1
    else fib(x - 1) + fib(x - 2)
  end
end

n = ARGV[0] || 40

puts fib(n.to_i)
