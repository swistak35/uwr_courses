
def fib(x)
  case x
    when 0 then 0
    when 1 then 1
    else fib(x - 1) + fib(x - 2)
  end
end

puts "1 PID: #{Process.pid}"

fork

puts "2 PID: #{Process.pid}"

t = Thread.new do
  puts fib(40)
end.join
