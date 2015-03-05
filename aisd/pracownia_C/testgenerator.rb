k = rand(10) + 5
l = rand(30) + k*2

puts "#{k} #{l}"
l.times do
	x = rand(30) + 1
	x = 80 if x >= 29
	print "#{x} "
end
