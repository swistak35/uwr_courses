$prog = "prog_cpp"
$test_dir = ARGV[0].nil? ? "testy" : ARGV[0]

dr = Dir.entries($test_dir)

dr.sort.each do |f|
	if f =~ /^test.*\.in$/
		name = f.split(".")[0..-2].join(".")
		mout = name + ".mout"
		out = name + ".out"
		system("./#{$prog} < #{$test_dir}/#{f} > #{$test_dir}/#{mout}")
		if dr.include? out
			res = `diff -Z #{$test_dir}/#{mout} #{$test_dir}/#{out}`
			if res == ""
				puts "= #{f}\t -> Done, Ok"
			else
				puts "= #{f}\t -> Done, Fail"
			end
		else
			puts "= #{f}\t -> Done"
		end
	end
end
