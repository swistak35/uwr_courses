
@m = 10
@n = 100000

@fill = "7"

@line = @fill * (@n+1)
@line = @line + "\n"

File.open("maxtest.in", 'w') do |f|
	f.write("#{@m} #{@n}\n")
	(@m+1).times do
		f.write(@line)
	end
end
