#!/usr/bin/env ruby

require 'benchmark'

TESTS = "tests"

SMALLER = [
  'empty',
  'onlychar',
  'onechar',
  'spacja',
  'dwaslowa',
  'jednozdanie',
  'krotkie',
  '253',
  '254',
  '255',
  '256',
  '257',
  '258',
  '1022',
  '1023',
  '1024',
  '1025',
  '1026',
  '2048',
  'minipantadeusz',
]

BIGGER = [
  'pantadeusz'
]

return unless system("make compress-lexi")
return unless system("make compress-suffix")
return unless system("make decompress0")
return unless system("make decompress1")

def compress_lexi(path, chunk_size = nil)
  result = nil
  timing = Benchmark.realtime do
    result = `./compress-lexi #{path}.input #{path}.compressed #{chunk_size.to_s}`
  end

  timing = (timing * 1000).to_i
  unless $?.success?
    puts "Runtime!"
    puts result
    return false
  end

  input_file = File.size("#{path}.input")
  compressed_file = File.size("#{path}.compressed")
  if input_file == 0
    efficiency = 100
  else
    efficiency = ((compressed_file.to_f / input_file) * 100).to_i
  end

  print timing.to_s.ljust(8)
  if efficiency >= 100
    print "no".ljust(4)
  else
    print "#{efficiency}%".ljust(4)
  end
  true
end

def compress_suffix(path, chunk_size = nil)
  result = nil
  timing = Benchmark.realtime do
    result = `./compress-suffix #{path}.input #{path}.compressed #{chunk_size.to_s}`
  end

  timing = (timing * 1000).to_i
  unless $?.success?
    puts "Runtime!"
    puts result
    return false
  end

  input_file = File.size("#{path}.input")
  compressed_file = File.size("#{path}.compressed")
  if input_file == 0
    efficiency = 100
  else
    efficiency = ((compressed_file.to_f / input_file) * 100).to_i
  end

  print timing.to_s.ljust(8)
  if efficiency >= 100
    print "no".ljust(4)
  else
    print "#{efficiency}%".ljust(4)
  end
  true
end

def decompress0(path, chunk_size = nil)
  result = nil
  timing = Benchmark.realtime do
    result = `./decompress0 #{path}.compressed #{path}.decompressed #{chunk_size.to_s}`
  end

  timing = (timing * 1000).to_i
  unless $?.success?
    puts "Runtime!"
    puts result
    return false
  end

  result = `diff #{path}.input #{path}.decompressed`
  unless $?.success?
    puts "Diff!"
    puts result
    return false
  end

  print timing.to_s.ljust(8)
  true
end

def decompress1(path, chunk_size = nil)
  result = nil
  timing = Benchmark.realtime do
    result = `./decompress1 #{path}.compressed #{path}.decompressed #{chunk_size.to_s}`
  end

  timing = (timing * 1000).to_i
  unless $?.success?
    puts "Runtime!"
    puts result
    return false
  end

  result = `diff #{path}.input #{path}.decompressed`
  unless $?.success?
    puts "Diff!"
    puts result
    return false
  end

  print timing.to_s.ljust(8)
  true
end

print "Informacje o teście".ljust(32)
print "Lexi (256)".ljust(36)
print "Lexi (1024)".ljust(36)
print "Suffix (256)".ljust(36)
print "Suffix (1024)".ljust(36)
puts

print "Nazwa".ljust(16)
print "Rozmiar".ljust(16)
4.times do
  print "comp.".ljust(8)
  print "eff.".ljust(4)
  print "decomp.".ljust(8)
end
puts

SMALLER.each do |name|
  path = File.join(TESTS, name)

  print name.ljust(16)
  print File.size("#{path}.input").to_s.ljust(16)

  next unless compress_lexi(path)
  next unless decompress0(path)
  next unless compress_lexi(path, 1024)
  next unless decompress0(path, 1024)
  next unless compress_suffix(path)
  next unless decompress1(path)

  puts
end

puts "\nWszystko OK!"
